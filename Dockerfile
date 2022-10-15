FROM ubuntu:22.04 AS base

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    gcc-riscv64-linux-gnu \
    libboost-filesystem1.74.0 \
    libboost-iostreams1.74.0 \
    libboost-program-options1.74.0 \
    libboost-thread1.74.0 \
    libftdi1 \
    libpython3.10 \
    libssl-dev \
    make \
    python3-pip \
    python3.10 \
    wget \
    && rm -rf /var/lib/apt/lists/*

FROM base AS build-deps

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    bison \
    build-essential \
    clang \
    cmake \
    flex \
    git \
    libboost-filesystem1.74-dev \
    libboost-iostreams1.74-dev \
    libboost-program-options1.74-dev \
    libboost-thread1.74-dev \
    libeigen3-dev \
    libftdi-dev \
    pkg-config \
    python3.10-dev

FROM build-deps AS build-yosys

RUN git clone -b yosys-0.21 https://github.com/YosysHQ/yosys.git \
    && cd yosys \
    && echo >>Makefile.conf "ENABLE_TCL := 0" \
    && echo >>Makefile.conf "ENABLE_GLOB := 0" \
    && echo >>Makefile.conf "ENABLE_PLUGINS := 0" \
    && echo >>Makefile.conf "ENABLE_READLINE := 0" \
    && echo >>Makefile.conf "ENABLE_COVER := 0" \
    && echo >>Makefile.conf "ENABLE_ZLIB := 0" \
    && make -j$(nproc) \
    && make install

FROM build-deps AS build-icestorm-nextpnr

RUN git clone -n https://github.com/YosysHQ/icestorm.git \
    && cd icestorm \
    && git checkout 2bc541743ada3542c6da36a50e66303b9cbd2059 \
    && make -j$(nproc) \
    && make install

RUN git clone -n https://github.com/YosysHQ/nextpnr.git \
    && cd nextpnr \
    && git checkout 664cec54b92844745e21a4e86dcf8e3cca09d781 \
    && cmake . -DARCH=ice40 -DICESTORM_INSTALL_PREFIX=/usr/local \
    && make -j$(nproc) \
    && make install

FROM base

RUN wget https://download.racket-lang.org/installers/8.5/racket-8.5-x86_64-linux-cs.sh \
    && sh racket-8.5-x86_64-linux-cs.sh --create-dir --unix-style --dest /usr/ \
    && rm racket-8.5-x86_64-linux-cs.sh

RUN pip3 install bin2coe

RUN pip3 install pyserial

RUN raco pkg install --no-docs --batch --auto --checksum v1.0.6 https://github.com/anishathalye/knox.git

COPY --from=build-yosys /usr/local/bin/* /usr/local/bin/
COPY --from=build-yosys /usr/local/share/yosys/ /usr/local/share/yosys/
COPY --from=build-icestorm-nextpnr /usr/local/bin/* /usr/local/bin/
COPY --from=build-icestorm-nextpnr /usr/local/share/icebox/ /usr/local/share/icebox/

WORKDIR /
