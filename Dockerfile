FROM ubuntu:22.04

WORKDIR /tmp

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    bison \
    build-essential \
    clang \
    cmake \
    flex \
    gcc-riscv64-linux-gnu \
    git \
    libboost-dev \
    libboost-dev \
    libboost-filesystem-dev \
    libboost-iostreams-dev \
    libboost-program-options-dev \
    libboost-thread-dev \
    libeigen3-dev \
    libftdi-dev \
    libssl-dev \
    make \
    pkg-config \
    python3-dev \
    python3-pip \
    wget \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install bin2coe

RUN wget https://download.racket-lang.org/installers/8.5/racket-8.5-x86_64-linux-cs.sh \
    && sh racket-8.5-x86_64-linux-cs.sh --create-dir --unix-style --dest /usr/ \
    && rm racket-8.5-x86_64-linux-cs.sh

RUN git clone https://github.com/YosysHQ/yosys.git \
    && cd yosys \
    && echo >>Makefile.conf "ENABLE_TCL := 0" \
    && echo >>Makefile.conf "ENABLE_GLOB := 0" \
    && echo >>Makefile.conf "ENABLE_PLUGINS := 0" \
    && echo >>Makefile.conf "ENABLE_READLINE := 0" \
    && echo >>Makefile.conf "ENABLE_COVER := 0" \
    && echo >>Makefile.conf "ENABLE_ZLIB := 0" \
    && make -j$(nproc) \
    && make install \
    && cd /tmp \
    && rm -rf yosys

RUN git clone https://github.com/YosysHQ/icestorm.git \
    && cd icestorm \
    && make -j$(nproc) \
    && make install \
    && cd /tmp \
    && rm -rf icestorm

RUN git clone https://github.com/YosysHQ/nextpnr.git \
    && cd nextpnr \
    && cmake . -DARCH=ice40 -DICESTORM_INSTALL_PREFIX=/usr/local \
    && make -j$(nproc) \
    && make install \
    && cd /tmp \
    && rm -rf nextpnr

RUN raco pkg install --no-docs --batch --auto https://github.com/anishathalye/knox.git

WORKDIR /
