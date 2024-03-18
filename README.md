# Knox HSMs [![Build Status](https://github.com/anishathalye/knox-hsm/workflows/CI/badge.svg)](https://github.com/anishathalye/knox-hsm/actions?query=workflow%3ACI)

Circuits and hardware security modules (HSMs) formally verified with [Knox].

![TOTP token demo, running on iCEBreaker FPGA](https://raw.githubusercontent.com/anishathalye/assets/master/knox-hsm/demo.png)

## HSMs

This repository contains both simple examples, for explanatory purposes, and
the case studies from the [Knox OSDI'22 paper][paper].

### Case studies from the paper

All of these use the PicoRV32 RISC-V CPU and C/assembly code running on top.
Some add hardware cryptographic accelerators, and some have crypto code in
C/assembly as well. All communicate over UART.

- **pin-protected-backup**: the PIN-protected backup HSM. It stores multiple
  secret/pins and enforces a guess limit for each.
- **password-hasher**: the password-hashing HSM.
- **otp**: the TOTP token.

### Simpler examples

- **counter**: a simple circuit that implements a saturating counter.
  Functional correctness ensures that the hardware implements saturation
  correctly, and physical equivalence (security) ensures that the circuit,
  among other things, doesn't leak past inputs to the counter, only revealing
  the current running total.
- **counter-picorv32**: a circuit that implements the same exact spec as the
  saturating counter above but using C code running on a CPU. The host-HSM
  interface is also different (UART), so the driver is different than the
  simple counter circuit. This example is the simplest example in this repo of
  an HSM with a CPU.
- **adder**: a simple circuit that adds two numbers together. A driver sends
  one number at a time, yielding in between, demonstrating support for
  nondeterminism in Knox. Functional correctness ensures that the numbers are
  added together correctly, and physical equivalence ensures that
  outputs/timing don't reveal information about past inputs.
- **lockbox**: a simplified pure-Verilog implementation of PIN-protected
  backup, with a simple I/O interface. Functional correctness ensures that the
  lockbox implements store/retrieve correctly, and physical equivalence ensures
  that the lockbox protects against brute-force attacks on the PIN and doesn't
  leak information through timing, among other properties.
- **multi-lockbox-leak**: a lockbox that can store multiple pin/secret pairs in
  slots. This example demonstrates Knox's support for a leakage specification:
  the lockbox leaks which slots are valid, and for valid slots, the tags of
  those slots (but not the pin/secret). The functional correctness proof
  requires hints.
- **multi-lockbox-pad**: an example that shares the driver and specification of
  multi-lockbox-leak, but instead of leaking information through timing, this
  example uses a constant-time implementation, so we don't need a leakage
  specification. Unlike the above example, this doesn't require hints for the
  functional correctness proof.
- **fifo1**: a 1-element FIFO. Functional correctness ensures that push/pop
  work correctly, and physical equivalence ensures that previously stored data
  is not leaked.
- **fifo**: a 3-element FIFO implemented using a circular buffer. Functional
  correctness ensures that push/peek/pop work correctly, and physical
  equivalence ensures that previously stored data is not leaked by the
  interface, even though the circular buffer does not zero out old data.

## Organization and use

Each HSM follows a similar organization. Some of the neat things to look at
include the specs (`spec/spec.rkt`) and emulators (`proof/emulator.rkt`).

- `impl/`: contains the implementation. For the simple examples, it includes
  only Verilog code. For the case studies from the paper, this includes both
  hardware (`hw/`) and software/firmware (`fw/`). For the case studies, running
  `make` will compile the code into a memory image.
- `spec/`: contains the specification.
    - `spec.rkt`: the functional specification for the HSM.
    - `driver.rkt`: the driver for the HSM.
    - In some cases, e.g., for the TOTP token, this directory will have spec
      code split into multiple files.
- `proof/`: contains the proof.
    - Run `make` in here to build a `.rkt` file with a representation of the
      circuit.
    - `circuit.rkt` describes some extra information about the circuit, like
      which state is persistent and what is the reset line.
    - `shared.rkt`: contains the refinement relation `R`, among other things.
      Sometimes includes an abstraction function (`AbsF`) and sometimes
      contains an invariant (`I`).
    - `emulator.rkt`: the emulator; physical equivalence proofs in Knox are
      constructive.
    - `correctness.rkt`: the correctness proof; run this with
      `racket correctness.rkt`. For development, there are some keyword
      arguments that can be specified at the top of the file to speed things
      up, like checking only a single method, disabling crash-safety
      verification, or disabling nondeterminism in the driver.
    - `security.rkt`: the security proof; run this with `racket security.rkt`.
      For development, adding the `#:skip-final-check #t` keyword at the top
      will enable debugging security proofs in the Racket REPL.
- `client/`: contains client libraries (only for the three case studies from
  the paper).

## Docker image

We provide a [Docker image] that includes all the dependencies. You can
download it with `docker pull anishathalye/knox`.

To mount the repository on `/knox-hsm` and get a shell in the Docker image,
run:

```bash
docker run -it --rm -v "${PWD}/:/knox-hsm" -w /knox-hsm anishathalye/knox
```

## Dependencies

If you want to install the dependencies locally, here is what you need:

- [RISC-V compiler toolchain]
- [Yosys]
- [bin2coe]
- [Racket]
- [Rosette]
- [Knox]: our verification framework

If you're trying to run this on an iCE40 FPGA, you also need the following:

- [icestorm]
- [nextpnr-ice40]

If you want to run the client libraries, you need:

- [pySerial]

## Artifacts

The proofs in this repo reason about post-compilation / post-synthesis HSMs: we
compile the C/assembly code using GCC, "inline" the results into the circuit,
and run the Yosys synthesis tool to produce the object that we reason about.
The proofs have hard-coded references to specific memory addresses, register
numbers, and circuit state elements. GCC and Yosys don't necessarily produce
identical output between different versions, so different versions of these
tools could produce output incompatible with our proofs.

We used the following tools (which are included in our Docker image):

```console
$ riscv64-linux-gnu-gcc --version
riscv64-linux-gnu-gcc (Ubuntu 11.2.0-16ubuntu1) 11.2.0

$ yosys --version
Yosys 0.19+20 (git sha1 a82eff2e2, clang 14.0.0-1ubuntu1 -fPIC -Os)
```

We also have an [archive][knox-hsm-artifacts] containing the outputs from the
compiler (`.elf` and `.lst` files) and output of the synthesis tool (`.rkt`
file), so you can run verification on these without having GCC or Yosys
installed.

## Running on an FPGA

For the case studies from the paper, you can run them on a Lattice iCE40 FPGA
We tested on a [1BitSquared iCEBreaker][iCEBreaker], so the pin configuration
is set up for that.

To install the HSM onto the FPGA, go into the `impl/` directory and run `make
prog`. If you just want to compile the bitstream but not actually flash the
FPGA, you can run `make fpga.bin`.

### I/O pin configuration

If you're using a 3.3v FTDI cable to communicate with the FPGA, this will be
helpful:

- FTDI Black (GND) <-> PMOD1A GND
- FTDI Brown (CTS input) <-> PMOD1A Pin 4, P1A4, IO line 45
- FTDI Orange (TxD output) <-> PMOD1A Pin 3, P1A3, IO line 47
- FTDI Yellow (RxD input) <-> PMOD1A Pin 2, P1A2, IO line 2
- FTDI Green (RTS output) <-> PMOD1A Pin 1, P1A1, IO line 4

## Citation

```bibtex
@inproceedings{knox:osdi22,
    author =    {Anish Athalye and M. Frans Kaashoek and Nickolai Zeldovich},
    title =     {Verifying Hardware Security Modules with Information-Preserving Refinement},
    month =     {jul},
    year =      {2022},
    booktitle = {Proceedings of the 16th USENIX Symposium on Operating Systems Design and Implementation~(OSDI)},
    address =   {Carlsbad, CA},
}
```

[Knox]: https://github.com/anishathalye/knox
[paper]: https://pdos.csail.mit.edu/papers/knox:osdi22.pdf
[RISC-V compiler toolchain]: https://github.com/riscv/riscv-gnu-toolchain
[Yosys]: https://github.com/YosysHQ/yosys
[Racket]: https://racket-lang.org/
[Rosette]: https://github.com/emina/rosette
[bin2coe]: https://github.com/anishathalye/bin2coe
[icestorm]: https://github.com/YosysHQ/icestorm
[nextpnr-ice40]: https://github.com/YosysHQ/nextpnr
[iCEBreaker]: https://1bitsquared.com/products/icebreaker
[Docker image]: https://hub.docker.com/repository/docker/anishathalye/knox
[knox-hsm-artifacts]: https://github.com/anishathalye/knox-hsm/releases/download/v1.0.1/knox-hsm-artifacts.tar.gz
[pySerial]: https://github.com/pyserial/pyserial
