# Pong

This repository implements Pong in Haskell, both as a software program
(using SDL for input and graphics) and as a hardware FPGA design via
Clash.

## Building

Use `stack build` to build and `stack run pong` to run the software
version.

For the hardware version, use the included Shakefile. An easy way to
run it is via the provided `mk` shell script. Create a `build.mk` file
with the following content:

    TARGET = nexys-a7-50t
    VIVADO_ROOT = /opt/somewhere/where/vivado/is/installed
    ISE_ROOT = /opt/somewhere/where/ise/is/installed
    
Alternatively, if you have Vivado/ISE installed in Docker or similar, you
can create a wrapper script and use that by setting `VIVADO` instead
of `VIVADO_ROOT`:

    TARGET = nexys-a7-50t
    VIVADO = /usr/local/lib/docker-scripts/xilinx-vivado-2019.1-ubuntu-18.04/run
    ISE = /usr/local/lib/docker-scripts/xilinx-ise-14.7-ubuntu-12.04/run

## Supported target boards

Currently, the supported targets are the **Nexys A7-50T**, a Xilinx
7-series FPGA based dev board, the **Papilio One** with a Xilinx
Spartan 3, and the **Papilio Pro** with a Xilinx Spartan 6.  Adding
support to other Vivado or ISE based FPGA dev boards is very
straightforward with the included Shake rules, as long as they have
VGA output and at least two input pushbuttons.

Targeting other FPGA toolchains will require adding support in the
Shake rules. Alternatively, you can always just run Clash, and import
the resulting Verilog files into your FPGA toolchain in a
non-automated way.
