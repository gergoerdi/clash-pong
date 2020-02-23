set -xe

BUILD_DIR=../_build
SYN_DIR=$BUILD_DIR/clash-syn/verilog
VERILOG_MODULE=Pong
VERILOG_DIR=$SYN_DIR/Pong/Pong
VERILOG_SRC=$VERILOG_MODULE.v

VERILATOR_FLAGS="-CFLAGS '-O3 -fPIC' -Wno-fatal --prefix VSim"

verilator $VERILATOR_FLAGS -cc --clk CLK_25MHZ -y "$VERILOG_DIR" -Mdir _verilator $VERILOG_SRC --exe DummyMain.cpp
(cd _verilator && make -f VSim.mk)
