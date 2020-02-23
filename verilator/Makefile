CPPFLAGS	= $(shell pkg-config --cflags verilator) -I_verilator
CXXFLAGS	= -fPIC -O3
VERILATOR_FLAGS	= -CFLAGS '-O3 -fPIC' -Wno-fatal --prefix VSim

VERILOG_SRCS	= ../_build/clash-syn/verilog/Pong/Pong/Pong.v

VERILATOR_DIR	= _verilator
VERILATOR_HDRS	= $(VERILATOR_DIR)/VSim.h
VERILATOR_OBJS	= $(VERILATOR_DIR)/verilated.o $(VERILATOR_DIR)/VSim__ALL.a

OUT_DIR		= ../_build/verilator

all: $(OUT_DIR)/libVerilatorFFI.a $(OUT_DIR)/SimMain

clean:
	rm -rf $(VERILATOR_DIR) $(OUT_DIR)

.PHONY: all clean

$(VERILATOR_DIR)/VSim.mk: $(VERILOG_SRCS)
	verilator $(VERILATOR_FLAGS) --cc -Mdir $(VERILATOR_DIR) --exe DummyMain.cpp $(VERILOG_SRCS)

$(VERILATOR_OBJS) $(VERILATOR_HDRS): $(VERILATOR_DIR)/VSim.mk
	cd $(VERILATOR_DIR) && make -f VSim.mk

$(OUT_DIR)/VerilatorFFI.o: VerilatorFFI.cpp VerilatorFFI.h VerilatorAPI.h $(VERILATOR_HDRS)

$(OUT_DIR)/libVerilatorFFI.a: $(OUT_DIR)/VerilatorFFI.o $(VERILATOR_OBJS)
	rm -f $@
	mkdir -p $(OUT_DIR)
	ar rcsT $@ $^

$(OUT_DIR)/SimMain: $(OUT_DIR)/SimMain.o $(OUT_DIR)/libVerilatorFFI.a
	$(CXX) -o $@ $^

$(OUT_DIR)/%.o : %.cpp
	mkdir -p $(OUT_DIR)
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

