VERILOG_SRCS	= ../_build/clash-syn/verilog/Pong/Pong/Pong.v

OUT_DIR		= ../_build/verilator
VERILATOR_HDRS	= $(OUT_DIR)/VSim.h
VERILATOR_LIB	= $(OUT_DIR)/VSim__ALL.a
VERILATOR_OBJS  = $(OUT_DIR)/verilated.o

CPPFLAGS	= $(shell pkg-config --cflags verilator) -I$(OUT_DIR)
CXXFLAGS	= -fPIC -O3
VERILATOR_FLAGS	= -CFLAGS '-O3 -fPIC' -Wno-fatal --prefix VSim

all: $(OUT_DIR)/libVerilatorFFI.a $(OUT_DIR)/SimMain

clean:
	rm -rf $(OUT_DIR)

.PHONY: all clean

$(OUT_DIR)/VSim.mk: $(VERILOG_SRCS)
	verilator $(VERILATOR_FLAGS) --cc -Mdir $(OUT_DIR) $(VERILOG_SRCS)

$(OUT_DIR)/verilated.o: $(shell pkg-config --variable=includedir verilator)/verilated.cpp
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

$(VERILATOR_LIB) $(VERILATOR_HDRS): $(OUT_DIR)/VSim.mk 
	cd $(OUT_DIR) && make -f VSim.mk

$(OUT_DIR)/VerilatorFFI.o: VerilatorFFI.cpp VerilatorFFI.h VerilatorAPI.h $(VERILATOR_HDRS)

$(OUT_DIR)/libVerilatorFFI.a: $(OUT_DIR)/VerilatorFFI.o $(VERILATOR_LIB) $(VERILATOR_OBJS)
	rm -f $@
	mkdir -p $(OUT_DIR)
	ar rcsT $@ $^

$(OUT_DIR)/SimMain: $(OUT_DIR)/SimMain.o $(OUT_DIR)/libVerilatorFFI.a
	$(CXX) -o $@ $^

$(OUT_DIR)/%.o : %.cpp
	mkdir -p $(OUT_DIR)
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

