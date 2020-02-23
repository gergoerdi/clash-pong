CPPFLAGS	= $(shell pkg-config --cflags verilator) -I_verilator
CXXFLAGS	= -fPIC -O3
VERILATOR_OBJS	= _verilator/verilated.o
VERILATOR_LIBS	= _verilator/VSim__ALL.a
OUT_DIR		= ../_build/verilator

all: $(OUT_DIR)/libVerilatorFFI.a $(OUT_DIR)/SimMain

clean:
	rm -rf $(OUT_DIR)

.PHONY: all clean

$(OUT_DIR)/VerilatorFFI.o: VerilatorFFI.cpp VerilatorFFI.h VerilatorAPI.h

$(OUT_DIR)/libVerilatorFFI.a: $(OUT_DIR)/VerilatorFFI.o $(VERILATOR_OBJS) $(VERILATOR_LIBS)
	rm -f $@
	mkdir -p $(OUT_DIR)
	ar rcsT $@ $^

$(OUT_DIR)/SimMain: $(OUT_DIR)/SimMain.o $(OUT_DIR)/libVerilatorFFI.a
	$(CXX) -o $@ $^

$(OUT_DIR)/%.o : %.cpp
	mkdir -p $(OUT_DIR)
	$(COMPILE.cc) $(OUTPUT_OPTION) $<

