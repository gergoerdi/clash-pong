#include "VSim.h"
#include "verilated.h"
#include "VerilatorAPI.h"

extern "C" {
    VSim* vinit();
    void vstep(VSim* top, const INPUT* input, OUTPUT* output);
    void vshutdown(VSim* top);
}
