#include "VSim.h"
#include "verilated.h"
#include "VerilatorFFI.h"

#include <iostream>

vluint64_t main_time = 0;

double sc_time_stamp ()
{
    return main_time;
}

VSim* vinit()
{
    // Verilated::commandArgs(0, 0);
    return new VSim();
}

void vshutdown(VSim *top)
{
    delete top;
}

void vstep(VSim* top, const INPUT* input, OUTPUT* output)
{
    top->RESET = input->RESET;
    top->SWITCHES = input->SWITCHES;
    top->BTN_UP = input->BTN_UP;
    top->BTN_DOWN = input->BTN_DOWN;

    top->CLK_25MHZ = true;
    top->eval();
    main_time++;
    top->CLK_25MHZ = false;
    top->eval();
    main_time++;

    output->VGA_HSYNC = top->VGA_HSYNC;
    output->VGA_VSYNC = top->VGA_VSYNC;
    output->VGA_DE = top->VGA_DE;
    output->VGA_RED = top->VGA_RED;
    output->VGA_GREEN = top->VGA_GREEN;
    output->VGA_BLUE = top->VGA_BLUE;
}
