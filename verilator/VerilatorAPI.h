#include <stdint.h>

typedef int Bool;
typedef Bool Bit;

typedef struct
{
    Bool RESET;
    uint8_t SWITCHES;
    Bit BTN_UP;
    Bit BTN_DOWN;
} INPUT;

typedef struct
{
    Bit VGA_HSYNC;
    Bit VGA_VSYNC;
    Bool VGA_DE;
    uint8_t VGA_RED;
    uint8_t VGA_GREEN;
    uint8_t VGA_BLUE;
} OUTPUT;
