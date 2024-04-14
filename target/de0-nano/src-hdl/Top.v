module Top(
           input wire         CLOCK_50,
           input wire [1:0]   KEY,
           output wire [33:0] GPIO_1
           );

    wire            CLK_25MHZ;
    wire            CLK_LOCKED;
    wire            VGA_HS;
    wire            VGA_VS;
    wire [7:0]      VGA_RED_FULL;
    wire [7:0]      VGA_GREEN_FULL;
    wire [7:0]      VGA_BLUE_FULL;


    assign GPIO_1[0]     = VGA_VS;
    assign GPIO_1[1]     = VGA_HS;
    assign GPIO_1[5:2]   = VGA_RED_FULL[7:4];
    assign GPIO_1[9:6]   = VGA_GREEN_FULL[7:4];
    assign GPIO_1[13:10] = VGA_BLUE_FULL[7:4];

    Clock25 u_Clock25
    (.inclk0(CLOCK_50),
        .c0(CLK_25MHZ),
        .locked(CLK_LOCKED)
    );

    topEntity u_topEntity
    (.CLK_25MHZ(CLK_25MHZ),
        .RESET(!CLK_LOCKED),
        .BTN_UP(!KEY[0]),
        .BTN_DOWN(!KEY[1]),
        .VGA_HSYNC(VGA_HS),
        .VGA_VSYNC(VGA_VS),
        .VGA_RED(VGA_RED_FULL),
        .VGA_GREEN(VGA_GREEN_FULL),
        .VGA_BLUE(VGA_BLUE_FULL)
    );
endmodule
