module Top(
           input wire        CLK100MHZ,
           input wire        BTNU,
           input wire        BTND,
           output wire       VGA_HS,
           output wire       VGA_VS,
           output wire [3:0] VGA_R,
           output wire [3:0] VGA_G,
           output wire [3:0] VGA_B
           );

   wire                 CLK_25MHZ_RAW;
   wire                 CLK_LOCKED;
   wire [7:0]           VGA_RED_FULL;
   wire [7:0]           VGA_GREEN_FULL;
   wire [7:0]           VGA_BLUE_FULL;

   assign VGA_R = VGA_RED_FULL[7:4];
   assign VGA_G = VGA_GREEN_FULL[7:4];
   assign VGA_B = VGA_BLUE_FULL[7:4];

   MMCM_25 u_MMCM_25
     (.CLKIN_100MHZ(CLK100MHZ),
      .CLKOUT_25MHZ(CLK_25MHZ_RAW),
      .LOCKED(CLK_LOCKED)
      );

   wire        CLK_25MHZ;
   BUFG BUFG(.I(CLK_25MHZ_RAW), .O(CLK_25MHZ));

   topEntity u_topEntity
     (.CLK_25MHZ(CLK_25MHZ),
      .RESET(!CLK_LOCKED),
      .BTN_UP(BTNU),
      .BTN_DOWN(BTND),
      .VGA_HSYNC(VGA_HS),
      .VGA_VSYNC(VGA_VS),
      .VGA_RED(VGA_RED_FULL),
      .VGA_GREEN(VGA_GREEN_FULL),
      .VGA_BLUE(VGA_BLUE_FULL)
      );
endmodule
