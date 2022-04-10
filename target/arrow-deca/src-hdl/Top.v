module Top(

    // CLOCK
    input MAX10_CLK1_50,
    input MAX10_CLK2_50,

	//////////// KEY //////////
	input 		     [1:0]		KEY,

	//////////// HDMI-TX //////////
	inout 		          		HDMI_I2C_SCL,
	inout 		          		HDMI_I2C_SDA,
	inout 		     [3:0]		HDMI_I2S,
	inout 		          		HDMI_LRCLK,
	inout 		          		HDMI_MCLK,
	inout 		          		HDMI_SCLK,
	output		          		HDMI_TX_CLK,
	output		    [23:0]		HDMI_TX_D,
	output		          		HDMI_TX_DE,
	output		          		HDMI_TX_HS,
	input 		          		HDMI_TX_INT,
	output		          		HDMI_TX_VS
);
    // PLL for 25MHz
    wire CLK_25MHZ;
    wire CLK_LOCKED;

    pll25mhz pll25mhz_inst (
        .areset ( 1'b0 ),
        .inclk0 ( MAX10_CLK1_50 ),
        .c0 ( CLK_25MHZ ),
        .locked ( CLK_LOCKED )
        );

    assign HDMI_TX_CLK = CLK_25MHZ;

    // Run top entity at 25 MHz
    topEntity u_topEntity
        ( .CLK_25MHZ(CLK_25MHZ)
        , .RESET(!CLK_LOCKED)
        , .BTN_UP(!KEY[0])
        , .BTN_DOWN(!KEY[1])
        , .VGA_HSYNC(HDMI_TX_HS)
        , .VGA_VSYNC(HDMI_TX_VS)
        , .VGA_DE(HDMI_TX_DE)
        , .VGA_RED(HDMI_TX_D[23:16])
        , .VGA_GREEN(HDMI_TX_D[15:8])
        , .VGA_BLUE(HDMI_TX_D[7:0])
        );


    // HDMI initialisation
    wire  HDMI_I2C_SCL_RD;
    wire  HDMI_I2C_SCL_WR_EN;
    wire  HDMI_I2C_SCL_WR_DAT;
    assign HDMI_I2C_SCL = HDMI_I2C_SCL_WR_EN ? HDMI_I2C_SCL_WR_DAT : 1'bz;

    wire  HDMI_I2C_SDA_RD;
    wire  HDMI_I2C_SDA_WR_EN;
    wire  HDMI_I2C_SDA_WR_DAT;
    assign HDMI_I2C_SDA = HDMI_I2C_SDA_WR_EN ? HDMI_I2C_SDA_WR_DAT : 1'bz;

    initHDMI u_initHDMI
        ( .CLK_50MHZ(MAX10_CLK2_50)
        // , .RESET(!HDMI_TX_INT)
        , .RESET(1'b0)
        , .I2C_SCL_RD(HDMI_I2C_SCL_RD)
        , .I2C_SCL_WR_EN(HDMI_I2C_SCL_WR_EN)
        , .I2C_SCL_WR_DAT(HDMI_I2C_SCL_WR_DAT)
        , .I2C_SDA_RD(HDMI_I2C_SDA_RD)
        , .I2C_SDA_WR_EN(HDMI_I2C_SDA_WR_EN)
        , .I2C_SDA_WR_DAT(HDMI_I2C_SDA_WR_DAT)
        );

endmodule
