-- Copyright (c) 2021 Terraonion
-- This program is GPL Licensed. See COPYING for the full license.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;

library UNISIM;
use UNISIM.VComponents.all;

entity HDMI is
    Port ( CLK100 : in STD_LOGIC;
	        CLK50 : in STD_LOGIC;
			  CLKHDMI720x480 : in STD_LOGIC;
			  CLKHDMI640x480 : in STD_LOGIC;
			  CLKHDMI1280x720: in STD_LOGIC;			  
			  
           COLOR_IN : in  STD_LOGIC_VECTOR (8 downto 0);
           DCK : in  STD_LOGIC;
           CART_VSYNC : in  STD_LOGIC;
           CART_HSYNC : in  STD_LOGIC;
			  CSYNC_FPGA_IN : in STD_LOGIC;
			  CSYNC_FPGA_OUT : out STD_LOGIC;
			  CART_BLANK : in STD_LOGIC;
			  
           HDMI_VSYNC : out  STD_LOGIC;
           HDMI_HSYNC : out  STD_LOGIC;
           HDMI_DE 	 : out  STD_LOGIC;
           HDMI_D 	 : out  STD_LOGIC_VECTOR (23 downto 0);
			  HDMI_CLK   : out STD_LOGIC;
			  
			  ADV_BLANK : out std_logic;
			  ADV_SYNC  : out std_logic;
			  ADV_SAVE  : out std_logic;
			  ADV_CLOCK : out std_logic;
			  
			  MCU_CMD	: in std_logic_vector(3 downto 0);
			  MCU_D		: in std_logic_vector(7 downto 0);
			  MCU_CLK	: in std_logic;
			  
			  FBRAM_A	  : out std_logic_vector(18 downto 0);
			  FBRAM_D	  : inout std_logic_vector(15 downto 0);
			  FBRAM_WEn   : out std_logic;
			  FBRAM_OEn   : out std_logic;
			  FBRAM_CEn   : out std_logic;
			  
           NRST : in  STD_LOGIC);
end HDMI;

architecture Behavioral of HDMI is


COMPONENT linebuf
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(8 DOWNTO 0)
  );
END COMPONENT;

COMPONENT rgb_spaceconv
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(23 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(23 DOWNTO 0)
  );
END COMPONENT;

component dpsram is
    Port ( CLK : in  STD_LOGIC;
			  NRST : in STD_LOGIC;
           RD_A : in  STD_LOGIC_VECTOR (18 downto 0);
           RD_D : out  STD_LOGIC_VECTOR (15 downto 0);
           WR_A : in  STD_LOGIC_VECTOR (18 downto 0);
           WR_D : in  STD_LOGIC_VECTOR (15 downto 0);
           WR_EN : in  STD_LOGIC;
           FBRAM_A : out  STD_LOGIC_VECTOR (18 downto 0);
           FBRAM_D : inout  STD_LOGIC_VECTOR (15 downto 0);
           FBRAM_CEn : out  STD_LOGIC;
           FBRAM_OEn : out  STD_LOGIC;
           FBRAM_WEn : out  STD_LOGIC);
end component;

COMPONENT csync
	PORT(
		clk : IN std_logic;
		hsync : IN std_logic;
		vsync : IN std_logic;          
		csync : OUT std_logic
		);
	END COMPONENT;

component clk_hdmi_pll
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic;
  CLK_OUT2          : out    std_logic;
  RESET				  : in 	  std_logic
 );
end component;

component clk720x2
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic;
  CLK_OUT2          : out    std_logic
 );
end component;


attribute keep : string;

--type palette_t is array (0 to 511) of std_logic_vector(8 downto 0);
--signal palette : palette_t;

type pixclock_t is array (0 to 1) of std_logic_vector(1 downto 0);
signal PIXCLOCK : pixclock_t;

signal CONV_RGB :  std_logic_vector(23 downto 0);
signal CONV_RGB_HDMI :  std_logic_vector(23 downto 0);
signal CONV_RGB_HDMI_PRV :  std_logic_vector(23 downto 0);
signal CONV_RGB_HDMI_CUR :  std_logic_vector(23 downto 0);

--signal RGB_OUT : std_logic := '0';
signal RGB_D : std_logic_vector(23 downto 0);

--signal WRn_Latch : std_logic;
--signal CEK_Latch : std_logic;

signal W_LINBUF : std_logic_vector(0 downto 0);
signal W_LINBUF_A : std_logic_vector(9 downto 0);
signal W_LINBUF_A_W : std_logic_vector(10 downto 0);
signal W_LINBUF_D : std_logic_vector(8 downto 0);
signal W_LINBUF_WR : std_logic;

signal R_LINBUF : std_logic;

--attribute keep of W_LINBUF_A : signal is "true";

signal R_LINEBUF_A : std_logic_vector(10 downto 0);
signal R_LINEBUF_D : std_logic_vector(8 downto 0);
signal R_LINEBUF_D_X : std_logic_vector(8 downto 0);

signal HDMI_RGB_D : std_logic_vector(23 downto 0);

signal CART_XPOS : integer range 0 to 2048;
signal CART_YPOS : integer range 0 to 2048;


signal LINE_X : integer range 0 to 1048575;
signal LINE_OV_X : integer range 0 to 1048575;
signal LINE_OV2_X : integer range 0 to 1048575;

--attribute keep of CART_XPOS : signal is "true";
--attribute keep of CART_YPOS : signal is "true";

--attribute keep of LINE_X : signal is "true";
--attribute keep of PRV_COLOR : signal is "true";


--HDMI 720p timmings
--CLK 74.25
--constant H_DATA : integer := 1280;
--constant H_FP   : integer := 110;
--constant H_SYNC : integer := 40;
--constant H_BP   : integer := 220;
--
--constant V_DATA : integer := 720;
--constant V_FP   : integer := 5;
--constant V_SYNC : integer := 5;
--constant V_BP   : integer := 20;
--(((720+19+62+57) * (480+9+6+30) / 357630 )* 21.47727

--HDMI 720x480
--CLK 27
constant H_DATA_16_9 : integer := 720;
constant H_FP_16_9   : integer := 19;	--16
constant H_SYNC_16_9 : integer := 62;	--60
constant H_BP_16_9   : integer := 57;	--62

constant V_DATA_16_9 : integer := 480;
constant V_FP_16_9   : integer := 7;
--constant V_FP2   : integer := V_FP+2;
constant V_SYNC_16_9 : integer := 6;
constant V_BP_16_9   : integer := 32;

constant HV_END_16_9 : integer := H_DATA_16_9 - 1;
constant HS_STA_16_9 : integer := HV_END_16_9 + H_FP_16_9;
constant HS_END_16_9 : integer := HS_STA_16_9 + H_SYNC_16_9;
constant H_END_16_9  : integer := HS_END_16_9 + H_BP_16_9;

constant VV_END_16_9 : integer := V_DATA_16_9 - 1;
constant VS_STA1_16_9 : integer := VV_END_16_9 + V_FP_16_9;
constant VS_END1_16_9 : integer := VS_STA1_16_9 + V_SYNC_16_9;
constant V_END1_16_9 : integer := VS_END1_16_9 + V_BP_16_9;


-- 640x480
--CLK 25
constant H_DATA_4_3 : integer := 640;
constant H_FP_4_3   : integer := 16;
constant H_SYNC_4_3 : integer := 96;
constant H_BP_4_3   : integer := 48;


constant V_DATA_4_3 : integer := 480;
constant V_FP_4_3   : integer := 11;
--constant V_FP2   : integer := V_FP+2;
constant V_SYNC_4_3 : integer := 2;
constant V_BP_4_3   : integer := 31;


constant HV_END_4_3 : integer := H_DATA_4_3 - 1;
constant HS_STA_4_3 : integer := HV_END_4_3 + H_FP_4_3;
constant HS_END_4_3 : integer := HS_STA_4_3 + H_SYNC_4_3;
constant H_END_4_3  : integer := HS_END_4_3 + H_BP_4_3;

constant VV_END_4_3 : integer := V_DATA_4_3 - 1;
constant VS_STA1_4_3 : integer := VV_END_4_3 + V_FP_4_3;
constant VS_END1_4_3 : integer := VS_STA1_4_3 + V_SYNC_4_3;
constant V_END1_4_3 : integer := VS_END1_4_3 + V_BP_4_3;



--1280x720
constant H_DATA_720P : integer := 1280;
constant H_FP_720P   : integer := 110-31;
constant H_SYNC_720P : integer := 40;
constant H_BP_720P   : integer := 220-31;

constant V_DATA_720P : integer := 720;
constant V_FP_720P   : integer := 5;
constant V_SYNC_720P : integer := 5;
constant V_BP_720P   : integer := 20;

constant HV_END_720P : integer := H_DATA_720P - 1;
constant HS_STA_720P : integer := HV_END_720P + H_FP_720P;
constant HS_END_720P : integer := HS_STA_720P + H_SYNC_720P;
constant H_END_720P  : integer := HS_END_720P + H_BP_720P;

constant VV_END_720P  : integer := V_DATA_720P - 1;
constant VS_STA1_720P : integer := VV_END_720P + V_FP_720P;
constant VS_END1_720P : integer := VS_STA1_720P + V_SYNC_720P;
constant V_END1_720P  : integer := VS_END1_720P + V_BP_720P;


--constant VS_STA2 : integer := VV_END + V_FP2;
--constant VS_END2 : integer := VS_STA2 + V_SYNC;
--constant V_END2 : integer := VS_END2 + V_BP;

constant X_STEP_256_16_9 : integer := 128;

constant X_OFFS_256_16_9 : integer := (16) * 256;

constant X_STEP_256_4_3 : integer := 102;

constant X_OFFS_256_4_3 : integer := (66) * 256;

constant X_STEP_256_720P : integer := 64;

constant X_OFFS_256_720P : integer := (24) * 256;


signal SX : integer range 0 to 2047 := 0;
signal SY : integer range 0 to 2047 := 0;

signal NEW_HSYNC : std_logic;
signal NEW_VSYNC : std_logic;

signal DCK_L   : std_logic;
signal PRV_DCK_L   : std_logic;
signal HSYNC_L : std_logic;
signal VSYNC_L : std_logic;
signal CSYNC_L : std_logic;
signal VSYNC_L2 : std_logic;

type sync_t is (active, hsync, vsync, vsync_in);
signal NEW_SYNC_STATE : sync_t := active;
signal NEW_HSYNC_CNT : integer range 0 to 255;
signal CART_X : integer range 0 to 2047;
signal CART_XX : integer range 0 to 2047;
signal CART_DE_X : integer range 0 to 2047;
signal CART_Y : integer range 0 to 2047;
signal LINE_SIZE : integer range 0 to 2047;
signal LINE_HSIZE : integer range 0 to 1023;
signal LINEBUF_X : integer range 0 to 2047;
signal NEW_VSYNC_P : std_logic := '1';
signal NEW_VSYNC_P2 : std_logic := '1';

signal R_VAL : std_logic_vector(15 downto 0);
signal G_VAL : std_logic_vector(15 downto 0);
signal B_VAL : std_logic_vector(15 downto 0);

signal R_VAL_NOINT : std_logic_vector(15 downto 0);
signal G_VAL_NOINT : std_logic_vector(15 downto 0);
signal B_VAL_NOINT : std_logic_vector(15 downto 0);

--signal SYNCTIME : integer := 1000;

signal SYNCMODE : std_logic := '0';
signal SCANFACTOR : integer range 0 to 255 := 128;
signal SCANMODE : integer range 0 to 3 := 0;
signal NUM_BUFFERS : integer range 0 to 3 := 1;
signal SYNCLINE : std_logic := '0';
signal INTERPMODE : std_logic := '1';
signal SQUAREMODE : std_logic := '0';
signal MODE720P   : std_logic := '0';

signal MCU_CLK_L : std_logic;
signal MCU_D_L : std_logic_vector(7 downto 0);
signal MCU_CMD_L : std_logic_vector(3 downto 0);
signal extracommand : std_logic_vector(7 downto 0);
signal mcu_addr : std_logic_vector(21 downto 0);
signal RGB_OUT : std_logic;
signal RGB_DATA_W : std_logic_vector(23 downto 0);
--signal RGB_ADDR_W : std_logic_vector(8 downto 0);
signal RGB_W_RGB : std_logic := '0';
signal RGB_W_HDMI : std_logic := '0';

signal FB_W_BUF : integer range 0 to 3;
signal FB_W_ADDR : std_logic_vector(18 downto 0);
signal FB_W_D : std_logic_vector(15 downto 0);
signal FB_W_WE : std_logic := '0';

signal FB_R_BUF : integer range 0 to 3;
signal FB_R_ADDR : std_logic_vector(18 downto 0);
signal FB_R_D : std_logic_vector(15 downto 0);
signal FB_NRST : std_logic := '0';


signal PRV_LINE_X : integer range 0 to 1048575;
signal PRV_LINE_X_1 : integer range 0 to 1048575;
signal PRV_LINE_X_2 : integer range 0 to 1048575;
signal PRV_LINE_X_3 : integer range 0 to 1048575;

signal LINE_Y_720P : integer range 0 to 1023 := 0;

signal HSYNC_GEN : std_logic;
signal VSYNC_GEN : std_logic;
signal CSYNC_GEN : std_logic;
signal RGB_BLANK : std_logic;

signal CLKHDMI : std_logic;

signal CLKHDMI720x : std_logic;
signal CLKHDMI720x2 : std_logic;
signal CLKHDMIX : std_logic;
signal CLKHDMI_PIX : std_logic;
signal CLKHDMIx1 : std_logic;
signal CLKHDMIx1_UNBUF : std_logic;
signal CLKHDMIx4 : std_logic;
signal CLKHDMIx4X: std_logic;
signal HDMICLKSEL : std_logic_vector(1 downto 0);

signal SY_DEBUG : integer range 0 to 65535;
signal SX_DEBUG : integer range 0 to 65535;


--attribute keep of PRV_LINE_X : signal is "true";
--attribute keep of PRV_LINE_X_1 : signal is "true";
--attribute keep of PRV_LINE_X_2 : signal is "true";
--attribute keep of PRV_LINE_X_3 : signal is "true";
--attribute keep of CONV_RGB_HDMI_PRV : signal is "true";
--attribute keep of CONV_RGB_HDMI_CUR : signal is "true";
--attribute keep of HDMI_RGB_D : signal is "true";
--attribute keep of HDMI_HSYNC : signal is "true";
--attribute keep of HDMI_VSYNC : signal is "true";
--attribute keep of HDMI_DE : signal is "true";
--attribute keep of CSYNC_GEN : signal is "true";
--attribute keep of RGB_BLANK : signal is "true";
--attribute keep of SY_DEBUG : signal is "true";
--attribute keep of SX_DEBUG : signal is "true";



begin


ClockMux : BUFGMUX
   generic map (
      CLK_SEL_TYPE => "ASYNC"  -- Glitchles ("SYNC") or fast ("ASYNC") clock switch-over
   )
   port map (
      O => CLKHDMIX,
      I0 => CLKHDMI720x480,
      I1 => CLKHDMI640x480,
      S => HDMICLKSEL(0)
   );

ClockMux2 : BUFGMUX
   generic map (
      CLK_SEL_TYPE => "ASYNC"  -- Glitchles ("SYNC") or fast ("ASYNC") clock switch-over
   )
   port map (
      O => CLKHDMI,
      I0 => CLKHDMIx1,
      I1 => CLKHDMI720X,
      S => HDMICLKSEL(1)
   );

--CLKHDMI <= CLKHDMI720X;
	
ClockMux3 : BUFGMUX
   generic map (
      CLK_SEL_TYPE => "ASYNC"  -- Glitchles ("SYNC") or fast ("ASYNC") clock switch-over
   )
   port map (
      O => CLKHDMIx4,
      I0 => CLKHDMIx4X,
      I1 => CLKHDMI720x2,
      S => HDMICLKSEL(1)
   );

--CLKHDMIx4 <= CLKHDMI720x2;	
	
clk720 : clk720x2
  port map
   (-- Clock in ports
    CLK_IN1 => CLKHDMI1280x720,
    -- Clock out ports
    CLK_OUT1 => CLKHDMI720X,
    CLK_OUT2 => CLKHDMI720x2
	 );


MAIN_TO_HDMI : clk_hdmi_pll
  port map
   (-- Clock in ports
    --CLK_IN1 => CLKHDMI2,	--4:3
	 CLK_IN1 => CLKHDMIX,		--16:9
    -- Clock out ports
    CLK_OUT1 => CLKHDMIx1,
	 CLK_OUT2 => CLKHDMIx4X,
	 RESET => '0'
	 );

ODDR2_inst : ODDR2
   generic map(
      DDR_ALIGNMENT => "NONE", -- Sets output alignment to "NONE", "C0", "C1" 
      INIT => '1', -- Sets initial state of the Q output to '0' or '1'
      SRTYPE => "SYNC") -- Specifies "SYNC" or "ASYNC" set/reset
   port map (
      Q => HDMI_CLK, -- 1-bit output data
      C0 => CLKHDMI, -- 1-bit clock input
      C1 => not CLKHDMI, -- 1-bit clock input
      CE => '1',  -- 1-bit clock enable input
      D0 => '1',   -- 1-bit data input (associated with C0)
      D1 => '0',   -- 1-bit data input (associated with C1)
      R => '0',    -- 1-bit reset input
      S => '0'     -- 1-bit set input
   );

space_conv : rgb_spaceconv
  PORT MAP (
    clka => CLK50,
    wea => "" & RGB_W_RGB,
    addra => mcu_addr(8 downto 0),
    dina => RGB_DATA_W,
    clkb => DCK_L,
    addrb => W_LINBUF_D(8 downto 0),
    doutb => CONV_RGB
  );

space_conv_HDMI : rgb_spaceconv
  PORT MAP (
    clka => CLK50,
    wea => "" & RGB_W_HDMI,
    addra => mcu_addr(8 downto 0),
    dina => RGB_DATA_W,
    clkb => CLKHDMI,
    addrb => R_LINEBUF_D_X(8 downto 0),
    doutb => CONV_RGB_HDMI
  );

  
  
linebuffer : linebuf
  PORT MAP (
    clka => DCK_L,
    wea => "" & W_LINBUF_WR,
    addra => W_LINBUF_A_W,
    dina => W_LINBUF_D(8 downto 0),
    clkb => CLKHDMI,
    addrb => R_LINEBUF_A,
    doutb => R_LINEBUF_D
  );
  
dpframebuffer: dpsram 
	PORT MAP(
		CLK => CLKHDMIx4,
		NRST => FB_NRST,
		RD_A => FB_R_ADDR,
		RD_D => FB_R_D,
		WR_A => FB_W_ADDR,
		WR_D => FB_W_D,
		WR_EN => FB_W_WE,
		FBRAM_A => FBRAM_A,
		FBRAM_D => FBRAM_D,
		FBRAM_CEn => FBRAM_CEn,
		FBRAM_OEn => FBRAM_OEn,
		FBRAM_WEn => FBRAM_WEn
	);
	
MCU_COMMAND : process(CLK50,nrst)
variable PRV_MCU_CLK : std_logic := '0';
begin
	if rising_edge(CLK50) then
	
		MCU_CLK_L <= MCU_CLK;
		MCU_D_L <= MCU_D;
		MCU_CMD_L <= MCU_CMD;
	
		if(MCU_CLK_L = '1') then
			case MCU_CMD_L is
				when X"2" => --set addr l
					mcu_addr <= mcu_addr(21 downto 8) & MCU_D_L;
				when X"3" => --set addr_m
					mcu_addr <= mcu_addr(21 downto 16) & MCU_D_L & mcu_addr(7 downto 0);
				when X"4" => --set addr_h
					mcu_addr <= MCU_D(5 downto 0) & mcu_addr(15 downto 0);
				--when X"5" => --set data
				--	mcu_data <= MCU_D;
				when X"E" => -- execute extracommand
					case extracommand is
						when X"11" => RGB_OUT <= MCU_D_L(0);
						when X"12" => RGB_DATA_W(7 downto 0) <= MCU_D_L;
						when X"13" => RGB_DATA_W(15 downto 8) <= MCU_D_L;
						when X"14" => RGB_DATA_W(23 downto 16) <= MCU_D_L;
						when X"15" => RGB_W_RGB <= MCU_D_L(0);
										  RGB_W_HDMI <= MCU_D_L(1);
						when X"16" => SYNCMODE    <= MCU_D_L(0);
										  INTERPMODE  <= MCU_D_L(1);
										  SYNCLINE    <= MCU_D_L(2);
										  SQUAREMODE  <= MCU_D_L(3);
										  NUM_BUFFERS <= to_integer(unsigned(MCU_D_L(5 downto 4)));
										  --RGB_SCALER  <= MCU_D_L(6);
										  --MODE720P <= MCU_D_L(7);
						when X"17" => SCANMODE    <= to_integer(unsigned(MCU_D_L(1 downto 0)));
						when X"18" => SCANFACTOR  <= to_integer(unsigned(MCU_D_L));
						when others => null;
					end case;
				when X"F" =>	-- set extracommand
					extracommand <= MCU_D_L;
				when others =>
					null;
			end case;
		end if;
	
		PRV_MCU_CLK := MCU_CLK_L;
	end if;
end process;

--DCK_L <= DCK;

SIGNAL_LATCH: process(CLK100)
variable PRV_DCKL : std_logic;
variable PRV_DCKL2 : std_logic;
variable HSYNC_L1 : std_logic;
variable HSYNC_L2 : std_logic;
begin
	if(rising_edge(CLK100)) then
	--if(rising_edge(CLKHDMIx4)) then
		if(PRV_DCKL = DCK and PRV_DCKL2 = PRV_DCKL) then
			DCK_L <= DCK;
--			if(DCK_L = '1' and DCK = '0') then
--				--WR_PIX <= xx
--				if(CART_YPOS < 220) then
--					FB_W_ADDR <= std_logic_vector(to_unsigned(FB_W_BUF,2)) & std_logic_vector(to_unsigned(CART_YPOS,8)) & std_logic_vector(to_unsigned(CART_XPOS,9));
--					FB_W_D <=  "00000" & CR(1 downto 0) & PALRAM_D_R(8 downto 0);
--					FB_W_WE <= '1';
--				else
--					FB_W_ADDR <= std_logic_vector(to_unsigned(FB_W_BUF,2)) & std_logic_vector(to_unsigned(CART_YPOS,8)) & std_logic_vector(to_unsigned(CART_XPOS,9));
--					FB_W_D <= (others => '0');
--					FB_W_WE <= '1';
--				end if;
--			end if;
		end if;
		PRV_DCKL2 := PRV_DCKL;
		PRV_DCKL := DCK;
		
		
		HSYNC_L <= HSYNC_L2;
		HSYNC_L2 := HSYNC_L1;
		HSYNC_L1 := CART_HSYNC;
		
	
		--HSYNC_L <= CART_HSYNC;
		VSYNC_L <= CART_VSYNC;
		CSYNC_L <= CSYNC_FPGA_IN;
		PRV_DCK_L <= DCK_L;
	end if;

end process;


LINEBUFFER_CAPTURE : process(CLKHDMI)
--variable PRV_DCK : std_logic_vector(7 downto 0);
variable PRV_HSYNC : std_logic;
variable PRV_VSYNC : std_logic;
variable CUR_HSYNC : std_logic;
variable CUR_VSYNC : std_logic;
begin
	--if(rising_edge(DCK)) then
	if(rising_edge(DCK_L)) then
	--if(rising_edge(CLKHDMI)) then
	--	PRV_DCK <= NEW_DCK;
	--	NEW_DCK <= DCK;
	
	--	if(PRV_DCK = '0' and NEW_DCK = '1') then
			W_LINBUF_WR <= '0';
			
			W_LINBUF_D <= COLOR_IN;
			
			W_LINBUF_WR <= '1';
			W_LINBUF_A <= std_logic_vector(to_unsigned(to_integer(unsigned(W_LINBUF_A) + 1),10));
			W_LINBUF_A_W <= W_LINBUF & W_LINBUF_A;
			
			CART_XPOS <= CART_XPOS + 1;
			
			if(PRV_HSYNC = '1' and CUR_HSYNC = '0') then
				W_LINBUF_A <= (others => '0');
				W_LINBUF <= not W_LINBUF;
				R_LINBUF <= W_LINBUF(0);
				W_LINBUF_A_W <= (not W_LINBUF) & "0000000000";
				CART_YPOS <= CART_YPOS + 1;
				CART_XPOS <= 0;
			end if;
			
			if(PRV_VSYNC = '1' and CUR_VSYNC = '0') then
				CART_YPOS <= 0;
				FB_W_BUF <= FB_W_BUF + 1;
			end if;
			
--			if(CART_YPOS < 220) then
--				FB_W_ADDR <= std_logic_vector(to_unsigned(FB_W_BUF,2)) & std_logic_vector(to_unsigned(CART_YPOS,8)) & std_logic_vector(to_unsigned(CART_XPOS,9));
--				FB_W_D <=  "00000" & CR(1 downto 0) & PALRAM_D_R(8 downto 0);
--				FB_W_WE <= '1';
--			else
--				FB_W_ADDR <= std_logic_vector(to_unsigned(FB_W_BUF,2)) & std_logic_vector(to_unsigned(CART_YPOS,8)) & std_logic_vector(to_unsigned(CART_XPOS,9));
--				FB_W_D <= (others => '0');
--				FB_W_WE <= '1';
--			end if;
			
			
			PRV_HSYNC := CUR_HSYNC;
			PRV_VSYNC := CUR_VSYNC;
			CUR_HSYNC := HSYNC_L;
			CUR_VSYNC := VSYNC_L;
		--end if;
	end if;
end process;

FRAMEBUFFER_CAPTURE : process(CLKHDMIx4)
variable PRV_XPOS : integer range 0 to 2048;
variable PRV_YPOS : integer range 0 to 2048;
variable PRV_DATA : std_logic_vector(8 downto 0);

begin
	if(rising_edge(CLKHDMIx4)) then
		if(PRV_XPOS = CART_XPOS and PRV_YPOS = CART_YPOS and PRV_DATA = COLOR_IN) then
			if(PRV_YPOS < 250 and PRV_XPOS < 512 + ((12 + 3 + 1) * 8) + 3) then
				FB_W_ADDR <= std_logic_vector(to_unsigned(FB_W_BUF,2)) & std_logic_vector(to_unsigned(PRV_YPOS - 18,8)) & std_logic_vector(to_unsigned(PRV_XPOS,9));
				FB_W_D <=  "0000000" & PRV_DATA(8 downto 0);
				FB_W_WE <= '1';
				--FB_W_D <= "0000000" & std_logic_vector(to_unsigned(PRV_XPOS,9));
			else
				FB_W_WE <= '0';
--			else
--				FB_W_ADDR <= std_logic_vector(to_unsigned(FB_W_BUF,2)) & std_logic_vector(to_unsigned(PRV_YPOS,8)) & std_logic_vector(to_unsigned(PRV_XPOS,9));
--				FB_W_D <= (others => '0');
--				FB_W_WE <= '1';
			end if;
		end if;
		PRV_XPOS := CART_XPOS;
		PRV_YPOS := CART_YPOS;-- - 16;
		PRV_DATA := COLOR_IN;
	end if;

end process;

COLOR_DECODE: process(CLK50)
--variable PALCOLOR : std_logic_vector(8 downto 0);
begin
	if(rising_edge(CLK50)) then
		--PALCOLOR := PALRAM_D_R;

--		RGB_D(23 downto 16) <= PALRAM_D_R(5) & PALRAM_D_R(4) & PALRAM_D_R(3) & "00000";
--		RGB_D(15 downto 8)  <= PALRAM_D_R(2) & PALRAM_D_R(1) & PALRAM_D_R(0) & "00000";
--		RGB_D(7 downto 0)   <= PALRAM_D_R(8) & PALRAM_D_R(7) & PALRAM_D_R(6) & "00000";
		
		
		RGB_D(23 downto 16) <= CONV_RGB(23 downto 16);	--R
		RGB_D(15 downto 8)  <= CONV_RGB(7 downto 0);		--B
		RGB_D(7 downto 0)   <= CONV_RGB(15 downto 8);	--G
		
		
		if(CART_VSYNC = '0' or CART_HSYNC = '0') then
			RGB_D <= (others => '0');
		end if;

		CSYNC_FPGA_OUT <= CSYNC_FPGA_IN;

	end if;

end process;


HDMI_PIX : process(CLKHDMI)
variable prv_HSYNC : std_logic := '1';
variable cur_HSYNC : std_logic := '1';
variable cur_VSYNC : std_logic := '1';

variable CUR_LINBUF : std_logic_vector(0 downto 0);

variable prv_CSYNC : std_logic := '1';
variable SYNC_WAIT : integer range 0 to 8192;
variable VSYNC_WAIT : integer range 0 to 3 := 0;
variable SYNC_HSYNC : boolean := true;
variable PRV_DE : std_logic := '0';
variable FIRSTHALF : boolean := false;
variable DTEST : std_logic_vector(23 downto 0);
variable LINE_Y : integer range 0 to 511;
variable LINE_TMP : std_logic_vector(19 downto 0);
variable LINE_Y_TMP : std_logic_vector(8 downto 0);
variable LINE_X_STEP : integer range 0 to 1023;
variable LINE_FRAC : integer range 0 to 65535;
variable PRV_LINE_FRAC : integer range 0 to 65535;
variable LINE_INT : std_logic_vector(11 downto 0);
variable PRV_LINE_INT : std_logic_vector(11 downto 0);
variable LINE_X_TMP : integer range 0 to 1048575;
variable LINE_Y_CNT : integer range 0 to 3;
variable LINE_X_CNT : integer range 0 to 3;

variable R_SCAN : std_logic_vector(15 downto 0);
variable G_SCAN : std_logic_vector(15 downto 0);
variable B_SCAN : std_logic_vector(15 downto 0);

variable PRV_COLOR : std_logic_vector(23 downto 0);

variable VS_STA : integer;
variable VS_END : integer;
variable V_END : integer;

variable HV_END : integer;
variable HS_STA : integer;
variable HS_END : integer;
variable H_END : integer;

variable VV_END : integer;
variable VS_STA1 : integer;
variable VS_END1 : integer;
variable V_END1 : integer;


variable LINE_P1 : std_logic_vector(19 downto 0);
variable LINE_P2 : std_logic_vector(19 downto 0);

variable SCAN_FACTOR : integer range 0 to 256;
begin
	if(NRST = '0') then
		--SX <= 0;
		--SY <= 0;
		--SYNC_HSYNC := true;
		--SYNCTIME <= 1000;
		FB_NRST <= '0';
		LINE_Y_CNT := 0;
	elsif(falling_edge(CLKHDMI)) then
		FB_NRST <= '1';
		LINE_TMP := std_logic_vector(to_unsigned(LINE_X, 20));
		R_LINEBUF_A <= (CUR_LINBUF) & LINE_TMP(17 downto 8);
		
		LINE_Y_TMP := std_logic_vector(to_unsigned(LINE_Y, 9));
		LINE_INT := LINE_TMP(19 downto 8);
		
		if(MODE720P = '1') then
			--LINE_INT := LINE_Y_720P;
			LINE_Y_TMP(8 downto 1) := std_logic_vector(to_unsigned(LINE_Y_720P,8));
			if(LINE_Y_CNT = 2) then
				LINE_Y_TMP(0) := '1';
			else
				LINE_Y_TMP(0) := '0';
			end if;
		end if;

		LINE_TMP := std_logic_vector(to_unsigned(PRV_LINE_X, 20));
		LINE_FRAC := to_integer(unsigned(LINE_TMP(7 downto 0)));
		
		if(CONV_RGB_HDMI_PRV /= CONV_RGB_HDMI) then
			PRV_COLOR := CONV_RGB_HDMI_PRV;
		end if;
		
		if(SYNCMODE = '0') then
			if(MODE720P = '1') then
				FB_R_ADDR <= std_logic_vector(to_unsigned(FB_R_BUF,2)) & LINE_Y_TMP(8 downto 1) & LINE_INT(8 downto 0);
				R_LINEBUF_D_X <= FB_R_D(8 downto 0);
			else
				FB_R_ADDR <= std_logic_vector(to_unsigned(FB_R_BUF,2)) & LINE_Y_TMP(8 downto 1) & LINE_INT(8 downto 0);
				R_LINEBUF_D_X <= FB_R_D(8 downto 0);
			end if;
		else
			--Clear last pixel in 256 mode
			if(FB_R_D(9) = '0')	then --256 mode
				R_LINEBUF_D_X <= R_LINEBUF_D;
			else
				R_LINEBUF_D_X <= R_LINEBUF_D;
			end if;
		end if;
		
		--R_LINEBUF_A <= "0" & (not W_LINBUF) & std_logic_vector(to_unsigned(SX,10));
		
		LINE_TMP := std_logic_vector(to_unsigned(PRV_LINE_X_2, 20));
		LINE_FRAC := to_integer(unsigned(LINE_TMP(7 downto 0)));
		
		if(INTERPMODE = '1') then
			R_VAL <= std_logic_vector(to_unsigned((to_integer(unsigned(CONV_RGB_HDMI_CUR(23 downto 16))) * (LINE_FRAC)) + (to_integer(unsigned(CONV_RGB_HDMI_PRV(23 downto 16))) * (256 - LINE_FRAC)),16));
			G_VAL <= std_logic_vector(to_unsigned((to_integer(unsigned(CONV_RGB_HDMI_CUR(15 downto 8))) * (LINE_FRAC)) + (to_integer(unsigned(CONV_RGB_HDMI_PRV(15 downto 8))) * (256 - LINE_FRAC)),16));
			B_VAL <= std_logic_vector(to_unsigned((to_integer(unsigned(CONV_RGB_HDMI_CUR(7 downto 0))) * (LINE_FRAC)) + (to_integer(unsigned(CONV_RGB_HDMI_PRV(7 downto 0))) * (256 - LINE_FRAC)),16));
			--R_VAL_NOINT <= CONV_RGB_HDMI_PRV(23 downto 16) & X"00";
			--G_VAL_NOINT <= CONV_RGB_HDMI_PRV(15 downto 8) & X"00";
			--B_VAL_NOINT <= CONV_RGB_HDMI_PRV(7 downto 0) & X"00";
		else
			R_VAL <= CONV_RGB_HDMI(23 downto 16) & X"00";
			G_VAL <= CONV_RGB_HDMI(15 downto 8) & X"00";
			B_VAL <= CONV_RGB_HDMI(7 downto 0) & X"00";
			--R_VAL_NOINT <= CONV_RGB_HDMI(23 downto 16) & X"00";
			--G_VAL_NOINT <= CONV_RGB_HDMI(15 downto 8) & X"00";
			--B_VAL_NOINT <= CONV_RGB_HDMI(7 downto 0) & X"00";
		end if;
		
		--R_VAL <= std_logic_vector(to_unsigned((to_integer(unsigned(CONV_RGB_HDMI(23 downto 16))) * (LINE_FRAC)) + (to_integer(unsigned(PRV_COLOR(23 downto 16))) * (256 - LINE_FRAC)),16));
		--G_VAL <= std_logic_vector(to_unsigned((to_integer(unsigned(CONV_RGB_HDMI(15 downto 8))) * (LINE_FRAC)) + (to_integer(unsigned(PRV_COLOR(15 downto 8))) * (256 - LINE_FRAC)),16));
		--B_VAL <= std_logic_vector(to_unsigned((to_integer(unsigned(CONV_RGB_HDMI(7 downto 0))) * (LINE_FRAC)) + (to_integer(unsigned(PRV_COLOR(7 downto 0))) * (256 - LINE_FRAC)),16));
		
		--R_VAL <= std_logic_vector(to_unsigned(LINE_X, 8)) & X"00";
		--G_VAL <= std_logic_vector(to_unsigned(LINE_X, 8)) & X"00";
		--B_VAL <= std_logic_vector(to_unsigned(LINE_X, 8)) & X"00";
		
--		if(LINE_TMP(7 downto 0) /= X"00") then
--		R_VAL <= PRV_COLOR(23 downto 16) & X"00";
--		G_VAL <= std_logic_vector(to_unsigned((to_integer(unsigned(CONV_RGB_HDMI(23 downto 16))) * LINE_FRAC) + (to_integer(unsigned(PRV_COLOR(23 downto 16))) * (256 - LINE_FRAC)),16));
--		B_VAL <= CONV_RGB_HDMI(23 downto 16) & X"00";
--		else
--			R_VAL <= (others => '0');
--			G_VAL <= (others => '0');
--			B_VAL <= (others => '0');
--		end if;

		
		
		
		--interpolation
		--if(PRV_LINE_INT /= LINE_INT) then
		--	PRV_COLOR <= CONV_RGB_HDMI_PRV;
		--end if;
		
		LINE_P1 := std_logic_vector(to_unsigned(PRV_LINE_X_1, 20));
		LINE_P2 := std_logic_vector(to_unsigned(PRV_LINE_X_2, 20));
		
		if(LINE_P1(19 downto 8) /= LINE_P2(19 downto 8)) then
			CONV_RGB_HDMI_PRV <= CONV_RGB_HDMI_CUR;
			CONV_RGB_HDMI_CUR <= CONV_RGB_HDMI;
		end if;
		
		--PRV_LINE_INT := LINE_INT;
		--PRV_LINE_FRAC := LINE_FRAC;
		PRV_LINE_X_3 <= PRV_LINE_X_2;
		PRV_LINE_X_2 <= PRV_LINE_X_1;
		PRV_LINE_X_1 <= PRV_LINE_X;
		PRV_LINE_X   <= LINE_X;
		
		--Scanlines
		if(SCANMODE = 0) then
			SCAN_FACTOR := 256;
		elsif(SCANMODE = 1) then
			SCAN_FACTOR := SCANFACTOR + 1;
		elsif(SCANMODE = 2) then
			--Adaptive
			--SCAN_FACTOR := ((to_integer(unsigned(R_VAL_NOINT(15 downto 8)))+to_integer(unsigned(G_VAL_NOINT(15 downto 8)))+to_integer(unsigned(B_VAL_NOINT(15 downto 8))))*43 + 127*256)/256;
			SCAN_FACTOR := ((to_integer(unsigned(R_VAL(15 downto 8)))+to_integer(unsigned(G_VAL(15 downto 8)))+to_integer(unsigned(B_VAL(15 downto 8))))*43 + 127*256)/256;
		end if;
		
		R_SCAN := std_logic_vector(to_unsigned(to_integer(unsigned(R_VAL(15 downto 8))) * SCAN_FACTOR,16));
		G_SCAN := std_logic_vector(to_unsigned(to_integer(unsigned(G_VAL(15 downto 8))) * SCAN_FACTOR,16));
		B_SCAN := std_logic_vector(to_unsigned(to_integer(unsigned(B_VAL(15 downto 8))) * SCAN_FACTOR,16));
		
		
		
		if(LINE_Y_TMP(0) = '0') then
			HDMI_RGB_D <= R_VAL(15 downto 8) & G_VAL(15 downto 8) & B_VAL(15 downto 8);
		else
			HDMI_RGB_D <= 	R_SCAN(15 downto 8) & G_SCAN(15 downto 8) & B_SCAN(15 downto 8);
		end if;
		
--		DTEST := std_logic_vector(to_unsigned(CART_DE_X,24));
--		
--		HDMI_RGB_D <=  DTEST(0) & "0000000" & 
--							DTEST(1) & "0000000" & 
--							DTEST(2) & "0000000";

		if(MODE720P = '1') then
			HV_END := HV_END_720P;
			HS_STA := HS_STA_720P;
			HS_END := HS_END_720P;
			H_END  := H_END_720P;
			
			VV_END  := VV_END_720P;
			VS_STA1 := VS_STA1_720P;
			VS_END1 := VS_END1_720P;
			V_END1  := V_END1_720P;
		elsif(SQUAREMODE = '1') then
			HV_END := HV_END_4_3;
			HS_STA := HS_STA_4_3;
			HS_END := HS_END_4_3;
			H_END := H_END_4_3;
			
			VV_END := VV_END_4_3;
			VS_STA1 := VS_STA1_4_3;
			VS_END1 := VS_END1_4_3;
			V_END1 := V_END1_4_3;
		else
			HV_END := HV_END_16_9;
			HS_STA := HS_STA_16_9;
			HS_END := HS_END_16_9;
			H_END := H_END_16_9;
			
			VV_END := VV_END_16_9;
			VS_STA1 := VS_STA1_16_9;
			VS_END1 := VS_END1_16_9;
			V_END1 := V_END1_16_9;
		end if;

		--Adjust here for 4_3 too
		if(MODE720P = '1') then
			if(SX < 160 or SX >= 160+960) then
				HDMI_RGB_D <= (others => '0');
				--HDMI_RGB_D(7 downto 0) <= X"FF";
			end if;
		elsif(SQUAREMODE = '0' and (SX < (70 + 16) or SX >= (70+16+512) or SY < 14 or SY > (13 + 224*2))) then
			HDMI_RGB_D <= (others => '0');
			--HDMI_RGB_D(23 downto 16) <= X"FF";
		end if;
		
		--Red
		if(NEW_VSYNC = '0' and SYNCLINE = '1') then
			HDMI_RGB_D(23 downto 16) <= X"FF";
		end if;
		
		--Green
--		if(SY >= VS_STA and SY <= VS_END) then
--			HDMI_RGB_D(15 downto 8) <= X"FF";
--		end if;
		
		--Blue
--		if(SX >= HS_STA and SX <= HS_END) then
--			HDMI_RGB_D(7 downto 0) <= X"FF";
--		end if;

		NEW_VSYNC_P <= NEW_VSYNC;
		
		VSYNC_L2 <= CART_VSYNC;
		NEW_VSYNC <= VSYNC_L2;
		
		VS_STA := VS_STA1;
		VS_END := VS_END1;
		V_END := V_END1;
		
	
	--check here old
	if(   (SX = HS_STA - 1 and SQUAREMODE = '0')
	   or (SX = HS_STA + 64 and SQUAREMODE = '1')) then
			if(MODE720P = '1') then
--				LINE_Y_CNT <= LINE_Y_CNT + 1;
--				if(LINE_Y_CNT = 1) then
--					CUR_LINBUF(0) := R_LINBUF;
--					LINE_Y_CNT <= 0;
--				else
--					LINE_Y_CNT <= LINE_Y_CNT + 1;
--				end if;
--				if(SY >= VV_END) then
--					LINE_Y_CNT <= 0;
--				end if;
			elsif(LINE_Y_TMP(0) = '0') then
				CUR_LINBUF(0) := R_LINBUF;
			end if;
	end if;
	
	
--resync to vsync
--		if(false) then
		if(NEW_VSYNC_P = '1' and NEW_VSYNC = '0' and SY <= VS_STA+1 and SYNCMODE = '1') then
			SX <= 0;
			--LINE_X <= 0;
			SY <= VS_STA+1;
			LINE_Y := VS_STA+1;
			
			LINE_OV_X <= 0;
			
			LINE_OV2_X <= 0;
		else
			VSYNC_WAIT := 0;
			if(SX >= H_END) then
				SX <= 0;
				PRV_COLOR := (others => '0');
				
				--LINE_X_CNT := 0;
				--LINE_X_720 := X_OFFS_320_720P_SHARP;
			elsif(SX = HS_STA-1) then
				SX <= SX + 1;
				
				if(SYNCMODE = '1') then
					LINE_X <= LINE_X + LINE_X_STEP;
				else
					LINE_X <= 128*256;
				end if;
			
				
				if(SY >= V_END) then
					FB_R_BUF <= FB_W_BUF - NUM_BUFFERS;
					LINE_Y := 0;
					SY <= 0;
					LINE_Y_720P <= 0;
					LINE_Y_CNT := 0;
				else
					LINE_Y := LINE_Y + 1;
					LINE_Y_CNT := LINE_Y_CNT + 1;
					if(LINE_Y_CNT = 3) then
						LINE_Y_720P <= LINE_Y_720P + 1;
						LINE_Y_CNT := 0;
					end if;
					SY <= SY + 1;
				end if;
				
			else
				SX <= SX + 1;
				LINE_X <= LINE_X + LINE_X_STEP;
			end if;
		end if;
		
		if(((SX = H_END - 6) and INTERPMODE = '1')
			 or (SX = H_END - 4 and INTERPMODE = '0')
			) then
			
			if(MODE720P = '1') then
				LINE_X_TMP := X_OFFS_256_720P + X_STEP_256_720P;
				LINE_X_STEP := X_STEP_256_720P;
			elsif(SQUAREMODE = '0') then
				LINE_X_TMP := X_OFFS_256_16_9 + X_STEP_256_16_9;
				LINE_X_STEP := X_STEP_256_16_9;
			else
				LINE_X_TMP := X_OFFS_256_4_3 + X_STEP_256_4_3;
				LINE_X_STEP := X_STEP_256_4_3;
			end if;
			
			LINE_X <= LINE_X_TMP;
			--PRV_LINE_X <= LINE_X_TMP - LINE_X_STEP;
			--PRV_LINE_X_1 <= LINE_X_TMP - LINE_X_STEP * 2;
			--PRV_LINE_X_2 <= LINE_X_TMP - LINE_X_STEP * 3;
			--PRV_LINE_X_3 <= LINE_X_TMP - LINE_X_STEP * 4;
			
		end if;

		if(SX >= HS_STA and SX <= HS_END) then
			HDMI_HSYNC <= '0';
		else
			HDMI_HSYNC <= '1';
		end if;

		if(SY >= VS_STA and SY <= VS_END) then
			HDMI_VSYNC <= '0';
		else
			HDMI_VSYNC <= '1';
		end if;

		if(SX <= HV_END and SY <= VV_END) then
			HDMI_DE <= '1';
		else
			HDMI_DE <= '0';
			HDMI_RGB_D <= (others => '0');
		end if;
		
	end if;
end process;

process (CLKHDMI)
variable prv_HSYNC : std_logic;
variable prv_VSYNC : std_logic;
begin
	if(rising_edge(CLKHDMI)) then
		SX_DEBUG <= SX_DEBUG + 1;
		if(prv_HSYNC = '0' and HSYNC_GEN = '1') then
			SY_DEBUG <= SY_DEBUG + 1;
			SX_DEBUG <= 0;
		end if;
		
		if(prv_VSYNC = '0' and VSYNC_GEN = '1') then
			SY_DEBUG <= 0;
		end if;
	
		prv_HSYNC := HSYNC_GEN;
		prv_VSYNC := VSYNC_GEN;
	
	end if;

end process;

HSYNC_GEN <= '1' when SX > HS_STA_720P and SX <= HS_END_720P and MODE720P = '1' else	
				 '1' when SX > HS_STA_4_3  and SX <= HS_END_4_3  and SQUAREMODE = '1' else
				 '1' when SX > HS_STA_16_9 and SX <= HS_END_16_9 and SQUAREMODE = '0' else	
				'0';

VSYNC_GEN <= '1' when SY > VS_STA1_720P and SY <= VS_END1_720P and MODE720P = '1' else
				 '1' when SY > VS_STA1_4_3  and SY <= VS_END1_4_3  and SQUAREMODE = '1' else
				 '1' when SY > VS_STA1_16_9 and SY <= VS_END1_16_9 and SQUAREMODE = '0' else	
				'0';
				
RGB_BLANK <= '1' when SX <= HV_END_4_3 and SY <= VV_END_4_3 else '0';


csync_generation: csync PORT MAP(
	clk => CLKHDMI,
	hsync => HSYNC_GEN,
	vsync => VSYNC_GEN,
	csync => CSYNC_GEN
);

--CSYNC_GEN <= not (HSYNC_GEN xor VSYNC_GEN);


--HDMI_CLK <= CLK;
HDMI_D <=      RGB_D when RGB_OUT = '1'
			 else HDMI_RGB_D;
--HDMI_HSYNC <= '0' when SX >= HS_STA and SX <= HS_END else '1';
--HDMI_VSYNC <= '0' when SY >= VS_STA and SY <= VS_END else '1';
--HDMI_DE <= '1' when SX <= HV_END and SY <= VV_END else '0';
--CSYNC_FPGA_OUT <= CSYNC_FPGA_IN;

--ADV_CLOCK <= '0';
ADV_SAVE <= RGB_OUT;
ADV_SYNC <= '0';
ADV_CLOCK <= (not DCK_L) when RGB_OUT = '1'
				 else '0';
ADV_BLANK <= CART_BLANK;

HDMICLKSEL <= MODE720P & SQUAREMODE;

end Behavioral;

