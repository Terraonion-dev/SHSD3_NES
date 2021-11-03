-- Copyright (c) 2021 Terraonion
-- This program is GPL Licensed. See COPYING for the full license.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;
--use ieee.std_logic_unsigned.all;

library UNISIM;
use UNISIM.VComponents.all;



entity PCESD_NES is

  port
  (
    -- Cart bus
   IN_A         : in std_logic_vector(20 downto 0);
	IN_D         : inout std_logic_vector(7 downto 0);
   IN_RDn       : in std_logic;
	IN_WRn       : in std_logic;
	RDYn	  : out std_logic;
	IRQ2 	  : out std_logic;
	CART_DIR  : out std_logic;
	CART_DOUT  : out std_logic;
	
	--Clocks
	CLK50_IN	  : in std_logic;
	CLK11_2	  : in std_logic;
	
	
	--RAM
	RAM_A	  : out std_logic_vector(21 downto 0);
	RAM_D	  : inout std_logic_vector(7 downto 0);
	RAM_WEn   : out std_logic;
	RAM_OEn   : out std_logic;
	RAM_CEn   : out std_logic;
	
	--FRAMEBUFFER RAM
	FBRAM_A	  : out std_logic_vector(18 downto 0);
	FBRAM_D	  : inout std_logic_vector(15 downto 0);
	FBRAM_WEn   : out std_logic;
	FBRAM_OEn   : out std_logic;
	FBRAM_CEn   : out std_logic;

	--Comm
	MCU_CMD		: in std_logic_vector(3 downto 0);
	MCU_D		: in std_logic_vector(7 downto 0);
	MCU_CLK		: in std_logic;
	FPGA_RST : in std_logic;

	--HDMI
	HDMI_D : out std_logic_vector(23 downto 0);
	HDMI_VSYNC : out std_logic;
	HDMI_HSYNC : out std_logic;
	HDMI_DE : out std_logic;
	HDMI_CLK : out std_logic;
	EXT_CLK : in std_logic;
	
	--RGB
	ADV_BLANK : out std_logic;
	ADV_SYNC : out std_logic;
	ADV_SAVE : out std_logic;
	ADV_CLOCK : out std_logic;
	
	CSYNC_FPGA_IN : in std_logic;
	CSYNC_FPGA_OUT : out std_logic;
	
	--Audio DAC (HDMI)
	DAC_DATA	: out std_logic;
	DAC_LRCK	: out std_logic;
	DAC_BCK		: out std_logic;
	DAC_SCK		: out std_logic;
	
	--AUDIO ADC
	ADC_BCK	: out std_logic;
	ADC_DOUT : in std_logic;
	ADC_LRCK : out std_logic;
	ADC_SCKI : out std_logic;
	
	--AUDIO_DATA	: in std_logic;
	AUDIO_CLK	: out std_logic;
	--AUDIO_FRAME	: out std_logic;
	
	--EXP
	FPGA_OTHER_IRQ : in std_logic;
	FPGA_OTHER_1 : in std_logic;
	FPGA_OTHER_2 : out std_logic;
	SPI_MOSI : in std_logic;
	
	--Video Bus
	CART_HSYNC : in std_logic;
	CART_VSYNC : in std_logic;
	VD : in std_logic_vector(7 downto 0);
	SPBG : in std_logic;
	DCK : in std_logic;
	CESEL : in std_logic;
	IN_CEK : in std_logic;

	--Xtra
	FPGA_CMD	: in std_logic_vector(2 downto 0);
	FPGA_D		: out std_logic_vector(7 downto 0);
	FPGA_CLK	: in std_logic;
	FPGA_A		: out std_logic_vector(10 downto 0);
	FPGA_ACK	: in std_logic;
	FPGA_RW		: out std_logic;
	FPGA_IRQ_1 : out std_logic;
	FPGA_IRQ_2 : out std_logic;
	FPGA_IRQ_3 : out std_logic
	
	
  );
end PCESD_NES;


architecture Behavioral of PCESD_NES is

attribute keep: boolean;

COMPONENT NES_SSDS3
	PORT(
		CLKNES : IN std_logic;
		CPU_RESET : IN std_logic;
		ines_in : IN std_logic_vector(127 downto 0);
		JOYPAD1 : IN std_logic_vector(7 downto 0);
		JOYPAD2 : IN std_logic_vector(7 downto 0);
		 
		
		vga_v : OUT std_logic;
		vga_h : OUT std_logic;
		vga_r : OUT std_logic_vector(3 downto 0);
		vga_g : OUT std_logic_vector(3 downto 0);
		vga_b : OUT std_logic_vector(3 downto 0);
		vga_clk : OUT std_logic;
		
		cycle : OUT std_logic_vector(8 downto 0);
		scanline : OUT std_logic_vector(8 downto 0);
		pixel_out : OUT std_logic_vector(14 downto 0);
		pixel_clock : OUT std_logic;
		color_out : OUT std_logic_vector(5 downto 0);
		
		MemOE : OUT std_logic;
		MemWR : OUT std_logic;
		RamCS : OUT std_logic;
		MemAdr : OUT std_logic_vector(23 downto 0);
		MemDB_in : IN std_logic_vector(7 downto 0);     
		MemDB_out : OUT std_logic_vector(7 downto 0);     
		
		audio_out : OUT std_logic_vector(15 downto 0);
		audio_clock : OUT std_logic;
		AUD_MCLK : OUT std_logic;
		AUD_LRCK : OUT std_logic;
		AUD_SCK : OUT std_logic;
		AUD_SDIN : OUT std_logic
		);
	END COMPONENT;


component clk50tee
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic;
  CLK_OUT2          : out    std_logic
 );
end component;

component clknes
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic
 );
end component;


COMPONENT csync
	PORT(
		clk : IN std_logic;
		hsync : IN std_logic;
		vsync : IN std_logic;          
		csync : OUT std_logic
		);
	END COMPONENT;
	
COMPONENT pce_intf
	PORT(
		IN_A : IN std_logic_vector(20 downto 0);
		IN_RDn : IN std_logic;
		IN_WRn : IN std_logic;
		CLK : IN std_logic;    
		IN_D : INOUT std_logic_vector(7 downto 0);      
		CART_DIR : OUT std_logic;
		CART_DOUT : OUT std_logic;
		JOY1 : OUT std_logic_vector(7 downto 0);
		JOY2 : OUT std_logic_vector(7 downto 0);
		INGAMERESET : OUT std_logic
		);
	END COMPONENT;	
	
	
COMPONENT HDMI
	PORT(
		CLK100 : IN std_logic;
		CLK50 : IN std_logic;
		CLKHDMI720x480 : in STD_LOGIC;
		CLKHDMI640x480 : in STD_LOGIC;
		CLKHDMI1280x720: in STD_LOGIC;	
		COLOR_IN : IN std_logic_vector(8 downto 0);
		DCK : IN std_logic;
		CART_VSYNC : IN std_logic;
		CART_HSYNC : IN std_logic;
		CSYNC_FPGA_IN : IN std_logic;
		CART_BLANK : IN std_logic;
		MCU_CMD : IN std_logic_vector(3 downto 0);
		MCU_D : IN std_logic_vector(7 downto 0);
		MCU_CLK : IN std_logic;
		NRST : IN std_logic;    
		FBRAM_D : INOUT std_logic_vector(15 downto 0);      
		CSYNC_FPGA_OUT : OUT std_logic;
		HDMI_VSYNC : OUT std_logic;
		HDMI_HSYNC : OUT std_logic;
		HDMI_DE : OUT std_logic;
		HDMI_D : OUT std_logic_vector(23 downto 0);
		HDMI_CLK : OUT std_logic;
		ADV_BLANK : OUT std_logic;
		ADV_SYNC : OUT std_logic;
		ADV_SAVE : OUT std_logic;
		ADV_CLOCK : OUT std_logic;
		FBRAM_A : OUT std_logic_vector(18 downto 0);
		FBRAM_WEn : OUT std_logic;
		FBRAM_OEn : OUT std_logic;
		FBRAM_CEn : OUT std_logic
		);
	END COMPONENT;
	
component pll_100
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic;
  CLK_OUT2          : out    std_logic;
  CLK_OUT3          : out    std_logic;
  CLK_OUT4          : out    std_logic;
  CLK_OUT5          : out    std_logic;
  CLK_OUT6          : out    std_logic
 );
end component;

COMPONENT MIXER
	PORT(
		PCM_L : IN std_logic_vector(15 downto 0);
		PCM_R : IN std_logic_vector(15 downto 0);
		CLK11 : IN std_logic;
		DAC_DATA : OUT std_logic;
		DAC_LRCK : OUT std_logic;
		DAC_BCK : OUT std_logic;
		DAC_SCK : OUT std_logic
		);
	END COMPONENT;
	
signal VGA_R : std_logic_vector(3 downto 0);
signal VGA_G : std_logic_vector(3 downto 0);
signal VGA_B : std_logic_vector(3 downto 0);
signal VGA_H : std_logic;
signal VGA_V : std_logic;
signal VGA_CLK : std_logic;
signal RAM_AX : std_logic_vector(23 downto 0);

signal PCM_LR : std_logic_vector(15 downto 0);
signal PCM_LR_L : std_logic_vector(15 downto 0);
signal PCM_CLOCK : std_logic;

signal NES_RAM_CEn : std_logic;
signal NES_RAM_OEn : std_logic;
signal NES_RAM_WEn : std_logic;
signal NES_RAM_DOUT : std_logic_vector(7 downto 0);
signal NES_CYCLE : std_logic_vector(8 downto 0);
signal NES_SCANLINE : std_logic_vector(8 downto 0);
signal NES_PIXEL : std_logic_vector(14 downto 0);
signal NES_COLOR : std_logic_vector(5 downto 0);
signal NES_COLOR_IN : std_logic_vector(8 downto 0);
signal NES_PIXEL_CLOCK : std_logic;
signal NES_VSYNC : std_logic;
signal NES_HSYNC : std_logic;
signal NES_CSYNC : std_logic;
signal NES_BLANK : std_logic;
signal NES_CLK : std_logic;
signal NES_JOY1 : std_logic_vector(7 downto 0);
signal NES_JOY2 : std_logic_vector(7 downto 0);
signal OVSCANX : integer range 0 to 255 := 8;
signal OVSCANY : integer range 0 to 255 := 0;


signal MCU_CLK_L : std_logic;
signal MCU_D_L : std_logic_vector(7 downto 0);
signal MCU_CMD_L : std_logic_vector(3 downto 0);
signal reset_ind : std_logic := '1';
signal programming : std_logic := '0';
signal mcu_addr : std_logic_vector(21 downto 0);
signal mcu_data : std_logic_vector(7 downto 0);
signal mcu_we : std_logic := '1';
signal mcu_oe : std_logic := '1';
signal mcu_ce : std_logic := '1';

signal CLK50 : std_logic;
--signal CLK50_2 : std_logic;
--signal CLK50_1 : std_logic;
signal CLK100 : std_logic;
--signal CLKHDMIX : std_logic;
--signal CLKHDMIx1 : std_logic;
--signal CLKHDMIDIV2 : std_logic;
--signal CLKHDMIx4 : std_logic;

signal CLKHDMI16_9 : std_logic;
signal CLKHDMI4_3 : std_logic;
signal CLKHDMI720 : std_logic;

signal extracommand : std_logic_vector(7 downto 0);

signal ines_hdr : std_logic_vector(127 downto 0) := X"4E" & X"45" & X"53" & X"1A" & X"02" & X"01" & X"01" & X"00" & X"00" & X"00" & X"00" & X"00" & X"00" & X"00" & X"00" & X"00";	--super mario bros

--attribute keep of RAM_AX: signal is true;
--attribute keep of RAM_A: signal is true;
--attribute keep of NES_CYCLE: signal is true;
--attribute keep of NES_SCANLINE: signal is true;
--attribute keep of NES_PIXEL: signal is true;
--attribute keep of NES_PIXEL_CLOCK: signal is true;
--attribute keep of NES_HSYNC: signal is true;
--attribute keep of NES_VSYNC: signal is true;


begin

--clktee : clk50tee
--  port map
--   (-- Clock in ports
--    CLK_IN1 => CLK50_IN,
--    -- Clock out ports
--    CLK_OUT1 => CLK50_1,
--    CLK_OUT2 => CLK50_2);

--clk_nes : clknes
--  port map
--   (-- Clock in ports
--    CLK_IN1 => CLK50_2,
--    -- Clock out ports
--    CLK_OUT1 => NES_CLK);
	 
NES_CLK <= EXT_CLK;

I_CLK_PLL_100 : pll_100
  port map
  (
    CLK_IN1             => CLK50_IN,
    CLK_OUT1           => CLK100,
	 CLK_OUT2           => CLK50,
	 CLK_OUT3			  => CLKHDMI16_9,
	 CLK_OUT4				=> open,
	 CLK_OUT5				=> CLKHDMI4_3,
	 CLK_OUT6			  => CLKHDMI720
  );


Inst_NES_SSDS3: NES_SSDS3 PORT MAP(
		CLKNES => NES_CLK,
		CPU_RESET => FPGA_RST,
		ines_in => ines_hdr,
		joypad1 => NES_JOY1,
		joypad2 => NES_JOY2,
		vga_v => VGA_V,
		vga_h => VGA_H,
		vga_r => VGA_R,
		vga_g => VGA_G,
		vga_b => VGA_B,
		vga_clk => VGA_CLK,
		CYCLE => NES_CYCLE,
		SCANLINE => NES_SCANLINE,
		PIXEL_OUT => NES_PIXEL,
		pixel_clock => NES_PIXEL_CLOCK,
		color_out => NES_COLOR,
		MemOE => NES_RAM_OEn,
		MemWR => NES_RAM_WEn,
		RamCS => NES_RAM_CEn,
		MemAdr => RAM_AX,
		MemDB_in => RAM_D,
		MemDB_out => NES_RAM_DOUT,
		audio_out => PCM_LR,
		audio_clock => PCM_CLOCK,
		AUD_MCLK => open,
		AUD_LRCK => open,
		AUD_SCK => open,
		AUD_SDIN => open 
	);
	
Inst_MIXER: MIXER PORT MAP(
		CLK11 => CLK11_2,
		PCM_L => PCM_LR_L,
		PCM_R => PCM_LR_L,
		DAC_DATA => DAC_DATA,
		DAC_LRCK => DAC_LRCK,
		DAC_BCK => DAC_BCK,
		DAC_SCK => DAC_SCK
	);

Inst_pce_intf: pce_intf PORT MAP(
		IN_A => IN_A,
		IN_D => IN_D,
		IN_RDn => IN_RDn,
		IN_WRn => IN_WRn,
		CART_DIR => CART_DIR,
		CART_DOUT => CART_DOUT,
		JOY1 => NES_JOY1,
		JOY2 => NES_JOY2,
		INGAMERESET => FPGA_OTHER_2,
		CLK => CLK50
	);
	
Inst_HDMI: HDMI PORT MAP(
		CLK100 => CLK100,
		CLK50 => CLK50,
		CLKHDMI720x480 => CLKHDMI16_9,
		CLKHDMI640x480 => CLKHDMI4_3,
		CLKHDMI1280x720 => CLKHDMI720,
		COLOR_IN => NES_COLOR_IN,
		DCK => not NES_PIXEL_CLOCK,
		CART_VSYNC => not NES_VSYNC,
		CART_HSYNC => not NES_HSYNC,
		CSYNC_FPGA_IN => not NES_CSYNC,
		CSYNC_FPGA_OUT => CSYNC_FPGA_OUT,
		CART_BLANK => not NES_BLANK,
		HDMI_VSYNC => HDMI_VSYNC,
		HDMI_HSYNC => HDMI_HSYNC,
		HDMI_DE => HDMI_DE,
		HDMI_D => HDMI_D,
		HDMI_CLK => HDMI_CLK,
		ADV_BLANK => ADV_BLANK,
		ADV_SYNC => ADV_SYNC,
		ADV_SAVE => ADV_SAVE,
		ADV_CLOCK => ADV_CLOCK,
		MCU_CMD => MCU_CMD,
		MCU_D => MCU_D,
		MCU_CLK => MCU_CLK,
		FBRAM_A => FBRAM_A,
		FBRAM_D => FBRAM_D,
		FBRAM_WEn => FBRAM_WEn,
		FBRAM_OEn => FBRAM_OEn,
		FBRAM_CEn => FBRAM_CEn,
		NRST => not FPGA_RST
	);


RAM_A <= RAM_AX(21 downto 0) when programming = '0' else mcu_addr;
RAM_CEn <= NES_RAM_CEn when programming = '0' else '0';
RAM_OEn <= NES_RAM_OEn when programming = '0' else mcu_oe;
RAM_WEn <= NES_RAM_WEn when programming = '0' else mcu_we;
RAM_D <= NES_RAM_DOUT when NES_RAM_OEn = '1' and programming = '0' else
			mcu_data when mcu_we = '0' and programming = '1' else
			"ZZZZZZZZ";


process (NES_PIXEL_CLOCK)
constant nes_vstart : integer := 243;
constant nes_vend : integer := nes_vstart + 2;
constant nes_hstart : integer := 279;
constant nes_hend : integer := 304;

begin
	if(rising_edge(NES_PIXEL_CLOCK)) then
		if(to_integer(unsigned(NES_SCANLINE)) >= nes_vstart and to_integer(unsigned(NES_SCANLINE)) <= nes_vend) then
			NES_VSYNC <= '1';
		else 
			NES_VSYNC <= '0';
		end if;
		if(to_integer(unsigned(NES_CYCLE)) >= nes_hstart and to_integer(unsigned(NES_CYCLE)) <= nes_hend) then
			NES_HSYNC <= '1';
		else
			NES_HSYNC <= '0';
		end if;
		
		if(to_integer(unsigned(NES_SCANLINE)) < 240) and (to_integer(unsigned(NES_CYCLE)) < 256) then
			NES_BLANK <= '0';
		else
			NES_BLANK <= '1';
		end if;

		if(to_integer(unsigned(NES_CYCLE)) >= OVSCANX and to_integer(unsigned(NES_CYCLE)) <= 255-OVSCANX and
			to_integer(unsigned(NES_SCANLINE)) >= OVSCANY and to_integer(unsigned(NES_SCANLINE)) <= 240-OVSCANY) then
			NES_COLOR_IN <= "000" & NES_COLOR;
		else
			NES_COLOR_IN <= "000" & "111111";
		end if;
				
	end if;
end process;

process (NES_CLK)
begin
	if(rising_edge(NES_CLK) and PCM_CLOCK = '1') then
		PCM_LR_L <= "00" & PCM_LR(15 downto 2);
	end if;
end process;

csync_gen: csync PORT MAP(
	clk => NES_PIXEL_CLOCK,
	hsync => NES_HSYNC,
	vsync => NES_VSYNC,
	csync => NES_CSYNC
);

MCU_COMMAND : process(CLK50)
variable PRV_MCU_CLK : std_logic := '0';
begin
	if rising_edge(CLK50) then
	
		MCU_CLK_L <= MCU_CLK;
		MCU_D_L <= MCU_D;
		MCU_CMD_L <= MCU_CMD;
	
		if(MCU_CLK_L = '1') then
			case MCU_CMD_L is
				when X"1" => --set mode
					reset_ind <= MCU_D_L(2);
					programming <= MCU_D_L(0);
				when X"2" => --set addr l
					mcu_addr <= mcu_addr(21 downto 8) & MCU_D_L;
				when X"3" => --set addr_m
					mcu_addr <= mcu_addr(21 downto 16) & MCU_D_L & mcu_addr(7 downto 0);
				when X"4" => --set addr_h
					mcu_addr <= MCU_D_L(5 downto 0) & mcu_addr(15 downto 0);
				when X"5" => --set data
					mcu_data <= MCU_D_L;
				when X"6" => --set control
						mcu_oe <= not MCU_D_L(0);
						mcu_we <= not MCU_D_L(1);
						mcu_ce <= not MCU_D_L(4);
						if(MCU_D_L(2) = '1' and PRV_MCU_CLK = '0')	then --increment addr
							mcu_addr <= std_logic_vector(unsigned(mcu_addr) + 1);
						end if;
--				when X"A" =>
--					fpga_status <= MCU_D_L;
				when X"E" => -- execute extracommand
					case extracommand is
						when X"C0" => OVSCANX <= to_integer(unsigned(MCU_D_L));
						when X"C1" => OVSCANY <= to_integer(unsigned(MCU_D_L));
					
						when X"D0" => ines_hdr(127 downto 120) <= MCU_D_L;
						when X"D1" => ines_hdr(119 downto 112) <= MCU_D_L;
						when X"D2" => ines_hdr(111 downto 104) <= MCU_D_L;
						when X"D3" => ines_hdr(103 downto 96) <= MCU_D_L;
						when X"D4" => ines_hdr(95 downto 88) <= MCU_D_L;
						when X"D5" => ines_hdr(87 downto 80) <= MCU_D_L;
						when X"D6" => ines_hdr(79 downto 72) <= MCU_D_L;
						when X"D7" => ines_hdr(71 downto 64) <= MCU_D_L;
						when X"D8" => ines_hdr(63 downto 56) <= MCU_D_L;
						when X"D9" => ines_hdr(55 downto 48) <= MCU_D_L;
						when X"DA" => ines_hdr(47 downto 40) <= MCU_D_L;
						when X"DB" => ines_hdr(39 downto 32) <= MCU_D_L;
						when X"DC" => ines_hdr(31 downto 24) <= MCU_D_L;
						when X"DD" => ines_hdr(23 downto 16) <= MCU_D_L;
						when X"DE" => ines_hdr(15 downto 8) <= MCU_D_L;
						when X"DF" => ines_hdr(7 downto 0) <= MCU_D_L;
						
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

IRQ2 <= '0';
RDYn <= '0';
FPGA_A <= (others => '0');
AUDIO_CLK <= '1';
FPGA_IRQ_1 <= '0';
FPGA_IRQ_2 <= '0';
FPGA_IRQ_3 <= '0';
FPGA_RW <= '0';
FPGA_D <= (others => '0');
ADC_LRCK <= '0';
ADC_SCKI <= '0';
ADC_BCK <= '0';

end Behavioral;
