-- Copyright (c) 2021 Terraonion
-- This program is GPL Licensed. See COPYING for the full license.


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_unsigned.all;

library UNISIM;
use UNISIM.VComponents.all;



entity MIXER is

  port
  (
   CLK11			: in std_logic;
	PCM_L 		: in std_logic_vector(15 downto 0);
	PCM_R 		: in std_logic_vector(15 downto 0);
	
	DAC_DATA		: out std_logic;
	DAC_LRCK		: out std_logic;
	DAC_BCK		: out std_logic;
	DAC_SCK		: out std_logic
  );
  
end MIXER;


architecture behavioral of MIXER is

attribute keep : string; 

signal clk_cnt_div : std_logic_vector(8 downto 0);
signal channel_side : std_logic;
signal current_sample : std_logic_vector(31 downto 0);
signal sample_side : std_logic;
signal sample_side_adc : std_logic;
signal rbit : std_logic;
signal bclk : std_logic;

signal current_input : std_logic_vector(63 downto 0);

--attribute keep of current_input: signal is "true";  
--attribute keep of clk_cnt_div: signal is "true";  

begin


--CLK11 is  256 times 44100, we divide by 4 so we get 64 half cycles per sample

CLOCKOUT : process(CLK11)
begin
	if(rising_edge(CLK11)) then
		clk_cnt_div <= clk_cnt_div + 1;

		--DAC
		if(clk_cnt_div(1 downto 0) = "11") then	-- / 32
			sample_side <= not sample_side;
			if(sample_side = '0') then		--falling edge, place sample output
				DAC_DATA <= current_sample(31);
				current_sample <= current_sample(30 downto 0) & current_sample(31);
			end if;
			
			DAC_SCK <= sample_side;
		end if;
		
		if(clk_cnt_div(6 downto 0) = "1111111") then	-- / 128
			channel_side <= not channel_side;
			if(channel_side = '0') then	-- 0 to 1, go to left channel, latch data
				current_sample(15 downto 0)  <= PCM_R;
				current_sample(31 downto 16) <= PCM_L;
			end if;
		
		end if;
	
	end if;

end process;

DAC_BCK <= CLK11;
DAC_LRCK <= clk_cnt_div(7);

end behavioral;
