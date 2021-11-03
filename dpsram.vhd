----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    10:27:57 02/03/2021 
-- Design Name: 
-- Module Name:    dpsram - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity dpsram is
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
end dpsram;

architecture Behavioral of dpsram is

type state_t is (idle, reading, writing);
signal state : state_t := idle;
signal latched_read : std_logic_vector(15 downto 0);
signal PRV_WR_EN : std_logic := '0';
signal WRITE_CYCLE : std_logic := '0';
signal READ_CYCLE : std_logic := '0';
signal PRV_WR_A : std_logic_vector(18 downto 0);
signal PRV_RD_A : std_logic_vector(18 downto 0);
signal PPRV_WR_A : std_logic_vector(18 downto 0);
signal PPRV_RD_A : std_logic_vector(18 downto 0);

signal PRV_WR_D : std_logic_vector(15 downto 0);

signal WR_D_L : std_logic_vector(15 downto 0);
signal WR_A_L : std_logic_vector(18 downto 0);


--attribute keep : string;
--attribute keep of FBRAM_OEn : signal is "true";
--attribute keep of FBRAM_CEn : signal is "true";
--attribute keep of FBRAM_WEn : signal is "true";
--attribute keep of FBRAM_A : signal is "true";
--attribute keep of RD_A : signal is "true";
--attribute keep of RD_D : signal is "true";
--attribute keep of WR_A : signal is "true";
--attribute keep of WR_D : signal is "true";


begin


process (CLK)
variable state : integer range 0 to 3 := 0;
--attribute keep of state : variable is "true";
begin
	if(rising_edge(CLK)) then
		if(NRST = '0') then
			state := 0;
			FBRAM_CEn <= '1';
			FBRAM_OEn <= '1';
			FBRAM_WEn <= '1';
		else
			case state is
				when 0 =>
					WR_A_L <= WR_A;
					WR_D_L <= WR_D;
					FBRAM_A <= RD_A;
					FBRAM_OEn <= '0';
					FBRAM_WEn <= '1';
					FBRAM_CEn <= '0';
					FBRAM_D <= (others => 'Z');
					state := 1;
				when 1 =>
					
					--keep address latched
					--FBRAM_A <= RD_A;
					FBRAM_OEn <= '0';
					FBRAM_WEn <= '1';
					FBRAM_CEn <= '0';
					FBRAM_D <= (others => 'Z');
					--latched_read <= FBRAM_D;
					
					state := 2;
				when 2 =>
					latched_read <= FBRAM_D;
					FBRAM_A <= WR_A_L;
					FBRAM_OEn <= '1';
					FBRAM_WEn <= not WR_EN;
					FBRAM_CEn <= '0';
					FBRAM_D <= WR_D_L;
					
					state := 3;
				when 3 =>
					FBRAM_A <= WR_A_L;
					FBRAM_OEn <= '1';
					FBRAM_WEn <= not WR_EN;
					FBRAM_CEn <= '0';
					FBRAM_D <= WR_D_L;
					
					state := 0;
			end case;
		end if;
	end if;
end process;

RD_D <= latched_read;

end Behavioral;

