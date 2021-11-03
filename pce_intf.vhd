-- Copyright (c) 2021 Terraonion
-- This program is GPL Licensed. See COPYING for the full license.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity pce_intf is

  port
  (
    -- Cart bus
   IN_A        : in std_logic_vector(20 downto 0);
	IN_D        : inout std_logic_vector(7 downto 0);
   IN_RDn      : in std_logic;
	IN_WRn      : in std_logic;
	CART_DIR  	: out std_logic;
	CART_DOUT  	: out std_logic;
	
	JOY1			: out std_logic_vector(7 downto 0);
	JOY2			: out std_logic_vector(7 downto 0);
	INGAMERESET : out std_logic;
	
	CLK	  		: in std_logic
  );

end pce_intf;


architecture Behavioral of pce_intf is


COMPONENT pce_rom
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;



signal ROM_DATA 	: std_logic_vector(7 downto 0);
signal IN_RDn_L 	: std_logic;
signal IN_A_L		: std_logic_vector(20 downto 0);
signal IGR 			: std_logic := '0';

begin

ROM : pce_rom
  PORT MAP (
    clka => CLK,
    addra => IN_A_L(9 downto 0),
    douta => ROM_DATA
  );


process (CLK)
begin 
	if(rising_edge(CLK)) then
		CART_DOUT <= '0'; --keep the buffer enabled
		CART_DIR <= '1';	--default set buffer to IN
		IN_D <= "ZZZZZZZZ";
		
		IN_A_L <= IN_A;
		
		IN_RDn_L <= IN_RDn;	--Latch previous value to extend the read cycle for hold time
	
		if(IN_A(20) = '0' and (IN_RDn = '0' or IN_RDn_L = '0')) then
			CART_DOUT <= '0';	--enable output buffer
			CART_DIR <= '0'; --set the buffer to output
			IN_D <= ROM_DATA;
		end if;
	
	
	end if;
end process;

process (IN_WRn)
begin
	if(rising_edge(IN_WRn)) then
		if(IN_A(20 downto 0) =  "111111111111000000000") then -- joy port 1 1FFFE00
			--Remap positions LDRU NS21 -> ABSN UDLR
			JOY1 <= IN_D(5) & IN_D(7) & IN_D(6) & IN_D(4) & IN_D(3) & IN_D(2) & IN_D(1) & IN_D(0);
		end if;
		
		if(IN_A(20 downto 0) =  "111111111111000000001") then -- joy port 2 1FFFE01
			JOY2 <= IN_D(5) & IN_D(7) & IN_D(6) & IN_D(4) & IN_D(3) & IN_D(2) & IN_D(1) & IN_D(0);
		end if;
		
		if(IN_A(20 downto 0) =  "111111111111000000010") then -- reset port 1FFFE02
			IGR <= IN_D(0);
		end if;
	end if;
end process;

INGAMERESET <= IGR;

end Behavioral;

