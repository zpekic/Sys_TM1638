----------------------------------------------------------------------------------
-- Company: @Home
-- Engineer: Zoltan Pekic (zpekic@hotmail.com)
-- 
-- Create Date:    15:42:44 02/20/2016 
-- Design Name: 
-- Module Name:    fourdigitsevensegled - Behavioral 
-- Project Name:   Alarm Clock
-- Target Devices: Mercury FPGA + Baseboard (http://www.micro-nova.com/mercury/)
-- Tool versions:  Xilinx ISE 14.7 (nt64)
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

entity fourdigitsevensegled is
    Port ( -- inputs
			  data : in  STD_LOGIC_VECTOR (15 downto 0);
           digsel : in  STD_LOGIC_VECTOR (1 downto 0);
           showdigit : in  STD_LOGIC_VECTOR (3 downto 0);
           showdot : in  STD_LOGIC_VECTOR (3 downto 0);
			  invert: in STD_LOGIC;
			  -- outputs
           anode : out  STD_LOGIC_VECTOR (3 downto 0);
           segment : out  STD_LOGIC_VECTOR (7 downto 0)
			 );
end fourdigitsevensegled;

architecture structural of fourdigitsevensegled is

signal internalseg, seg: std_logic_vector(7 downto 0); -- 7th is the dot!
signal dotmask, invmask: std_logic_vector(7 downto 0);
signal dot: std_logic;
signal hexdata, an: std_logic_vector(3 downto 0);

begin
---- digit selection
with digsel select
	hexdata <= 	data(3 downto 0) when "00",	
					data(7 downto 4) when "01",
					data(11 downto 8) when "10",
					data(15 downto 12) when others;
					
---- DP for each digit individually
	with digsel select dot <= 
				not showdot(0) when "00",
				not showdot(1) when "01",
				not showdot(2) when "10",
				not showdot(3) when "11",
				'0' when others;
---- decode position
	with digsel select an <=
				("111" & not showdigit(0)) when "00",
				("11" & not showdigit(1) & "1") when "01",
				("1" & not showdigit(2) & "11") when "10",
				(not showdigit(3) & "111") when "11",
				"1111" when others;
						
---- hook up the cathodes

		with hexdata select internalseg <= 
				 "11000000" when "0000",   --0
				 "11111001" when "0001",   --1
				 "10100100" when "0010",   --2
				 "10110000" when "0011",   --3
				 "10011001" when "0100",   --4
				 "10010010" when "0101",   --5
				 "10000010" when "0110",   --6
				 "11111000" when "0111",   --7
				 "10000000" when "1000",   --8
				 "10010000" when "1001",   --9
				 "10001000" when "1010",   --A
				 "10000011" when "1011",   --b
				 "11000110" when "1100",   --C
				 "10100001" when "1101",   --d
				 "10000110" when "1110",   --E
				 "10001110" when "1111",   --F
				 "11111111" when others;

	dotmask <= "10000000" when (dot = '1') else "00000000"; 
	seg <= dotmask xor internalseg;

-- invert if needed (common anode)
invmask <= X"00" when (invert = '0') else X"FF";
anode <= an xor invmask(3 downto 0);
segment <= seg xor invmask;

end structural;

