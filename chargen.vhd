----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    18:21:10 10/07/2021 
-- Design Name: 
-- Module Name:    chargen - Behavioral 
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity chargen is
    Port ( a : in  STD_LOGIC_VECTOR (7 downto 0);
           d : out  STD_LOGIC_VECTOR (7 downto 0));
end chargen;

architecture Behavioral of chargen is

-- adapted from https://github.com/dmadison/LED-Segment-ASCII/blob/master/7-Segment/7-Segment-ASCII_HEX.txt
type table128x8 is array (0 to 127) of std_logic_vector(7 downto 0);
constant internalseg: table128x8 := (
	X"00", --/* (00) */	-- segment pattern start 
	X"01", --/* (01) */
	X"02", --/* (02) */
	X"03", --/* (03) */
	X"04", --/* (04) */
	X"05", --/* (05) */
	X"06", --/* (06) */
	X"07", --/* (07) */
	X"08", --/* (08) */
	X"09", --/* (09) */
	X"0A", --/* (0A) */
	X"0B", --/* (0B) */
	X"0C", --/* (0C) */
	X"0D", --/* (0D) */
	X"0E", --/* (0E) */
	X"0F", --/* (0F) */
	X"10", --/* (10) */
	X"11", --/* (11) */
	X"12", --/* (12) */
	X"13", --/* (13) */
	X"14", --/* (14) */
	X"15", --/* (15) */
	X"16", --/* (16) */
	X"17", --/* (17) */
	X"18", --/* (18) */
	X"19", --/* (19) */
	X"1A", --/* (1A) */
	X"1B", --/* (1B) */
	X"1C", --/* (1C) */
	X"1D", --/* (1D) */
	X"1E", --/* (1E) */
	X"1F", --/* (1F) */ -- segment patterns end
	X"00", --/* (space) */
	X"86", --/* ! */
	X"22", --/* " */
	X"7E", --/* # */
	X"6D", --/* $ */
	X"D2", --/* % */
	X"46", --/* & */
	X"20", --/* ' */
	X"29", --/* ( */
	X"0B", --/* ) */
	X"21", --/* * */
	X"70", --/* + */
	X"10", --/* , */
	X"40", --/* - */
	X"80", --/* . */
	X"52", --/* / */
	X"3F", --/* 0 */
	X"06", --/* 1 */
	X"5B", --/* 2 */
	X"4F", --/* 3 */
	X"66", --/* 4 */
	X"6D", --/* 5 */
	X"7D", --/* 6 */
	X"07", --/* 7 */
	X"7F", --/* 8 */
	X"6F", --/* 9 */
	X"09", --/* : */
	X"0D", --/* ; */
	X"61", --/* < */
	X"48", --/* = */
	X"43", --/* > */
	X"D3", --/* ? */
	X"5F", --/* @ */
	X"77", --/* A */
	X"7C", --/* B */
	X"39", --/* C */
	X"5E", --/* D */
	X"79", --/* E */
	X"71", --/* F */
	X"3D", --/* G */
	X"76", --/* H */
	X"30", --/* I */
	X"1E", --/* J */
	X"75", --/* K */
	X"38", --/* L */
	X"15", --/* M */
	X"37", --/* N */
	X"3F", --/* O */
	X"73", --/* P */
	X"6B", --/* Q */
	X"33", --/* R */
	X"6D", --/* S */
	X"78", --/* T */
	X"3E", --/* U */
	X"3E", --/* V */
	X"2A", --/* W */
	X"76", --/* X */
	X"6E", --/* Y */
	X"5B", --/* Z */
	X"39", --/* [ */
	X"64", --/* \ */
	X"0F", --/* ] */
	X"23", --/* ^ */
	X"08", --/* _ */
	X"02", --/* ` */
	X"5F", --/* a */
	X"7C", --/* b */
	X"58", --/* c */
	X"5E", --/* d */
	X"7B", --/* e */
	X"71", --/* f */
	X"6F", --/* g */
	X"74", --/* h */
	X"10", --/* i */
	X"0C", --/* j */
	X"75", --/* k */
	X"30", --/* l */
	X"14", --/* m */
	X"54", --/* n */
	X"5C", --/* o */
	X"73", --/* p */
	X"67", --/* q */
	X"50", --/* r */
	X"6D", --/* s */
	X"78", --/* t */
	X"1C", --/* u */
	X"1C", --/* v */
	X"14", --/* w */
	X"76", --/* x */
	X"6E", --/* y */
	X"5B", --/* z */
	X"46", --/* { */
	X"30", --/* | */
	X"70", --/* } */
	X"01", --/* ~ */
	X"00" --/* (del) */
);

begin

d <= a when (a(7) = '1') else internalseg(to_integer(unsigned(a(6 downto 0))));

end Behavioral;

