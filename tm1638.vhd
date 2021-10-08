----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:42:59 10/02/2021 
-- Design Name: 
-- Module Name:    tm1638 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: https://www.futurashop.it/image/catalog/data/Download/TM1638_V1.3_EN.pdf
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

entity tm1638 is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           mode : in  STD_LOGIC_VECTOR (1 downto 0);
           bright : in  STD_LOGIC_VECTOR (3 downto 0);
           din : in  STD_LOGIC_VECTOR (7 downto 0);
           rd : buffer  STD_LOGIC;
           dout : out  STD_LOGIC_VECTOR (7 downto 0);
           wr : out  STD_LOGIC;
           a : out  STD_LOGIC_VECTOR (3 downto 0);
           tm_clk : buffer  STD_LOGIC;
           tm_stb : out  STD_LOGIC;
           tm_dio : inout  STD_LOGIC;
           debug : out  STD_LOGIC_VECTOR (15 downto 0));
end tm1638;

architecture Behavioral of tm1638 is

--component chargen is
--    Port ( a : in  STD_LOGIC_VECTOR (7 downto 0);
--           d : out  STD_LOGIC_VECTOR (7 downto 0));
--end component;

type table16x8 is array(0 to 15) of std_logic_vector(7 downto 0);
constant hexmap: table16x8 := (
	std_logic_vector(to_unsigned(natural(character'pos('0')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('1')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('2')), 8)), 	
	std_logic_vector(to_unsigned(natural(character'pos('3')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('4')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('5')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('6')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('7')), 8)),
	std_logic_vector(to_unsigned(natural(character'pos('8')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('9')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('A')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('B')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('C')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('D')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('E')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('F')), 8))
);

constant decmap: table16x8 := (
	std_logic_vector(to_unsigned(natural(character'pos('0')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('1')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('2')), 8)), 	
	std_logic_vector(to_unsigned(natural(character'pos('3')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('4')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('5')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('6')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('7')), 8)),
	std_logic_vector(to_unsigned(natural(character'pos('8')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('9')), 8)),	
	std_logic_vector(to_unsigned(natural(character'pos('-')), 8)), -- for calculator minus	
	std_logic_vector(to_unsigned(natural(character'pos('+')), 8)),	-- for calculator plus
	std_logic_vector(to_unsigned(natural(character'pos('o')), 8)),	-- for calculator errOr
	std_logic_vector(to_unsigned(natural(character'pos('r')), 8)),	-- for calculator eRRoR
	std_logic_vector(to_unsigned(natural(character'pos('E')), 8)),	-- for calculator Error
	std_logic_vector(to_unsigned(natural(character'pos(' ')), 8))	-- for calculator blanking
);

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

-- display modes
constant mode_binary: std_logic_vector(1 downto 0) := "00";
constant mode_hex: std_logic_vector(1 downto 0) := "01";
constant mode_decimal: std_logic_vector(1 downto 0) := "10";
constant mode_ascii: std_logic_vector(1 downto 0) := "11";
-- operations
constant opr_init: std_logic_vector(2 downto 0) := O"0";
constant opr_ainc: std_logic_vector(2 downto 0) := O"1";
constant opr_saddr: std_logic_vector(2 downto 0) := O"2";
constant opr_led: std_logic_vector(2 downto 0) := O"3";
constant opr_ledx: std_logic_vector(2 downto 0) := O"4";
constant opr_kbd: std_logic_vector(2 downto 0) := O"5";
constant opr_keyin: std_logic_vector(2 downto 0) := O"6";
constant opr_nop: std_logic_vector(2 downto 0) := O"7";

type table32x10 is array (0 to 31) of std_logic_vector(9 downto 0);
constant program: table32x10 := (
	opr_nop		& "000" & X"0", -- nop
	opr_nop		& "000" & X"0", -- nop
	opr_init		& "001" & X"0", -- output command 8<bright>
	opr_kbd		& "001" & X"0", -- read buttons command
	opr_keyin	& "010" & X"0", -- shift-in byte 0 and write to host M(0)	0	0	0	<s5>	0	0	0	<s1>
	opr_keyin	& "010" & X"2", -- shift-in byte 1 and write to host M(2)	0	0	0	<s6>	0	0	<s2>	0
	opr_keyin	& "010" & X"4", -- shift-in byte 2 and write to host M(4)	0  0	0	<s7>	0	<s3>	0	0	
	opr_keyin	& "010" & X"6", -- shift-in byte 3 and write to host M(6)	0	0	0	<s8>	<s4>	0	0	0
	opr_nop		& "000" & X"0", -- nop
	opr_nop		& "000" & X"0", -- nop
	opr_nop		& "000" & X"0", -- nop
	opr_nop		& "000" & X"0", -- nop
	opr_nop		& "000" & X"0", -- nop
	opr_nop		& "000" & X"0", -- nop
	opr_ainc		& "001" & X"0", -- output command 40 (set autoinc mode), strobe is reverse of clk_en
	opr_saddr	& "001" & X"0", -- output command C0 (set start), strobe is low
	opr_led		& "101" & X"0", -- read M(0) from host, strobe is low 
	opr_led		& "101" & X"1", -- read M(1) from host, strobe is low
	opr_led		& "101" & X"2", -- read M(2) from host, strobe is low
	opr_led		& "101" & X"3", -- read M(3) from host, strobe is low
	opr_led		& "101" & X"4", -- read M(4) from host, strobe is low
	opr_led		& "101" & X"5", -- read M(5) from host, strobe is low
	opr_led		& "101" & X"6", -- read M(6) from host, strobe is low
	opr_led		& "101" & X"7", -- read M(7) from host, strobe is low
	opr_led		& "101" & X"8", -- read M(8) from host, strobe is low
	opr_led		& "101" & X"9", -- read M(9) from host, strobe is low
	opr_led		& "101" & X"A", -- read M(A) from host, strobe is low
	opr_led		& "101" & X"B", -- read M(B) from host, strobe is low
	opr_led		& "101" & X"C", -- read M(C) from host, strobe is low 
	opr_led		& "101" & X"D", -- read M(D) from host, strobe is low
	opr_led		& "101" & X"E", -- read M(E) from host, strobe is low
	opr_ledx		& "101" & X"F"  -- read M(F) from host, strobe is high last 2 
);

constant sequence: table16x8 := (
	"00101100",   --0
	"10101100",   --1
	"00001000",   --2
	"00001000",   --3
	"00000010",   --4
	"00000010",   --5
	"00000010",   --6
	"00000010",   --7
	"00000010",   --8
	"00000010",   --9
	"00000010",   --A
	"00000010",   --b
	"00001000",   --C
	"00001000",   --d
	"01111000",   --E
	"00111000"	  --F
);

	
-- "state machine" (it is actually a linear program that executes in continous loop)	
signal cnt: std_logic_vector(8 downto 0);
alias pc: std_logic_vector(4 downto 0) is cnt(8 downto 4); -- "program counter" is simply incrementing and wrapping around
alias cycle: std_logic_vector(3 downto 0) is cnt(3 downto 0); -- each instruction is 16 clock cycles

-- determine what to do on the interface
signal i: std_logic_vector(9 downto 0);
alias i_opr: std_logic_vector(2 downto 0) is i(9 downto 7);
alias i_rd: std_logic is i(6);
alias i_wr: std_logic is i(5);
alias i_oe: std_logic is i(4); -- output enable
alias i_a: std_logic_vector(3 downto 0) is i(3 downto 0);

-- determine what signal to give out 
signal s: std_logic_vector(7 downto 0);
alias s_rd: std_logic is s(7);
alias s_wr: std_logic is s(6);
alias s_cmd: std_logic is s(5);
alias s_end: std_logic is s(4);
alias s_clkout: std_logic is s(3);
alias s_start: std_logic is s(2);
alias s_clkin: std_logic is s(1);

-- internal data
signal data_in, data_out, serin_cnt: std_logic_vector(7 downto 0);
alias dot: std_logic is data_in(7); -- assume DP always comes in directly (unless in ASCII mode)
alias nibble_lo: std_logic_vector(3 downto 0) is data_in(3 downto 0);
signal ascii: std_logic_vector(7 downto 0);

--
signal output: std_logic_vector(7 downto 0);
signal segments, dotmask: std_logic_vector(7 downto 0);

signal serin, serout: std_logic;
signal clk_en: std_logic;

begin

-- debug!
debug <= ("000" & pc & output) when (i_oe = '1') else ("000" & pc & data_out);
--debug <= ("000" & pc & output) when (i_oe = '1') else (serin_cnt & data_out);

-- clock equals input clock gated with enable (but only if not executing "nop")
with i_opr select tm_clk <=
	'1' when opr_nop,
	(clk and s_clkin) when opr_keyin,
	(clk or s_clkout) when others;
	
-- strobe depends on i_sel
with i_opr select tm_stb <=
		s_cmd when opr_init,		-- 	--____________--
		s_cmd when opr_ainc,		-- 	--____________--
		'0' when opr_saddr,		-- 	________________
		'0' when opr_led,			-- 	________________
		s_end when opr_ledx, 	-- 	______________--
		s_start when opr_kbd, 	-- 	--______________
		'0' when opr_keyin,		--		________________
		'1' when others;			-- 	----------------
		
-- tm_dio is bidirectional!
serin <= tm_dio;
tm_dio <= serout when (i_oe = '1') else 'Z';

-- use MUX instead of shift register for output
with cycle select serout <= 
	output(0) when X"4", -- LSB first
	output(1) when X"5",
	output(2) when X"6",
	output(3) when X"7",
	output(4) when X"8",
	output(5) when X"9",
	output(6) when X"A",
	output(7) when X"B",	-- MSB last
	'1' when others;

-- output depends on the i_sel
with i_opr select output <=
	X"8" & bright when opr_init, 	-- init command contains the brightness in lower nibble
	X"40" when opr_ainc,				-- autoinc mode
	X"C0" when opr_saddr,			-- start address
	segments when opr_led,			-- segments
	segments when opr_ledx,			-- segments
	X"42"	when opr_kbd,				-- read buttons
	X"00" when others;				-- ignore

-- lookup table based on the mode
with mode select ascii <= 
		("0011000" & data_in(0)) when mode_binary,	-- will map to 0x30 or 0x31
		hexmap(to_integer(unsigned(nibble_lo))) when mode_hex,
		decmap(to_integer(unsigned(nibble_lo))) when mode_decimal,
		"0" & data_in(6 downto 0) when others;		-- 128 chars in ASCII mode
--		data_in when others;									-- 256 chars in ASCII mode (32 - 127 as "chars", other direct pass-through)
		
-- combine dot with segments before sending out
dotmask <= "00000000" when (mode = mode_ascii) else (dot & "0000000"); -- in ASCII mode, dot is in the char definition 
-- if reading from odd address, interpret segment directly. This allows driving single LED digits using
-- addresses 1, 3, 5, 7, 9, 11, 13, 15
segments <= data_in when (i(0) = '1') else (dotmask or internalseg(to_integer(unsigned(ascii))));

-- convert ASCII code to segment outputs
--ascii2led: chargen port map (
--	a => ascii,
--	d => segments
--);

	
-- address comes directly from command
a <= i_a;

-- generate read early in the cycle
rd <= i_rd and s_rd;

-- generate write late in the cycle
wr <= i_wr and s_wr;

-- output data comes right from shift register when done
dout <= data_out;

-- capture data from TM1638 (keyboard)
-- see here for wacky encoding: http://www.picbasic.co.uk/forum/showthread.php?t=24154&s=ca06b9a32536c24ffaf72f2ff8d1f12e
on_tm_clk: process(tm_clk, serin, i_opr)
begin
	if (falling_edge(tm_clk)) then
		if (i_opr = opr_keyin) then
			if (serin_cnt(3) = '0') then 
				data_out <= serin & data_out(7 downto 1);
			end if;
			if (s_clkout = '1')  then
				serin_cnt <= (others => '0');
			else
				serin_cnt <= std_logic_vector(unsigned(serin_cnt) + 1);
			end if;
		else
			data_out <= data_out;
		end if;
	end if;
end process;
 
-- capture data from host
on_rd: process(rd, din)
begin
	if (falling_edge(rd)) then
		data_in <= din;
	end if;
end process;

-- drive the sequence counter
on_clk: process(reset, clk, cnt)
begin
	if (reset = '1') then
		cnt <= (others => '0');
	else
		if (rising_edge(clk)) then
			cnt <= std_logic_vector(unsigned(cnt) + 1);
		end if;
	end if;
end process;

i <= program(to_integer(unsigned(pc)));		-- 32 instructions!
s <= sequence(to_integer(unsigned(cycle)));	-- 16 states per instructions!

end Behavioral;

