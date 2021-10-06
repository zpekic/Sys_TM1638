----------------------------------------------------------------------------------
-- Company: @Home
-- Engineer: zpekic@hotmail.com
-- 
-- Create Date: 08/29/2020 11:13:02 PM
-- Design Name: 
-- Module Name: 
-- Project Name: 
-- Target Devices: https://store.digilentinc.com/anvyl-spartan-6-fpga-trainer-board/
-- Input devices: 
--
-- Tool Versions: ISE 14.7 (nt)
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.99 - Kinda works...
-- Additional Comments:
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
--use work.tms0800_package.all;

entity sys_tm1638_anvyl is
    Port ( 
	 				-- 100MHz on the Anvyl board
				CLK: in std_logic;
				-- Switches
				-- SW(0) -- LED display selection
				-- SW(2 downto 1) -- tracing selection
				-- SW(3)
				-- SW(4)
				-- SW(6 downto 5) -- system clock speed 
				-- SW7
				SW: in std_logic_vector(7 downto 0); 
				-- Push buttons 
				-- BTN0 - 
				-- BTN1 - 
				-- BTN2 - 
				-- BTN3 - 
				BTN: in std_logic_vector(3 downto 0); 
				-- 6 7seg LED digits
				SEG: out std_logic_vector(6 downto 0); 
				AN: out std_logic_vector(5 downto 0); 
				DP: out std_logic; 
				-- 8 single LEDs
				LED: out std_logic_vector(7 downto 0);
				--PMOD interface
				JA1: inout std_logic;
				JA2: inout std_logic;
				JA3: inout std_logic;
				JA4: inout std_logic;
				--JB1: inout std_logic;
				--JB2: buffer std_logic;
				--JB3: in std_logic;
				--JB4: inout std_logic;
				--DIP switches
				--DIP_B4, DIP_B3, DIP_B2, DIP_B1: in std_logic;
				--DIP_A4, DIP_A3, DIP_A2, DIP_A1: in std_logic;
--				-- Hex keypad
				--KYPD_COL: out std_logic_vector(3 downto 0);
				--KYPD_ROW: in std_logic_vector(3 downto 0);
				-- SRAM --
				SRAM_CS1: out std_logic;
				SRAM_CS2: out std_logic;
				SRAM_OE: out std_logic;
				SRAM_WE: out std_logic;
				SRAM_UPPER_B: out std_logic;
				SRAM_LOWER_B: out std_logic;
				Memory_address: out std_logic_vector(18 downto 0);
				Memory_data: inout std_logic_vector(15 downto 0);
				-- Red / Yellow / Green LEDs
				--LDT1G: out std_logic;
				--LDT1Y: out std_logic;
				--LDT1R: out std_logic;
				--LDT2G: out std_logic;
				--LDT2Y: out std_logic;
				--LDT2R: out std_logic;
				-- VGA
				HSYNC_O: out std_logic;
				VSYNC_O: out std_logic;
				RED_O: out std_logic_vector(3 downto 0);
				GREEN_O: out std_logic_vector(3 downto 0);
				BLUE_O: out std_logic_vector(3 downto 0);
				-- TFT
--				TFT_R_O: out std_logic_vector(7 downto 0);
--				TFT_G_O: out std_logic_vector(7 downto 0);
--				TFT_B_O: out std_logic_vector(7 downto 0);
--				TFT_CLK_O: out std_logic;
--				TFT_DE_O: out std_logic;
--				TFT_DISP_O: out std_logic;
--				TFT_BKLT_O: out std_logic;
--				TFT_VDDEN_O: out std_logic;
				-- breadboard signal connections
				BB1: inout std_logic;
				BB2: inout std_logic;
				BB3: inout std_logic;
				BB4: out std_logic;
				--BB5: inout std_logic;
				--BB6: inout std_logic;
				--BB7: inout std_logic;
				--BB8: inout std_logic;
				--BB9: inout std_logic;
				BB10: in std_logic
          );
end sys_tm1638_anvyl;

architecture Structural of sys_tm1638_anvyl is

component tm1638 is
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
end component;

-- Misc components
component fourdigitsevensegled is
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
end component;

component sn74hc4040 is
    Port ( clock : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           q : out  STD_LOGIC_VECTOR(11 downto 0));
end component;

component freqcounter is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           freq : in  STD_LOGIC;
           bcd : in  STD_LOGIC;
			  add: in STD_LOGIC_VECTOR(15 downto 0);
			  cin: in STD_LOGIC;
			  cout: out STD_LOGIC;
           value : out  STD_LOGIC_VECTOR (15 downto 0));
end component;

component debouncer8channel is
    Port ( clock : in STD_LOGIC;
           reset : in STD_LOGIC;
           signal_raw : in STD_LOGIC_VECTOR (7 downto 0);
           signal_debounced : out STD_LOGIC_VECTOR (7 downto 0));
end component;

component vga_controller is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
			  mode_tms: in STD_LOGIC;
			  offsetclk: in STD_LOGIC;
			  offsetcmd: in STD_LOGIC_VECTOR(3 downto 0);
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
			  h_valid: buffer STD_LOGIC;
			  v_valid: buffer STD_LOGIC;
			  h : buffer STD_LOGIC_VECTOR(9 downto 0);
			  v : buffer STD_LOGIC_VECTOR(9 downto 0);
			  x_valid: out STD_LOGIC;
			  y_valid: buffer STD_LOGIC;
           x : out  STD_LOGIC_VECTOR (8 downto 0);
           y : out  STD_LOGIC_VECTOR (8 downto 0));
end component;

constant color_transparent:				std_logic_vector(7 downto 0):= "11111010";
constant color_medgreen: 					std_logic_vector(7 downto 0):= "00010000";
constant color_dkgreen:						std_logic_vector(7 downto 0):= "00001000";
constant color_dkblue:						std_logic_vector(7 downto 0):= "00000010";
constant color_medred:						std_logic_vector(7 downto 0):= "01100000";
constant color_dkred:						std_logic_vector(7 downto 0):= "01000000";
constant color_ltcyan:						std_logic_vector(7 downto 0):= "00001110";
constant color_dkyellow:					std_logic_vector(7 downto 0):= "10010000";
constant color_magenta:						std_logic_vector(7 downto 0):= "01100010";

constant color_black:						std_logic_vector(7 downto 0):= "00000000";
constant color_blue,		color_ltblue:	std_logic_vector(7 downto 0):= "00000011";
constant color_green,	color_ltgreen:	std_logic_vector(7 downto 0):= "00011100";
constant color_cyan:							std_logic_vector(7 downto 0):= "00011111";
constant color_red,		color_ltred:	std_logic_vector(7 downto 0):= "11100000";
constant color_purple:						std_logic_vector(7 downto 0):= "11100011";
constant color_yellow,	color_ltyellow:std_logic_vector(7 downto 0):= "11111100";
constant color_white:						std_logic_vector(7 downto 0):= "11111111";
constant color_ltgray:						std_logic_vector(7 downto 0):= "01101110"; 
constant color_dkgray,  color_gray:		std_logic_vector(7 downto 0):= "10010010";

type table_32x8 is array (0 to 31) of std_logic_vector(7 downto 0);
constant video_color: table_32x8 := (
-- TIM-011 has a 4-color palette, here we have 4 variations of those
	color_black,	-- grayish
	color_dkgray,
	color_ltgray,
	color_white,

	color_black,	-- reddish
	color_dkred,
	color_ltred,
	color_white,

	color_black,	-- greenish
	color_dkgreen,
	color_ltgreen,
	color_white,

	color_black,	-- blueish
	color_dkblue,
	color_ltblue,
	color_white,
	
-- standard TMS9918 16-color palette (http://www.cs.columbia.edu/~sedwards/papers/TMS9918.pdf page 26) 
	color_transparent,	-- VGA does not support it, so "pinkish"
	color_black,
	color_medgreen,	
	color_ltgreen,
	
	color_dkblue,
	color_ltblue,	
	color_dkred,	
	color_cyan,	

	color_medred,
	color_ltred,
	color_dkyellow,
	color_ltyellow,

	color_dkgreen,
	color_magenta,
	color_gray,
	color_white
	);
	
type table_8x16 is array (0 to 7) of std_logic_vector(15 downto 0);
constant uartmode_debug: table_8x16 := (
	X"8001",	-- 8N1
	X"8001",
	X"8001",
	X"8001",
	X"8111",	-- 8, parity space, 1 stop
	X"8002",	-- 8, parity mark, 1 == 8, no parity, 2 stop
	X"8101",	-- 8, parity even, 1 stop
	X"8011"	-- 8, parity odd, 1 stop
);

type prescale_lookup is array (0 to 7) of integer range 0 to 65535;
constant prescale_value: prescale_lookup := (
		(96000000 / (16 * 600)),
		(96000000 / (16 * 1200)),
		(96000000 / (16 * 2400)),
		(96000000 / (16 * 4800)),
		(96000000 / (16 * 9600)),
		(96000000 / (16 * 19200)),
		(96000000 / (16 * 38400)) - 1,
		(96000000 / (16 * 57600)) - 1
	);
	
signal RESET: std_logic;

signal ram: table_32x8 := (
X"00",
X"00",
X"01",
X"01",
X"02",
X"02",
X"03",
X"03",
X"04",
X"04",
X"05",
X"05",
X"06",
X"06",
X"07",
X"07",
X"80",
X"81",
X"82",
X"83",
X"84",
X"85",
X"86",
X"87",
X"88",
X"89",
X"8A",
X"8B",
X"8C",
X"8D",
X"8E",
X"8F"
);

-- Connect to PmodUSBUART 
alias PMOD_RTS: std_logic is JA1;	
alias PMOD_RXD: std_logic is JA2;
alias PMOD_TXD: std_logic is JA3;
alias PMOD_CTS: std_logic is JA4;	

-- Connect to TM1638
alias TM_STB: std_logic is BB1;	-- BLUE
alias TM_CLK: std_logic is BB2;	-- WHITE
alias TM_DIO: std_logic is BB3;  -- GRAY
alias TM_TST: std_logic is BB4;  -- YELLOW, not for TM1638 but test only
-- 											BLACK, GND
--												ORANGE, 3.3V (use step-up voltage booster to get 5.0V) 

-- debug
signal showdigit, showdot: std_logic_vector(3 downto 0);
--signal charpat: std_logic_vector(7 downto 0);
--signal symbol_d: std_logic_vector(7 downto 0);
--signal symbol_a: std_logic_vector(12 downto 0);
signal led_debug, hex_debug, hexin_debug, hexout_debug, baudrate_debug, errcount_debug: std_logic_vector(15 downto 0);

--- frequency signals
signal freq96M: std_logic;
signal freq: std_logic_vector(11 downto 0);
alias byte_clk: std_logic is freq(4); -- 3MHz
alias debounce_clk: std_logic is freq(9);
alias digsel: std_logic_vector(1 downto 0) is freq(11 downto 10);
signal prescale_baud, prescale_power: integer range 0 to 65535;
signal freq4096, freq1, freq2, freq4, freq8, freq16, freq32, freq25M: std_logic;		

--- video sync signals
signal x_valid, y_valid: std_logic;
signal h_valid, v_valid : std_logic;
signal tim_window, vga_window, vga_hsync, vga_vsync, vga_sel, h_sync, v_sync: std_logic;
signal vga_x: std_logic_vector(8 downto 0); -- 512 pixels horizontally
signal vga_y: std_logic_vector(8 downto 0); -- 512 pixels vertically (either 256 or 384 are used)
signal vga_a: std_logic_vector(14 downto 0);
signal h, v: std_logic_vector(9 downto 0);
alias col: std_logic_vector(6 downto 0) is h(9 downto 3);
alias row: std_logic_vector(6 downto 0) is v(9 downto 3);
-- video data signals
signal vga_color, text_color, window_color: std_logic_vector(7 downto 0);
signal pair, color_sel: std_logic_vector(1 downto 0); -- 2 bit pixel and color lookup
signal char, pattern: std_logic_vector(7 downto 0);
signal text_pix: std_logic;
signal color_index: std_logic_vector(4 downto 0);
signal nibble: std_logic_vector(3 downto 0);

-- video memory bus
signal vram_dina, vram_doutb: std_logic_vector(7 downto 0);
signal vram_addra, vram_addrb: std_logic_vector(14 downto 0);
signal vram_wea: std_logic_vector(0 downto 0);

-- input by switches and buttons
signal switch, button: std_logic_vector(7 downto 0);
alias switch_hexout:	std_logic is switch(0);
alias switch_tms: std_logic is switch(1);
alias switch_timpalette: std_logic_vector(1 downto 0) is switch(3 downto 2);
alias switch_tmmode: std_logic_vector(1 downto 0) is switch(3 downto 2);
alias switch_tmbright: std_logic_vector(3 downto 0) is switch(7 downto 4);
alias switch_tmclk: std_logic_vector(2 downto 0) is switch(6 downto 4);
signal btn_command, btn_window: std_logic_vector(3 downto 0);

-- HEX common 
signal baudrate_x1, baudrate_x2, baudrate_x4, baudrate_x8: std_logic;
signal mux_clk: std_logic;

-- HEX output path
signal hexout_send, hexout_ready, hexout_nrd, hexout_nbusreq, hexout_nbusack, hexout_serout: std_logic;
signal hexout_char: std_logic_vector(7 downto 0);
signal hexout_a: std_logic_vector(15 downto 0);
signal hexout_nwait: std_logic;
alias hexout_start: std_logic is btn_command(0);

-- HEX input path
signal hexin_ready, hexin_txdready, hexin_serout: std_logic;
signal hexin_char, hexin_txdchar: std_logic_vector(7 downto 0);
signal hexin_nwr, hexin_a15, hexin_error, hexin_txdsend, hexin_zero: std_logic;
signal hexin_nwait: std_logic;

-- UART control registers
signal uart_baudsel, uart_modesel: std_logic_vector(2 downto 0);

-- Small RAM
signal ram_a: std_logic_vector(4 downto 0);
signal ram_in, ram_out: std_logic_vector(7 downto 0);
signal ram_rd, ram_wr: std_logic;

alias JA_RTS: std_logic is JA1;
alias JA_RXD: std_logic is JA2;
alias JA_TXD: std_logic is JA3;
alias JA_CTS: std_logic is JA4;

begin

-- button on Mercury board	
--RESET <= BTN(0);

-- various clock signal generation (main clock domain is derived from external clock)
freq96M <= BB10;
	
LED(3 downto 0) <= "0000";
AN(5 downto 4) <= "00";
	
clockgen: sn74hc4040 port map (
			clock => freq96M,	-- 96MHz "half-size can" crystal on Mercury baseboard
			reset => RESET,
			q => freq 
		);
		
prescale: process(freq96M, baudrate_x8, freq4096, uart_baudsel)
begin
	if (rising_edge(freq96M)) then
		if (prescale_baud = 0) then
			baudrate_x8 <= not baudrate_x8;
			prescale_baud <= prescale_value(to_integer(unsigned(uart_baudsel)));
			--prescale_baud <= prescale_value(7); -- 57600 
		else
			prescale_baud <= prescale_baud - 1;
		end if;
		if (prescale_power = 0) then
			freq4096 <= not freq4096;
			prescale_power <= (96000000 / (2 * 4096));
		else
			prescale_power <= prescale_power - 1;
		end if;
	end if;
end process;

powergen: sn74hc4040 port map (
			clock => freq4096,
			reset => RESET,
			q(5 downto 0) => open,
			q(6) => freq32,
			q(7) => freq16,
			q(8) => freq8,
			q(9) => freq4,	
			q(10) => freq2,	
			q(11) => freq1	
		);
	
baudgen: sn74hc4040 port map (
			clock => baudrate_x8,
			reset => RESET,
			q(0) => baudrate_x4, 
			q(1) => baudrate_x2,
			q(2) => baudrate_x1,
			q(11 downto 3) => open		
		);	
		
-- common clock for hex input and output processors
--hex_clk <= freq(to_integer(4 + unsigned(switch_hexclk)));
with switch_tmclk select mux_clk <=
	freq16 when "000",			-- lentissimo
	freq4096 when "001",		-- vivace
	freq(9) when "010",	-- 0.09375MHz
	freq(8) when "011",	-- 0.1875HHz
	freq(7) when "100",	-- 0.375MHz
	freq(6) when "101",	-- 0.75MHz	
	freq(5) when "110",	-- 1.50MHz	
	freq(4) when others;	-- 3.00MHz	

-- internal 50MHz clock is only used for VGA
on_clk: process(CLK)
begin
	if (rising_edge(CLK)) then
		freq25M <= not freq25M;	-- close to official 640*480 dot clock
	end if;
end process;
--	

	debounce_sw: debouncer8channel Port map ( 
		clock => debounce_clk, 
		reset => RESET,
		signal_raw => SW,
		signal_debounced => switch
	);

	debounce_btn: debouncer8channel Port map ( 
		clock => debounce_clk, 
		reset => RESET,
		signal_raw(7 downto 4) => "0000",
		signal_raw(3 downto 0) => BTN,
		signal_debounced => button
	);
	
counter: freqcounter Port map ( 
		reset => RESET,
      clk => freq1,
      freq => baudrate_x1,
		bcd => '1',
		add => X"0001",
		cin => '1',
		cout => open,
      value => baudrate_debug
	);
			
driver: tm1638 Port map ( 
		reset => RESET,
		clk => mux_clk,
		mode => switch_tmmode,
		bright(3 downto 1) => switch_tmbright(3 downto 1),
		bright(0) => '1', -- to prevent blanking
		din => ram_out,
		rd =>  ram_rd,
		dout => ram_in,
		wr => ram_wr,
		a => ram_a(3 downto 0),
		tm_clk => TM_CLK,
		tm_stb => TM_STB,
		tm_dio => TM_DIO,
		debug => led_debug
	);

LED(7) <= ram_rd;
LED(6) <= ram_wr;
LED(5) <= TM_STB;
LED(4) <= TM_CLK;
TM_TST <= mux_clk;
			
-- small RAM 16*8
ram_out <= ram(to_integer(unsigned(ram_a)));

on_ram_wr: process(ram_wr, ram_in)
begin
	if (rising_edge(ram_wr)) then
		ram(to_integer(unsigned(ram_a))) <= ram_in;
	end if;
end process;
-- 
			
-- 7 seg LED debug display		
showdigit <= "1111" when (freq(9) = '0') else "0000";						-- only light up when data inputs settled to prevent "ghosts"
showdot <= "1111" when ((hexin_error and freq8) = '1') else "0000";	-- flash the dots when hexin is erroring

leds: fourdigitsevensegled Port map ( 
			-- inputs
			data => led_debug,
			digsel => digsel,
			showdigit => showdigit,
			showdot => showdot,
			invert => '1',
			-- outputs
			anode(3) => AN(2),
			anode(2) => AN(3),
			anode(1) => AN(0),
			anode(0) => AN(1),
			segment(7) => DP,
			segment(6 downto 0) => SEG
		);
			
---
vga: vga_controller Port map ( 
		reset => RESET,
      clk => freq25M,
		mode_tms => switch_tms,
		offsetclk => freq4, 
		offsetcmd => btn_window, 
      hsync => h_sync,
      vsync => v_sync,
		h_valid => h_valid,
		v_valid => v_valid,
		h => h,
		v => v,
		x_valid => x_valid,
		y_valid => y_valid,
      x => vga_x,
      y => vga_y
	);

HSYNC_O <= h_sync;
VSYNC_O <= v_sync;

--vram: ram32k8_dualport PORT MAP(
--		-- sampler only writes
--    clka => byte_clk,
--    ena => '1',
--    wea => vram_wea,
--    addra => vram_addra,
--    dina => vram_dina,
--		-- vga only reads
--    clkb => CLK,
--    addrb => vram_addrb,
--    doutb => vram_doutb
--  );

-- HEX out processor has read access if it managed to grab bus during high VSYNC
vram_addrb <= hexout_a(14 downto 0) when (hexout_nrd = '0') else vga_a; 

tim_window <= x_valid and y_valid;
vga_window <= v_valid and h_valid;

vga_a <= vga_y(8 downto 1) & vga_x(8 downto 2) when (switch_tms = '1') else vga_y(7 downto 0) & vga_x(8 downto 2); 

-- only allow write from hexin in 0x0000 to 0x7FFF range
vram_wea <= (others => not (hexin_a15 or hexin_nwr));

-- TIM sample: pixels are stored 11003322
-- see https://github.com/zpekic/Sys_TIM-011/blob/master/Img2Tim/Img2Tim/Program.cs
with vga_x(1 downto 0) select pair <=
	vram_doutb(5 downto 4) when "00",
	vram_doutb(7 downto 6) when "01",
	vram_doutb(1 downto 0) when "10",
	vram_doutb(3 downto 2) when others;

-- V9958 sample: pixels are stored XRGBXRGB
-- high nibble contains higher x-coordinate pixel (as sampler shifts MSB <- LSB)
with vga_x(1) select nibble <=  
	vram_doutb(3 downto 0) when '1',
	vram_doutb(7 downto 4) when others;

-- index depends on the V9958 or TIM mode
color_index <= ('1' & nibble) when (switch_tms = '1') else ('0' & switch_timpalette & pair);	
	
vga_color <= video_color(to_integer(unsigned(color_index))) when (tim_window = '1') else text_color;

-- now convert to VGA 8-bit color
RED_O(3 downto 1) <= vga_color(7 downto 5);
RED_O(0) <= '0';
GREEN_O(3 downto 1) <= vga_color(4 downto 2);
GREEN_O(0) <= '0';
BLUE_O(3 downto 2) <= vga_color(1 downto 0);
BLUE_O(1 downto 0) <= "00";

--background text display for fun
--char <= (row & '0') xor (col & '0');
--
--chargen: chargen_rom Port map ( 
--		a(10 downto 3) => char,
--		a(2 downto 0) => v(2 downto 0),
--      d => pattern
--	);
--
--with h(2 downto 0) select text_pix <= 
--	pattern(7) when O"0",
--	pattern(6) when O"1",
--	pattern(5) when O"2",
--	pattern(4) when O"3",
--	pattern(3) when O"4",
--	pattern(2) when O"5",
--	pattern(1) when O"6",
--	pattern(0) when others;
	
--text_color <= color_blue when (text_pix = '1') else color_cyan;
--text_color <= h(5 downto 3) & v(5 downto 3) & h(6) & v(6);
text_color <= h(8 downto 6) & v(8 downto 6) & h(9) & v(9);
				
end;
