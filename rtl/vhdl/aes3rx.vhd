----------------------------------------------------------------------------------
-- Company:  Czech Television
-- Engineer: Petr Nohavica
-- 
-- Create Date:    09:02:45 05/09/2009 
-- Module Name:    aes3rx - Behavioral 
-- Project Name:   AES3 minimalistic receiver
-- Target Devices: Spartan 3
-- Tool versions:  ISE 10.1
--
-- Revision: 
-- Revision 0.01 - File Created
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity aes3rx is
	generic (
		reg_width : integer := 5
	);
	port (
		clk 	: in  std_logic; -- master clock
		aes3 	: in  std_logic; -- input 
		reset : in  std_logic; -- synchronous reset
		--ch_a  : out std_logic_vector(27 downto 0) := (others => '0'); -- channel A output register
		--ch_b	: out std_logic_vector(27 downto 0) := (others => '0'); -- channel B output register
		--ch_a_w: out std_logic := '0'; -- channel A reg has new data (active high for one clk period)
		--ch_b_w: out std_logic := '0'; -- channel B reg has new data (active high for one clk period)
		sdata : out std_logic := '0';
		sclk  : out std_logic := '0';
		bsync	: out std_logic := '0'; -- block start (active high for one clk period)
		fsync	: out std_logic := '0';
		active: out std_logic := '0' -- receiver has valid data on its outputs
	);
end aes3rx;

architecture Behavioral of aes3rx is
	constant X_PREAMBLE		: std_logic_vector(7 downto 0) := "01000111";
	constant Y_PREAMBLE		: std_logic_vector(7 downto 0) := "00100111";
	constant Z_PREAMBLE		: std_logic_vector(7 downto 0) := "00010111";
	signal aes3_sync 			: std_logic_vector(3 downto 0) := (others => '0'); -- input shift reg for double sampling, change detection and input delaying
	signal change 				: std_logic := '0'; -- signal signifying a change on the input
	signal aes3_clk			: std_logic := '0'; -- recovered clock signal (actually a stream of pulses on supposed clock edges for implementation on single edge driven FFs)
	signal decoder_shift		: std_logic_vector(7 downto 0) := (others => '0');
	signal align_counter		: std_logic := '0';
	signal clk_counter 		: std_logic_vector(reg_width - 1 downto 0) := (others => '0'); -- counter for aes3 clock regeneration
	signal dur_counter		: std_logic_vector(reg_width + 1 downto 0) := (others => '0'); -- counts durration (in clk periods) of current input invariant state (i.e. from "edge to edge")
	signal upd_timer			: std_logic_vector(5 downto 0) := (others => '0'); -- timer counting input changes
	signal reg_reset			: std_logic := '0'; -- resets reg_shortest on upd_timer overflow
	signal reg_shortest		: std_logic_vector(reg_width - 1 downto 0) := (others => '1'); -- stores durration of shortest measured input invariant state
	signal reg_shortest_ref : std_logic_vector(reg_width - 1 downto 0) := (others => '1'); -- copied from reg_shortest on update counter overflows, serves as reference for aes3 clock regeneration
	
	signal sdata_int			: std_logic := '0';
	signal bsync_int			: std_logic := '0';
	signal fsync_int			: std_logic := '0';
	signal sync_ok				: std_logic := '0';
	signal active_int			: std_logic := '0';
	signal timeout				: std_logic := '1';	
begin

	------------------------------------------
	-- input_shift_reg_proc
	-- Carries out input double sampling in 
	-- order to avoid metastable states on FFs
	-- and creation of delayed signals for
	-- change detector (1 clk period) and 
	-- decoder (2 clk periods)
	------------------------------------------
	input_shift_reg_proc: process (clk)
	begin
		if clk'event and clk = '1' then
			if reset = '1' then
				aes3_sync <= (others => '0');
			else
				aes3_sync <= aes3 & aes3_sync(3 downto 1); -- synthetizes  shift reg
			end if;
		end if;
	end process;	
	
	------------------------------------------
	-- change_detect_proc
	-- Detects edge on sampled input in the 
	-- way of comparsion of delayed input 
	-- and current state on XOR gate
	------------------------------------------	
	change_detect_proc: process (clk)
	begin
		if clk'event and clk = '1' then
			if reset = '1' then
				change <= '0';
			else
				change <= '0';				
				if aes3_sync(2) /= aes3_sync(1) then
					change <= '1'; 
				end if;
			end if;
		end if;
	end process;
	
	shortest_pulse_dur_cnt_proc: process (clk)
	begin
		if clk'event and clk = '1' then
			if reset = '1' then
				reg_shortest <= (others => '1');
				dur_counter <= (others => '0');
			else
				if change = '1' then
					timeout <= '0';
					if dur_counter < reg_shortest then
						reg_shortest <= dur_counter(reg_width - 1 downto 0);
					end if;
					dur_counter <= (others => '0');
				elsif dur_counter = 2**(reg_width + 2) - 1 then
					timeout <= '1';
				else
					dur_counter <= dur_counter + 1;
				end if;
				if reg_reset = '1' then
					reg_shortest <= (others => '1');
				end if;
			end if;
		end if;
	end process;
	
	register_update_timer_proc: process (clk)
	begin
		if clk'event and clk = '1' then
			reg_reset <= '0';
			if reset = '1' then
				reg_shortest_ref <= (others => '1');
				upd_timer <= (others => '0');
			else	
				if change = '1' then 	
					if upd_timer = 63 then
						reg_shortest_ref <= reg_shortest;
						reg_reset <= '1';
					end if;
					upd_timer <= upd_timer + 1;
				end if;
			end if;
		end if;
	end process;

	aes3_clk_regen_proc: process (clk)
	begin
		if clk'event and clk = '1' then
			if reset = '1' then
				clk_counter <= (others => '0');
				aes3_clk <= '0';
			else
				clk_counter <= clk_counter - 1;
				aes3_clk <= '0';
				if change = '1' then
					clk_counter <= '0' & reg_shortest_ref(reg_width - 1 downto 1);
				elsif clk_counter = 0 then
					clk_counter <= reg_shortest_ref;
					aes3_clk <= '1';
				end if;
			end if;
		end if;
	end process;
	
	decoder_shift_reg_proc: process (clk)
	begin
		if clk'event and clk = '1' then
			if reset = '1' then
				decoder_shift <= (others => '0');
			elsif aes3_clk = '1' then
				decoder_shift <= aes3_sync(0) & decoder_shift(7  downto 1);
			end if;
		end if;
	end process;
	
	decoder_proc: process (clk)
	begin
		if clk'event and clk = '1' then
			if timeout = '1' then
				sync_ok <= '0';
			end if;
			if aes3_clk = '1' then
				align_counter <= not align_counter;
				if decoder_shift = X_PREAMBLE or decoder_shift = not X_PREAMBLE then
					fsync_int <= '1';
					bsync_int <= '0';
					sync_ok <= '1';
					align_counter <= '0';
				elsif decoder_shift = Y_PREAMBLE or decoder_shift = not Y_PREAMBLE then
					fsync_int <= '0';
					bsync_int <= '0';
					sync_ok <= '1';
					align_counter <= '0';
				elsif decoder_shift = Z_PREAMBLE or decoder_shift = not Z_PREAMBLE then
					fsync_int <= '1';
					bsync_int <= '1';
					sync_ok <= '1';
					align_counter <= '0';
				end if;
				if align_counter = '1' then
					if decoder_shift(1) = decoder_shift(0) then
						sdata_int <= '0';
					else
						sdata_int <= '1';
					end if;
				end if;
			end if;
		end if;
	end process;
	
	active_int <= sync_ok and not timeout;
	
	sclk <= align_counter and active_int;
	sdata <= sdata_int and active_int;
	fsync <= fsync_int and active_int;
	bsync <= bsync_int and active_int;
	active <= active_int;
	
	
end Behavioral;

