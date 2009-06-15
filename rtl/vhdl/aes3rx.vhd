----------------------------------------------------------------------------------
-- Company:  Czech Television
-- Engineer: Petr Nohavica
-- 
-- Create Date:    09:02:45 05/09/2009 
-- Module Name:    aes3rx - Behavioral 
-- Project Name:   AES3 minimalistic receiver
-- Target Devices: Spartan 3
-- Tool versions:  ISE 10.1
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

Library UNISIM;
use UNISIM.vcomponents.all;

entity aes3rx is
   generic (
      reg_width : integer := 4
   );
   port (
      clk_in: in  std_logic; -- master clock
      aes3  : in  std_logic; -- input 
      reset : in  std_logic; -- synchronous reset
      
      sdata : out std_logic := '0'; -- output serial data
      sclk  : out std_logic := '0'; -- output serial data clock
      bsync : out std_logic := '0'; -- block start (high when Z subframe is being transmitted)
      lrck  : out std_logic := '0'; -- frame sync (high for channel A, low for B)
      active: out std_logic := '0'  -- receiver has valid data on its outputs
   );
end aes3rx;

architecture Behavioral of aes3rx is
   constant X_PREAMBLE     : std_logic_vector(7 downto 0) := "01000111"; -- X preamble bit sequence
   constant Y_PREAMBLE     : std_logic_vector(7 downto 0) := "00100111"; -- Y preamble bit sequence
   constant Z_PREAMBLE     : std_logic_vector(7 downto 0) := "00010111"; -- Z preamble bit sequence
   
   signal aes3_sync        : std_logic_vector(3 downto 0) := (others => '0'); -- input shift reg for double sampling, change detection and input delaying
   signal change           : std_logic := '0'; -- signal signifying a change on the input
   signal aes3_clk         : std_logic := '0'; -- recovered clock signal (actually a stream of pulses on supposed clock edges for implementation on single edge driven FFs)
   signal decoder_shift    : std_logic_vector(7 downto 0) := (others => '0'); -- decoder shift reg for preamble detection and logical state decoding
   signal align_counter    : std_logic := '0'; -- 1 bit counter reset on preamble detection, provides correct bit alignement for decoder
   signal clk_counter      : std_logic_vector(reg_width - 1 downto 0) := (others => '0'); -- counter for aes3 clock regeneration
   signal sync_timer       : std_logic_vector(5 downto 0) := (others => '0'); -- timer counting input changes
   signal reg_clk_period   : std_logic_vector(reg_width - 1 downto 0) := (others => '1'); -- copied from reg_shortest on update counter overflow, serves as reference for aes3 clock regeneration
   signal sync_lost        : std_logic := '1';
   signal preamble_detected: std_logic := '0';

   signal sdata_int        : std_logic := '0'; -- internal sdata signal
   signal bsync_int        : std_logic := '0'; -- internal bsync signal
   signal lrck_int         : std_logic := '0'; -- internal lrck signal
   
   signal clk : std_logic := '0';
   
begin

   clk <= clk_in;

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
     
   aes3_clk_feedback: process (clk)
   begin
      if clk'event and clk = '1' then
         if reset = '1' then
            reg_clk_period <= (others => '1');
            sync_timer <= (others => '0');
         else	
            if preamble_detected = '1' then
               sync_timer <= (others => '0');
               sync_lost <= '0';
            elsif aes3_clk = '1' then 	
               if sync_timer = 63 then
                  if sync_lost = '1' then
                     reg_clk_period <= reg_clk_period - 1;
                  else
                     reg_clk_period <= (others => '1');
                     sync_lost <= '1';                                  
                  end if;
               end if;
               sync_timer <= sync_timer + 1;
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
               clk_counter <= '0' & reg_clk_period(reg_width - 1 downto 1);
            elsif clk_counter = 0 then
               clk_counter <= reg_clk_period;
               aes3_clk <= '1';
            end if;
         end if;
      end if;
   end process;

   ------------------------------------------
   -- decoder_shift_reg_proc
   -- Eight bit shift register for preamble
   -- detection and decoder functionality.
   ------------------------------------------   
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
   
   ------------------------------------------
   -- decoder_proc
   -- Compares shift register with preamble
   -- bit sequences and when one is detected,
   -- accoridngly changes sync signals and
   -- resets bit alignment counter 
   -- (align_counter). Bits are decoded when
   -- align_counter is high.
   ------------------------------------------
   decoder_proc: process (clk)
   begin
      if clk'event and clk = '1' then
         if reset = '1' then
            lrck_int <= '0';
            bsync_int <= '0';
            align_counter <= '0';
         else
            if aes3_clk = '1' then
               align_counter <= not align_counter;
               preamble_detected <= '0';
               if decoder_shift = X_PREAMBLE or decoder_shift = not X_PREAMBLE then
                  preamble_detected <= '1';
                  align_counter <= '0';
                  bsync_int <= '0';
                  lrck_int <= '1';
               elsif decoder_shift = Y_PREAMBLE or decoder_shift = not Y_PREAMBLE then
                  preamble_detected <= '1';
                  align_counter <= '0';
                  bsync_int <= '0';
                  lrck_int <= '0';
               elsif decoder_shift = Z_PREAMBLE or decoder_shift = not Z_PREAMBLE then
                  preamble_detected <= '1';
                  align_counter <= '0';
                  bsync_int <= '1';
                  lrck_int <= '1';
               end if;
               if align_counter = '1' then
                  if decoder_shift(5) = decoder_shift(4) then
                     sdata_int <= '0';
                  else
                     sdata_int <= '1';
                  end if;
               end if;
            end if;
         end if;
      end if;
   end process;
   
   activity_eval_proc: process (clk)
   begin
      if clk'event and clk = '1' then
         active <= not sync_lost;
         sclk <= align_counter and not sync_lost;
         sdata <= sdata_int and not sync_lost;
         lrck <= lrck_int and not sync_lost;
         bsync <= bsync_int and not sync_lost;
      end if;
   end process;   
end Behavioral;

