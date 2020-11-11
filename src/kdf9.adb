-- kdf9.adb
--
-- The machine-state manipulations used by the CPU microcode.
--
-- This file is part of ee9 (V2.0r), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2015, W. Findlay; all rights reserved.
--
-- The ee9 program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Unchecked_Conversion;
--
with disassembly;
with exceptions;
with KDF9.compressed_opcodes;
with KDF9.CPU;
with KDF9.PHU_store;
with KDF9.store;
with settings;
with tracing;

use  disassembly;
use  exceptions;
use  KDF9.compressed_opcodes;
use  KDF9.CPU;
use  KDF9.PHU_store;
use  KDF9.store;
use  settings;
use  tracing;

package body KDF9 is

   C_part_scale : constant KDF9.word := 2**32;
   I_part_scale : constant KDF9.word := 2**16;

   function as_Q (the_word : KDF9.word)
   return KDF9.Q_register is
   begin
      return (
              C => KDF9.Q_part(KDF9.word'(the_word / C_part_scale)),
              I => KDF9.Q_part(KDF9.word'(the_word / I_part_scale) and Q_part_mask),
              M => KDF9.Q_part(the_word and Q_part_mask)
             );
   end as_Q;

   function as_word (the_Q : KDF9.Q_register)
   return KDF9.word is
   begin
      return KDF9.word(the_Q.C) * C_part_scale
           + KDF9.word(the_Q.I) * I_part_scale
           + KDF9.word(the_Q.M);
   end;

   function sign_extended (Q : KDF9.Q_part)
   return KDF9.word is
   begin
      return unsign(CPU.signed(resign(Q)));
   end sign_extended;

   function as_word (the_link : KDF9.code_link) return KDF9.word is
      function link_Q_part is new Ada.Unchecked_Conversion(KDF9.code_link, KDF9.Q_part);
   begin
      return KDF9.word(link_Q_part(the_link));
   end as_word;

   function as_link (the_word : KDF9.word) return KDF9.code_link is
      function Q_part_link is new Ada.Unchecked_Conversion(KDF9.Q_part, KDF9.code_link);
   begin
      return Q_part_link(KDF9.Q_part(the_word and Q_part_mask));
   end as_link;

   procedure deal_with_empty_sjns is
   begin
      raise NOUV_trap with "SJNS empty";
   end deal_with_empty_sjns;

   procedure deal_with_full_sjns is
   begin
      raise NOUV_trap with "SJNS full";
   end deal_with_full_sjns;

   procedure ensure_that_the_sjns_is_not_empty is
   begin
      if the_sjns_depth > 0 or else the_CPU_state = Director_state then return; end if;
      deal_with_empty_sjns;
   end ensure_that_the_sjns_is_not_empty;

   procedure ensure_that_the_sjns_is_not_full is
   begin
      if the_sjns_depth < 16 or else the_CPU_state = Director_state then return; end if;
      deal_with_full_sjns;
   end ensure_that_the_sjns_is_not_full;

   procedure push (the_link : in KDF9.code_point) is
      pragma Assert(the_sjns_depth < 16 or else the_CPU_state = Director_state);
   begin
      the_sjns(the_sjns_depth) := KDF9.code_link(the_link);
      the_sjns_depth := the_sjns_depth + 1;
   end push;

   function pop
   return KDF9.code_point is
      pragma Assert(the_sjns_depth > 0 or else the_CPU_state = Director_state);
   begin
      the_sjns_depth := the_sjns_depth - 1;
      return KDF9.code_point(the_sjns(the_sjns_depth));
   end pop;

   function sjns_top
   return KDF9.code_link is
      pragma Assert(the_sjns_depth > 0 or else the_CPU_state = Director_state);
   begin
      return the_sjns(the_sjns_depth-1);
   end sjns_top;

   procedure deal_with_empty_nest is
   begin
      raise NOUV_trap with "NEST too empty";
   end deal_with_empty_nest;

   procedure deal_with_full_nest is
   begin
      raise NOUV_trap with "NEST too full";
   end deal_with_full_nest;

   procedure check_whether_the_nest_holds_an_operand is
   begin
      if the_nest_depth > 0                  or else
            the_authenticity_mode = lax_mode or else
               the_CPU_state = Director_state   then
         return;
      end if;
      deal_with_empty_nest;
   end check_whether_the_nest_holds_an_operand;

   procedure check_whether_the_nest_holds_2_operands is
   begin
      if the_nest_depth > 1                  or else
            the_authenticity_mode = lax_mode or else
               the_CPU_state = Director_state   then
         return;
      end if;
      deal_with_empty_nest;
   end check_whether_the_nest_holds_2_operands;

   procedure check_whether_the_nest_holds (at_least : in KDF9.nest_depth) is
   begin
      if the_nest_depth >= at_least          or else
            the_authenticity_mode = lax_mode or else
               the_CPU_state = Director_state   then
         return;
      end if;
      deal_with_empty_nest;
   end check_whether_the_nest_holds;

   procedure ensure_that_the_nest_holds_an_operand is
   begin
      if the_nest_depth > 0                or else
            the_CPU_state = Director_state    then
         return;
      end if;
      deal_with_empty_nest;
   end ensure_that_the_nest_holds_an_operand;

   procedure ensure_that_the_nest_holds_2_operands is
   begin
      if the_nest_depth > 1                or else
            the_CPU_state = Director_state    then
         return;
      end if;
      deal_with_empty_nest;
   end ensure_that_the_nest_holds_2_operands;

   procedure ensure_that_the_nest_holds (at_least : in KDF9.nest_depth) is
   begin
      if the_nest_depth >= at_least        or else
            the_CPU_state = Director_state    then
         return;
      end if;
      deal_with_empty_nest;
   end ensure_that_the_nest_holds;

   procedure ensure_that_the_nest_has_room_for_a_result is
   begin
      if the_nest_depth < 16               or else
            the_CPU_state = Director_state    then
         return;
      end if;
      deal_with_full_nest;
   end ensure_that_the_nest_has_room_for_a_result;

   procedure ensure_that_the_nest_has_room_for_2_results is
   begin
      if the_nest_depth < 15               or else
            the_CPU_state = Director_state    then
         return;
      end if;
      deal_with_full_nest;
   end ensure_that_the_nest_has_room_for_2_results;

   procedure ensure_that_the_nest_has_room_for (at_least : in KDF9.nest_depth) is
   begin
      if 16-the_nest_depth >= at_least     or else
            the_CPU_state = Director_state    then
         return;
      end if;
      deal_with_full_nest;
   end ensure_that_the_nest_has_room_for;

   procedure push (the_word : in KDF9.word) is
      pragma Assert(16-the_nest_depth > 0 or the_CPU_state = Director_state);
   begin
      the_nest(the_nest_depth) := the_word;
      the_nest_depth := the_nest_depth + 1;
   end push;

   function pop
   return KDF9.word is
      pragma Assert(the_nest_depth > 0               or
                    the_authenticity_mode = lax_mode or
                    the_CPU_state = Director_state);
   begin
      return result : constant KDF9.word := the_nest(the_nest_depth - 1) do
         the_nest(the_nest_depth - 1) := 0;
         the_nest_depth := the_nest_depth - 1;
      end return;
   end pop;

   procedure pop is
      pragma Assert(the_nest_depth > 0               or
                    the_authenticity_mode = lax_mode or
                    the_CPU_state = Director_state);
   begin
      the_nest(the_nest_depth - 1) := 0;
      the_nest_depth := the_nest_depth - 1;
   end pop;

   function read_top
   return KDF9.word is
      pragma Assert(the_nest_depth > 0               or
                    the_authenticity_mode = lax_mode or
                    the_CPU_state = Director_state);
   begin
      return the_nest(the_nest_depth-1);
   end read_top;

   procedure write_top (the_word : in KDF9.word) is
      pragma Assert(the_nest_depth > 0               or
                    the_authenticity_mode = lax_mode or
                    the_CPU_state = Director_state);
   begin
      the_nest(the_nest_depth-1) := the_word;
   end write_top;

   procedure push (the_pair : in KDF9.pair) is
      pragma Assert(16-the_nest_depth > 1 or the_CPU_state = Director_state);
   begin
      the_nest(the_nest_depth+0) := the_pair.lsw;
      the_nest(the_nest_depth+1) := the_pair.msw;
      the_nest_depth := the_nest_depth + 2;
   end push;

   function pop
   return KDF9.pair is
      pragma Assert(the_nest_depth > 1               or
                    the_authenticity_mode = lax_mode or
                    the_CPU_state = Director_state);
   begin
      return result : constant KDF9.pair := (msw => the_nest(the_nest_depth-1),
                                             lsw => the_nest(the_nest_depth-2)) do
         the_nest(the_nest_depth-1) := 0;
         the_nest(the_nest_depth-2) := 0;
         the_nest_depth := the_nest_depth - 2;
      end return;
   end pop;

   function read_top
   return KDF9.pair is
      pragma Assert(the_nest_depth > 1               or
                    the_authenticity_mode = lax_mode or
                    the_CPU_state = Director_state);
   begin
      return (msw => the_nest(the_nest_depth-1),
              lsw => the_nest(the_nest_depth-2));
   end read_top;

   procedure write_top (the_pair : in KDF9.pair) is
      pragma Assert(the_nest_depth > 1               or
                    the_authenticity_mode = lax_mode or
                    the_CPU_state = Director_state);
   begin
      the_nest(the_nest_depth-1) := the_pair.msw;
      the_nest(the_nest_depth-2) := the_pair.lsw;
   end write_top;


--
   -- Support for Director-only operations.
--

   -- Set BA (bits D38:47), CPL (D34:35) and NOL (D24:33).
   procedure set_K1_register (setting : in KDF9.word) is
   begin
      BA  := KDF9.address(setting mod 2**10) * 2**5;
      CPL := KDF9.priority((setting / 2**12) and 2#11#);
      NOL := KDF9.address((setting / 2**14) mod 2**10) * 2**5 + 31;
   end set_K1_register;

   -- Set CPDAR (bits D32:47).
   procedure set_K2_register (setting : in KDF9.word) is
      CPDAR_Q : KDF9.Q_part := KDF9.Q_part(setting mod 2**16) and KDF9.Q_part_mask;
   begin
      for i in KDF9.buffer_number loop
         the_CPDAR(15-i) := KDF9.one_bit(CPDAR_Q mod 2);
         CPDAR_Q := CPDAR_Q / 2;
      end loop;
   end set_K2_register;

   -- Set context (bits D0:1), nest_depth (D2:6) and sjns_depth (D7:11).
   procedure set_K3_register (setting : in KDF9.word) is
   begin
      -- Save the current register values in the register bank.
      register_bank(the_context).nest := the_nest;
      register_bank(the_context).sjns := the_sjns;
      register_bank(the_context).Q_store := the_Q_store;
      -- Set the new context.
      the_context := KDF9.context(KDF9.word'(setting / 2**46));
      the_nest_depth := KDF9.nest_depth(setting / 2**41 mod 2**5);
      the_sjns_depth := KDF9.sjns_depth(setting / 2**36 mod 2**5);
      -- Restore the register values for the new context.
      the_nest := register_bank(the_context).nest;
      the_sjns := register_bank(the_context).sjns;
      the_Q_store := register_bank(the_context).Q_store;
   end set_K3_register;

   a_jiffy : constant := 1.0 / 1_048_576.0;  -- = 2 **(-20)
   type seconds is delta a_jiffy range 0.0 .. 31_622_400_000.0;  -- 1000 leapyears!

   -- Let the real elapsed time catch up with the_real_time virtual seconds.
   procedure delay_until (the_real_time : in KDF9.microseconds) is
      quantum : constant seconds := seconds(2**10) * a_jiffy;  -- ~= TR character time
      the_lag : seconds;
   begin
      the_lag := seconds(the_real_time - the_last_delay_time) * a_jiffy;
      if the_lag >= quantum then  -- More than a quantum of virtual elapsed time has passed.
         delay Duration(the_lag);
         the_last_delay_time := the_real_time;
      end if;
      the_elapsed_time := the_real_time;
   end delay_until;

   procedure delay_by (the_delay_time : in KDF9.microseconds) is
   begin
      if authentic_timing_is_wanted then
         delay_until(the_clock_time + the_delay_time);
      end if;
   end delay_by;

   -- Advance to the larger of the_CPU_time, the_elapsed_time, and the_last_delay_time.
   -- Cap the increase to prevent a spurious double-clock (RESET) interrupt in Director.
   procedure update_the_elapsed_time is
      max_elapsed_time : constant KDF9.microseconds := the_last_K4_time + 2**20 - 1;
   begin
      the_elapsed_time := KDF9.microseconds'Max(the_elapsed_time, the_last_delay_time);
      the_elapsed_time := KDF9.microseconds'Max(the_elapsed_time, the_CPU_time);
      if the_execution_mode = boot_mode and the_CPU_state = Director_state then
         the_elapsed_time := KDF9.microseconds'Min(the_elapsed_time, max_elapsed_time);
      end if;
   end update_the_elapsed_time;

   -- The virtual elapsed time.
   function the_clock_time
   return KDF9.microseconds is
   begin
      update_the_elapsed_time;
      return the_elapsed_time;
   end the_clock_time;

   procedure advance_the_clock_past (this_time : in KDF9.microseconds) is
   begin
      the_elapsed_time := KDF9.microseconds'Max(the_elapsed_time, this_time);
      update_the_elapsed_time;
      if authentic_timing_is_wanted then
         delay_until(the_elapsed_time);
      end if;
   end advance_the_clock_past;

   procedure synchronize_the_real_and_virtual_times is
   begin
      update_the_elapsed_time;
      if authentic_timing_is_wanted then
         delay_until(the_elapsed_time);
      end if;
   end synchronize_the_real_and_virtual_times;

   -- Get clock (bits D0:15) and RFIR (D16:31).
   function get_K4_operand
   return KDF9.word is

      function RFIR_in_a_word return KDF9.word is
         result : KDF9.word := 0;
      begin
         for r in the_RFIR'Range loop
            result := result*2;
            if the_RFIR(r) then
               result := result or 1;
            end if;
         end loop;
         return result;
      end RFIR_in_a_word;

      -- The KDF9's clock ticks once per 32 µs;
      --    the emulator's virtual time has a resolution of 1µs.
      time_now : constant KDF9.microseconds := the_clock_time;
      interval : KDF9.microseconds := (time_now - the_last_K4_time) / 32;
   begin
      the_last_K4_time := time_now;
      if interval >= 2**16 then
         the_RFIR(RESET_flag) := True;
         interval := interval mod 2**16;
      end if;
      return (KDF9.word(interval) * 2**16 or RFIR_in_a_word) * 2**16;
   end get_K4_operand;

   -- Get PHUi (bits D6i:6i+5, i = 0 .. 3).
   function get_K5_operand
   return KDF9.word is
   begin
      return K5_operand;
   end get_K5_operand;

   -- Get context (bits D0:1), nest_depth (D2:6) and sjns_depth (D7:11).
   function get_K7_operand
   return KDF9.word is
   begin
      return (KDF9.word(the_context)    * 2**46)
          or (KDF9.word(the_nest_depth) * 2**41)
          or (KDF9.word(the_sjns_depth) * 2**36);
   end get_K7_operand;

   procedure reset_the_internal_registers (the_new_state : in CPU_state := Director_state) is
   begin
      -- Set the state of a newly bootstrapped CPU.  ??
      the_V_bit := 0;
      the_T_bit := 0;
      CIA := (0, 0);
      CPL := 0;
      BA  := 0;
      NOL := max_address;
      the_RFIR := (others => False);
      ICR := 0;
      the_CPU_time := 0;
      the_elapsed_time := 0;
      the_last_delay_time := 0;
      the_last_K4_time := 0;
      the_CPU_state := the_new_state;
      the_CPDAR := (0 => 1, others => 0);  -- FW0 is always allocated.
   end reset_the_internal_registers;

   procedure reset_the_CPU_state is
   begin
      the_context := 0;
      for set in register_bank'Range loop
         register_bank(set) := (
                                nest    => (others => 0),
                                sjns    => (others => (0, 0)),
                                Q_store => (others => (0, 0, 0))
                               );
      end loop;
      the_nest_depth := 0;
      the_nest       := (others => 0);
      the_sjns_depth := 0;
      the_sjns       := (others => (0, 0));
      the_Q_store    := (others => (0, 0, 0));
      if the_execution_mode = program_mode then
         reset_the_internal_registers(program_state);
      else
         reset_the_internal_registers(Director_state);
      end if;
      -- Setting NIA must follow program loading, as it fetches E0 into the IWBs.
      set_NIA_to((0, 0));
   end reset_the_CPU_state;

   procedure reset_the_program_state is
   begin
      the_nest_depth := 0;
      the_nest       := (others => 0);
      the_sjns_depth := 0;
      the_sjns       := (others => (0, 0));
      the_V_bit := 0;
      the_T_bit := 0;
      the_CPDAR := (0 => 1, others => 0);  -- FW0 is always allocated.
      -- Setting NIA must follow program loading, as it fetches E0 into the IWBs.
      set_NIA_to((0, 0));
   end reset_the_program_state;

   procedure signal_interrupt (the_reason : in KDF9.interrupt_number) is
   begin
      take_note_of(the_reason,
                   ICR, CIA, the_elapsed_time,
                   (the_CPU_state = Director_state), CPL
                  );
      the_RFIR(the_reason) := True;
      case the_execution_mode is
         when boot_mode =>
            -- Interrupts are either effected or deferred to Director.
            if the_CPU_state = program_state or the_reason = RESET_flag then
               -- Effect an actual interrupt into Director.
               if the_reason = LOV_flag or the_reason = LIV_flag then
                  push(CIA);  -- Resume after LOV at the interrupted instruction.
               else
                  push(NIA);  -- Restart after the interrupted instruction.
               end if;
               BA := 0;
               the_CPU_state := Director_state;
               set_NIA_to((0, 0));
            else
               -- Defer: Director will eventually find any request left in the_RFIR.
               -- NOUV is completely suppressed in Director state.
               the_RFIR(NOUV_flag) := False;
            end if;
         when test_program_mode =>
            -- Interrupts other than LOV and RESET are ignored.
            case the_reason is
               when LOV_flag =>
                  raise LOV_trap;
               when RESET_flag =>
                  raise RESET_trap;
               when others =>
                  null;
            end case;
         when program_mode =>
            -- Interrupts other than LOV are treated as failures.
            case the_reason is
               when PR_flag =>
                  raise PR_trap;
               when FLEX_flag =>
                  raise FLEX_trap;
               when LIV_flag =>
                  raise LIV_trap;
               when NOUV_flag =>
                  raise NOUV_trap;
               when EDT_flag =>
                  raise EDT_trap;
               when OUT_flag =>
                  raise OUT_trap;
               when LOV_flag =>
                  raise LOV_trap;
               when RESET_flag =>
                  raise RESET_trap;
               when others =>
                  raise emulation_failure with "invalid RFI";
            end case;
      end case;
   end signal_interrupt;

   procedure LIV_if_user_mode (the_reason : in String := "Director-only instruction") is
   begin
      if the_CPU_state = Director_state then
         return;
      end if;
      if the_execution_mode = boot_mode then
         signal_interrupt(LIV_flag);
      else
         raise LIV_trap with the_reason;
      end if;
   end LIV_if_user_mode;

   procedure LOV_if_user_mode is
   begin
      -- LOV was TOTALLY suppressed in Director state.
      if the_CPU_state /= Director_state then
         set_NIA_to(CIA);
         signal_interrupt(LOV_flag);
      end if;
   end LOV_if_user_mode;

   procedure trap_invalid_instruction (the_message : in String := "invalid instruction") is
   begin
      LIV_if_user_mode(the_message);
      -- We get here only in Director mode.
      -- Invalid operations in Director raise a debugging exception for now.
      raise Director_failure with the_message & ": " & the_name_of(INS);
   end trap_invalid_instruction;

   procedure change_to_user_state_at (new_IAR : in KDF9.code_point) is  -- STUB
   begin
      the_CPU_state := program_state;
      set_NIA_to(new_IAR);
   end change_to_user_state_at;

   procedure increment_by_1 (the_link : in out KDF9.code_point) is
   begin
      if the_link.syllable_number < 5 then
         the_link.syllable_number := the_link.syllable_number + 1;
      else
         the_link.syllable_number := 0;
         the_link.word_number     := the_link.word_number + 1;
      end if;
   end increment_by_1;

   procedure increment_by_2 (the_link : in out KDF9.code_point) is
   begin
      if the_link.syllable_number < 4 then
         the_link.syllable_number := the_link.syllable_number + 2;
      else
         the_link.syllable_number := the_link.syllable_number - 4;
         the_link.word_number     := the_link.word_number + 1;
      end if;
   end increment_by_2;

   procedure increment_by_3 (the_link : in out KDF9.code_point) is
   begin
      if the_link.syllable_number < 3 then
         the_link.syllable_number := the_link.syllable_number + 3;
      else
         the_link.syllable_number := the_link.syllable_number - 3;
         the_link.word_number     := the_link.word_number + 1;
      end if;
   end increment_by_3;

   -- the_syllable_cache holds two instruction words, pre-split into syllables.
   -- They would have been held in IWB0 and IWB1 by Main Control in the KDF9.

   subtype syllable_cache_range is Natural range 0 .. 11;

   the_syllable_cache  : array (syllable_cache_range) of KDF9.syllable;
   the_cache_index     : syllable_cache_range := 0;
   the_cached_location : KDF9.code_location   := 0;

   -- The amount by which the_CPU_time is increased, for a refill of the_syllable_cache.
   the_instruction_fetch_time : constant := 8;  -- microseconds

   function NIA return KDF9.code_point is
   begin
      if the_cache_index > 5 then
         return (KDF9.syllable_code(the_cache_index-6), the_cached_location);
      else
         return (KDF9.syllable_code(the_cache_index), the_cached_location-1);
      end if;
   end NIA;

   function NIA_word_number return KDF9.code_location is
   begin
      if the_cache_index > 5 then
         return the_cached_location;
      else
         return the_cached_location - 1;
      end if;
   end NIA_word_number;

   procedure set_NIA_to (new_NIA : in KDF9.code_point) is
      mask  : constant := 8#377#;
      shift : constant := 8#400#;
      IWB0  : KDF9.word := fetch_word(KDF9.address(new_NIA.word_number) + 0);
      IWB1  : KDF9.word := fetch_word(KDF9.address(new_NIA.word_number) + 1);
   begin
      if new_NIA.syllable_number > 5 then
         raise RESET_trap with "syllable number > 5";
      end if;

      the_cache_index := syllable_cache_range(new_NIA.syllable_number);
      the_cached_location := new_NIA.word_number + 1;

      the_syllable_cache(5+0) := KDF9.syllable(IWB0 and mask);
      IWB0 := IWB0 / shift;
      the_syllable_cache(4+0) := KDF9.syllable(IWB0 and mask);
      IWB0 := IWB0 / shift;
      the_syllable_cache(3+0) := KDF9.syllable(IWB0 and mask);
      IWB0 := IWB0 / shift;
      the_syllable_cache(2+0) := KDF9.syllable(IWB0 and mask);
      IWB0 := IWB0 / shift;
      the_syllable_cache(1+0) := KDF9.syllable(IWB0 and mask);
      IWB0 := IWB0 / shift;
      the_syllable_cache(0+0) := KDF9.syllable(IWB0);

      the_syllable_cache(5+6) := KDF9.syllable(IWB1 and mask);
      IWB1 := IWB1 / shift;
      the_syllable_cache(4+6) := KDF9.syllable(IWB1 and mask);
      IWB1 := IWB1 / shift;
      the_syllable_cache(3+6) := KDF9.syllable(IWB1 and mask);
      IWB1 := IWB1 / shift;
      the_syllable_cache(2+6) := KDF9.syllable(IWB1 and mask);
      IWB1 := IWB1 / shift;
      the_syllable_cache(1+6) := KDF9.syllable(IWB1 and mask);
      IWB1 := IWB1 / shift;
      the_syllable_cache(0+6) := KDF9.syllable(IWB1);
   end set_NIA_to;

   procedure set_NIA_to_the_INS_target_address is
   begin
      set_NIA_to(INS.target);
   end set_NIA_to_the_INS_target_address;

   procedure set_IWB0_and_IWB1_for_a_JCqNZS_loop is
   begin
      if CIA.syllable_number = 5 then
         -- KDF9 did not actually detect this error, and the JCqNZS instruction often worked,
         --    unless broken-into by an interrupt, which returned to the word following that
         --       containing the first syllable of the JCqNZS instruction.
         -- I see no case for reproducing this behaviour.
         trap_invalid_instruction ("JCqNZS instruction at syllable 5");
      end if;
      set_NIA_to((word_number => CIA.word_number-1, syllable_number => 0));
      fetching_normally := False;
   end set_IWB0_and_IWB1_for_a_JCqNZS_loop;

   procedure go_back_to_the_start_of_IWB0 is
   begin
      the_cache_index := 0;
   end go_back_to_the_start_of_IWB0;

   procedure continue_after_JCqNZS is
   begin
      if CIA.syllable_number = 4 and the_cached_location = CIA.word_number then
         set_NIA_to((word_number => CIA.word_number+1, syllable_number => 0));
         -- Part-overlapped order-word fetch: can happen only once per instruction,
         --    and only before the instruction is executed, so no need to ADD to the_CPU_delta.
         -- The formula implements a small pseudo-random variation.
         the_CPU_delta := the_instruction_fetch_time - (the_elapsed_time and 2) / 2;
      end if;
      fetching_normally := True;
   end continue_after_JCqNZS;

   function next_order_syllable
   return KDF9.syllable;
   pragma Inline(next_order_syllable);

   function next_order_syllable
   return KDF9.syllable is
      the_next_syllable : KDF9.syllable;
   begin
      the_next_syllable := the_syllable_cache(the_cache_index);
      if the_cache_index < 11 then
         the_cache_index := the_cache_index + 1;
      elsif fetching_normally then
         set_NIA_to((word_number => CIA.word_number+1, syllable_number => 0));
         -- Part-overlapped order-word fetch: can happen only once per instruction,
         --    and only before the instruction is executed, so no need to add to the_CPU_delta.
         -- The formula implements a small pseudo-random variation.
         the_CPU_delta := the_instruction_fetch_time - (the_elapsed_time and 2) / 2;
      else
         go_back_to_the_start_of_IWB0;
      end if;
      return the_next_syllable;
   end next_order_syllable;

   procedure decode_syllable_0 (decoded : in out KDF9.decoded_order);
   pragma Inline(decode_syllable_0);

   procedure decode_syllable_1 (decoded : in out KDF9.decoded_order);
   pragma Inline(decode_syllable_1);

   procedure decode_a_jump_order (decoded : in out KDF9.decoded_order);
   pragma Inline(decode_a_jump_order);

   procedure decode_a_store_access_order (decoded : in out KDF9.decoded_order);
   pragma Inline(decode_a_store_access_order);

   procedure decode_a_set_literal_order (decoded : in out KDF9.decoded_order);
   pragma Inline(decode_a_set_literal_order);

   procedure decode_syllable_0 (decoded : in out KDF9.decoded_order) is
   begin
      decoded.syndrome := decoded.order.syllable_0 and 8#77#;
      decoded.kind := KDF9.INS_kind(decoded.order.syllable_0 / 2**6);
   end decode_syllable_0;

   procedure process_syllable_0_of_INS is
   begin
      if the_cache_index > 5 then
         CIA.word_number := the_cached_location;
         CIA.syllable_number := KDF9.syllable_code(the_cache_index-6);
      else
         CIA.word_number := the_cached_location - 1;
         CIA.syllable_number := KDF9.syllable_code(the_cache_index);
      end if;
      INS.order.syllable_0 := next_order_syllable;
      INS.syndrome := INS.order.syllable_0 and 8#77#;
      INS.kind := KDF9.INS_kind(INS.order.syllable_0 / 2**6);
   end process_syllable_0_of_INS;

   procedure decode_syllable_1 (decoded : in out KDF9.decoded_order) is
   begin
      decoded.Qq := KDF9.Q_number(decoded.order.syllable_1 / 2**4);
      decoded.Qk := KDF9.Q_number(decoded.order.syllable_1 and 8#17#);
   end decode_syllable_1;

   procedure process_syllable_1_of_INS is
   begin
      INS.order.syllable_1 := next_order_syllable;
      INS.Qq := KDF9.Q_number(INS.order.syllable_1 / 2**4);
      INS.Qk := KDF9.Q_number(INS.order.syllable_1 and 8#17#);
   end process_syllable_1_of_INS;

   syllable_nr_mask : constant := 2#111#;
   D4_mask          : constant := 2#1000#;
   D2_mask          : constant := 2#00_100_000#;
   D0_thru_3_mask   : constant := 2#11_110_000#;

   procedure decode_a_jump_order (decoded : in out KDF9.decoded_order) is
   begin
      decoded.target.syllable_number := KDF9.syllable_code(decoded.order.syllable_0 and syllable_nr_mask);
      decoded.target.word_number := KDF9.code_location(decoded.order.syllable_2)
                           + KDF9.code_location(decoded.Qk) * 2**8
                           + KDF9.code_location(decoded.order.syllable_0 and D4_mask) * 2**9;
      if (decoded.syndrome and D2_mask) /= 0 then -- not JrCq ...
         decoded.syndrome := decoded.syndrome and D0_thru_3_mask;
      else
         decoded.syndrome := (decoded.syndrome and D0_thru_3_mask) or KDF9.syllable(decoded.Qq);
      end if;
      if decoded.syndrome = EXIT_9 then
         -- The syllable part of EXIT is actually a halfword offset,
         --    so convert it to an actual syllable number.
         if decoded.target.syllable_number = 2 then
            decoded.target.syllable_number := 0;
         else
            decoded.target.syllable_number := 3;
         end if;
      end if;
   end decode_a_jump_order;

   procedure process_syllables_1_and_2_of_a_jump_order is
   begin
      process_syllable_1_of_INS;
      INS.order.syllable_2 := next_order_syllable;
      decode_a_jump_order(INS);
   end process_syllables_1_and_2_of_a_jump_order;

   D5_thru_7_mask : constant := 2#111#;
   D2_thru_4_mask : constant := 2#111000#;

   procedure decode_a_store_access_order (decoded : in out KDF9.decoded_order) is
   begin
      decoded.operand := KDF9.Q_part(decoded.order.syllable_2) + KDF9.Q_part(decoded.Qk)*2**8
                       + KDF9.Q_part((decoded.order.syllable_0 and D2_thru_4_mask)) * 2**9;
      decoded.syndrome := decoded.syndrome and D5_thru_7_mask;
   end decode_a_store_access_order;

   procedure decode_a_set_literal_order (decoded : in out KDF9.decoded_order) is
   begin
      decoded.operand := KDF9.Q_part(decoded.order.syllable_2)
                       + KDF9.Q_part(decoded.order.syllable_1)*2**8;
      decoded.syndrome := 2#100#;
   end decode_a_set_literal_order;

   procedure process_syllables_1_and_2_of_a_data_access_order is
   begin
      if (INS.syndrome and D5_thru_7_mask) < 2#100# then
         process_syllable_1_of_INS;
         INS.order.syllable_2 := next_order_syllable;
         decode_a_store_access_order(INS);
      else  -- SET n and some invalid opcodes (to be discarded later)
         INS.order.syllable_1 := next_order_syllable;
         INS.order.syllable_2 := next_order_syllable;
         decode_a_set_literal_order(INS);
      end if;
   end process_syllables_1_and_2_of_a_data_access_order;

   procedure decode_the_next_order is
   begin
      -- The CPU time is adjusted by a fudge factor to account for
      --    the instruction-fetch time being partly overlapped.
      process_syllable_0_of_INS;
      case INS.kind is
         when one_syllable_order =>
            return;
         when two_syllable_order =>
            process_syllable_1_of_INS;
         when normal_jump_order =>
            process_syllables_1_and_2_of_a_jump_order;
         when data_access_order =>
            process_syllables_1_and_2_of_a_data_access_order;
      end case;
   end decode_the_next_order;

   procedure decode (the_order : in out KDF9.decoded_order) is
   begin
      decode_syllable_0(the_order);
      case the_order.kind is
         when one_syllable_order =>
            null;
         when two_syllable_order =>
            decode_syllable_1(the_order);
         when normal_jump_order =>
            decode_syllable_1(the_order);
            decode_a_jump_order(the_order);
         when data_access_order =>
            if (the_order.syndrome and D5_thru_7_mask) < 2#100# then
               decode_syllable_1(the_order);
               decode_a_store_access_order(the_order);
            else  -- SET n and some invalid opcodes (to be discarded later)
               decode_a_set_literal_order(the_order);
            end if;
      end case;
   end decode;

   -- the_order_at_NIA gets three syllables starting at [NIA].  It is FOR DIAGNOSTIC USE ONLY!
   -- It does NOT update the CPU time properly and MUST NOT be used inside an instruxtion cycle.
   function the_order_at_NIA
   return KDF9.syllable_group is
      saved_NIA : constant KDF9.code_point := NIA;
      result    : KDF9.syllable_group;
   begin
      result.syllable_0 := next_order_syllable;
      result.syllable_1 := next_order_syllable;
      result.syllable_2 := next_order_syllable;
      set_NIA_to(saved_NIA);
      return result;
   end the_order_at_NIA;

   -- This is the initial jump from the top halfword of E0 just after loading.
   E0U : KDF9.word := 0;  -- N.B. only the upper halfword is used.

   procedure save_the_initial_jump is
   begin
      E0U := fetch_halfword(0, 0);
   end save_the_initial_jump;

   procedure restore_the_initial_jump is
   begin
       store_halfword(E0U, 0, 0);
   end restore_the_initial_jump;

   function the_initial_jump_was_corrupted
   return Boolean is
   begin
      return E0U /= fetch_halfword(0, 0);
   end the_initial_jump_was_corrupted;

   function is_an_invalid_order (decoded : KDF9.decoded_order)
   return Boolean is
   begin
      return (decoded.kind = normal_jump_order and decoded.target.syllable_number > 5)
         or else decoded.order.syllable_0 = 8#000#
            or else decoded.order.syllable_0 = 8#006#
               or else decoded.order.syllable_0 = 8#040#
                  or else decoded.order.syllable_0 = 8#046#
                     or else decoded.order.syllable_0 = 8#055#
                        or else decoded.order.syllable_0 = 8#073#
                           or else decoded.order.syllable_0 = 8#076#
                              or else decoded.order.syllable_0 = 8#150#;
   end is_an_invalid_order;

   the_signature_hash : KDF9.word := 0;

   function the_digital_signature
   return KDF9.word is
   begin
      return the_signature_hash;
   end the_digital_signature;

   function visible_state_hash
   return KDF9.word;
   pragma Inline(visible_state_hash);

   function visible_state_hash
   return KDF9.word is
      hash : KDF9.word;
   begin
      hash := rotate_word_right(the_signature_hash, 1) xor KDF9.word(ICR);
      hash := rotate_word_right(hash, 1) xor as_word(the_Q_store(INS.Qq));
      hash := rotate_word_right(hash, 1) xor as_word(the_Q_store(INS.Qk));
      if the_sjns_depth > 0 then
         for s in reverse KDF9.sjns_depth range 0 .. the_sjns_depth-1 loop
               hash := rotate_word_right(hash, 1) xor as_word(the_sjns(s));
         end loop;
      end if;
      if the_nest_depth > 0 then
         for n in reverse KDF9.nest_depth range 0 .. the_nest_depth-1 loop
               hash := rotate_word_right(hash, 1) xor the_nest(n);
         end loop;
      end if;
      return hash;
   end visible_state_hash;

   procedure update_the_digital_signature is
   begin
      the_signature_hash := visible_state_hash;
   end update_the_digital_signature;

end KDF9;
