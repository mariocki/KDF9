-- The machine-state manipulations used by the CPU microcode.
--
-- This file is part of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
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
with exceptions;
with KDF9.CPU;
with KDF9.decoding;
with KDF9.PHU_store;
with KDF9.store;
with settings;
with tracing;

use  exceptions;
use  KDF9.CPU;
use  KDF9.decoding;
use  KDF9.PHU_store;
use  KDF9.store;
use  settings;
use  tracing;

package body KDF9 is

   C_part_scale : constant := 2**32;
   I_part_scale : constant := 2**16;

   function as_Q (the_word : KDF9.word)
   return KDF9.Q_register
   is (
       (C => KDF9.Q_part(KDF9.word'(the_word / C_part_scale)),
        I => KDF9.Q_part(KDF9.word'(the_word / I_part_scale) and Q_part_mask),
        M => KDF9.Q_part(the_word and Q_part_mask)
       )
      );

   function as_word (the_Q : KDF9.Q_register)
   return KDF9.word
   is (KDF9.word(the_Q.C)*C_part_scale + KDF9.word(the_Q.I)*I_part_scale + KDF9.word(the_Q.M));

   function sign_extended (Q : KDF9.Q_part)
   return KDF9.word
   is (unsign(CPU.signed(resign(Q))));

   function as_word (the_link : KDF9.SJNS_link)
   return KDF9.word is
      function link_Q_part is new Ada.Unchecked_Conversion(KDF9.SJNS_link, KDF9.Q_part);
   begin
      return KDF9.word(link_Q_part(the_link));
   end as_word;

   function as_link (the_word : KDF9.word)
   return KDF9.SJNS_link is
      function Q_part_link is new Ada.Unchecked_Conversion(KDF9.Q_part, KDF9.SJNS_link);
   begin
      return Q_part_link(KDF9.Q_part(the_word and Q_part_mask));
   end as_link;

   procedure ensure_that_the_SJNS_is_not_empty is
   begin
      if the_SJNS_depth > 0             or else
            the_CPU_state = Director_state then
         return;
      end if;
      effect_interrupt(caused_by_NOUV, "empty SJNS");
   end ensure_that_the_SJNS_is_not_empty;

   procedure ensure_that_the_SJNS_is_not_full is
   begin
      if the_SJNS_depth < 16             or else
            the_CPU_state = Director_state  then
         return;
      end if;
      effect_interrupt(caused_by_NOUV, "full SJNS");
   end ensure_that_the_SJNS_is_not_full;

   procedure push (the_link : in KDF9.syllable_address) is
   begin
      the_SJNS(the_SJNS_depth) := KDF9.SJNS_link(the_link);
      the_SJNS_depth := the_SJNS_depth + 1;
   end push;

   function pop
   return KDF9.syllable_address is
   begin
      the_SJNS_depth := the_SJNS_depth - 1;
      return KDF9.syllable_address(the_SJNS(the_SJNS_depth));
   end pop;

   function SJNS_top
   return KDF9.SJNS_link
   is (the_SJNS(the_SJNS_depth-1));

   function operand_words_needed (need : KDF9.NEST_depth)
   return String
   is ("NEST lacks" & need'Image & " operand" & (if need > 1 then "s" else ""));

   procedure ensure_that_the_NEST_holds (at_least : in KDF9.NEST_depth) is
   begin
      if the_NEST_depth >= at_least          or else
            the_CPU_state = Director_state      then
         return;
      end if;
      effect_interrupt(caused_by_NOUV, operand_words_needed(need => at_least-the_NEST_depth));
   end ensure_that_the_NEST_holds;

   procedure ensure_that_the_NEST_holds_an_operand is
   begin
      ensure_that_the_NEST_holds (at_least => 1);
   end ensure_that_the_NEST_holds_an_operand;

   procedure ensure_that_the_NEST_holds_2_operands is
   begin
      ensure_that_the_NEST_holds (at_least => 2);
   end ensure_that_the_NEST_holds_2_operands;

   function result_space_needed (need : KDF9.NEST_depth)
   return String
      with Inline => False;

   function result_space_needed (need : KDF9.NEST_depth)
   return String
   is (if need = 1 then "full NEST" else "NEST too full for" & need'Image & " operands");

   procedure ensure_that_the_NEST_has_room_for (at_least : in KDF9.NEST_depth) is
   begin
      if the_NEST_depth <= 16-at_least     or else
            the_CPU_state = Director_state    then
         return;
      end if;
      effect_interrupt(caused_by_NOUV, result_space_needed(need => at_least - (16-the_NEST_depth)));
   end ensure_that_the_NEST_has_room_for;

   procedure ensure_that_the_NEST_has_room_for_a_result is
   begin
      ensure_that_the_NEST_has_room_for (at_least => 1);
   end ensure_that_the_NEST_has_room_for_a_result;

   procedure ensure_that_the_NEST_has_room_for_2_results is
   begin
      ensure_that_the_NEST_has_room_for (at_least => 2);
   end ensure_that_the_NEST_has_room_for_2_results;

   procedure push (the_word : in KDF9.word) is
   begin
      the_NEST(the_NEST_depth) := the_word;
      the_NEST_depth := the_NEST_depth + 1;
   end push;

   function pop
   return KDF9.word is
   begin
      return result : constant KDF9.word := the_NEST(the_NEST_depth - 1) do
         the_NEST(the_NEST_depth - 1) := 0;
         the_NEST_depth := the_NEST_depth - 1;
      end return;
   end pop;

   procedure pop is
   begin
      the_NEST(the_NEST_depth - 1) := 0;
      the_NEST_depth := the_NEST_depth - 1;
   end pop;

   function read_top
   return KDF9.word
   is (the_NEST(the_NEST_depth-1));

   procedure write_top (the_word : in KDF9.word) is
   begin
      the_NEST(the_NEST_depth-1) := the_word;
   end write_top;

   procedure push (the_pair : in KDF9.pair) is
   begin
      the_NEST(the_NEST_depth+0) := the_pair.lsw;
      the_NEST(the_NEST_depth+1) := the_pair.msw;
      the_NEST_depth := the_NEST_depth + 2;
   end push;

   function pop
   return KDF9.pair is
   begin
      return result : constant KDF9.pair := (msw => the_NEST(the_NEST_depth-1),
                                             lsw => the_NEST(the_NEST_depth-2)) do
         the_NEST(the_NEST_depth-1) := 0;
         the_NEST(the_NEST_depth-2) := 0;
         the_NEST_depth := the_NEST_depth - 2;
      end return;
   end pop;

   procedure pop_pair is
   begin
      the_NEST(the_NEST_depth-1) := 0;
      the_NEST(the_NEST_depth-2) := 0;
      the_NEST_depth := the_NEST_depth - 2;
   end pop_pair;

   function read_top
   return KDF9.pair
   is ((msw => the_NEST(the_NEST_depth-1), lsw => the_NEST(the_NEST_depth-2)));

   procedure write_top (the_pair : in KDF9.pair) is
   begin
      the_NEST(the_NEST_depth-1) := the_pair.msw;
      the_NEST(the_NEST_depth-2) := the_pair.lsw;
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
      CPDAR_Q : KDF9.Q_part := as_Q(setting).M;
   begin
      for i in KDF9.buffer_number loop
         the_CPDAR(i) := (CPDAR_Q mod 2) = 1;
         CPDAR_Q := CPDAR_Q / 2;
      end loop;
   end set_K2_register;

   -- Set context (bits D0:1), NEST_depth (D2:6) and SJNS_depth (D7:11).

   procedure set_K3_register (setting : in KDF9.word) is
   begin
      -- Save the current register values in the register bank.
      register_bank(the_context).NEST := the_NEST;
      register_bank(the_context).SJNS := the_SJNS;
      register_bank(the_context).Q_store := the_Q_store;
      -- Set the new context.
      the_context := KDF9.context(KDF9.word'(setting / 2**46));
      the_NEST_depth := KDF9.NEST_depth(setting / 2**41 mod 2**5);
      the_SJNS_depth := KDF9.SJNS_depth(setting / 2**36 mod 2**5);
      -- Restore the register values for the new context.
      the_NEST := register_bank(the_context).NEST;
      the_SJNS := register_bank(the_context).SJNS;
      the_Q_store := register_bank(the_context).Q_store;
   end set_K3_register;

   a_microsecond : constant := 1.0 / 2.0**20;

   type seconds is delta a_microsecond range 0.0 .. 1000.0*365.2425*24.0*3600.0;  -- 1000 years!

   procedure update_the_elapsed_time;

   -- Let the real elapsed time catch up with the_real_time virtual seconds.

   procedure delay_until (the_real_time : in KDF9.us) is
      a_jiffy : constant seconds := seconds(2**10) * a_microsecond;  -- ca. TR character-read time of 1ms
      the_lag : seconds;
   begin
       if the_real_time < the_last_delay_time then
          the_last_delay_time := the_real_time;
       end if;
      the_lag := seconds(the_real_time - the_last_delay_time) * a_microsecond;
      if the_lag >= a_jiffy then  -- More than a a_jiffy of virtual elapsed time has passed.
         delay Duration(the_lag);
         the_last_delay_time := the_real_time;
      end if;
     -- the_elapsed_time := the_real_time;
      update_the_elapsed_time;
   end delay_until;

   procedure delay_by (the_delay_time : in KDF9.us) is
   begin
      if authentic_timing_is_enabled then
         delay_until(the_clock_time + the_delay_time);
      end if;
   end delay_by;

   -- Advance to the larger of the_CPU_time, the_elapsed_time, and the_last_delay_time.
   -- Cap the increase to prevent a spurious double-clock (RESET) interrupt in Director.

   procedure update_the_elapsed_time is
      max_elapsed_time : constant KDF9.us := the_last_K4_time + 2**20 - 1;
   begin
      the_elapsed_time := KDF9.us'Max(the_elapsed_time, the_last_delay_time);
      the_elapsed_time := KDF9.us'Max(the_elapsed_time, the_CPU_time);
      if the_execution_mode = boot_mode and the_CPU_state = Director_state then
         the_elapsed_time := KDF9.us'Min(the_elapsed_time, max_elapsed_time);
      end if;
   end update_the_elapsed_time;

   -- The virtual elapsed time.

   function the_clock_time
   return KDF9.us is
   begin
      update_the_elapsed_time;
      return the_elapsed_time;
   end the_clock_time;

   procedure advance_the_clock (past : in KDF9.us) is
   begin
      the_elapsed_time := KDF9.us'Max(the_elapsed_time, past);
      update_the_elapsed_time;
      if authentic_timing_is_enabled then
         delay_until(the_elapsed_time);
      end if;
   end advance_the_clock;

   procedure synchronize_the_real_and_virtual_times is
   begin
      if authentic_timing_is_enabled then
         update_the_elapsed_time;
         delay_until(the_elapsed_time);
      end if;
   end synchronize_the_real_and_virtual_times;

   -- Get clock (bits D0:15) and RFIR (D16:31).

   function get_K4_operand
   return KDF9.word is

      function RFIR_in_a_word
      return KDF9.word is
         result : KDF9.word := 0;
      begin
         for r of the_RFIR loop
            result := result*2;
            if r then
               result := result or 1;
            end if;
         end loop;
         return result;
      end RFIR_in_a_word;

      -- The KDF9's interval timing clock ticks once per 32 µs;
      --    the emulator virtual time has a resolution of 1 µs.

      time_now : constant KDF9.us := the_clock_time;
      interval : constant KDF9.us := (time_now - the_last_K4_time);

   begin
      the_last_K4_time := time_now;
      if interval / 32 >= 2**16 then
         effect_interrupt(caused_by_RESET, "double clock");
         the_RFIR(caused_by_RESET ) := True;
      elsif interval / 32 >= 2**15 then
         effect_interrupt(caused_by_CLOCK, "time since a K4" & interval'Image & "us");
         the_RFIR(caused_by_CLOCK ) := True;  --?? why is this needed?
      end if;
      return (KDF9.word(interval / 32) * 2**32) or (RFIR_in_a_word * 2**16);
   end get_K4_operand;

   -- Get PHUi (bits D6i:6i+5, i = 0 .. 3).

   function get_K5_operand
   return KDF9.word
   is (K5_operand);

   -- Get context (bits D0:1), NEST_depth (D2:6) and SJNS_depth (D7:11).

   function get_K7_operand
   return KDF9.word
   is (
       (KDF9.word(the_context)    * 2**46) or
       (KDF9.word(the_NEST_depth) * 2**41) or
       (KDF9.word(the_SJNS_depth) * 2**36)
      );

   procedure reset_V_and_T is
   begin
      the_V_bit_is_set := False;
      the_T_bit_is_set := False;
   end reset_V_and_T;

   procedure reset_the_internal_registers (the_new_state : in CPU_state) is
   begin
      -- Set the state of a newly bootstrapped CPU.
      reset_V_and_T;
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
      the_CPDAR := (0 => True, others => False);  -- FW0 is always allocated.
   end reset_the_internal_registers;

   empty_NEST : constant NEST := (others => 0);
   empty_SJNS : constant SJNS := (others => (0, 0));
   empty_Q_s  : constant Q_store := (others => (0, 0, 0));

   procedure reset_the_CPU_state (initial_entry : KDF9.syllable_address) is
   begin
      the_context := 0;
      for bank of register_bank loop
         bank := (NEST => empty_NEST, SJNS => empty_SJNS, Q_store => empty_Q_s);
      end loop;
      the_NEST_depth := 0;
      the_NEST       := empty_NEST;
      the_SJNS_depth := 0;
      the_SJNS       := empty_SJNS;
      the_Q_store    := empty_Q_s;
      if the_execution_mode = program_mode then
         reset_the_internal_registers(program_state);
      else
         reset_the_internal_registers(Director_state);
      end if;
      -- Setting NIA must follow program loading, as it fetches E0 into the IWBs.
      set_NIA_to(initial_entry);
   end reset_the_CPU_state;

   procedure reset_the_program_state is
   begin
      the_NEST_depth := 0;
      the_NEST       := empty_NEST;
      the_SJNS_depth := 0;
      the_SJNS       := empty_SJNS;
      reset_V_and_T;
      the_CPDAR := (0 => True, others => False);  -- FW0 is always allocated.
      -- Setting NIA must follow program loading, as it fetches E0 into the IWBs.
      set_NIA_to((0, 0));
   end reset_the_program_state;

   procedure effect_interrupt (caused_by_this : in KDF9.interrupt_number; message : in String) is
      return_address : KDF9.syllable_address;
   begin
      take_note_of_interrupt(caused_by_this, message);
      the_RFIR(caused_by_this) := True;
      case the_execution_mode is
         when boot_mode =>
            -- Interrupts are either actioned or deferred to Director.
            if the_CPU_state = program_state or else caused_by_this = caused_by_RESET  then
               -- Action an actual interrupt into Director.
               if caused_by_this in caused_by_LOV  | caused_by_OUT then
                  return_address := CIA;  -- Restart the interrupted instruction.
               else
                  return_address := NIA;  -- Proceed after the interrupted instruction.
               end if;
               if the_SJNS_depth < 16 then
                  push(return_address);                  -- The program link fits into the SJNS.
               else
                  JB := KDF9.SJNS_link(return_address);  -- The program link overwrites JB.
               end if;
               BA := 0;
               fetching_normally := True;
               set_NIA_to((0, 0));
               the_CPU_state := Director_state;
               raise abandon_this_order;
            else
               -- Defer: Director will eventually find any request left in the_RFIR.
               -- NOUV is completely suppressed in Director state.
               the_RFIR(caused_by_NOUV) := False;
            end if;

         when privileged_mode =>
            -- Interrupts other than LOV and RESET are ignored.
            -- There is no need to accurately emulate the address placed by the hardware in JB.
            case caused_by_this is
               when caused_by_LOV  =>
                  raise LOV_exception with message;
               when caused_by_RESET  =>
                  raise RESET_exception with message;
               when others =>
                  null;
            end case;

         when program_mode =>
            -- Interrupts other than LOV are treated as failures.
            -- There is no need to accurately emulate the address placed by the hardware in JB.
            case caused_by_this is
               when caused_by_PR =>
                  raise PR_exception with message;
               when caused_by_FLEX =>
                  raise FLEX_exception with message;
               when caused_by_LIV =>
                  raise LIV_exception with message;
               when caused_by_NOUV =>
                  raise NOUV_exception with message;
               when caused_by_EDT =>
                  raise EDT_exception with message;
               when caused_by_OUT =>
                  raise OUT_exception with message;
               when caused_by_LOV  =>
                  raise LOV_exception with message;
               when caused_by_RESET  =>
                  raise RESET_exception with message;
               when others =>
                  raise emulation_failure with "invalid RFI in effect_interrupt";
            end case;
      end case;
   end effect_interrupt;

   procedure effect_clock_interrupt (interval : in KDF9.us)
      with Inline => False;

   procedure effect_clock_interrupt (interval : in KDF9.us) is
   begin
      effect_interrupt(caused_by_CLOCK, interval'Image & " KDF9 us");
   end effect_clock_interrupt;

   procedure check_for_a_clock_interrupt is
      interval : KDF9.us;
   begin
      -- Clock ticks are ignored in program_mode and privileged_mode.
      -- In boot_mode:
      --    they are actioned in program_state;
      --    they are deferred in Director_state: Director will eventually find the time for itself.
      if the_execution_mode = boot_mode and then
            the_CPU_state = program_state   then
         interval := (the_clock_time - the_last_K4_time);
         if interval >= 2**20 then
            effect_clock_interrupt(interval);
         end if;
      end if;
   end check_for_a_clock_interrupt;

   procedure fail_in_problem_program_state is
   begin
      case the_execution_mode is
         when program_mode =>
            -- The unprivileged program has attempted a privileged operation.
            raise LIV_exception with "%Director-only instruction";
         when privileged_mode =>
            -- The privileged program is allowed to use privileged instructions.
            return;
         when boot_mode =>
            if the_CPU_state = program_state then
               -- Punt the error to Director.
               effect_interrupt(caused_by_LIV, "Director-only instruction");
            else
               -- All privileged operations are permitted to Director.
               return;
            end if;
      end case;
   end fail_in_problem_program_state;

   procedure LOV_if_user_mode (cause : in String) is
   begin
      -- LOV was TOTALLY suppressed in Director state.
      if the_CPU_state /= Director_state then
         set_NIA_to(CIA);
         effect_interrupt(caused_by_LOV, cause);
      end if;
   end LOV_if_user_mode;

   -- The %  prepended to the_message aids parsing of exception error messages in failure shutdown.

   procedure trap_illegal_instruction (the_message : in String := "invalid opcode") is
   begin
      -- The program has failed in a manner that could cause a LIV interrupt.
      case the_execution_mode is
         when program_mode
            | privileged_mode =>
            raise LIV_exception with "%" & the_message;
         when boot_mode =>
            if the_CPU_state = program_state then
               -- Punt the problem to Director.
               effect_interrupt(caused_by_LIV, the_message);
            else
               -- The Director itself has gone seriously wrong.
               -- LIV is impossible in Director, so ee9 takes responsibility for stopping the run
               --    to avert consequential emulation failure.
               raise Director_failure with "%" & the_message;
            end if;
      end case;
   end trap_illegal_instruction;

   procedure trap_operator_error (the_message : in String) is
   begin
      -- The program has failed for a reason, such as a misconfigured environment,
      --    that is beyond its control and prevents further execution.
      raise operator_error with "%" & the_message;
   end trap_operator_error;

   procedure trap_unimplemented_feature (the_message : in String) is
   begin
      -- The program has attempted to use something that ee9 does not (yet) support.
      raise not_yet_implemented with "%" & the_message;
   end trap_unimplemented_feature;

   procedure trap_failing_OUT (OUT_number : in KDF9.word; the_message : in String) is
      OUT_name : constant String := OUT_number'Image;
   begin
      -- The program has issued an invalid OUT.
      raise OUT_error with "%" & OUT_name(2..OUT_name'Last) & ": " & the_message;
   end trap_failing_OUT;

   procedure trap_invalid_paper_tape (the_message : in String) is
   begin
      -- The paper tape file given to load or boot has defects.
      raise invalid_paper_tape_file with "%" & the_message;
   end trap_invalid_paper_tape;

   procedure return_from_Director_to (new_IAR : in KDF9.syllable_address) is
   begin
      the_CPU_state := program_state;
      set_NIA_to(new_IAR);
   end return_from_Director_to;

   procedure increment_by_1 (the_link : in out KDF9.syllable_address) is
   begin
      if the_link.syllable_index < 5 then
         the_link.syllable_index := the_link.syllable_index + 1;
      else
         the_link.syllable_index := 0;
         the_link.order_word_number     := the_link.order_word_number + 1;
      end if;
   end increment_by_1;

   procedure increment_by_2 (the_link : in out KDF9.syllable_address) is
   begin
      if the_link.syllable_index < 4 then
         the_link.syllable_index := the_link.syllable_index + 2;
      else
         the_link.syllable_index := the_link.syllable_index - 4;
         the_link.order_word_number     := the_link.order_word_number + 1;
      end if;
   end increment_by_2;

   procedure increment_by_3 (the_link : in out KDF9.syllable_address) is
   begin
      if the_link.syllable_index < 3 then
         the_link.syllable_index := the_link.syllable_index + 3;
      else
         the_link.syllable_index := the_link.syllable_index - 3;
         the_link.order_word_number     := the_link.order_word_number + 1;
      end if;
   end increment_by_3;

   -- the_syllable_cache holds two instruction words, pre-split into syllables.
   -- They would have been held in IWB0 and IWB1 by Main Control in the KDF9.

   subtype syllable_cache_range is Natural range 0 .. 11;

   the_syllable_cache  : array (syllable_cache_range) of KDF9.syllable;
   the_cache_index     : syllable_cache_range   := 0;
   the_cached_location : KDF9.order_word_number := 0;

   function NIA
   return KDF9.syllable_address
   is (
       if the_cache_index > 5 then
          (the_cached_location, KDF9.syllable_index(the_cache_index-6))
       else
          (the_cached_location-1, KDF9.syllable_index(the_cache_index))
      );

   function NIA_word_number
   return KDF9.order_word_number
   is (the_cached_location - (if the_cache_index > 5 then 0 else 1));

   procedure trap_an_invalid_order_address (new_NIA : in KDF9.syllable_address)
      with Inline => False;

   procedure trap_an_invalid_order_address (new_NIA : in KDF9.syllable_address) is
   begin
      if new_NIA.syllable_index = 6 then
         effect_interrupt(caused_by_RESET, "syllable number = 6");
      elsif new_NIA.syllable_index = 7 then
         effect_interrupt(caused_by_RESET, "syllable number = 7");
      else
         effect_interrupt(caused_by_LIV, "jump to 8191");  -- See EE Report K/GD y 82.
      end if;
   end trap_an_invalid_order_address;

   procedure set_NIA_to (new_NIA : in KDF9.syllable_address) is
      mask        : constant := 8#377#;
      shift       : constant := 8#400#;
      IWB0, IWB1  : KDF9.word;
   begin
      if new_NIA.order_word_number = 8191 or else
            new_NIA.syllable_index > 5       then
         trap_an_invalid_order_address(new_NIA);
      end if;

      IWB0 := fetch_word(KDF9.address(new_NIA.order_word_number) + 0);
      IWB1 := fetch_word(KDF9.address(new_NIA.order_word_number) + 1);

      the_cache_index := syllable_cache_range(new_NIA.syllable_index);
      the_cached_location := new_NIA.order_word_number + 1;

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
      set_NIA_to((order_word_number => CIA.order_word_number-1, syllable_index => 0));
      fetching_normally := False;
   end set_IWB0_and_IWB1_for_a_JCqNZS_loop;

   procedure go_back_to_the_start_of_IWB0 is
   begin
      the_cache_index := 0;
   end go_back_to_the_start_of_IWB0;

   procedure continue_after_JCqNZS is
   begin
      if CIA.syllable_index = 4 and the_cached_location = CIA.order_word_number then
         set_NIA_to((order_word_number => CIA.order_word_number+1, syllable_index => 0));
      end if;
      fetching_normally := True;
   end continue_after_JCqNZS;

   function next_order_syllable
   return KDF9.syllable
      with Inline;

   -- The amount by which the_CPU_time is increased, for a refill of both Instruction Word Buffers.

   the_IWB01_reload_time : constant KDF9.us := 7;  -- microseconds

   function next_order_syllable
   return KDF9.syllable is
      the_next_syllable : KDF9.syllable;
   begin
      the_next_syllable := the_syllable_cache(the_cache_index);
      if the_cache_index < 11 then
         the_cache_index := the_cache_index + 1;
      elsif fetching_normally then
         set_NIA_to((order_word_number => CIA.order_word_number+1, syllable_index => 0));
         -- Part-overlapped order-word fetch: can happen only once per instruction,
         --    and only before the instruction is executed, so no need to ADD to the_CPU_delta.
         if (CIA.order_word_number and 15) < 10 then
            -- The fudge factor applied here gives the Whetstone Benchmark its historical run time.
            the_CPU_delta := the_IWB01_reload_time + 1;
         else
            the_CPU_delta := the_IWB01_reload_time;
         end if;
      else
         go_back_to_the_start_of_IWB0;
      end if;
      return the_next_syllable;
   end next_order_syllable;

   procedure decode_syllable_0 (decoded : in out KDF9.decoded_order)
      with Inline;

   procedure decode_syllable_1 (decoded : in out KDF9.decoded_order)
      with Inline;

   procedure decode_a_jump_order (decoded : in out KDF9.decoded_order)
      with Inline;

   procedure decode_a_store_access_order (decoded : in out KDF9.decoded_order)
      with Inline;

   procedure decode_a_set_literal_order (decoded : in out KDF9.decoded_order)
      with Inline;

   procedure decode_syllable_0 (decoded : in out KDF9.decoded_order) is
   begin
      decoded.compressed_opcode := decoded.order.syllable_0 and 8#77#;
      decoded.kind := KDF9.INS_kind(decoded.order.syllable_0 / 2**6);
   end decode_syllable_0;

   procedure process_syllable_0_of_INS is
   begin
      if the_cache_index > 5 then
         CIA.order_word_number := the_cached_location;
         CIA.syllable_index   := KDF9.syllable_index(the_cache_index-6);
      else
         CIA.order_word_number := the_cached_location - 1;
         CIA.syllable_index   := KDF9.syllable_index(the_cache_index);
      end if;
      INS.order.syllable_0 := next_order_syllable;
      INS.compressed_opcode := INS.order.syllable_0 and 8#77#;
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
      decoded.target.syllable_index
         := KDF9.syllable_index(decoded.order.syllable_0 and syllable_nr_mask);
      decoded.target.order_word_number
         := KDF9.order_word_number(decoded.order.syllable_2)
          + KDF9.order_word_number(decoded.Qk) * 2**8
          + KDF9.order_word_number(decoded.order.syllable_0 and D4_mask) * 2**9;
      if (decoded.compressed_opcode and D2_mask) /= 0 then -- not JrCq ...
         decoded.compressed_opcode := decoded.compressed_opcode and D0_thru_3_mask;
      else
         decoded.compressed_opcode := (decoded.compressed_opcode and D0_thru_3_mask) or KDF9.syllable(decoded.Qq);
      end if;
      if decoded.compressed_opcode = EXIT_n then
         -- The syllable part of EXIT is actually a halfword offset,
         --    so convert it to an actual syllable number.
         if decoded.target.syllable_index = 2 then
            decoded.target.syllable_index := 0;
         else
            decoded.target.syllable_index := 3;
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
   D5_and_7_mask  : constant := 2#101#;
   D2_thru_4_mask : constant := 2#111000#;

   procedure decode_a_store_access_order (decoded : in out KDF9.decoded_order) is
   begin
      decoded.operand := KDF9.Q_part(decoded.order.syllable_2) + KDF9.Q_part(decoded.Qk)*2**8
                       + KDF9.Q_part((decoded.order.syllable_0 and D2_thru_4_mask)) * 2**9;
      decoded.compressed_opcode := decoded.compressed_opcode and D5_thru_7_mask;
   end decode_a_store_access_order;

   procedure decode_a_set_literal_order (decoded : in out KDF9.decoded_order) is
   begin
      decoded.operand := KDF9.Q_part(decoded.order.syllable_2)
                       + KDF9.Q_part(decoded.order.syllable_1)*2**8;
      decoded.compressed_opcode := 2#100#;
   end decode_a_set_literal_order;

   procedure process_syllables_1_and_2_of_a_data_access_order is
   begin
      if (INS.compressed_opcode and D5_thru_7_mask) < SET then
         process_syllable_1_of_INS;
         INS.order.syllable_2 := next_order_syllable;
         decode_a_store_access_order(INS);
      elsif (INS.compressed_opcode and D5_and_7_mask) = SET then
         -- SET n
         INS.order.syllable_1 := next_order_syllable;
         INS.order.syllable_2 := next_order_syllable;
         decode_a_set_literal_order(INS);
      else
         INS.order.syllable_1 := next_order_syllable;
         INS.order.syllable_2 := next_order_syllable;
         decode_a_set_literal_order(INS);
         INS.compressed_opcode := 7;  -- an invalid compression.
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
            if (the_order.compressed_opcode and D5_thru_7_mask) < SET then
               decode_syllable_1(the_order);
               decode_a_store_access_order(the_order);
            elsif (INS.compressed_opcode and D5_and_7_mask) = SET then
               -- SET n
               decode_a_set_literal_order(the_order);
            else
               decode_a_set_literal_order(the_order);
               INS.compressed_opcode := 7;  -- an invalid compression.
            end if;
      end case;
   end decode;

   -- the_order_at_NIA gets three syllables starting at [NIA].  It is FOR DIAGNOSTIC USE ONLY!
   -- It does NOT update the CPU time properly and MUST NOT be used inside an instruction cycle.

   function the_order_at_NIA
   return KDF9.syllable_group is
      saved_NIA : constant KDF9.syllable_address := NIA;
      result    : KDF9.syllable_group;
   begin
      result.syllable_0 := next_order_syllable;
      result.syllable_1 := next_order_syllable;
      result.syllable_2 := next_order_syllable;
      set_NIA_to(saved_NIA);
      return result;
   end the_order_at_NIA;

   -- This is the initial jump from the top halfword of E0 just after loading.

   E0U : KDF9.word := 0;  -- N.B. the lower halfword is used for option flags.

   procedure save_the_initial_jump is
   begin
      E0U := fetch_halfword(0, 0);
   end save_the_initial_jump;

   procedure restore_the_initial_jump is
   begin
       store_halfword(E0U, 0, 0);
   end restore_the_initial_jump;

   function the_initial_jump_was_corrupted
   return Boolean
   is (E0U /= fetch_halfword(0, 0));

   function is_an_invalid_order (decoded : KDF9.decoded_order)
   return Boolean
   is (
       (decoded.kind = data_access_order and then (decoded.order.syllable_0 and 2#101#) > 2#100#)
         or else (decoded.kind = normal_jump_order and then decoded.target.syllable_index > 5)
            -- 0 is now treated as a valid DUMMY0 order for KAlgol
               or else decoded.order.syllable_0 = 8#006#
                  or else decoded.order.syllable_0 = 8#040#
                     or else decoded.order.syllable_0 = 8#046#
                        or else decoded.order.syllable_0 = 8#055#
                           or else decoded.order.syllable_0 = 8#073#
                              or else decoded.order.syllable_0 = 8#076#
                                 or else decoded.order.syllable_0 = 8#150#
      );

   the_signature_hash : KDF9.word := 0;

   function the_digital_signature
   return KDF9.word
   is (the_signature_hash);

   function visible_state_hash
   return KDF9.word
      with Inline;

   function visible_state_hash
   return KDF9.word is
      hash : KDF9.word;
   begin
      hash := rotate_word_right(the_signature_hash, 1) xor KDF9.word(ICR);
      hash := rotate_word_right(hash, 1) xor as_word(the_Q_store(INS.Qq));
      hash := rotate_word_right(hash, 1) xor as_word(the_Q_store(INS.Qk));
      if the_SJNS_depth > 0 then
         for s in reverse KDF9.SJNS_depth range 0 .. the_SJNS_depth-1 loop
               hash := rotate_word_right(hash, 1) xor as_word(the_SJNS(s));
         end loop;
      end if;
      if the_NEST_depth > 0 then
         for n in reverse KDF9.NEST_depth range 0 .. the_NEST_depth-1 loop
               hash := rotate_word_right(hash, 1) xor the_NEST(n);
         end loop;
      end if;
      return hash;
   end visible_state_hash;

   procedure update_the_digital_signature is
   begin
      the_signature_hash := visible_state_hash;
   end update_the_digital_signature;

end KDF9;
