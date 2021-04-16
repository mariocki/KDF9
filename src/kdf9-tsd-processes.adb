-- Implement OUTs 0, 1 and 2 of the EE Time Sharing Directors.
--
-- This file is part of ee9 (6.2r), the GNU Ada emulator of the English Electric KDF9.
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

with dumping;
with IOC;
with IOC.fast.DR.TSD_OUTs;
with IOC.fast.FD.TSD_OUTs;
with IOC.slow.shift.TR;
with KDF9_char_sets;
with KDF9.CPU;
with KDF9.store;
with KDF9.TSD.peripherals;

with environmental_value_of;

use  dumping;
use  IOC;
use  IOC.fast.DR.TSD_OUTs;
use  IOC.fast.FD.TSD_OUTs;
use  IOC.slow.shift.TR;
use  KDF9_char_sets;
use  KDF9.store;
use  KDF9.TSD.peripherals;

package body KDF9.TSD.processes is

--
-- OUTs 0, 1 and 2 terminate execution in various ways and call for appropriate finalization.
--

   procedure free_any_allocated_tapes (OUT_number : in KDF9.word) is
   begin
      for b in KDF9.buffer_number loop
         if buffer(b) /= null                      and then
               buffer(b).kind in MT_kind | ST_kind and then
                  is_allocated(buffer(b))              then
            free_the_device_on_buffer(b, OUT_number);
         end if;
      end loop;
   end free_any_allocated_tapes;


--
-- OUT 0: terminate the run.
--

   procedure do_OUT_0 is
   begin
      free_any_allocated_tapes(OUT_number => 0);
      notify_state_display_of_final_ICR;
      log_API_message("OUT 0: end of run");
      raise program_exit;
   end do_OUT_0;


--
-- OUT 1: terminate the run and overlay another program in a fresh start.
--

   procedure finalize_interim_diagnostics (OUT_number : in KDF9.word) is
   begin
      if the_log_is_wanted and pre_overlay_state_is_enabled then
         show_final_state("before overlay by OUT" & OUT_number'Image);
      end if;

      -- Deal with any dump requests.
      if the_log_is_wanted and nr_of_post_dumping_areas /= 0 then
         log_new_line;
         log_rule;
         log_title("Post-run Dump:");
         print_postrun_dump_areas;
      end if;
      remove_prerun_dump_areas;
      remove_postrun_dump_areas;

      -- Restart tracing.
      clear_retro_FIFO;
      clear_IOC_FIFO;
      clear_the_histogram;
      clear_the_profile;
      the_profile_is_wanted := False;
      the_INS_plot_is_wanted := False;
      if the_external_trace_is_enabled then
         log_an_external_trace_header("ee9: Restarting the run for OUT" & OUT_number'Image);
      end if;
   end finalize_interim_diagnostics;

   procedure prepare_successor (new_limits : in KDF9.word; new_program_name : in String := "" ) is
   begin
      -- Set up any new options.
      get_settings_from_file("2");
      display_execution_modes(new_program_name);

      -- Complete the core image.
      poke_all_amendments;
      save_the_initial_jump;
      store_word(new_limits, 1);

      -- Display initial dumps.
      the_program_has_been_analysed := False;
      show_all_prerun_dump_areas;

      -- Establish the new CPU state.
      reset_V_and_T;
      set_NIA_to((0, 0));
   end prepare_successor;

   procedure do_OUT_1 is
      P : KDF9.pair;
      W : KDF9.word;
   begin
      -- Get the program name.
      ensure_that_the_NEST_holds_2_operands;
      P := pop;
      P := KDF9.CPU.shift_logical(P, 24);

      declare
         program_name : constant String := trimmed(to_string(P));
         overlay_name : constant String := environmental_value_of("KDF9_BINARY", default => "Binary")
                                         & "/"
                                         & program_name;
      begin
         if program_name = "" then
            trap_failing_OUT(1, "the given program name is an empty string");
         end if;

         if program_name = "KMW0201--UPU" then
            -- The Whetstone Controller is trying to overlay itself with the Translator.
            -- This is so inconvenient in practice that I simply prevent it.
            notify_state_display_of_final_ICR;
            log_API_message("OUT 1: ee9 will not return to the Whetstone Translator", skip => 2);
            raise program_exit;
         end if;

         -- Tidy up the running program.
         free_all_devices;
         complete_all_extant_transfers;
         notify_state_display_of_final_ICR;
         finalize_interim_diagnostics(OUT_number => 1);
         log_API_message("OUT 1: ICR ="
                       & ICR'Image
                       & "; RAN/EL ="
                       & the_CPU_time'Image
                       & " /"
                       & KDF9.us'Image(the_clock_time)
                       & " KDF9 us"
                        );

         -- The overlaid program inherits the predecessor's time and store limits.
         W := fetch_word(1);
         load_a_program(overlay_name);
         prepare_successor(new_limits => W, new_program_name => overlay_name);

         raise mode_change_request;
      end;
   end do_OUT_1;


--
-- OUT 2: terminate the run and then restart it with a program already in core.
-- complete_TSD_OUT_2 is called by KDF9.microcode after it has finalized the OUT instruction.
--

   time_limit : KDF9.word;

   procedure do_OUT_2 is
   begin
      ensure_that_the_NEST_holds_an_operand;
      time_limit := pop;
      the_trace_operand := time_limit;

      -- Tidy up the running program.
      free_any_allocated_tapes(OUT_number => 2);
      free_any_reserved_disc_space;
      free_any_reserved_drum_space;
      notify_state_display_of_final_ICR;

      raise OUT_2_restart;
   end do_OUT_2;

   procedure complete_TSD_OUT_2 is
   begin
      finalize_interim_diagnostics(OUT_number => 2);
      if time_limit >= 2**24 then
         trap_failing_OUT(2, "the new time limit =" & time_limit'Image & "s is too big");
      end if;

      log_API_message("OUT 2: ICR ="
                    & ICR'Image
                    & "; RAN/EL ="
                    & the_CPU_time'Image
                    & " /"
                    & KDF9.us'Image(the_clock_time)
                    & " KDF9 us"
                    & "; new time limit = "
                    & time_limit'Image &"s"
                     );

      --The successor has a new time limit and inherits the predecessor's store limit.
      prepare_successor(new_limits => time_limit * 2**24 + fetch_halfword(1, 1) / 2**24);

      reset_the_program_state;
   end complete_TSD_OUT_2;


--
-- OUT 97: this not a genuine TSD OUT.
--

   procedure do_OUT_97 is
      P      : constant KDF9.word := pop;
      name   : constant String    := trimmed(to_string(P));
      value  : constant String    := environmental_value_of(name, "0");
      number : KDF9.word;
   begin
      number := KDF9.word'Value(value);
      the_trace_operand := number;
      push(number);
   exception
      when others =>
         trap_failing_OUT(97, name & " = «"& value & "», not a valid integer");
   end do_OUT_97;

end KDF9.TSD.processes;
