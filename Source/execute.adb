-- execute.adb
--
-- This is the emulation-mode coordinate module.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

with GNAT.Ctrl_C;
--
with Ada.Command_Line;
with Ada.Exceptions;
--
with break_in;
with dumping;
with exceptions;
with HCI;
with IOC.slow.shift.TR;
with KDF9;
with KDF9.microcode;
with settings;
with state_display;

with say_goodbye;

use  Ada.Command_Line;
use  Ada.Exceptions;

--
use  dumping;
use  exceptions;
use  HCI;
use  IOC.slow.shift.TR;
use  KDF9;
use  KDF9.microcode;
use  settings;
use  state_display;

procedure execute (program_name : in String) is

   procedure check_times_and_modes
      with Inline;

   pause_count : KDF9.order_counter := 0;

   procedure check_times_and_modes is
   begin
      if ICR > pause_count then
         if ICR >= time_limit then
            raise time_expired;
         end if;
         pause_count := pause_count + time_slice;
         change_diagnostic_mode_if_requested;
      end if;
   end check_times_and_modes;

begin  -- execute
   GNAT.Ctrl_C.Install_Handler(break_in.note_user_interrupt'Access);

   if the_external_trace_is_enabled then
      log_an_external_trace_header;
   end if;

   case the_execution_mode is
      when boot_mode =>
         reset_the_internal_registers(Director_state);
         boot_the_KDF9(program_name);
      when test_program_mode=>
         reset_the_internal_registers(Director_state);
         load_a_program(program_name);
      when program_mode =>
         reset_the_internal_registers(program_state);
         load_a_program(program_name);
   end case;

   if not loading_was_successful then
      say_goodbye("Could not load the specified program.");
      return;
   end if;

   poke_all_amendments;
   show_all_prerun_dump_areas;

   if do_not_execute then
      log_new_line;
      log_line("Run abandoned as requested.");
      return;
   end if;

   reset_the_CPU_state;

execution_loop:
   loop

      begin

         check_times_and_modes;
         if the_diagnostic_mode /= fast_mode then
            -- Do a single, traced instruction, breaking-in conditionally.
            do_a_traced_instruction_cycle;
         else
            -- Fast mode is designed for minimal overhead;
            --    it interacts with the user only at the end of a time slice.
            loop
               do_a_fast_time_slice;
               check_times_and_modes;
            end loop;
         end if;

      exception  -- handler for execution_loop

         when debugging_stop =>
            null;

         when mode_change_request =>
            quit_if_requested;

         when abandon_this_order =>
            null;  -- Just get on with it after an interrupt or nullified order.

         when LOV_trap =>
            IOC.handle_a_main_store_lockout;

         when program_exit =>
            say_goodbye("Normal end of run", status => Success);
            exit execution_loop;

         when quit_request =>
            say_goodbye("Run stopped by the user", status => Success);
            exit execution_loop;

         when time_expired =>
            say_goodbye("Infinite loop? Run failed by exceeding the time limit");
            exit execution_loop;

         when diagnostic : NOUV_trap =>
            say_goodbye("NOUV interrupt", Exception_Message(diagnostic));
            exit execution_loop;

         when input_is_impossible =>
            say_goodbye("Noninteractive mode cannot handle a prompt");
            exit execution_loop;

         when diagnostic : not_yet_implemented =>
            say_goodbye("Not yet implemented", Exception_Message(diagnostic));
            exit execution_loop;

         when diagnostic : RESET_trap =>
            say_goodbye("RESET interrupt", Exception_Message(diagnostic));
            exit execution_loop;

         when diagnostic : LIV_trap =>
            say_goodbye( "LIV interrupt", Exception_Message(diagnostic));
            exit execution_loop;

         when diagnostic : Director_failure =>
            say_goodbye("Invalid operation in Director", Exception_Message(diagnostic));
            exit execution_loop;

         when diagnostic : IO_error =>
            say_goodbye("Impossible I/O operation", Exception_Message(diagnostic));
            exit execution_loop;

         when diagnostic : Director_IO_error =>
            say_goodbye("Impossible I/O operation in Director", Exception_Message(diagnostic));
            exit execution_loop;

         when diagnostic : operator_error =>
            say_goodbye("The KDF9 operator has made a mistake", Exception_Message(diagnostic));
            exit execution_loop;

      end;

   end loop execution_loop;

exception  -- handler for execute

   when diagnostic : invalid_paper_tape_file =>
      say_goodbye("Invalid paper tape file supplied", Exception_Message(diagnostic));

   when diagnostic : operator_error =>
      say_goodbye("The KDF9 operator must have made a mistake", Exception_Message(diagnostic));

   when diagnostic : others =>
      say_goodbye("Apologies for this dismal failure", Exception_Message(diagnostic));

end execute;
