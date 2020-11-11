-- execute.adb
--
-- This is the emulation-mode coordinate module.
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

with GNAT.Ctrl_C;
--
with Ada.Command_Line;
with Ada.Exceptions;
--
with configuration;
with exceptions;
with HCI;
with IOC;
with IOC.two_shift.TR;
with KDF9;
with KDF9.microcode;
with KDF9.store;
with map_CTRL_C_to_FLEX;
with finalize_ee9;
with settings;
with state_display;

use  Ada.Command_Line;
--
use  exceptions;
use  HCI;
use  IOC;
use  IOC.two_shift.TR;
use  KDF9;
use  KDF9.microcode;
use  KDF9.store;
use  settings;
use  state_display;

procedure execute is

   pragma Unsuppress(All_Checks);

   procedure check_times_and_modes;
   pragma Inline(check_times_and_modes);

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

   procedure make_a_dignified_exit (status : in Exit_Status := Failure) is
   begin
      finalize_ee9;
      Set_Exit_Status(status);
   exception
      when error : others =>
         log_line("Failure in ee9: "
                & Ada.Exceptions.Exception_Information(error)
                & " was raised in 'make_a_dignified_exit'!");
         Set_Exit_Status(Failure);
   end make_a_dignified_exit;

begin
   GNAT.Ctrl_C.Install_Handler(map_CTRL_C_to_FLEX'Access);

   if the_external_trace_is_enabled then
      log_an_external_trace_header;
   end if;

   reset_the_internal_registers;

   case the_execution_mode is
      when boot_mode =>
         bootstrap_the_KDF9;
      when program_mode =>
         load_a_program;
      when test_program_mode =>
         load_a_program;
   end case;

   show_all_prerun_dump_areas;

   reset_the_CPU_state;

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
      exception
         when mode_change_request =>
            quit_if_requested;
         when LOV_trap =>
            IOC.handle_a_main_store_lockout(the_locked_out_address);
      end;
   end loop;

exception

   when program_exit =>
      make_a_dignified_exit(Success);

   when quit_request =>
      log_new_line;
      log_line("Run stopped by the user.");
      make_a_dignified_exit(Success);

   when time_expired =>
      log_new_line;
      log_line("Run terminated on reaching time limit! Infinite loop?");
      make_a_dignified_exit;

   when NYI_trap =>
      log_new_line;
      log_line("Instruction Not Yet Implemented!");
      make_a_dignified_exit;

   when NOUV_trap =>
      log_new_line;
      log_line("NOUV (NEST/SJNS Overflow/Underflow Violation)!");
      make_a_dignified_exit;

   when error : LIV_trap =>
      log_new_line;
      log_line("LIV (Lock-In Violation)! "
             & Ada.Exceptions.Exception_Message(error));
      make_a_dignified_exit;

   when RESET_trap =>
      log_new_line;
      log_line("RESET (Reset Violation)!");
      make_a_dignified_exit;

   when error : Director_failure =>
      log_new_line;
      log_line("Failure detected in Director! "
             & Ada.Exceptions.Exception_Information(error));
      make_a_dignified_exit;

   when error : input_is_impossible =>
      log_new_line;
      log_line("Noninteractive mode; "
             & Ada.Exceptions.Exception_Message(error));
      make_a_dignified_exit;

   when error : others =>
      log_new_line;
      log_line("Failure in ee9! "
             & Ada.Exceptions.Exception_Information(error));
      make_a_dignified_exit;

end execute;
