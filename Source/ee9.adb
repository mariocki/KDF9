-- ee9.adb
--
-- This is the "main program" co-ordinate module for the entire emulator.
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

with Ada.Command_Line;
with Ada.Exceptions;
--
with exceptions;
with execute;
with HCI;
with settings;
with toggle_the_shared_buffer;

use  exceptions;
use  HCI;
use  settings;

procedure ee9 is

   pragma Unsuppress(All_Checks);

   package CLI renames Ada.Command_Line;

   the_log_file_name : constant String := "KDF9.log";

   a_command_line_error_was_detected : exception;

   procedure show_proper_usage (message : in String := "") is
   begin
      if message /= "" then
         log_line(message);
      end if;
      log_line("usage: ee9 { -s{b|p|t} | -d{f|p|t|x} | -m"
             & miscellany_prompt
             & " } program_file_name");
      CLI.Set_Exit_Status(CLI.Failure);
      raise a_command_line_error_was_detected;
   end show_proper_usage;

   procedure complain (about : in String) is
   begin
      show_proper_usage("Parameter " & about & " is not valid!");
   end complain;

   procedure check_all_flag_settings is

      procedure check_flag_setting (i : in Positive) is
         argument : constant String := CLI.Argument(i);
      begin
         -- Fail an empty parameter.
         if argument'Length = 0 then
            complain(about => "number" & Positive'Image(i) & " being empty");
         end if;

         -- Ignore a program_file_name parameter.
         if argument(argument'First) /= '-'  then
            return;
         end if;

         -- Fail a too-short flag parameter.
         if argument'Length = 2  and then
               argument = "-m"       then
            return;
         end if;
         if argument'Length < 3 then
            complain(about => "'" & argument & "'");
         end if;

         -- Check for a miscellany parameter.
         if argument'Length in 2 .. miscellany_flags'Length+2 and then
               argument(argument'First..argument'First+1) = "-m"  then
            for i in argument'First+2 .. argument'Last loop
               if is_invalid_miscellany_flag(argument(i)) then
                  complain(about => "'" & argument & "'" & " at '" & argument(i) & "'");
               end if;
            end loop;
            return;
         end if;

         -- Check for a state or diagnostic parameter.
         if argument'Length = 3                and then
              (argument = "-sb"                 or else
                  argument = "-sp"              or else
                     argument = "-st"           or else
                        argument = "-df"        or else
                           argument = "-dt"     or else
                              argument = "-dp"  or else
                                 argument = "-dx") then
            return;
         else
            complain(about => "'" & argument & "'");
         end if;
      end check_flag_setting;

   begin -- check_all_flag_settings
      if CLI.Argument_Count = 0 then
         return;
      end if;
      for i in 1..CLI.Argument_Count loop
         check_flag_setting(i);
      end loop;
   end check_all_flag_settings;

   procedure impose_all_flag_settings is

      procedure impose_flag_setting (i : in Positive) is
         argument : constant String := CLI.Argument(i);
      begin
         -- Assume the argument is valid.

         -- Ignore a program_file_name parameter.
         if argument(argument'First) /= '-'  then
            return;
         end if;

         -- Impose a miscellany parameter.
         if argument'Length in 2 .. miscellany_flags'Length+2 and then
               argument(argument'First..argument'First+1) = "-m"  then
            for i in argument'First+2 .. argument'Last loop
               set_this_miscellany_flag(argument(i));
            end loop;
            return;
         end if;

         -- Impose a state or diagnostic parameter.
         if    argument = "-sb" then
            set_execution_mode(boot_mode);
         elsif argument = "-sp" then
            set_execution_mode(program_mode);
         elsif argument = "-st" then
            set_execution_mode(test_program_mode);
         elsif argument = "-df" then
            set_diagnostic_mode(fast_mode);
         elsif argument = "-dt" then
            set_diagnostic_mode(trace_mode);
         elsif argument = "-dp" then
            set_diagnostic_mode(pause_mode);
         elsif argument = "-dx" then
            set_diagnostic_mode(external_mode);
         end if;
      end impose_flag_setting;

   begin -- impose_all_flag_settings
      if CLI.Argument_Count = 0 then
         return;
      end if;
      for i in 1..CLI.Argument_Count loop
         impose_flag_setting(i);
      end loop;
   end impose_all_flag_settings;

begin

   open(the_log_file_name);

   log_line("Welcome to ee9 V2.0r, the GNU Ada KDF9 emulator.", iff => the_log_is_wanted);

   check_all_flag_settings;

   get_settings_from_file("1");

   impose_all_flag_settings;

   toggle_the_shared_buffer;

   display_execution_modes;

   execute;

   log_rule(iff => the_log_is_wanted);

   close(the_log_file_name);

exception

   when quit_request =>
      close(the_log_file_name);

   when a_command_line_error_was_detected =>
      close(the_log_file_name);

   when error : others =>
      log_new_line;
      log_line("Failure in ee9; unexpected exception: "
             & Ada.Exceptions.Exception_Information(error)
             & " was raised!");
      close(the_log_file_name);

end ee9;
