-- This is the "main program" for the entire emulator.
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

with GNAT.Source_Info;
--
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
--
with exceptions;
with HCI;
with IOC.equipment;
with IOC.slow.shift.TP;
with IOC.slow.shift.TR;
with settings;
with get_runtime_paths;

with execute;
with say_goodbye;

use  Ada.Text_IO;
use  Ada.Exceptions;
--
use  exceptions;
use  HCI;
use  IOC.equipment;
use  settings;

procedure ee9 is

   package CLI renames Ada.Command_Line;

   the_log_file_name : constant String := get_runtime_paths & "logs/KDF9.log";

   a_command_line_error_is_detected : exception;

   procedure show_proper_usage (message : in String := "") is
   begin
      if message /= "" then
         log_line(message);
      end if;
      log_line(
               "usage: ee9 { [ -s{b|p|t} ] | [ -d{f|p|t|x|-} ] | [ -m"
             & miscellany_prompt
             & " ] [ -TP{k|l}{k|l} ] [ -TR{k|l}{k|l} ] } +program_file_name"
              );
      CLI.Set_Exit_Status(CLI.Failure);
      raise a_command_line_error_is_detected;
   end show_proper_usage;

   procedure complain (about : in String; because : in String := "") is
   begin
      show_proper_usage(
                        "Parameter «" & about & "» is not valid"
                      & (if because = "" then "." else " because " & because & ".")
                       );
   end complain;

   the_program_name_position : Natural := 0;

   procedure check_all_flag_settings is

      procedure check_flag_setting (i : in Positive) is

         subtype tape_code_flags is Character
            with Predicate => tape_code_flags in 'K' | 'L' | 'k' | 'l';

         subtype tape_device_flags is Character
            with Predicate => tape_device_flags in 'P' | 'R' |'p' | 'r';

         argument : constant String   := CLI.Argument(i);
         index    : constant Positive := argument'First;

      begin
         -- Ignore an empty parameter.
         if argument'Length = 0 then
            return;
         end if;

         -- Take note of a program name parameter.
         if argument(index) = '+' then
            if the_program_name_position /= 0 then
               complain(about   => argument,
                        because => "more than one program has been specified");
            end if;
            if argument'Length < 3 then
               complain(about   => argument,
                        because => "it is too short for a program file name");
            end if;
            the_program_name_position := i;
            return;
         end if;

         -- Fail any non-flag parameter.
         if argument(index) /= '-'  then
            complain(about => argument);
         end if;

         -- Fail a too-short flag parameter.
         if argument'Length < 2 then
            complain(about   => argument,
                     because => "it is too short");
         end if;

         -- Check for a miscellany parameter.
         if argument'Length in 2 .. miscellany_flags'Length+2 and then
               argument(index..index+1) = "-m"                    then
            for i in index+2 .. argument'Last loop
               if is_invalid_miscellany_flag(argument(i)) then
                  complain(about => argument);
               end if;
            end loop;
            return;
         end if;

         -- Check for a state or diagnostic parameter.
         if argument in "-sb" | "-sp" | "-st" | "-df" | "-dt" | "-dp" | "-dx" | "-d-" then
            return;
         end if;

         -- Check for a Tape Punch/Reader parameter.
         if argument'Length in 4 .. 5                    and then
               argument(index+1) in 'T' | 't'            and then
                  argument(index+2) in tape_device_flags and then
                     argument(index+3) in tape_code_flags    then
            if argument'Length = 4                  or else
                  argument(index+4) in tape_code_flags then
               return;
            end if;
         end if;

         complain(about => argument);

      end check_flag_setting;

   begin -- check_all_flag_settings
      for i in 1..CLI.Argument_Count loop
         check_flag_setting(i);
      end loop;
      if the_program_name_position = 0 then
         show_proper_usage("No program name parameter was given.");
      end if;
   end check_all_flag_settings;

   procedure impose_all_flag_settings is

      procedure impose_flag_setting (i : in Positive) is

         subtype Latin_1_code_flags is Character
            with Predicate => Latin_1_code_flags in 'L' | 'l';

         subtype punch_device_flags is Character
            with Predicate => punch_device_flags in 'P' |'p';

         argument : constant String   := CLI.Argument(i);
         index    : constant Positive := argument'First;

      begin -- impose_flag_setting
         -- Ignore an empty parameter.
         if argument'Length = 0 then
            return;
         end if;

         -- Ignore a program_file_name parameter.
         if argument(index) = '+'  then
            return;
         end if;

         -- Impose a miscellany parameter.
         if argument'Length in 2 .. miscellany_flags'Length+2 and then
               argument(index..index+1) = "-m"  then
            for i in index+2 .. argument'Last loop
               set_this_miscellany_flag(argument(i));
            end loop;
            return;
         end if;

         -- Impose  a Tape Punch/Reader parameter.
         if argument'Length in 4 .. 5 then
            -- Set the code for the first device.
            if argument(index+2) in punch_device_flags then
               IOC.slow.shift.TP.set_unit_code(0, argument(index+3) in Latin_1_code_flags);
            else
               IOC.slow.shift.TR.set_unit_code(0, argument(index+3) in Latin_1_code_flags);
            end if;
         end if;
         if argument'Length = 5 then
            -- Set the code for the second device.
            if argument(index+2) in punch_device_flags then
               IOC.slow.shift.TP.set_unit_code(1, argument(index+4) in Latin_1_code_flags);
            else
               IOC.slow.shift.TR.set_unit_code(1, argument(index+4) in Latin_1_code_flags);
            end if;
         end if;

         -- Impose a state or diagnostic parameter; argument is known to be 3 characters long.
         if    argument = "-sb" then
            set_execution_mode(boot_mode);
         elsif argument = "-sp" then
            set_execution_mode(program_mode);
         elsif argument = "-st" then
            set_execution_mode(privileged_mode);
         elsif argument = "-d-" then
            set_diagnostic_mode(fast_mode);
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

   function name_part_of (f : String)
   return String
   is (f(f'First+1 .. f'Last));

   function the_program_name
   return String
   is (name_part_of(CLI.Argument(the_program_name_position)));

begin -- ee9

   check_all_flag_settings;
   open(the_log_file_name);
   get_settings_from_file("1");
   configure_the_IOC;
   impose_all_flag_settings;
   revise_the_IOC_configuration;
   log_line(
            "This is ee9 6.2e, compiled by "
          & Standard'Compiler_Version
          & " on "
          & GNAT.Source_Info.Compilation_ISO_Date
          & ".",
            iff => the_log_is_wanted
           );
   display_execution_modes(the_program_name);

   execute(the_program_name);

   close(the_log_file_name);

exception

   when a_command_line_error_is_detected =>
      close(the_log_file_name);

   when diagnostic : operator_error =>
      say_goodbye("The operator has made a mistake", Exception_Message(diagnostic));

   when error : others =>
      Put_Line(Standard_Error, "Failure in ee9: " & Exception_Information(error) & ".");
      close(the_log_file_name);
      CLI.Set_Exit_Status(CLI.Failure);

end ee9;
