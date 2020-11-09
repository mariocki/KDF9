-- settings.adb
--
-- execution mode, diagnostic mode, and other emulation-control settings
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

with Ada.Exceptions;
with Ada.Text_IO;
--
with dumping;
with exceptions;
with formatting;
with HCI;
with KDF9.store;
with postscript;
with settings.IO;
with tracing;

use  Ada.Text_IO;
--
use  dumping;
use  exceptions;
use  formatting;
use  HCI;
use  KDF9.store;
use  settings.IO;
use  tracing;

package body settings is

   pragma Unsuppress(All_Checks);

   function is_invalid_miscellany_flag (option : in Character)
   return Boolean is
   begin
      for i in miscellany_flags'Range loop
         if miscellany_flags(i) = option then
            return False;
         end if;
      end loop;
      return True;
   end is_invalid_miscellany_flag;

   procedure set_this_miscellany_flag (option : in Character) is
   begin
      if is_invalid_miscellany_flag(option) then
         log_line("***** Error in a miscellany specification: '"
                & option
                & "'.");
         return;
      end if;
      case option is
         when '1' .. '9' =>
            time_limit := (Character'Pos(option) - Character'Pos('0')) * 10_000_000;
         when 'a' =>
            API_logging_is_requested := False;
         when 'd' =>
            debugging_is_enabled := True;
         when 'e' =>
            the_log_is_wanted := False;
         when 'f' =>
            the_final_state_is_wanted := False;
         when 'g' =>
            the_graph_plotter_is_configured := True;
         when 'h' =>
            the_histogram_is_requested := False;
         when 'i' =>
            interrupt_tracing_is_requested := False;
         when 'l' =>
            time_limit := offline_time_limit;
         when 'n' =>
            noninteractive_usage_is_enabled := True;
            time_limit := offline_time_limit;
         when 'p' =>
            peripheral_tracing_is_requested := False;
         when 'r' =>
            retrospective_tracing_is_requested := False;
         when 's' =>
            the_signature_is_requested := False;
         when 't' =>
            authentic_timing_is_wanted := True;
         when 'z' =>
            API_logging_is_requested := False;
            debugging_is_enabled := False;
            the_log_is_wanted := False;
            the_final_state_is_wanted := False;
            the_histogram_is_requested := False;
            interrupt_tracing_is_requested := False;
            peripheral_tracing_is_requested := False;
            retrospective_tracing_is_requested := False;
            the_signature_is_requested := False;
         when others =>
            raise emulation_failure;
      end case;
      set_diagnostic_mode(the_diagnostic_mode);
   end set_this_miscellany_flag;

   procedure display_execution_modes is
      needs_comma : Boolean := False;

      procedure append_option (flag : in Boolean; name : in String) is
      begin
         if flag then
            if needs_comma then
               log(", ");
            end if;
            log(name);
            needs_comma := True;
         end if;
      end append_option;

   begin
      if not the_log_is_wanted then return; end if;
      case the_execution_mode is
         when boot_mode =>
            log("Performing a cold boot in ");
         when program_mode =>
            log("Running a problem program in ");
         when test_program_mode =>
            log("Running a test program in ");
      end case;
      if authentic_timing_is_wanted then
         log("real time ");
      end if;
      case the_diagnostic_mode is
         when fast_mode =>
            log("fast mode (without diagnostics)");
         when trace_mode =>
            if the_external_trace_is_enabled then
               log("external trace mode");
            else
               log("trace mode");
            end if;
         when pause_mode =>
            log("pause mode");
         when external_mode =>
            log("external trace mode");
      end case;
      if the_histogram_is_enabled                          or else
            the_interrupt_trace_is_enabled                 or else
               the_peripheral_trace_is_enabled             or else
                  the_retrospective_trace_is_enabled       or else
                     the_signature_is_enabled              or else
                        the_external_trace_is_enabled      or else
                           debugging_is_enabled            or else
                              noninteractive_usage_is_enabled then
         log_new_line;
         log(" ... with option(s): ");
         append_option(the_histogram_is_enabled,           "histogram");
         append_option(the_interrupt_trace_is_enabled,     "interrupt trace");
         append_option(the_peripheral_trace_is_enabled,    "peripheral trace");
         append_option(the_retrospective_trace_is_enabled, "retro trace");
         append_option(the_signature_is_enabled,           "signature hash");
         append_option(debugging_is_enabled,               "debugging output");
         append_option(noninteractive_usage_is_enabled,    "noninteractive");
         if needs_comma then
            log_line(".");
         else
            log_line("all disabled.");
            raise emulation_failure with "option processing failure";
         end if;
      else
         log_line(".");
      end if;
      log_rule;
   end display_execution_modes;

   procedure quit_if_requested is
   begin
      if quit_was_requested then
         raise quit_request;
      end if;
   end quit_if_requested;

   procedure change_diagnostic_mode_if_requested is
   begin
      if the_diagnostic_mode_changed then
         the_diagnostic_mode_changed := False;
         raise mode_change_request;
      end if;
   end change_diagnostic_mode_if_requested;

   procedure set_diagnostic_mode (a_diagnostic_mode : in settings.diagnostic_mode) is
      the_signature_is_appropriate,
      the_histogram_is_appropriate,
      retrospective_tracing_is_appropriate,
      peripheral_tracing_is_appropriate,
      interrupt_tracing_is_appropriate : Boolean;
   begin
      if a_diagnostic_mode = external_mode then
         if (the_diagnostic_mode /= external_mode) and (not the_external_trace_is_enabled) then
            open(the_external_trace_file, the_external_trace_file_name);
         end if;
         the_diagnostic_mode := trace_mode;
         the_external_trace_is_enabled := True;
      else
         the_diagnostic_mode := a_diagnostic_mode;
      end if;
      case a_diagnostic_mode is
         when fast_mode =>
            debugging_is_enabled := False;
            the_signature_is_appropriate := False;
            the_histogram_is_appropriate := False;
            retrospective_tracing_is_appropriate := False;
            peripheral_tracing_is_appropriate := False;
            interrupt_tracing_is_appropriate := False;
         when trace_mode | external_mode | pause_mode =>
            the_signature_is_appropriate := True;
            the_histogram_is_appropriate := True;
            retrospective_tracing_is_appropriate := True;
            peripheral_tracing_is_appropriate := True;
            interrupt_tracing_is_appropriate := (the_execution_mode = boot_mode);
      end case;
      the_signature_is_enabled :=
         the_signature_is_requested and the_signature_is_appropriate;
      the_histogram_is_enabled :=
         the_histogram_is_requested and the_histogram_is_appropriate;
      the_retrospective_trace_is_enabled :=
         retrospective_tracing_is_requested and retrospective_tracing_is_appropriate;
      the_peripheral_trace_is_enabled :=
         peripheral_tracing_is_requested and peripheral_tracing_is_appropriate;
      the_interrupt_trace_is_enabled :=
         interrupt_tracing_is_requested and interrupt_tracing_is_appropriate;
   end set_diagnostic_mode;

   procedure set_execution_mode (an_execution_mode : in settings.execution_mode) is
   begin
      the_execution_mode := an_execution_mode;
   end set_execution_mode;

   package diagnostic_mode_IO    is new Ada.Text_IO.Enumeration_IO(settings.diagnostic_mode);

   package execution_mode_IO     is new Ada.Text_IO.Enumeration_IO(settings.execution_mode);

   package authenticity_mode_IO  is new Ada.Text_IO.Enumeration_IO(settings.authenticity_mode);

   procedure get_settings_from_file (version : in String) is

      the_settings_file_name : constant String := "settings_" & version & ".txt";

      settings_file  : File_Type;
      flag           : Character;
      counts_are_set : Boolean := False;

      procedure set_this_miscellany_flag is
         option : Character;
      begin
         loop
            get(settings_file, option);
            if is_invalid_miscellany_flag(option) then
               raise Data_Error;
            else
               set_this_miscellany_flag(option);
            end if;
         exit when End_Of_Line(settings_file);
         end loop;
      exception
         when error : others =>
            if not End_Of_Line(settings_file) then
               Skip_Line(settings_file);
            end if;
            log_new_line;
            log_line("***** Error in a miscellany specification: '"
                   & option
                   & "' at "
                   & Ada.Exceptions.Exception_Message(error));
      end set_this_miscellany_flag;

      procedure set_breakpoints is
         start, end_point : KDF9.code_location;
      begin
         begin
            get_address(settings_file, KDF9.word(start));
         exception
            when others =>
               log_new_line;
               log_line("***** Error in lower address; no breakpoint set.");
               return;
         end;
         log_new_line;
         log_line("Lower breakpoint: #" & oct_of(KDF9.code_point'(0, start))
                                 & " (" & dec_of(KDF9.code_point'(0, start)) & ")",
                  iff => the_log_is_wanted);
         is_a_breakpoint(start) := True;
         begin
            get_address(settings_file, KDF9.word(end_point));
         exception
            when Data_Error =>
               log_line("      No upper address: one breakpoint set.", iff => the_log_is_wanted);
               set_breakpoints(start, start);
               return;
         end;
         log_line("Upper breakpoint: #" & oct_of(KDF9.code_point'(5, end_point))
                                 & " (" & dec_of(KDF9.code_point'(5, end_point)) & ")",
                  iff => the_log_is_wanted);
         set_breakpoints(start, end_point);
      exception
         when others =>
            log_line("***** Error setting breakpoints; ignored.");
      end set_breakpoints;

      procedure set_fetch_points is
         start, end_point : KDF9.address;
      begin
         begin
            get_address(settings_file, KDF9.word(start));
         exception
            when others =>
               log_new_line;
               log_line("***** Error in lower address; no fetchpoint set.");
               return;
         end;
         log_new_line;
         log_line("Lower fetchpoint: #" & oct_of(start) & " (" & dec_of(start) & ")",
                  iff => the_log_is_wanted);
         begin
            get_address(settings_file, KDF9.word(end_point));
         exception
            when Data_Error =>
               log_line("      No upper address: one fetchpoint set.", iff => the_log_is_wanted);
               set_fetch_points(start, start);
               return;
         end;
         log_line("Upper fetchpoint: #" & oct_of(end_point) & " (" & dec_of(end_point) & ")",
                  iff => the_log_is_wanted);
         set_fetch_points(start, end_point);
      exception
         when others =>
            log_line("***** Error setting fetch points; ignored.");
      end set_fetch_points;

      procedure set_store_points is
         start, end_point : KDF9.address;
      begin
         begin
            get_address(settings_file, KDF9.word(start));
         exception
            when others =>
               log_new_line;
               log_line("***** Error in lower address; no storepoint set.");
               return;
         end;
         log_new_line;
         log_line("Lower storepoint: #" & oct_of(start) & " (" & dec_of(start) & ")",
                  iff => the_log_is_wanted);
         begin
            get_address(settings_file, KDF9.word(end_point));
         exception
            when Data_Error =>
               log_line("      No upper address: one storepoint set.", iff => the_log_is_wanted);
               set_store_points(start, start);
               return;
         end;
         log_line("Upper storepoint: #" & oct_of(end_point) & " (" & dec_of(end_point) & ")",
                  iff => the_log_is_wanted);
         set_store_points(start, end_point);
      exception
         when others =>
            log_line("***** Error setting store_points; ignored.");
      end set_store_points;

      procedure set_watchpoints is
         start, end_point : KDF9.address;
      begin
         begin
            get_address(settings_file, KDF9.word(start));
         exception
            when others =>
               log_new_line;
               log_line("***** Error in lower address; no watchpoint set.");
               return;
         end;
         log_new_line;
         log_line("Lower watchpoint: #" & oct_of(start) & " (" & dec_of(start) & ")",
                  iff => the_log_is_wanted);
         begin
            get_address(settings_file, KDF9.word(end_point));
         exception
            when Data_Error =>
               log_line("      No upper address: one watchpoint set.", iff => the_log_is_wanted);
               set_store_points(start, start);
               return;
         end;
         log_line("Upper watchpoint: #" & oct_of(end_point) & " (" & dec_of(end_point) & ")",
                  iff => the_log_is_wanted);
         set_fetch_points(start, end_point);
         set_store_points(start, end_point);
      exception
         when others =>
            log_line("***** Error setting watch points; ignored.");
      end set_watchpoints;

      procedure set_specified_dumping_ranges (epoch : in dumping.flag) is
         use dumping.flag_support;
         format : dumping.format_set := no_dumping_flag or epoch;
         first_address, last_address : KDF9.address := 0;
         bad_range : Boolean := False;
         data : KDF9.word;
         c  : Character;
         OK : Boolean;
      begin
         while not End_Of_Line(settings_file) loop
            get(settings_file, c);
         exit when c = ' ';
            format := format or dumping_flag(c);
         end loop;
         log_new_line;
         log_line("Dump: format " & format_image(format), iff => the_log_is_wanted);
         if (format and is_parameter_flag) /= no_dumping_flag then
            get_address(settings_file, data);
            if data > max_address then
               log_line("***** Error: Lower dump address  = #" & oct_of(data) & " > 32K-1");
               bad_range := True;
            else
               first_address := KDF9.address(data);
               log_line("      Lower dump address: #" & oct_of(first_address)
                                               & " (" & dec_of(first_address) & ")",
                        iff => the_log_is_wanted);
            end if;
            get_address(settings_file, data);
            if data > max_address then
               log_line("***** Error: Upper dump address: #" & oct_of(data) & " > 32K-1");
               bad_range := True;
            else
               last_address := KDF9.address(data);
               log_line("      Upper dump address: #" & oct_of(last_address)
                                               & " (" & dec_of(last_address) & ")",
                        iff => the_log_is_wanted);
            end if;
         end if;
         if bad_range then
            log_line("***** Error: No dump specification set.");
         else
            request_a_dumping_area(format, first_address, last_address, OK);
         if not OK then
            log_line("***** Error: Too many dump specifications (ignored).");
         end if;
         end if;
         if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            log_new_line;
            log_line("***** Error in a dump area specification: "
                      & "format "
                      & format_image(format)
                      & "; Lower dump address = #" & oct_of(first_address)
                      & " (" & dec_of(first_address) & ")"
                      & "; Upper dump address = #" & oct_of(last_address)
                      & " (" & dec_of(last_address) & ")");
      end set_specified_dumping_ranges;

      procedure set_initial_dumping_ranges is
      begin
         set_specified_dumping_ranges(initial_dump_flag);
      end set_initial_dumping_ranges;

      procedure set_final_dumping_ranges is
      begin
         set_specified_dumping_ranges(final_dump_flag);
      end set_final_dumping_ranges;

      procedure set_time_limit is
      begin
         get_decimal(settings_file, KDF9.word(time_limit));
         if not counts_are_set then
            high_count := time_limit;
         end if;
         log_new_line;
         log_line("Time limit (in instructions) =" & KDF9.order_counter'Image(time_limit),
                  iff => the_log_is_wanted);
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            time_limit := time_limit_default;
            log_new_line;
            log_line("***** Error in a time limit; default used.");
            log_line("Time limit (in instructions) =" & KDF9.order_counter'Image(time_limit));
      end set_time_limit;

      procedure set_tracing_counts is

         procedure show_counts is
         begin
            if not the_log_is_wanted then return; end if;
            log_new_line;
            log_line("Lower tracing count:" & KDF9.order_counter'Image(low_count));
            log_line("Upper tracing count:" & KDF9.order_counter'Image(high_count));
         end show_counts;

      begin
         get_decimal(settings_file, KDF9.word(low_count));
         get_decimal(settings_file, KDF9.word(high_count));
         show_counts;
         if low_count > high_count then
            log_new_line;
            log_line("***** Error: Low count > high count");
            raise Data_Error;
         end if;
         counts_are_set := True;
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            low_count  := low_count_default;
            high_count := high_count_default;
            log_new_line;
            log_line("***** Error in a tracing count; defaults used.");
            show_counts;
      end set_tracing_counts;

      procedure set_tracing_range is

         procedure show_range is
         begin
            if not the_log_is_wanted then return; end if;
            log_new_line;
            log_line("Lower trace address: #" & oct_of(KDF9.code_point'(0, low_bound))
                                       & " (" & dec_of(KDF9.code_point'(0, low_bound)) & ")");
            log_line("Upper trace address: #" & oct_of(KDF9.code_point'(5, high_bound))
                                       & " (" & dec_of(KDF9.code_point'(5, high_bound)) & ")");
         end show_range;

      begin
         get_address(settings_file, KDF9.word(low_bound));
         get_address(settings_file, KDF9.word(high_bound));
         if low_bound > high_bound then
            log_new_line;
            log_line("***** Error: Low bound > high bound");
            raise Data_Error;
         end if;
         show_range;
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            low_bound  := low_bound_default;
            high_bound := high_bound_default;
            log_new_line;
            log_line("***** Error in a tracing address; defaults used.");
            show_range;
      end set_tracing_range;

      procedure set_diagnostic_mode is
         use diagnostic_mode_IO;
         the_diagnostic_mode : settings.diagnostic_mode;
      begin
         ensure_not_at_end_of_line(settings_file);
         get(settings_file, the_diagnostic_mode);
         set_diagnostic_mode(the_diagnostic_mode);
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            set_diagnostic_mode(the_diagnostics_default);
            log_new_line;
            log_line("***** Error in the diagnostic mode; default used.");
      end set_diagnostic_mode;

      procedure set_execution_mode is
         use execution_mode_IO;
      begin
         ensure_not_at_end_of_line(settings_file);
         get(settings_file, the_execution_mode);
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            the_execution_mode := the_execution_default;
            log_new_line;
            log_line("***** Error in the testing mode; default used.");
      end set_execution_mode;

      procedure set_authenticity is
         use authenticity_mode_IO;
      begin
         ensure_not_at_end_of_line(settings_file);
         get(settings_file, the_authenticity_mode);
         if the_authenticity_mode = authentic_time_mode then
            authentic_timing_is_wanted := True;
         end if;
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            the_authenticity_mode := the_authenticity_default;
            log_new_line;
            log_line("***** Error in the authenticity mode; default used.");
      end set_authenticity;

      procedure set_graph_plotting_pen is
         use postscript;
         use colour_IO;
         use  width_IO;
         the_colour  : pen_colour := the_default_colour;
         the_pen_tip : pen_tip_size  := the_default_pen_tip;

         procedure show_pen_options is
         begin
            if not the_log_is_wanted then return; end if;
            log_new_line;
            if the_colour /= the_default_colour then
               log_line("The graph plotter pen colour is " & pen_colour'Image(the_colour) & ".");
            end if;
            if the_pen_tip /= the_default_pen_tip then
               log_line("The graph plotter pen tip is " & pen_tip_size'Image(the_pen_tip) & ".");
            end if;
         end show_pen_options;

         procedure configure_the_plotter is
         begin
            if the_colour /= the_default_colour or the_pen_tip /= the_default_pen_tip then
               set_the_pen_properties(the_colour, the_pen_tip);
               show_pen_options;
            end if;
            the_graph_plotter_is_configured := True;
         end configure_the_plotter;

      begin  -- set_graph_plotting_pen
         ensure_not_at_end_of_line(settings_file);
         begin
            Get(settings_file, the_colour);
         exception
            when others =>
               log_new_line;
               log_line("***** Error in the plotter pen the_colour; default used.");
         end;
         ensure_not_at_end_of_line(settings_file);
         begin
            Get(settings_file, the_pen_tip);
         exception
            when others =>
               log_new_line;
               log_line("***** Error in the plotter pen tip; default used.");
         end;
         configure_the_plotter;
      exception
         when Data_Error =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            configure_the_plotter;
      end set_graph_plotting_pen;

      procedure set_non_interactivity is
      begin
         noninteractive_usage_is_enabled := True;
         time_limit := offline_time_limit;
         begin
            ensure_not_at_end_of_line(settings_file);
            get_decimal(settings_file, KDF9.word(time_limit));
         exception
            when others =>
               if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
               log_new_line;
               log_line("***** Error in a time limit; default used.");
         end;
         log_new_line;
         log_line("Non-interactive mode; time limit (in instructions) ="
                & KDF9.order_counter'Image(time_limit),
                  iff => the_log_is_wanted);
      end set_non_interactivity;

   begin
      high_count := time_limit;
      open_options_file(settings_file, the_settings_file_name);
      if end_of_file(settings_file) then
         raise End_Error;
      end if;

      loop
         skip_to_next_nonempty_line(settings_file);
         get(settings_file, flag);
         the_final_state_is_wanted := True;
         case flag is
            when 'A' | 'a' =>
               set_authenticity;
            when 'B' | 'b' =>
               set_breakpoints;
            when 'C' | 'c' =>
               set_tracing_counts;
            when 'D' | 'd' =>
               set_diagnostic_mode;
            when 'F' | 'f' =>
               set_fetch_points;
            when 'G' | 'g' =>
               set_graph_plotting_pen;
            when 'I' | 'i' =>
               set_initial_dumping_ranges;
            when 'L' | 'l' =>
               set_time_limit;
            when 'N' | 'n' =>
               set_non_interactivity;
            when 'P' | 'p' =>
               set_final_dumping_ranges;
            when 'Q' | 'q' =>
               raise quit_request;
            when 'R' | 'r' =>
               set_tracing_range;
            when 'S' | 's' =>
               set_store_points;
            when 'T' | 't' =>
               set_execution_mode;
            when 'V' | 'v' =>
               set_this_miscellany_flag;
            when 'W' | 'w' =>
               set_watchpoints;
            when 'X' | 'x' =>
               only_signature_tracing := True;
            when others =>
               log_new_line;
               log_line("Invalid flag: """
                      & flag
                      & """ at line/column "
                      & Integer'Image(line_number) & "/"
                      & Ada.Text_IO.Count'Image(Col(settings_file))
                      & " of the settings file!");
               log_line(" ...  the valid flags are A,B,C,D,F,G,I,L,N,P,Q,R,S,T,V,W,X, and |");
               skip_line(settings_file);
         end case;
      end loop;

   exception

      when End_Error =>
         close_options_file(settings_file, the_settings_file_name);

      when Data_Error =>
         close_options_file(settings_file, the_settings_file_name);
         log_new_line;
         log_line("***** Error: invalid data in the settings file.");
         log_line("Reading of settings abandoned at line "
                & Integer'Image(line_number)
                & " of '" & the_settings_file_name & "'.");

      when Status_Error =>
         log_new_line;
         log_line("***** Error: could not read from "
                & the_settings_file_name
                & " - default settings in force.");

      when quit_request =>
         close_options_file(settings_file, the_settings_file_name);
         log_new_line;
         log_line("Quit requested at line "
                & Integer'Image(line_number)
                & " of '" & the_settings_file_name & "'.");
         log_rule;
         raise;

      when error : others =>
         close_options_file(settings_file, the_settings_file_name);
         log_new_line;
         log_line("Failure in ee9: "
                & Ada.Exceptions.Exception_Information(error)
                & " was raised in 'get_settings_from_file'!");
         log_line("Reading of settings abandoned at line "
                & Integer'Image(line_number)
                & " of '" & the_settings_file_name & "'!");
         log_rule;
         raise emulation_failure;

   end get_settings_from_file;

end settings;
