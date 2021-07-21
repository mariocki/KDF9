-- execution mode, diagnostic mode, and other emulation-control settings
--
-- This file is part of ee9 (8.0k), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Exceptions;
with Ada.Long_Float_Text_IO;
with Ada.Text_IO;
--
with dumping;
with exceptions;
with formatting;
with HCI;
with IOC.equipment;
with KDF9.store;
with postscript;
with settings.IO;
with disassembly.symbols;
with tracing;

use  Ada.Exceptions;
use  Ada.Long_Float_Text_IO;
use  Ada.Text_IO;
--
use  dumping;
use  exceptions;
use  formatting;
use  HCI;
use  KDF9.store;
use  settings.IO;
use  disassembly.symbols;
use  tracing;

package body settings is

   procedure reset_default_visibility_options is
   begin
      API_logging_is_wanted            := True;
      flexowriter_output_is_wanted     := True;
      histogramming_is_wanted          := True;
      interrupt_tracing_is_wanted      := True;
      peripheral_tracing_is_wanted     := True;
      realistic_FW_output_is_wanted    := True;
      retrospective_tracing_is_wanted  := True;
      the_final_state_is_wanted        := True;
      the_log_is_wanted                := True;
      the_signature_is_wanted          := True;
      authentic_timing_is_enabled      := False;
      debugging_is_enabled             := False;
      histogramming_is_enabled         := False;
      interrupt_tracing_is_enabled     := False;
      peripheral_tracing_is_enabled    := False;
      retrospective_tracing_is_enabled := False;
      the_signature_is_enabled         := False;
   end reset_default_visibility_options;

   function is_invalid_miscellany_flag (option : in Character)
   return Boolean is
   begin
      for f of miscellany_flags loop
         if f = option then
            return False;
         end if;
      end loop;
      if option = '-' then  -- Ignore hyphens to make the calling scripts easier.
         return False;
      end if;
      return True;
   end is_invalid_miscellany_flag;

   procedure set_this_miscellany_flag (option : in Character) is
      use IOC.equipment;
   begin
      if is_invalid_miscellany_flag(option) then
         log_line(
                  "***** Error in a miscellany specification: «"
                & option
                & "»."
                 );
         return;
      end if;
      case option is
         when '-'        =>
            null;  -- Ignore hyphens, to make the calling scripts easier.
         when '.'        =>
            time_limit := 1_000_000;
         when '0' .. '9' =>
            time_limit := (Character'Pos(option) - Character'Pos('0') + 1) * 100_000_000;
         when 'a' | 'A' =>
            API_logging_is_wanted := False;
         when 'b' | 'B' =>
            choice(KDF9.buffer_number'(15)) := SI;
         when 'd' | 'D' =>
            debugging_is_enabled := True;
         when 'e' | 'E' =>
            the_log_is_wanted := False;
         when 'f' | 'F' =>
            the_final_state_is_wanted := False;
         when 'g' | 'G' =>
            choice(if TP1_number = 0 then TP1_default else TP1_number) := GP;
         when 'h' | 'H' =>
            histogramming_is_wanted := False;
         when 'i' | 'I' =>
            interrupt_tracing_is_wanted := False;
         when 'k' | 'K' =>
            choice(DR0_default) := DR;
         when 'n' | 'N' =>
            noninteractive_usage_is_enabled := True;
            time_limit := offline_time_limit;
          when 'o' |'O' =>
            pre_overlay_state_is_enabled := True;
         when 'p' |'P' =>
            peripheral_tracing_is_wanted := False;
         when 'q' | 'Q' =>
            do_not_execute := True;
         when 'r' | 'R' =>
            retrospective_tracing_is_wanted := False;
         when 's' | 'S' =>
            the_signature_is_wanted := False;
         when 't' | 'T' =>
            authentic_timing_is_enabled := True;
         when 'w' | 'W' =>
            flexowriter_output_is_wanted := False;
         when 'x' | 'X' =>
            only_signature_tracing := True;
         when 'y' | 'Y' =>
            this_is_a_bare_Director := True;
         when 'z' | 'Z' =>
            API_logging_is_wanted           := False;
            debugging_is_enabled            := False;
            histogramming_is_wanted         := False;
            interrupt_tracing_is_wanted     := False;
            peripheral_tracing_is_wanted    := False;
            retrospective_tracing_is_wanted := False;
            the_final_state_is_wanted       := False;
            the_log_is_wanted               := False;
            the_signature_is_wanted         := False;
         when others =>
            null;
      end case;
      set_diagnostic_mode(the_diagnostic_mode);
   end set_this_miscellany_flag;

   procedure display_execution_modes (for_this : in String := "") is
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

      function run_type (type_of_run : String)
      return String
      is (if for_this = "" then type_of_run else type_of_run & " " & for_this);

   begin -- display_execution_modes
      if not the_log_is_wanted then return; end if;
      log_new_line;
      if for_this = "" then
         log("Resuming the run");
      else
         log(
             case the_execution_mode is
               when boot_mode        => (if this_is_a_bare_Director then
                                            "Running the bare " & run_type("Director")
                                         else
                                            "Booting the KDF9 " & run_type("Director")
                                        ),
               when program_mode     => "Running the KDF9 " & run_type("problem program"),
               when privileged_mode  => "Running the KDF9 " & run_type("privileged program")
            );
      end if;
      log(" in ");
      log(
          case the_diagnostic_mode is
             when trace_mode    =>
                (if the_external_trace_is_enabled then "external trace mode" else "trace mode"),
             when fast_mode     => "fast mode",
             when pause_mode    => "pause mode",
             when external_mode => "external trace mode"
         );

      if authentic_timing_is_enabled      or else
         debugging_is_enabled             or else
         histogramming_is_enabled         or else
         interrupt_tracing_is_enabled     or else
         noninteractive_usage_is_enabled  or else
         peripheral_tracing_is_enabled    or else
         retrospective_tracing_is_enabled or else
         the_external_trace_is_enabled    or else
         the_signature_is_enabled            then

         log_line(", with option(s):");
         log("   ");
         append_option(authentic_timing_is_enabled,      "authentic timing");
         append_option(debugging_is_enabled,             "debugging output");
         append_option(histogramming_is_enabled,         "histogram(s)");
         append_option(interrupt_tracing_is_enabled,     "interrupt trace");
         append_option(noninteractive_usage_is_enabled,  "noninteractive");
         append_option(peripheral_tracing_is_enabled,    "peripheral trace");
         append_option(retrospective_tracing_is_enabled, "retro trace");
         append_option(the_external_trace_is_enabled,    "external trace");
         append_option(the_signature_is_enabled,         "signature hash");
      end if;
      log_line(".");
      log_rule;
   end display_execution_modes;

   procedure quit_if_requested is
   begin
      if quit_was_requested then
         raise quit_request with ""; -- "" suppresses the default message.
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
      tracing_is_allowed    : constant Boolean := a_diagnostic_mode /= fast_mode;
      interrupts_can_happen : constant Boolean := the_execution_mode = boot_mode and tracing_is_allowed;
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
      histogramming_is_enabled         := histogramming_is_wanted         and tracing_is_allowed;
      interrupt_tracing_is_enabled     := interrupt_tracing_is_wanted     and interrupts_can_happen;
      peripheral_tracing_is_enabled    := peripheral_tracing_is_wanted    and tracing_is_allowed;
      retrospective_tracing_is_enabled := retrospective_tracing_is_wanted and tracing_is_allowed;
      the_signature_is_enabled         := the_signature_is_wanted         and tracing_is_allowed;
   end set_diagnostic_mode;

   procedure set_execution_mode (an_execution_mode : in settings.execution_mode) is
   begin
      the_execution_mode := an_execution_mode;
   end set_execution_mode;

   package diagnostic_mode_IO   is new Ada.Text_IO.Enumeration_IO(settings.diagnostic_mode);

   package execution_mode_IO    is new Ada.Text_IO.Enumeration_IO(settings.execution_mode);

   package authenticity_mode_IO is new Ada.Text_IO.Enumeration_IO(KDF9.authenticity_mode);

   package equipment_IO         is new Ada.Text_IO.Enumeration_IO(IOC.equipment.kind);

   procedure get_settings_from_file (version : in String) is

      the_settings_file_name : constant String := "settings_" & version & ".txt";
      HT                     : constant Character := Character'Val(9);
      counts_are_set : Boolean := False;
      settings_file  : File_Type;
      flag           : Character;

      procedure set_the_miscellany_flags is
         c  : Character := ' ';
      begin
         skip_to_next_non_blank(settings_file);
         if End_Of_Line(settings_file) then
            reset_default_visibility_options;
            return;
         end if;
         loop
            get(settings_file, c);
            if is_invalid_miscellany_flag(c) then
               if c not in  ' ' | HT  then
                  raise Data_Error;
               end if;
            else
               set_this_miscellany_flag(c);
            end if;
         exit when End_Of_Line(settings_file);
         end loop;
      exception
         when Data_Error =>
            if not End_Of_Line(settings_file) then
               Skip_Line(settings_file);
            end if;
            log_new_line;
            log_line(
                     "***** Error in a miscellany specification: "
                   & (if c = ' ' then "no option was given." else "invalid data «"& c & "».")
                    );
      end set_the_miscellany_flags;

      procedure set_breakpoints is
         start, end_point : KDF9.code_address;
      begin
         begin
            get_word(settings_file, KDF9.word(start));
         exception
            when others =>
               log_new_line;
               log_line("***** Error in lower address; no breakpoint set.");
               return;
         end;

         log_new_line;
         log_line(
                  "Lower breakpoint: "
                & oct_of(KDF9.syllable_address'(start, 0))
                & " ("
                & dec_of(KDF9.syllable_address'(start, 0))
                & ")",
                  iff => the_log_is_wanted
                 );
         breakpoints(start) := True;

         begin
            get_word(settings_file, KDF9.word(end_point));
         exception
            when Data_Error =>
               log_line("      No upper address: one breakpoint set.", iff => the_log_is_wanted);
               set_breakpoints(start, start);
               return;
         end;
         log_line(
                  "Upper breakpoint: "
                & oct_of(KDF9.syllable_address'(end_point, 5))
                & " (" & dec_of(KDF9.syllable_address'(end_point, 5))
                & ")",
                  iff => the_log_is_wanted
                 );
         set_breakpoints(start, end_point);
      exception
         when others =>
            log_line("***** Error setting breakpoints; ignored.");
      end set_breakpoints;

      procedure set_store_points is
         start, end_point : KDF9.address;
      begin
         begin
            get_word(settings_file, KDF9.word(start));
         exception
            when others =>
               log_new_line;
               log_line("***** Error in lower address; no storepoint set.");
               return;
         end;
         log_new_line;
         log_line(
                  "Lower storepoint: #"
                & oct_of(start)
                & " ("
                & dec_of(start)
                & ")",
                  iff => the_log_is_wanted
                 );
         begin
            get_word(settings_file, KDF9.word(end_point));
         exception
            when Data_Error =>
               log_line("      No upper address: one storepoint set.", iff => the_log_is_wanted);
               set_store_points(start, start);
               return;
         end;
         log_line(
                  "Upper storepoint: #"
                & oct_of(end_point)
                & " ("
                & dec_of(end_point)
                & ")",
                  iff => the_log_is_wanted
                 );
         set_store_points(start, end_point);
      exception
         when others =>
            log_line("***** Error setting storepoints; ignored.");
      end set_store_points;

      procedure set_watchpoints is
         start, end_point : KDF9.address;
      begin
         begin
            get_word(settings_file, KDF9.word(start));
         exception
            when others =>
               log_new_line;
               log_line("***** Error in lower address; no watchpoint set.");
               return;
         end;
         log_new_line;
         log_line(
                  "Lower watchpoint: #"
                & oct_of(start)
                & " ("
                & dec_of(start)
                & ")",
                  iff => the_log_is_wanted
                 );
         begin
            get_word(settings_file, KDF9.word(end_point));
         exception
            when Data_Error =>
               log_line("      No upper address: one watchpoint set.", iff => the_log_is_wanted);
               set_store_points(start, start);
               set_fetch_points(start, start);
               return;
         end;
         log_line("Upper watchpoint: #" & oct_of(end_point) & " (" & dec_of(end_point) & ")",
                  iff => the_log_is_wanted);
         set_fetch_points(start, end_point);
         set_store_points(start, end_point);
      exception
         when others =>
            log_line("***** Error setting watchpoints; ignored.");
      end set_watchpoints;

      procedure set_specified_dumping_ranges (epoch : in dumping.flag) is
         use dumping.flag_support;
         epoch_flag   : constant Character := (if epoch = initial_flag then 'I' else 'F');
         format       : dumping.format_set := no_dumping_flags or epoch;
         first_address,
         last_address : KDF9.address := 0;
         bad_range    : Boolean := False;
         data         : KDF9.word;
         c            : Character;
         OK           : Boolean;
      begin
         log("Dump: format " & epoch_flag, iff => the_log_is_wanted);
         while not End_Of_Line(settings_file) loop
            get(settings_file, c);
            log(c, iff => the_log_is_wanted);
         exit when c = ' ';
            if is_parameter_flag/dumping_flag(to_upper(c)) then
               format := format or dumping_flag(to_upper(c));
            else
               if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
               log_new_line;
               log_line("***** Error: «"& c & "» is not a valid dump type.");
               return;
            end if;
         end loop;
         log_new_line;

         if (format and is_parameter_flag) /= no_dumping_flags then
            get_word(settings_file, data);
            if data > max_address                     or else
                  (format/Usercode_flag and data > 8191) then
               log_line(
                        "***** Error: Lower dump address  = #"
                      & oct_of(data)
                      & " =" & data'Image
                      & " is too large for this option."
                       );
               bad_range := True;
            else
               first_address := KDF9.address(data);
               last_address  := KDF9.address(data);
               log_line(
                        "      Lower dump address: #"
                      & oct_of(first_address)
                      & " (" & dec_of(first_address)
                      & ")",
                        iff => the_log_is_wanted
                       );
            end if;

           skip_to_next_non_blank (settings_file);

            if not end_of_line(settings_file) then
               get_word(settings_file, data);
               if data > max_address                     or else
                  (format/Usercode_flag and data > 8191) then
                  log_line(
                           "***** Error: Upper dump address: #"
                         & oct_of(data)
                         & " =" & data'Image
                         & " is too large for this option."
                          );
                  bad_range := True;
               else
                  last_address := KDF9.address(data);
                  log_line(
                           "      Upper dump address: #"
                         & oct_of(last_address)
                         & " ("
                         & dec_of(last_address)
                         & ")",
                           iff => the_log_is_wanted
                          );
               end if;
            end if;

            if first_address > last_address then
               log_line(
                        "***** Error: Upper dump address: #"
                      & oct_of(last_address)
                      & " =" & last_address'Image
                      & " is less than lower dump address: #"
                      & oct_of(first_address)
                      & " =" & first_address'Image
                      & "."
                       );
               bad_range := True;
            end if;
         end if;

         if not bad_range then
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
            log_line("***** Error in a dump area specification (ignored)." );
      end set_specified_dumping_ranges;

      procedure set_initial_dumping_ranges is
      begin
         set_specified_dumping_ranges(initial_flag);
      end set_initial_dumping_ranges;

      procedure set_final_dumping_ranges is
      begin
         set_specified_dumping_ranges(final_flag);
      end set_final_dumping_ranges;

      procedure set_histogram_options is
         c : Character;
      begin
         while not End_Of_Line(settings_file) loop
            get(settings_file, c);
         exit when c = ' ';
            if c not in 'P' | 'p' | 'T' | 't' then
               raise Data_Error;
            end if;
            if c in 'P' | 'p' then
               the_profile_is_wanted  := True;
               clear_the_profile;
            elsif c in  'T' | 't' then
               the_INS_plot_is_wanted := True;
               clear_the_histogram;
            end if;
         end loop;
         ensure_not_at_end_of_line(settings_file);
         get(settings_file, histogram_cutoff);
         if histogram_cutoff >= 100.0 or histogram_cutoff < 0.0 then
            raise Data_Error;
         end if;
         get(settings_file, c);
         if c /= '%' then
            raise Data_Error;
         end if;
      exception
         when others =>
            histogram_cutoff := cutoff_default;
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            log_new_line;
            log_line("***** Error in the histogram option; default used.");
      end set_histogram_options;

      procedure set_time_limit is
         begin
            begin
            get_decimal(settings_file, KDF9.word(time_limit));
         exception
            when others =>
               if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
               time_limit := offline_time_limit;
         end;

         if not counts_are_set then
            high_count := time_limit;
         end if;

         log_new_line;
         log_line("Time limit (in instructions) =" & time_limit'Image,
                  iff => the_log_is_wanted);
      end set_time_limit;

      procedure set_tracing_counts is

         procedure show_counts is
         begin
            if not the_log_is_wanted then return; end if;
            log_new_line;
            log_line("Lower tracing count:" & low_count'Image);
            log_line("Upper tracing count:" & high_count'Image);
         end show_counts;

      begin
         get_decimal(settings_file, KDF9.word(low_count));
         get_decimal(settings_file, KDF9.word(high_count));
         show_counts;
         if low_count > high_count then
            log_new_line;
            log_line("***** Error: Low count > high count.");
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
            log_line(
                     "Lower trace address: #"
                   & oct_of(KDF9.syllable_address'(low_bound, 0))
                   & " ("
                   & dec_of(KDF9.syllable_address'(low_bound, 0))
                   & ")"
                    );
            log_line(
                     "Upper trace address: #"
                   & oct_of(KDF9.syllable_address'(high_bound, 5))
                   & " ("
                   & dec_of(KDF9.syllable_address'(high_bound, 5))
                   & ")"
                    );
         end show_range;

      begin
         get_word(settings_file, KDF9.word(low_bound));
         get_word(settings_file, KDF9.word(high_bound));
         if low_bound > high_bound then
            log_new_line;
            log_line("***** Error: Low bound > high bound.");
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
            authentic_timing_is_enabled := True;
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
         the_colour   : pen_colour   := the_default_colour;
         the_pen_size : pen_tip_size := the_default_tip_size;

         procedure show_pen_options is
         begin
            if not the_log_is_wanted then return; end if;
            log_new_line;
            if the_colour /= the_default_colour then
               log_line("The graph plotter pen colour is " & the_colour'Image & ".");
            end if;
            if the_pen_size /= the_default_tip_size then
               log_line("The graph plotter pen tip is " & the_pen_size'Image & ".");
            end if;
         end show_pen_options;

         procedure configure_the_plotter is
         begin
            if the_colour /= the_default_colour or the_pen_size /= the_default_tip_size then
               set_the_pen_properties(the_colour, the_pen_size);
               show_pen_options;
            end if;
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
            Get(settings_file, the_pen_size);
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
         set_time_limit;
      end set_non_interactivity;

      procedure save_poke_value is
         -- W: full Word, U: Upper halfword, L: Lower halfword, S: Syllable, C: Character
         address  : KDF9.address;
         sub_word : Character;
         position : KDF9.address;
         value    : KDF9.word;
         OK       : Boolean;
      begin
         begin
            get_word(settings_file, KDF9.word(address));
         exception
            when others =>
               log_line("***** Error in poke word address.");
               Skip_Line(settings_file);
               return;
         end;

         get_char(settings_file, sub_word);
         if sub_word not in 'S' | 's' | 'C' | 'c' | 'L' | 'l' | 'U' | 'u' | 'W' | 'w' then
            log_line(
                     "***** Error in (sub)word indicator; "
                   & sub_word
                   & " should be W, L, U, S, or C."
                    );
            Skip_Line(settings_file);
            return;
         end if;

         if sub_word in 'S' | 's' | 'C' | 'c' then
            begin
               get_word(settings_file, KDF9.word(position));
               if (sub_word in 'S' | 's' and position > 5) or else
                  (sub_word in 'C' | 'c' and position > 7)    then
                  log_line(
                           "***** Error in position given for a "
                         & (if sub_word in 'S' | 's' then "syllable:" else "character:")
                         & position'Image
                         & " is too large, poke request ignored."
                          );
                  Skip_Line(settings_file);
                  return;
               end if;
            exception
               when others =>
                  log_line(
                           "***** Error in position given for a "
                         & (if sub_word in 'S' | 's' then "syllable" else "character")
                         & ", poke request ignored."
                          );
                  Skip_Line(settings_file);
                  return;
            end;
         else
            position := 0;
         end if;

         begin
            get_word(settings_file, value);
         exception
            when others =>
               log_line("***** Error in poked value.");
               Skip_Line(settings_file);
               return;
         end;

         if (sub_word in 'L' | 'l' | 'U' | 'u' and value > 2**24-1) or else
               (sub_word in 'S' | 's'          and value > 255)     or else
                  (sub_word in 'C' | 'c'       and value > 63)      then
            log_line(
                     "***** Error in poked value #"
                   & oct_of(value)
                   & ": out of range for a "
                   & (case sub_word is
                         when 'L' | 'l' | 'U' | 'u' => "halfword",
                         when 'S' | 's'             => "syllable",
                         when 'C' | 'c'             => "character",
                         when others                => "word")
                   & ", poke request ignored."
                    );
            Skip_Line(settings_file);
            return;
         end if;

         add_to_poke_list(address, sub_word, position, value, OK);

         if not OK then
            log_line("***** Error setting up a poke: poke list full; request ignored.");
         end if;

      exception

         when others =>
            null;  -- to skip line at end of input loop

      end save_poke_value;

      procedure set_KDF9_configuration is
         use equipment_IO;
         use IOC.equipment;
         d : IOC.equipment.kind := AD;
         b : KDF9.buffer_number;
      begin
         if version = "1" then
            for i in IOC.equipment.setup'Range loop
            exit when end_of_line(settings_file);
               get_word(settings_file, KDF9.word(b));
               ensure_not_at_end_of_line(settings_file);
               get(settings_file, d);
               IOC.equipment.choice(b) := d;
            end loop;
         else
            log_new_line;
            log_line("The previous KDF9 configuration is still being used.");
         end if;
         if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
      exception
         when others =>
            if not End_Of_Line(settings_file) then Skip_Line(settings_file); end if;
            IOC.equipment.choice := IOC.equipment.default;
            log_new_line;
            log_line("***** Error in the device configuration; defaults used.");
      end set_KDF9_configuration;

      procedure set_symbols is
         c : Character := ' ';
         w : KDF9.word;
         a : KDF9.address;
         v : KDF9.address;
         p : Natural;
      begin
         if End_Of_Line(settings_file) then
            clear_all_symbol_definitions;
            return;
         end if;
         get(settings_file, c);
         if    c = '#' then
            get(settings_file, c);
            get_word(settings_file, KDF9.word(a));
            if c = 'V' then
               declare_P0(a);
            elsif c = 'Y' then
               define_Y_size(if a = KDF9.address'Last then 0 else a);
            else
               raise Data_Error;
            end if;
         elsif c = 'W' then
            get_word(settings_file, KDF9.word(a));
            define_W0(a);
         elsif c = 'Y' then
            get_word(settings_file, KDF9.word(a));
            define_Y0(a);
         elsif c = 'Z' then
            get_word(settings_file, KDF9.word(a));
            define_Z0(a);
         elsif c = 'P' then
            loop
               skip_to_next_non_blank(settings_file);
               if End_Of_Line(settings_file) then
                  return;
               end if;
               get_word(settings_file, KDF9.word(p));
               skip_to_next_non_blank(settings_file);
               get(settings_file, c);
            exit when c /= '#';
               get_decimal(settings_file, w);
               v := KDF9.address(w+1);
               skip_to_next_non_blank(settings_file);
               get(settings_file, c);
            exit when c /= '@';
               get_word(settings_file, KDF9.word(a));
               declare_Pp(p, v, a);
            end loop;
         elsif c = ' ' then
            loop
               skip_to_next_non_blank(settings_file);
               if End_Of_Line(settings_file) then
                  return;
               end if;
               get(settings_file, c);
               if c not in 'A' .. 'Z'  then
                     raise Data_Error;
               end if;
               get_word(settings_file, KDF9.word(a));
               define_Yy0(c, a);
            end loop;
         else
            raise Data_Error;
         end if;
      exception
         when Data_Error =>
            if not End_Of_Line(settings_file) then
               Skip_Line(settings_file);
            end if;
            log_new_line;
            log_line(
                     "***** Error in a Y flag specification: "
                   & "invalid data after «"
                   & c
                   & "»."
                    );
      end set_symbols;

      error_count : Natural := 0;

   begin -- get_settings_from_file

      do_not_execute := False;
      high_count := time_limit;
      open_options_file(settings_file, the_settings_file_name);
      if End_of_File(settings_file) then
         raise End_Error;
      end if;

      loop
         skip_to_next_nonempty_line(settings_file);
         get(settings_file, flag);
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
               set_final_dumping_ranges;
            when 'G' | 'g' =>
               set_graph_plotting_pen;
            when 'H' | 'h' =>
               set_histogram_options;
            when 'I' | 'i' =>
               set_initial_dumping_ranges;
            when 'K' | 'k' =>
               set_KDF9_configuration;
            when 'L' | 'l' =>
               set_time_limit;
            when 'N' | 'n' =>
               set_non_interactivity;
               time_limit := offline_time_limit;
            when 'O' |'o' =>
               set_this_miscellany_flag(flag);
            when 'P' | 'p' =>
               save_poke_value;
            when 'Q' | 'q' =>
               do_not_execute := True;
            when 'R' | 'r' =>
               set_tracing_range;
            when 'S' | 's' =>
               set_store_points;
            when 'T' | 't' =>
               set_execution_mode;
            when 'V' | 'v' =>
               set_the_miscellany_flags;
            when 'W' | 'w' =>
               set_watchpoints;
            when 'X' | 'x' =>
               only_signature_tracing := True;
            when 'Y' | 'y' =>
               set_symbols;
            when '-' | '/' =>
               Skip_Line(settings_file);
            when others =>
               if error_count > 10 then
                  log_new_line;
                  log_line("There are too many invalid flags in " & the_settings_file_name & ".");
                  raise operator_error;
               end if;
               error_count := error_count + 1;
               log_line(
                        "Invalid flag: """
                      & flag
                      & """ at line "
                      & line_number'Image
                      & " of the settings file!"
                       );
               log_line("The valid flags are A,B,C,D,F,G,I,K,L,N,O,P,Q,R,S,T,V,W,X, -, and /");
               Skip_Line(settings_file);
               line_number := line_number + 1;
         end case;
      end loop;

   exception

      when Status_Error =>
         log_line("***** Error: " & the_settings_file_name & " was not found; defaults used.");
         log_new_line;

      when End_Error =>
         close_options_file(settings_file, the_settings_file_name);

      when operator_error  =>
         trap_operator_error("Abandoning the run");

      when Data_Error =>
         close_options_file(settings_file, the_settings_file_name);
         if error_count < 10 then
            log_new_line;
            log_line("***** Error: invalid data in the settings file.");
            log_line(
                     "Reading of settings abandoned at line "
                   & line_number'Image
                   & " of "
                   & the_settings_file_name
                   & "."
                    );
         else
            raise;
         end if;

      when error : others =>
         close_options_file(settings_file, the_settings_file_name);
         log_new_line;
         log_line(
                  "Failure in ee9; unexpected exception: "
                & Exception_Information(error)
                & " in get_settings_from_file!"
                 );
         log_line(
                  "Reading of settings abandoned at line "
                & line_number'Image
                & " of "
                & the_settings_file_name
                & "!"
                 );
         log_rule;
         raise;

   end get_settings_from_file;

end settings;
