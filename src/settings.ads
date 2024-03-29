-- execution mode, diagnostic mode, and other emulation-control settings
--
-- This file is part of ee9 (9.0p), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2022, W. Findlay; all rights reserved.
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

with KDF9;
with logging.file;

use  KDF9;
use  logging.file;

package settings is

--
   -- In fast mode: code runs as efficiently as possible, without diagnostics.

   -- In trace mode: breakpoints, watchpoints, tracing address bounds and
   --    tracing instruction count bounds are all honoured;
   --       entries may be made in all the retrospective trace logs;
   --          a digital execution signature may be computed,
   --             and an instruction-frequency histogram may be generated.

   -- In pause mode: execution proceeds as in trace mode;
   --    additionally, breakpoints occur on every order executed within trace bounds.

   -- The external mode is a user-interface value only. It requests the trace mode,
   --    combined with the logging of a running trace to an external file.
--

   type diagnostic_mode is (fast_mode,
                            trace_mode,
                            pause_mode,
                            external_mode);

   procedure set_diagnostic_mode (a_diagnostic_mode : in settings.diagnostic_mode);

   procedure change_diagnostic_mode_if_requested;

   the_diagnostics_default  : constant settings.diagnostic_mode := fast_mode;
   the_diagnostic_mode      : settings.diagnostic_mode := the_diagnostics_default;

   the_external_trace_file_name : constant String := "trace.txt";
   only_signature_tracing       : Boolean := False;
   the_external_trace_file      : logging.file.output;

   -- The diagnostic generation and display controls, inter alia.
   -- The *_trace_is_wanted flags are set to True iff
   --    they are both requested, and offered by the_diagnostic_mode.
   -- These requests may be set by the miscellany and visibilty options.

   miscellany_flags  : constant String := "abcdefghiknopqrstwxyz.0123456789ABCDEFGHIKMNOPQRSTWXYZ";
   miscellany_prompt : constant String := "{a|b|c|d|e|f|g|h|i|k|n|o|p|q|r|s|t|w|x|z|.|0..9}";

   -- *_is_wanted  iff the facility is provided by default.
   -- *_is_enabled iff the facility is wanted and not suppressed by other considerations,
   --    such as the diagnostic mode (e.g. fast mode suppresses all tracing).

   API_logging_is_wanted,
   flexowriter_output_is_wanted,
   histogramming_is_wanted,
   interrupt_tracing_is_wanted,
   peripheral_tracing_is_wanted,
   realistic_FW_output_is_wanted,
   retrospective_tracing_is_wanted,
   the_final_state_is_wanted,
   the_log_is_wanted,
   the_signature_is_wanted         : Boolean := True;

   authentic_timing_is_enabled,
   core_file_is_enabled,
   debugging_is_enabled,
   do_not_execute,
   histogramming_is_enabled,
   interrupt_tracing_is_enabled,
   noninteractive_usage_is_enabled,
   peripheral_tracing_is_enabled,
   pre_overlay_state_is_enabled,
   retrospective_tracing_is_enabled,
   the_external_trace_is_enabled,
   the_signature_is_enabled        : Boolean := False;

   procedure reset_default_visibility_options;

   -- In boot_mode: a Director program is read from TR0 and executed
   --    in Director state, with full use of the emulated hardware.
   -- In program_mode: a user program is read from TR0 and executed
   --    in program state, with basic OUTs implemented by the emulator.
   -- In privileged_mode: a user program is read from TR0 and executed
   --    as in program state, but allowing orders that should LIV to succeed,
   --    this being useful for executing "hardware test" programs.

   type execution_mode is (boot_mode, program_mode, privileged_mode);

   procedure set_execution_mode (an_execution_mode : in settings.execution_mode);

   the_execution_default   : constant settings.execution_mode := program_mode;
   the_execution_mode      :          settings.execution_mode := the_execution_default;

   this_is_a_bare_Director : Boolean := False;

   --
   -- Tracing bound settings.
   --

   -- time_limit bounds the number of KDF9 instructions executed.

   time_limit_default : constant KDF9.order_counter := KDF9.order_counter'Last;
   time_slice         : constant KDF9.order_counter := 10_000;
   offline_time_limit : constant KDF9.order_counter := 10_000 * time_slice;
   time_limit         :          KDF9.order_counter := time_limit_default;


   -- low_bound and high_bound bound the static scope of tracing.

   low_bound_default  : constant KDF9.code_address := 0;
   high_bound_default : constant KDF9.code_address := KDF9.code_address'Last;
   low_bound          :          KDF9.code_address := low_bound_default;
   high_bound         :          KDF9.code_address := high_bound_default;

   -- low_count and high_count bound the dynamic scope of tracing.

   low_count_default  : constant KDF9.order_counter := 0;
   high_count_default : constant KDF9.order_counter := time_limit_default;
   low_count          :          KDF9.order_counter := low_count_default;
   high_count         :          KDF9.order_counter := high_count_default;

   -- Histogram bin frequencies less than histogram_cutoff are not logged.
   the_profile_is_wanted  :          Boolean := False;
   the_INS_plot_is_wanted :          Boolean := False;
   cutoff_default         : constant Long_Float := 0.0;
   histogram_cutoff       :          Long_Float := cutoff_default;

   function is_invalid_miscellany_flag (option : in Character)
   return Boolean;

   procedure set_this_miscellany_flag (option : in Character);

   procedure get_settings_from_file (version : in String);

   procedure display_execution_modes (for_this : in String := "");

   procedure quit_if_requested;

   quit_was_requested          : Boolean := False;

   the_diagnostic_mode_changed : Boolean := False;

   loading_was_successful      : Boolean := False;

   mode_change_request         : exception;

end settings;
