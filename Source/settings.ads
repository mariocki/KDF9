-- settings.ads
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

with KDF9;
with logging.file;

use  KDF9;
use  logging.file;

package settings is

   pragma Unsuppress(All_Checks);

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

   miscellany_flags  : constant String := "adefghilnprstz0123456789";
   miscellany_prompt : constant String := "{a|d|e|f|g|h|i|l|n|p|r|s|t|z|1..9}";

   the_log_is_wanted,
   API_logging_is_requested,
   the_signature_is_requested,
   the_histogram_is_requested,
   peripheral_tracing_is_requested,
   interrupt_tracing_is_requested,
   retrospective_tracing_is_requested,
   the_final_state_is_wanted  : Boolean := True;

   the_graph_plotter_is_configured,
   noninteractive_usage_is_enabled,
   debugging_is_enabled,
   the_signature_is_enabled,
   the_histogram_is_enabled,
   the_peripheral_trace_is_enabled,
   the_interrupt_trace_is_enabled,
   the_retrospective_trace_is_enabled,
   the_external_trace_is_enabled  : Boolean := False;

   -- This option may also be set by an authenticity option.
   authentic_timing_is_wanted : Boolean := False;

   -- In boot_mode: a Director program is read from TR0 and executed
   --    in Director state, with full use of the emulated hardware.
   -- In program_mode: a user program is read from TR0 and executed
   --    in program state, with basic OUTs implemented by the emulator.
   -- In test_program_mode: a user program is read from TR0 and executed
   --    in Director state, with basic OUTs implemented by the emulator,
   --    this being useful for executing "hardware test" programs.

   type execution_mode is (boot_mode, program_mode, test_program_mode);

   procedure set_execution_mode (an_execution_mode : in settings.execution_mode);

   the_execution_default : constant settings.execution_mode := program_mode;
   the_execution_mode    :          settings.execution_mode := the_execution_default;


   -- In lax_mode, NOUV is NOT caused by an order that increases the nest depth,
   --    or leaves it the same, even if the nest contains too few cells for the operation:
   --       e.g., obeying REV or DUP with a nest depth of less than 2.
   -- KDF9 did not detect an error in these cases but ee9 does, in strict_mode, by default.
   -- N.B. In both nest modes, the test is made BEFORE the operation, unlike KDF9.
   --
   -- Moreover, in lax_mode, any attempt to update Q0 is ignored,
   --    but in strict_mode it is treated as an invalid instruction.

   type authenticity_mode is (lax_mode, strict_mode, authentic_time_mode);

   the_authenticity_default : constant settings.authenticity_mode := strict_mode;
   the_authenticity_mode    :          settings.authenticity_mode := the_authenticity_default;


   -- Tracing bound settings.

   -- time_limit bounds the number of KDF9 instructions executed.

   time_limit_default : constant KDF9.order_counter := KDF9.order_counter'Last;
   time_slice         : constant KDF9.order_counter := 10_000;
   offline_time_limit : constant KDF9.order_counter := 10_000 * time_slice;
   time_limit         :          KDF9.order_counter := time_limit_default;


   -- low_bound and high_bound bound the static scope of tracing.

   low_bound_default  : constant KDF9.code_location := 0;
   high_bound_default : constant KDF9.code_location := KDF9.code_location'Last;
   low_bound          :          KDF9.code_location := low_bound_default;
   high_bound         :          KDF9.code_location := high_bound_default;

   -- low_count and high_count bound the dynamic scope of tracing.

   low_count_default  : constant KDF9.order_counter := 0;
   high_count_default : constant KDF9.order_counter := time_limit_default;
   low_count          :          KDF9.order_counter := low_count_default;
   high_count         :          KDF9.order_counter := high_count_default;


   function is_invalid_miscellany_flag (option : in Character)
   return Boolean;

   procedure set_this_miscellany_flag (option : in Character);

   procedure get_settings_from_file (version : in String);

   procedure display_execution_modes;

   procedure quit_if_requested;

   quit_was_requested          : Boolean := False;

   the_diagnostic_mode_changed : Boolean := False;

   mode_change_request         : exception;

end settings;
