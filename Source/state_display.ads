-- state_display.ads
--
-- Provide the comprehensive machine-state display panel KDF9 never had.
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

use  KDF9;

package state_display is

   pragma Unsuppress(All_Checks);

   procedure show_all_prerun_dump_areas;

   procedure show_CIA_and_NIA;

   procedure show_V_and_T;

   procedure show_nest;

   procedure show_sjns;

   procedure show_Q_register (the_Q_register : in KDF9.Q_register;
                              width          : in Positive := 8;
                              with_FD_C_part : in Boolean  := False);

   procedure show_Q_store;

   procedure show_registers;

   procedure show_execution_context;

   procedure long_witness;

   procedure short_witness;

   procedure log_an_external_trace_header;

   procedure log_to_external_trace;

   procedure show_progress;

   procedure show_Director_registers;

   procedure show_retrospective_traces;

   procedure show_current_state;

   procedure show_final_state;

   procedure mark_all_code_blocks_and_data_blocks;

   the_code_space_has_been_marked : Boolean := False;

   procedure show_core_as_word_forms (first, last : in KDF9.address);

   procedure show_core_as_syllables (first, last : in KDF9.code_point);

   procedure show_core_as_Usercode (first, last  : in KDF9.code_point;
                                    octal_option : in Boolean);

   procedure show_core_in_print_code (first, last : in KDF9.address);

   procedure show_core_in_card_code (first, last : in KDF9.address);

   procedure show_core_in_tape_code (first, last : in KDF9.address);

   procedure show_core_in_case_normal (first, last : in KDF9.address);

   procedure show_core_in_case_shift (first, last : in KDF9.address);

   procedure show_core_in_Latin_1 (first, last : in KDF9.address);

end state_display;
