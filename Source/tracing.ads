-- tracing.ads
--
-- Provide diagnostic trace, breakpoint, and watchpoint support.
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
with generic_sets; pragma Elaborate_All(generic_sets);

use  KDF9;

package tracing is

   pragma Unsuppress(All_Checks);

   -- Support for significant-operand evaluation and tracing.

   the_trace_operand : KDF9.word;
   the_trace_address : KDF9.address;

   procedure preview_a_one_syllable_order;

   procedure preview_a_two_syllable_order;

   procedure preview_a_jump_order;

   procedure preview_a_data_access_order;

   procedure look_back_at_a_one_syllable_order;

   procedure look_back_at_a_two_syllable_order;

   procedure look_back_at_a_jump_order;

   procedure look_back_at_a_data_access_order;

   -- Support for breakpoints.

   package order_flags is new generic_sets(member => KDF9.code_location);

   is_a_breakpoint : order_flags.set := order_flags.empty_set;

   procedure clear_all_breakpoints;

   procedure set_breakpoints (first, last : in KDF9.code_location);

   procedure handle_breakpoint;

   -- Support for watchpoints.

   package watch_flags is new generic_sets(member => KDF9.address);

   is_a_fetch_point : watch_flags.set := watch_flags.empty_set;
   is_a_store_point : watch_flags.set := watch_flags.empty_set;

   procedure clear_all_watchpoints;

   procedure set_fetch_points (first, last : in KDF9.address);

   procedure set_store_points (first, last : in KDF9.address);

   -- These two procedures must NOT be called in fast_mode.

   procedure act_on_any_two_syllable_order_watchpoints;

   procedure act_on_any_data_access_order_watchpoints;


   --
   -- Retrospective tracing.
   --

   FIFO_size : constant := 256;

   type FIFO_index is mod FIFO_size;

   -- Support for all-instruction retrospective tracing.

   type retro_FIFO_entry is
      record
         location  : KDF9.code_point := (0, 0);
         order     : KDF9.syllable_group := (0, 0, 0);
         parameter : KDF9.word := 0;
         ICR_value : KDF9.order_counter := 0;
         CPU_time  : KDF9.microseconds := 0;
         nested    : KDF9.nest_depth := 0;
         called    : KDF9.sjns_depth := 0;
         V, T      : KDF9.word := 0;
      end record;

   retro_FIFO  : array (tracing.FIFO_index) of tracing.retro_FIFO_entry;

   retro_FIFO_index : tracing.FIFO_index := 0;

   retro_FIFO_count : Natural range 0 .. FIFO_size := 0;

   procedure clear_retro_FIFO;

   procedure take_note_of (the_value : in KDF9.word);

   -- Support for retrospective peripheral I/O tracing.

   type IOC_event_kind is (start_transfer, finis_transfer,
                           store_lockout, buffer_lockout,
                           test_buffer_status);

   type IOC_FIFO_entry (kind : IOC_event_kind := start_transfer) is
      record
         ICR_value       : KDF9.order_counter := 0;
         order_address   : KDF9.code_point := (0, 0);
         decoded_order   : KDF9.decoded_order;
         initiation_time : KDF9.microseconds := 0;
         device_name     : KDF9.logical_device_name;
         case kind is
            when start_transfer | finis_transfer =>
               completion_time : KDF9.microseconds := 0;
               is_for_Director : Boolean := False;
               priority_level  : KDF9.priority := 0;
               control_word    : KDF9.Q_register := (0, 0, 0);
            when store_lockout =>
               data_address : KDF9.Q_part := 0;
            when buffer_lockout =>
               null;
            when test_buffer_status =>
               Q_register : KDF9.Q_register := (0, 0, 0);
               status : KDF9.word := 0;
         end case;
      end record;

   IOC_FIFO  : array (tracing.FIFO_index) of tracing.IOC_FIFO_entry;

   IOC_FIFO_index : tracing.FIFO_index := 0;

   IOC_FIFO_count : Natural range 0 .. FIFO_size := 0;

   procedure clear_IOC_FIFO;

   procedure take_note_of (
                           kind            : in IOC_event_kind;
                           ICR_value       : in KDF9.order_counter;
                           order_address   : in KDF9.code_point;
                           decoded_order   : in KDF9.decoded_order;
                           initiation_time : in KDF9.microseconds;
                           device_name     : in KDF9.logical_device_name;
                           completion_time : in KDF9.microseconds := 0;
                           is_for_Director : in Boolean := False;
                           priority_level  : in KDF9.priority := 0;
                           control_word    : in KDF9.Q_register := (0, 0, 0)
                          );

   procedure take_note_of (
                           Q_register      : in KDF9.Q_register;
                           device_name     : in KDF9.logical_device_name := "   ";
                           status          : in KDF9.word := KDF9.word'Last
                          );

   -- Support for retrospective interrupt-request tracing.

   type interrupt_FIFO_entry is
      record
         interrupt_code  : KDF9.interrupt_number := RESET_flag;
         ICR_value       : KDF9.order_counter := 0;
         order_address   : KDF9.code_point := (0, 0);
         busy_time       : KDF9.microseconds := 0;
         in_Director     : Boolean := False;
         priority_level  : KDF9.priority := 0;
      end record;

   interrupt_FIFO  : array (tracing.FIFO_index) of tracing.interrupt_FIFO_entry;

   interrupt_FIFO_index : tracing.FIFO_index := 0;

   interrupt_FIFO_count : Natural range 0 .. FIFO_size := 0;

   procedure clear_interrupt_FIFO;

   procedure take_note_of (
                           interrupt_code  : in KDF9.interrupt_number;
                           ICR_value       : in KDF9.order_counter;
                           order_address   : in KDF9.code_point;
                           busy_time       : in KDF9.microseconds;
                           in_Director     : in Boolean := False;
                           priority_level  : in KDF9.priority
                          );

   -- Support for the instruction-type frequency histogram.

   type histogram is array (KDF9.syllable) of KDF9.order_counter;

   nul_histogram : constant histogram := (others => 0);

   the_histogram : histogram := nul_histogram;

   procedure add_INS_to_the_histogram;

end tracing;
