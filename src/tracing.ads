-- Provide diagnostic trace, breakpoint, and watchpoint support.
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

with generic_sets;
with IOC;
with KDF9;
with settings;

use  settings;

package tracing is

   -- Support for significant-operand evaluation and tracing.

   the_trace_operand : KDF9.word;
   the_trace_address : KDF9.address;

   procedure preview_a_one_syllable_order;

   procedure preview_a_two_syllable_order;

   procedure preview_a_jump_order;

   procedure preview_a_data_access_order;

   procedure look_back_at_a_one_syllable_order;

   procedure look_back_at_a_two_syllable_order;

   procedure look_back_at_a_jump_order
      with Inline => False;

   procedure look_back_at_a_data_access_order;

   -- Support for breakpoints.

   package order_flags is new generic_sets(member => KDF9.code_address);

   breakpoints : order_flags.set := order_flags.empty_set;

   procedure set_breakpoints (first, last : in KDF9.code_address);

   procedure handle_breakpoint;

   -- Support for watchpoints.

   package watch_flags is new generic_sets(member => KDF9.address);

   fetchpoints : watch_flags.set := watch_flags.empty_set;
   storepoints : watch_flags.set := watch_flags.empty_set;

   procedure clear_all_watchpoints;

   procedure set_fetch_points (first, last : in KDF9.address);

   procedure set_store_points (first, last : in KDF9.address);

   procedure act_on_any_two_syllable_order_watchpoints
      with Inline => False;

   procedure act_on_any_data_access_order_watchpoints
      with Inline => False;


   --
   -- Retrospective tracing.
   --

   FIFO_size : constant := 256;

   type FIFO_index is mod FIFO_size;

   -- Support for all-instruction retrospective tracing.

   type retro_FIFO_entry is
      record
         location  : KDF9.syllable_address;
         order     : KDF9.syllable_group;
         parameter : KDF9.word;
         ICR_value : KDF9.order_counter;
         CPU_time  : KDF9.us;
         nested    : KDF9.NEST_depth;
         called    : KDF9.SJNS_depth;
         V, T, D   : Boolean;
         level     : KDF9.priority;
      end record;

   retro_FIFO  : array (tracing.FIFO_index) of tracing.retro_FIFO_entry;

   retro_FIFO_index : tracing.FIFO_index := 0;

   retro_FIFO_count : Natural range 0 .. FIFO_size := 0;

   procedure clear_retro_FIFO;

   procedure take_note_of (the_value : in KDF9.word);

   -- Support for retrospective peripheral I/O tracing.

   type IOC_event_kind is (start_transfer,
                           finis_transfer,
                           buffer_lockout,
                           store_lockout,
                           buffer_status);


   type IOC_FIFO_entry (kind : IOC_event_kind := start_transfer) is
      record
         ICR_value       : KDF9.order_counter;
         order_address   : KDF9.syllable_address;
         decoded_order   : KDF9.decoded_order;
         initiation_time : KDF9.us;
         device_name     : IOC.device_name;
         is_for_Director : Boolean;
         priority_level  : KDF9.priority;
         context         : KDF9.context;
         operation       : IOC.transfer_kind := IOC.some_other_operation;
         case kind is
            when start_transfer | finis_transfer =>
               completion_time : KDF9.us;
               control_word    : KDF9.Q_register;
            when store_lockout =>
               data_address : KDF9.Q_part;
            when buffer_lockout =>
               null;
            when buffer_status =>
               Q_register : KDF9.Q_register;
               status     : Boolean;
         end case;
      end record;

   IOC_FIFO  : array (tracing.FIFO_index) of tracing.IOC_FIFO_entry;

   IOC_FIFO_index : tracing.FIFO_index := 0;

   IOC_FIFO_count : Natural range 0 .. FIFO_size := 0;

   procedure clear_IOC_FIFO;

   procedure take_note_of_IO_start (
                                    device_name     : in IOC.device_name;
                                    completion_time : in KDF9.us;
                                    control_word    : in KDF9.Q_register;
                                    operation       : in IOC.transfer_kind := IOC.some_other_operation
                                   );

   procedure take_note_of_IO_finis (
                                    ICR_value       : in KDF9.order_counter;
                                    order_address   : in KDF9.syllable_address;
                                    decoded_order   : in KDF9.decoded_order;
                                    initiation_time : in KDF9.us;
                                    device_name     : in IOC.device_name;
                                    is_for_Director : Boolean;
                                    priority_level  : in KDF9.priority;
                                    completion_time : in KDF9.us;
                                    control_word    : in KDF9.Q_register;
                                    operation       : in IOC.transfer_kind := IOC.some_other_operation
                                   );

   procedure take_note_of_store_lockout  (device_name : in IOC.device_name);

   procedure take_note_of_buffer_lockout (device_name : in IOC.device_name;
                                          operation   : in IOC.transfer_kind := IOC.some_other_operation);

   procedure take_note_of_test (
                                device_name : in IOC.device_name;
                                Q_register  : in KDF9.Q_register;
                                status      : in Boolean
                                );


   -- Support for retrospective interrupt-request tracing.

   max_interrupt_message_length : constant := 100;
   type interrupt_FIFO_entry is
      record
         interrupt_code : KDF9.interrupt_number;
         ICR_value      : KDF9.order_counter;
         order_address  : KDF9.syllable_address;
         busy_time      : KDF9.us;
         priority_level : KDF9.priority;
         context        : KDF9.context;
         message        : String (1..max_interrupt_message_length);
      end record;

   interrupt_FIFO  : array (tracing.FIFO_index) of tracing.interrupt_FIFO_entry;

   interrupt_FIFO_index : tracing.FIFO_index := 0;

   interrupt_FIFO_count : Natural range 0 .. FIFO_size := 0;

   procedure clear_interrupt_FIFO;

   procedure take_note_of_interrupt (interrupt_code : in KDF9.interrupt_number; message : in String)
      with Inline => False;


   -- Support for the instruction-type and instruction-word frequency histograms.

   the_histogram : array (KDF9.syllable) of KDF9.order_counter;

   procedure clear_the_histogram;

   procedure add_INS_to_the_histogram
      with Inline;

   the_profile   : array (KDF9.code_address) of KDF9.order_counter;

   procedure clear_the_profile;

   procedure add_CIA_to_the_profile
      with Inline;

end tracing;
