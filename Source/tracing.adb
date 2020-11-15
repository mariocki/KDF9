-- tracing.adb
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

with exceptions;
with formatting;
with HCI;
with KDF9.compressed_opcodes;
with KDF9.store;
with settings;
with state_display;

use exceptions;
use formatting;
use HCI;
use KDF9.compressed_opcodes;
use KDF9.store;
use settings;
use state_display;

package body tracing is

   pragma Unsuppress(All_Checks);

   procedure clear_all_breakpoints is
   begin
      is_a_breakpoint := (others => False);
   end clear_all_breakpoints;

   procedure set_breakpoints (first, last : in KDF9.code_location) is
   begin
      for p in first .. last loop
         is_a_breakpoint(p) := True;
      end loop;
   end set_breakpoints;

   procedure handle_breakpoint is
   begin
      short_witness;
      continue_when_GO_is_pressed(caption => " at " & oct_of(NIA));
      quit_if_requested;
      change_diagnostic_mode_if_requested;
   end handle_breakpoint;

   procedure clear_all_watchpoints is
   begin
      for p in is_a_fetch_point'Range loop
         is_a_fetch_point(p) := False;
      end loop;
      is_a_store_point := is_a_fetch_point;
   end clear_all_watchpoints;

   procedure set_fetch_points (first, last : in KDF9.address) is
   begin
      for p in first .. last loop
         is_a_fetch_point(p) := True;
      end loop;
   end set_fetch_points;

   procedure set_store_points (first, last : in KDF9.address) is
   begin
      for p in first .. last loop
         is_a_store_point(p) := True;
      end loop;
   end set_store_points;

   procedure clear_retro_FIFO is
   begin
      retro_FIFO_count := 0; retro_FIFO_index := 0;
   end clear_retro_FIFO;

   procedure take_note_of (the_IAR   : in KDF9.code_point;
                           the_value : in KDF9.word) is
   begin
      if the_retrospective_trace_is_enabled           and then
            ICR in low_count .. high_count            and then
               NIA_word_number in low_bound .. high_bound then
         if retro_FIFO_count = 0 then
            retro_FIFO(0) := (location   => the_IAR,
                              order      => INS.order,
                              parameter  => the_value,
                              ICR_value  => ICR,
                              CPU_time   => the_CPU_time,
                              nested     => the_nest_depth,
                              called     => the_sjns_depth,
                              V          => the_V_bit,
                              T          => the_T_bit);
            retro_FIFO_count := 1;
         else
            retro_FIFO_index := retro_FIFO_index + 1;
            retro_FIFO(retro_FIFO_index) := (location => the_IAR,
                                            order     => INS.order,
                                            parameter => the_value,
                                            ICR_value => ICR,
                                            CPU_time  => the_CPU_time,
                                            nested    => the_nest_depth,
                                            called    => the_sjns_depth,
                                            V         => the_V_bit,
                                            T         => the_T_bit);
            if retro_FIFO_count < FIFO_size then
               retro_FIFO_count := retro_FIFO_count + 1;
            end if;
         end if;
      end if;
   end take_note_of;

   procedure take_note_of (the_value : in KDF9.word) is
   begin
      take_note_of(CIA, the_value);
   end take_note_of;

   procedure clear_IOC_FIFO is
   begin
      IOC_FIFO_count := 0; IOC_FIFO_index := 0;
   end clear_IOC_FIFO;

   procedure take_note_of
         (
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
         ) is

      function the_note
      return IOC_FIFO_entry is
      begin
         case take_note_of.kind is
            when start_transfer =>
               return (
                       kind            => start_transfer,
                       ICR_value       => take_note_of.ICR_value,
                       order_address   => take_note_of.order_address,
                       decoded_order   => take_note_of.decoded_order,
                       initiation_time => take_note_of.initiation_time,
                       completion_time => take_note_of.completion_time,
                       is_for_Director => take_note_of.is_for_Director,
                       priority_level  => take_note_of.priority_level,
                       control_word    => take_note_of.control_word,
                       device_name     => take_note_of.device_name
                      );
            when finis_transfer =>
               return (
                       kind            => finis_transfer,
                       ICR_value       => take_note_of.ICR_value,
                       order_address   => take_note_of.order_address,
                       decoded_order   => take_note_of.decoded_order,
                       initiation_time => take_note_of.initiation_time,
                       completion_time => take_note_of.completion_time,
                       is_for_Director => take_note_of.is_for_Director,
                       priority_level  => take_note_of.priority_level,
                       control_word    => take_note_of.control_word,
                       device_name     => take_note_of.device_name
                      );
            when store_lockout =>
               return (
                       kind            => store_lockout,
                       ICR_value       => take_note_of.ICR_value,
                       order_address   => take_note_of.order_address,
                       decoded_order   => take_note_of.decoded_order,
                       initiation_time => take_note_of.initiation_time,
                       data_address    => the_locked_out_address,
                       device_name     => take_note_of.device_name
                      );
            when buffer_lockout =>
               return (
                       kind             => buffer_lockout,
                       ICR_value        => take_note_of.ICR_value,
                       order_address    => take_note_of.order_address,
                       decoded_order    => take_note_of.decoded_order,
                       initiation_time  => take_note_of.initiation_time,
                       device_name      => take_note_of.device_name
                      );
            when test_buffer_status =>
               raise emulation_failure with "in the_note";
         end case;
      end the_note;

   begin
      if the_peripheral_trace_is_enabled              and then
            ICR in low_count .. high_count            and then
               NIA_word_number in low_bound .. high_bound then
         if IOC_FIFO_count = 0 then
            IOC_FIFO(0) := the_note;
            IOC_FIFO_count := 1;
         else
            IOC_FIFO_index := IOC_FIFO_index + 1;
            IOC_FIFO(IOC_FIFO_index) := the_note;
            if IOC_FIFO_count < FIFO_size then
               IOC_FIFO_count := IOC_FIFO_count + 1;
            end if;
         end if;
      end if;
   end take_note_of;

   procedure take_note_of
         (
          Q_register  : in KDF9.Q_register;
          device_name : in KDF9.logical_device_name := "   ";
          status      : in KDF9.word := KDF9.word'Last
         ) is

      function the_note
      return IOC_FIFO_entry is
      begin
         return (
                 kind            => test_buffer_status,
                 ICR_value       => ICR+1,  -- ICR is not incremented until the end of an order.
                 order_address   => CIA,
                 decoded_order   => INS,
                 initiation_time => the_clock_time,
                 device_name     => take_note_of.device_name,
                 Q_register      => take_note_of.Q_register,
                 status          => take_note_of.status
                );
      end the_note;

   begin
      if the_peripheral_trace_is_enabled              and then
            ICR in low_count .. high_count            and then
               NIA_word_number in low_bound .. high_bound then
         if IOC_FIFO_count = 0 then
            IOC_FIFO(0) := the_note;
            IOC_FIFO_count := 1;
         else
            IOC_FIFO_index := IOC_FIFO_index + 1;
            IOC_FIFO(IOC_FIFO_index) := the_note;
            if IOC_FIFO_count < FIFO_size then
               IOC_FIFO_count := IOC_FIFO_count + 1;
            end if;
         end if;
      end if;
   end take_note_of;


   procedure clear_interrupt_FIFO is
   begin
      interrupt_FIFO_count := 0; interrupt_FIFO_index := 0;
   end clear_interrupt_FIFO;

   procedure take_note_of
         (
          interrupt_code : in KDF9.interrupt_number;
          ICR_value      : in KDF9.order_counter;
          order_address  : in KDF9.code_point;
          busy_time      : in KDF9.microseconds;
          in_Director    : in Boolean := False;
          priority_level : in KDF9.priority
         ) is

      function the_note
      return interrupt_FIFO_entry is
      begin
         return (
                 interrupt_code => take_note_of.interrupt_code,
                 ICR_value      => take_note_of.ICR_value,
                 order_address  => take_note_of.order_address,
                 busy_time      => take_note_of.busy_time,
                 in_Director    => take_note_of.in_Director,
                 priority_level => take_note_of.priority_level
                );
      end the_note;

   begin
      if the_interrupt_trace_is_enabled               and then
            ICR in low_count .. high_count            and then
               NIA_word_number in low_bound .. high_bound then
         if interrupt_FIFO_count = 0 then
            interrupt_FIFO(0) := the_note;
            interrupt_FIFO_count := 1;
         else
            interrupt_FIFO_index := interrupt_FIFO_index + 1;
            interrupt_FIFO(interrupt_FIFO_index) := the_note;
            if interrupt_FIFO_count < FIFO_size then
               interrupt_FIFO_count := interrupt_FIFO_count + 1;
            end if;
         end if;
      end if;
   end take_note_of;

   procedure add_INS_to_the_histogram is
      syllable_0 : KDF9.syllable := INS.order.syllable_0;
   begin
      if INS.kind = normal_jump_order then
         syllable_0 := (syllable_0 and 2#1111_0000#) or INS.Qq;
      elsif INS.kind = data_access_order then
         syllable_0 := (syllable_0 and 2#11_000_111#);
      end if;
      the_histogram(syllable_0) := the_histogram(syllable_0) + 1;
   end add_INS_to_the_histogram;

   procedure preview_a_one_syllable_order is
   begin
      null;
   end preview_a_one_syllable_order;

   procedure preview_a_two_syllable_order is
   begin
      case INS.syndrome is
         when TO_MkMq
            | TO_MkMqQ
            | TO_MkMqH
            | TO_MkMqQH
            | TO_MkMqN
            | TO_MkMqQN
            | TO_MkMqHN
            | TO_MkMqQHN =>
            the_trace_operand := read_top;
         when others =>
            the_trace_operand := as_word(the_Q_store(INS.Qq));
      end case;
   end preview_a_two_syllable_order;

   procedure preview_a_jump_order is
   begin
      case INS.syndrome is
         when JrEQ
            | JrNE
            | JrGTZ
            | JrLTZ
            | JrEQZ
            | JrLEZ
            | JrGEZ
            | JrNEZ
            | OUT_9 =>
            if the_nest_depth > 0 then
               the_trace_operand := read_top;
            end if;
         when JrV
            | JrNV =>
            the_trace_operand := the_V_bit;
         when JrEN
            | JrNEN =>
            the_trace_operand := KDF9.word(the_nest_depth);
         when JrEJ
            | JrNEJ =>
            the_trace_operand := KDF9.word(the_sjns_depth);
         when JrTR
            | JrNTR =>
            the_trace_operand := the_T_bit;
         when EXIT_9
            | EXITD =>
            if the_sjns_depth > 0 then
               the_trace_operand := as_word(sjns_top);
            end if;
         when JrCqZ
            | JrCqNZ =>
            the_trace_operand := as_word(the_Q_store(INS.Qq));
         when others =>
            null;
      end case;
   end preview_a_jump_order;

   procedure preview_a_data_access_order is
   begin
      case INS.syndrome is
         when TO_EaMq
            | TO_EaMqQ =>
            the_trace_operand := read_top;
         when others =>
            null;
      end case;
   end preview_a_data_access_order;

   procedure look_back_at_a_one_syllable_order is
      AB : KDF9.pair;
   begin
      case INS.syndrome is
         when TO_TR =>
            the_trace_operand := the_T_bit;
         when XDF
            | XPLUSF
            | MINUSDF
            | PLUSDF
            | FLOATD
            | NEGDF
            | MAXF
            | PERM
            | CAB
            | MAX
            | XD
            | NEGD
            | DUPD
            | DIVI
            | STR
            | REVD
            | MINUSD
            | PLUSD
            | DIVR =>
            AB := read_top;
            the_trace_operand := AB.msw;
         when others =>
            if the_nest_depth > 0 then
               the_trace_operand := read_top;
            end if;
      end case;
   end look_back_at_a_one_syllable_order;

   procedure look_back_at_an_IO_order is
   begin
      null;
   end look_back_at_an_IO_order;

   procedure look_back_at_a_two_syllable_order is
      AB : KDF9.pair;
   begin
      case INS.syndrome is
         when MkMq
            | MkMqQ
            | MkMqH
            | MkMqQH
            | MkMqQN
            | MkMqHN
            | MkMqQHN
            | QCIMq
            | SHA
            | SHL
            | SHC
            | TO_Kk
            | Kk
            | LINK =>
            the_trace_operand := read_top;
         when TO_MkMq
            | TO_MkMqQ
            | TO_MkMqH
            | TO_MkMqQH
            | TO_MkMqN
            | TO_MkMqQN
            | TO_MkMqHN
            | TO_MkMqQHN =>
            null;
         when M_PLUS_Iq
            | M_MINUS_Iq
            | NCq
            | DCq
            | POS1_TO_Iq
            | NEG1_TO_Iq
            | POS2_TO_Iq
            | NEG2_TO_Iq
            | TO_RCIMq
            | ADD_TO_QCIMq
            | JCqNZS =>
            the_trace_operand := as_word(the_Q_store(INS.Qq));
         when CqTOQk
            | IqTOQk
            | MqTOQk
            | QqTOQk
            | CIqTOQk
            | IMqTOQk
            | CMqTOQk =>
            the_trace_operand := as_word(the_Q_store(INS.Qk));
         when SHLD
            | SHAD
            | MACC =>
            AB := read_top;
            the_trace_operand := AB.msw;
         when TO_LINK =>
            the_trace_operand := as_word(sjns_top);
         when others =>
            look_back_at_an_IO_order;
      end case;
   end look_back_at_a_two_syllable_order;

   procedure look_back_at_a_jump_order is
   begin
      case INS.syndrome is
         when Jr =>
            the_trace_operand := as_word(code_link(NIA));
         when JSr =>
            the_trace_operand := as_word(sjns_top);
         when others =>
            null;
      end case;
   end look_back_at_a_jump_order;

   procedure look_back_at_a_data_access_order is
   begin
      case INS.syndrome is
         when EaMq
            | EaMqQ
            | SET =>
            the_trace_operand := read_top;
         when others =>
            null;
      end case;
   end look_back_at_a_data_access_order;

   procedure act_on_any_fetchpoint is
      use type watch_flags.set;
   begin
      if the_trace_address / is_a_fetch_point then
         log_new_line;
         log("Fetch watchhpoint: N1 := [#");
         log(oct_of(the_trace_address));
         log("]");
         short_witness;
         continue_when_GO_is_pressed;
         quit_if_requested;
         change_diagnostic_mode_if_requested;
      end if;
   end act_on_any_fetchpoint;

   procedure act_on_any_storepoint is
      use type watch_flags.set;
   begin
      if the_trace_address / is_a_store_point then
         log_new_line;
         log("Store watchpoint: #");
         log(oct_of(the_trace_address));
         log(" := [N1] = #");
         log(oct_of(the_trace_operand));
         short_witness;
         continue_when_GO_is_pressed;
         quit_if_requested;
         change_diagnostic_mode_if_requested;
      end if;
   end act_on_any_storepoint;

   procedure act_on_any_two_syllable_order_watchpoints is
   begin
      case INS.syndrome is
         when MkMq
            | MkMqQ
            | MkMqH
            | MkMqQH
            | MkMqQN
            | MkMqHN
            | MkMqQHN =>
            act_on_any_fetchpoint;
         when TO_MkMq
            | TO_MkMqQ
            | TO_MkMqH
            | TO_MkMqQH
            | TO_MkMqN
            | TO_MkMqQN
            | TO_MkMqHN
            | TO_MkMqQHN =>
            act_on_any_storepoint;
         when others =>
            null;
      end case;
   end act_on_any_two_syllable_order_watchpoints;

   procedure act_on_any_data_access_order_watchpoints is
   begin
      case INS.syndrome is
         when EaMq
            | EaMqQ =>
            act_on_any_fetchpoint;
         when TO_EaMq
            | TO_EaMqQ =>
            act_on_any_storepoint;
         when others =>
            null;
      end case;
   end act_on_any_data_access_order_watchpoints;

end tracing;
