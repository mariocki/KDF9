-- Provide diagnostic trace, breakpoint, and watchpoint support.
--
-- This file is part of ee9 (6.1a), the GNU Ada emulator of the English Electric KDF9.
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

with exceptions;
with formatting;
with HCI;
with KDF9.decoding;
with KDF9.store;
with state_display;

use exceptions;
use formatting;
use HCI;
use KDF9;
use KDF9.decoding;
use KDF9.store;
use state_display;

package body tracing is

   procedure clear_the_histogram is
   begin
      the_histogram := (others => 0);
   end clear_the_histogram;

   procedure clear_the_profile is
   begin
      the_profile := (others => 0);
   end clear_the_profile;

   procedure set_breakpoints (first, last : in KDF9.order_word_number) is
   begin
      for p in first .. last loop
         breakpoints(p) := True;
      end loop;
   end set_breakpoints;

   procedure handle_breakpoint is
   begin
      short_witness;
      interact;
      quit_if_requested;
      change_diagnostic_mode_if_requested;
   end handle_breakpoint;

   procedure clear_all_watchpoints is
   begin
      fetchpoints := (others => False);
      storepoints := (others => False);
   end clear_all_watchpoints;

   procedure set_fetch_points (first, last : in KDF9.address) is
   begin
      for p in first .. last loop
         fetchpoints(p) := True;
      end loop;
   end set_fetch_points;

   procedure set_store_points (first, last : in KDF9.address) is
   begin
      for p in first .. last loop
         storepoints(p) := True;
      end loop;
   end set_store_points;

   procedure clear_retro_FIFO is
   begin
      retro_FIFO_count := 0; retro_FIFO_index := 0;
   end clear_retro_FIFO;

   procedure take_note_of (the_IAR   : in KDF9.syllable_address;
                           the_value : in KDF9.word) is
   begin
      if the_retrospective_trace_is_enabled           and then
            ICR in low_count .. high_count            and then
               NIA_word_number in low_bound .. high_bound then
         declare
            the_note : constant retro_FIFO_entry
                     := (
                         location   => the_IAR,
                         order      => INS.order,
                         parameter  => the_value,
                         ICR_value  => ICR,
                         CPU_time   => the_CPU_time,
                         nested     => the_NEST_depth,
                         called     => the_SJNS_depth,
                         V          => the_V_bit_is_set,
                         T          => the_T_bit_is_set,
                         D          => the_CPU_state = Director_state,
                         level      => CPL
                        );
         begin
            if retro_FIFO_count = 0 then
               retro_FIFO(0) := the_note;
               retro_FIFO_count := 1;
            else
               retro_FIFO_index := retro_FIFO_index + 1;
               retro_FIFO(retro_FIFO_index) := the_note;
               if retro_FIFO_count < FIFO_size then
                  retro_FIFO_count := retro_FIFO_count + 1;
               end if;
            end if;
         end;
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

   procedure register_IO_event (the_note : in IOC_FIFO_entry) is
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
   end register_IO_event;

   procedure take_note_of_IO_start (
                                    device_name     : in IOC.device_name;
                                    completion_time : in KDF9.us;
                                    control_word    : in KDF9.Q_register;
                                    operation       : in IOC.transfer_kind := IOC.some_other_operation
                                   )
   is
      the_note : constant  IOC_FIFO_entry
               :=
                (
                 kind            => start_transfer,
                 ICR_value       => ICR,
                 order_address   => CIA,
                 decoded_order   => INS,
                 initiation_time => the_clock_time,
                 device_name     => take_note_of_IO_start.device_name,
                 completion_time => take_note_of_IO_start.completion_time,
                 is_for_Director => (the_CPU_state = Director_state),
                 priority_level  => CPL,
                 context         => the_context,
                 control_word    => take_note_of_IO_start.control_word,
                 operation       => take_note_of_IO_start.operation
                );
   begin
      register_IO_event(the_note);
   end take_note_of_IO_start;

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
                                   )
   is
      the_note : constant  IOC_FIFO_entry
               :=
                (
                 kind            => finis_transfer,
                 ICR_value       => take_note_of_IO_finis.ICR_value,
                 order_address   => take_note_of_IO_finis.order_address,
                 decoded_order   => take_note_of_IO_finis.decoded_order,
                 initiation_time => take_note_of_IO_finis.initiation_time,
                 device_name     => take_note_of_IO_finis.device_name,
                 is_for_Director => take_note_of_IO_finis.is_for_Director,
                 priority_level  => take_note_of_IO_finis.priority_level,
                 context         => the_context,
                 completion_time => take_note_of_IO_finis.completion_time,
                 control_word    => take_note_of_IO_finis.control_word,
                 operation       => take_note_of_IO_finis.operation
                );

   begin
      register_IO_event(the_note);
   end take_note_of_IO_finis;

   procedure take_note_of_store_lockout (device_name : in IOC.device_name) is
      the_note : constant  IOC_FIFO_entry
               :=
                (
                 kind            => store_lockout,
                 ICR_value       => ICR,
                 order_address   => CIA,
                 decoded_order   => INS,
                 initiation_time => the_clock_time,
                 device_name     => take_note_of_store_lockout.device_name,
                 is_for_Director => False,
                 priority_level  => CPL,
                 context         => the_context,
                 data_address    => the_locked_out_address,
                 operation       => IOC.some_other_operation
                );
   begin
      register_IO_event(the_note);
   end take_note_of_store_lockout;

   procedure take_note_of_buffer_lockout (device_name : in IOC.device_name;
                                          operation   : in IOC.transfer_kind := IOC.some_other_operation) is
      the_note : constant  IOC_FIFO_entry
               :=
                (
                 kind            => buffer_lockout,
                 ICR_value       => ICR,
                 order_address   => CIA,
                 decoded_order   => INS,
                 initiation_time => the_clock_time,
                 device_name     => take_note_of_buffer_lockout.device_name,
                 is_for_Director => False,
                 priority_level  => CPL,
                 context         => the_context,
                 operation       => take_note_of_buffer_lockout.operation
                );
   begin
      register_IO_event(the_note);
   end take_note_of_buffer_lockout;

   procedure take_note_of_test (
                                device_name : in IOC.device_name;
                                Q_register  : in KDF9.Q_register;
                                status      : in Boolean
                               )
   is
      the_note : constant  IOC_FIFO_entry
               :=
                (
                 kind            => buffer_status,
                 ICR_value       => ICR+1,  -- ICR is not incremented until the end of an order.
                 order_address   => CIA,
                 decoded_order   => INS,
                 initiation_time => the_clock_time,
                 device_name     => take_note_of_test.device_name,
                 is_for_Director => (the_CPU_state = Director_state),
                 priority_level  => CPL,
                 context         => the_context,
                 Q_register      => take_note_of_test.Q_register,
                 status          => take_note_of_test.status,
                 operation       => IOC.some_other_operation
                );
   begin
      register_IO_event(the_note);
   end take_note_of_test;

   procedure clear_interrupt_FIFO is
   begin
      interrupt_FIFO_count := 0; interrupt_FIFO_index := 0;
   end clear_interrupt_FIFO;

   procedure take_note_of_interrupt (interrupt_code : in KDF9.interrupt_number; message : in String)
   is
      length  : constant Natural  := Natural'Min(message'Length, max_interrupt_message_length);
      content : constant String   := message(message'First .. message'First+length-1);
      padding : constant String   := (1 .. max_interrupt_message_length-length => ' ');
   begin
      declare
         the_note : constant interrupt_FIFO_entry
                  :=
                   (
                    interrupt_code => take_note_of_interrupt.interrupt_code,
                    ICR_value      => ICR,
                    order_address  => CIA,
                    busy_time      => the_clock_time,
                    priority_level => CPL,
                    context        => the_context,
                    message        => content & padding
                   );
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
      end;
   end take_note_of_interrupt;

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

   procedure add_CIA_to_the_profile is
   begin
      the_profile(CIA.order_word_number) := the_profile(CIA.order_word_number) + 1;
   end add_CIA_to_the_profile;

   procedure preview_a_one_syllable_order is null;

   procedure preview_a_two_syllable_order is
   begin
      case INS.compressed_opcode is
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
      case INS.compressed_opcode is
         when JrEQ
            | JrNE
            | JrGTZ
            | JrLTZ
            | JrEQZ
            | JrLEZ
            | JrGEZ
            | JrNEZ
            | OS_OUT =>
            if the_NEST_depth > 0 then
               the_trace_operand := read_top;
            end if;
         when JrEN
            | JrNEN =>
            the_trace_operand := KDF9.word(the_NEST_depth);
         when JrEJ
            | JrNEJ =>
            the_trace_operand := KDF9.word(the_SJNS_depth);
         when EXIT_n
            | EXITD =>
            if the_SJNS_depth > 0 then
               the_trace_operand := as_word(SJNS_top);
            else
               the_trace_operand := -1;
            end if;
         when JrCqZ
            | JrCqNZ =>
            the_trace_operand := as_word(the_Q_store(INS.Qq));
         when JrV
            | JrNV =>
            the_trace_operand := (if the_V_bit_is_set then 1 else 0);
         when JrTR
            | JrNTR =>
            the_trace_operand := (if the_T_bit_is_set then 1 else 0);
         when others =>
            null;
      end case;
   end preview_a_jump_order;

   procedure preview_a_data_access_order is
   begin
      case INS.compressed_opcode is
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
      case INS.compressed_opcode is
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
            if the_NEST_depth > 0 then
               the_trace_operand := read_top;
            end if;
      end case;
   end look_back_at_a_one_syllable_order;

   procedure look_back_at_an_IO_order is null;

   procedure look_back_at_a_two_syllable_order is
      AB : KDF9.pair;
   begin
      case INS.compressed_opcode is
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
            | TO_Kq
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
            the_trace_operand := as_word(SJNS_top);
         when others =>
            look_back_at_an_IO_order;
      end case;
   end look_back_at_a_two_syllable_order;

   procedure look_back_at_a_jump_order is
      BA_image  : constant String := "BA #" & oct_of(BA);
      NOL_image : constant String := "NOL"  & NOL'Image;
   begin
      case INS.compressed_opcode is
         when Jr =>
            the_trace_operand := as_word(SJNS_link(NIA));
         when JSr =>
            the_trace_operand := as_word(SJNS_top);
         when EXITD =>
            take_note_of_interrupt(EXITD_flag, BA_image & " " & NOL_image & " @ " & oct_of(NIA));
         when others =>
            null;
      end case;
   end look_back_at_a_jump_order;

   procedure look_back_at_a_data_access_order is
   begin
      case INS.compressed_opcode is
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
      if fetchpoints/the_trace_address then
         log_new_line;
         log("Fetch watchhpoint: N1 := [#" & oct_of(the_trace_address) & "]");
         short_witness;
         interact;
         quit_if_requested;
         change_diagnostic_mode_if_requested;
      end if;
   end act_on_any_fetchpoint;

   procedure act_on_any_storepoint is
      use type watch_flags.set;
   begin
      if storepoints/the_trace_address then
         log_new_line;
         log(
             "Store watchpoint: #"
           & oct_of(the_trace_address)
           & " := [N1] = #"
           & oct_of(the_trace_operand)
            );
         short_witness;
         interact;
         quit_if_requested;
         change_diagnostic_mode_if_requested;
      end if;
   end act_on_any_storepoint;

   procedure act_on_any_two_syllable_order_watchpoints is
   begin
      case INS.compressed_opcode is
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
      case INS.compressed_opcode is
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
