-- Emulation of the common functionality of a KDF9 "slow", byte-by-byte, devices.
--
-- This file is part of ee9 (8.2z), the GNU Ada emulator of the English Electric KDF9.
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

with HCI;
with OS_specifics;
with imported_value_of;

use HCI;

package body IOC.slow is

   overriding
   function is_open (the_buffer : slow.device)
   return Boolean
   is (the_buffer.stream.is_open);

   overriding
   procedure add_in_the_IO_CPU_time (the_buffer  : in slow.device;
                                     bytes_moved : in KDF9.word) is
      pragma Unreferenced(the_buffer);
   begin
      the_CPU_delta := the_CPU_delta + KDF9.us(bytes_moved) * 6; -- 6µs/char
   end add_in_the_IO_CPU_time;

   procedure display_device_usage (the_buffer  : in slow.device;
                                   the_action  : in String;
                                   the_amount  : in KDF9.word;
                                   the_quantum : in String) is
   begin
         if the_final_state_is_wanted and then
               the_log_is_wanted      and then
                  the_buffer.is_open  and then
               the_amount /= 0            then
         if (the_buffer.number = 0) and not (API_logging_is_wanted or the_log_is_wanted) then
            -- Take a new line at the head of the list, for low-visibility modes.
            log_new_line;
         end if;
         log_line(
                  the_buffer.device_name
                + "on buffer #"
                & oct_of(KDF9.Q_part(the_buffer.number), 2)
                + the_action
                & the_amount'Image
                + the_quantum
                & "."
                 );
      end if;
   end display_device_usage;

   procedure close (the_buffer  : in out slow.device;
                    the_action  : in String;
                    the_amount  : in KDF9.word;
                    the_quantum : in String) is
   begin
      display_device_usage (the_buffer, the_action, the_amount, the_quantum);
      IOC.device(the_buffer).close;
   end close;

   function atomic_item_count (the_buffer : slow.device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word is
      words : constant KDF9.Q_part := Q_operand.M - Q_operand.I + 1;
   begin
      if the_buffer.is_open then
         return KDF9.word(words) * 8;
      else
         return 0;
      end if;
   end atomic_item_count;

   procedure reattach (the_buffer   : in out slow.device;
                       the_file     : in String) is
   begin
      reattach(the_buffer.stream, the_file, read_mode);
   end reattach;

   procedure deal_with_end_of_data (the_buffer : in out slow.device) is

      procedure reattach_the_text_file (name : in String) is
      begin
         if name = "" then
            the_buffer.is_abnormal := True;
            raise end_of_stream;
         elsif exists(name) then
            reattach(the_buffer, name);
            return;
         elsif exists(name & ".txt") then
            reattach(the_buffer, name & ".txt");
            return;
         else
            raise operator_error;
         end if;
      end reattach_the_text_file;

      response : response_kind;
      inline   : Boolean;

   begin
      output_line(BEL & "");
      output_line("ee9: End of given data for" + the_buffer.device_name & ".");
      loop
         POSIX.data_prompt
            (
             noninteractive_usage_is_enabled,
             "Type @ or / to name a file, = to type the data, ENTER key for EOF, Q or q to quit",
             response,
             inline
            );
         case response is
            when wrong_response
               | debug_response =>
               null;  -- repeat the prompt
            when LF_response
               | EOF_response =>
               if response = EOF_response then
                  -- Need to take a new line after ^D to preserve the format.
                  output_line;
               end if;
               the_buffer.is_abnormal := True;
               raise end_of_stream;
            when quit_response =>
               raise quit_request with "quit requested";
            when here_response =>
               reattach(the_buffer, OS_specifics.UI_in_name);
               the_buffer.is_reading_a_file := False;
               return;
            when at_response =>
               declare
                  here : constant String := imported_value_of("KDF9_DATA", default => "Data") & "/";
                  next : constant String
                     := next_file_name(BEL & "Give the name of a file in" + here, inline);
                  this : constant String := here & next;
               begin
                  if next = "" then
                     raise operator_error;
                  end if;
                  reattach_the_text_file(this);
                  return;
               exception
                  when operator_error =>
                     output_line(BEL & "ee9: The file" + abs this + "could not be found.");
               end;
            when path_response =>
               declare
                  next : constant String
                       := next_file_name(BEL & "Give the pathname of the file", inline);
               begin
                  if next = "" then
                     raise operator_error;
                  end if;
                  reattach_the_text_file(next);
                  return;
               exception
                  when operator_error =>
                     output_line(BEL & "ee9: The file" + abs next + "could not be found.");
               end;
         end case;
      end loop;
   end deal_with_end_of_data;

   procedure start_slow_transfer (the_buffer   : in out slow.device;
                                  Q_operand    : in KDF9.Q_register;
                                  set_offline  : in Boolean;
                                  operation    : in IOC.transfer_kind := some_other_operation) is
      atomic_items : constant KDF9.word := atomic_item_count(the_buffer, Q_operand);
      time_needed  : constant KDF9.us := IO_elapsed_time(the_buffer, atomic_items);
   begin
      start_data_transfer(the_buffer, Q_operand, set_offline,
                          busy_time => time_needed,
                          operation => start_slow_transfer.operation);
   end start_slow_transfer;

   procedure get_char_from_stream (char       : out Character;
                                   the_buffer : in out slow.device) is
   begin
      loop
         begin
            get_char(char, the_buffer.stream);
            return;
         exception
            when end_of_stream =>
               deal_with_end_of_data(the_buffer);
         end;
      end loop;
   end get_char_from_stream;

end IOC.slow;
