-- ioc-slow_devices.adb
--
-- Emulation of the common functionality of a KDF9 "slow", byte-by-byte, devices.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020, W. Findlay; all rights reserved.
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
with value_of;

use HCI;

package body IOC.slow is

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
                & " on buffer #"
                & oct_of(KDF9.Q_part(the_buffer.number), 2)
                & " "
                & the_action
                & the_amount'Image
                & " "
                & the_quantum
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
      BEL      : constant String := (1 => Character'Val(7));   -- Audible prompt
      response : response_kind;
   begin
      output_line(BEL & "");
      output_line("ee9: End of given data for " & the_buffer.device_name & ".");
      loop
         POSIX.data_prompt(
                           noninteractive_usage_is_enabled,
                           "Type @ or / to name a file, = to type in the data, or ENTER key for EOF",
                           response
                          );
         if response = wrong_response then
            null;  -- repeat the prompt
         elsif response = EOF_response then
            the_buffer.is_abnormal := True;
            raise end_of_stream;
         elsif response = here_response then
            output_line(BEL & "ee9: Type the data for " & the_buffer.device_name & ":");
            reattach(the_buffer, OS_specifics.UI_in_name);
            return;
         elsif response = at_response then
            declare
               here : constant String := value_of("KDF9ROOT", default => "") & "Data/";
               next : constant String := next_file_name(BEL & "Give the name of a file in " & here);
            begin
               if next = "" then
                  the_buffer.is_abnormal := True;
                  raise end_of_stream;
               else
                  reattach(the_buffer, here & next);
                  return;
               end if;
            end;
         elsif response = name_response then
            declare
               next : constant String := next_file_name(BEL & "Give the pathname of the file");
            begin
               if next = "" then
                  the_buffer.is_abnormal := True;
                  raise end_of_stream;
               else
                  reattach(the_buffer, next);
                  return;
               end if;
            end;
         end if;
      end loop;
   end deal_with_end_of_data;

   procedure start_slow_transfer (the_buffer   : in out slow.device;
                                  Q_operand    : in KDF9.Q_register;
                                  set_offline  : in Boolean) is
      atomic_items : constant KDF9.word := atomic_item_count(the_buffer, Q_operand);
      time_needed  : constant KDF9.us := IO_elapsed_time(the_buffer, atomic_items);
   begin
      start_data_transfer(the_buffer, Q_operand, set_offline,
                          busy_time => time_needed,
                          is_DMAing => True);
   end start_slow_transfer;

   procedure get_byte_from_stream (byte       : out Character;
                                   the_buffer : in out slow.device) is
   begin
      loop
         begin
            get_byte(byte, the_buffer.stream);
            return;
         exception
            when end_of_stream =>
               deal_with_end_of_data(the_buffer);
         end;
      end loop;
   end get_byte_from_stream;

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
