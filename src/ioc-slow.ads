-- Emulation of the common functionality of a KDF9 "slow", i.e. byte-by-byte, devices.
--
-- This file is part of ee9 (8.2a), the GNU Ada emulator of the English Electric KDF9.
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

private with IOC.equipment;
private with tracing;

package IOC.slow is

   --
   -- This is the root type for all slow I/O device types.
   --

   type device is abstract new IOC.device with private;

   -- Log the usage statistics for the device on the buffer.
   procedure display_device_usage (the_buffer  : in slow.device;
                                   the_action  : in String;
                                   the_amount  : in KDF9.word;
                                   the_quantum : in String);

   -- Change the file associated with a device.
   procedure reattach (the_buffer : in out slow.device;
                       the_file   : in String);

private

   use IOC.equipment; pragma Warnings(Off, IOC.equipment);
   use tracing;       pragma Warnings(Off, tracing);

   type device is abstract new IOC.device with
      record
         is_transcribing   : Boolean := True;
         is_reading_a_file : Boolean := True;
         byte_count        : KDF9.word := 0;
      end record;

   overriding
   function is_open (the_buffer : slow.device)
   return Boolean;

   overriding
   procedure add_in_the_IO_CPU_time (the_buffer  : in slow.device;
                                     bytes_moved : in KDF9.word);

   -- Optionally log an activity message for the device; close its I/O stream.
   procedure close (the_buffer  : in out slow.device;
                    the_action  : in String;
                    the_amount  : in KDF9.word;
                    the_quantum : in String);

   -- The number of timed transfer units in the designated core-store area.
   -- In the case of unit-record devices, such as card readers and line printers,
   --    this is the number of unit records (cards, or lines, respectively).
   -- In all other cases it is the number of characters in the designated core-store area.
   function atomic_item_count (the_buffer : slow.device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word;

   -- Check the IO parameters and the buffer state, and handle any old lockout.
   -- Set the new buffer state, and project the next interrupt time.
   procedure start_slow_transfer (the_buffer   : in out slow.device;
                                  Q_operand    : in KDF9.Q_register;
                                  set_offline  : in Boolean;
                                  operation    : in IOC.transfer_kind := some_other_operation);

   procedure deal_with_end_of_data (the_buffer : in out slow.device);

   -- Read a raw byte from the stream and deal with any input file concatenation.
   procedure get_char_from_stream (char       : out Character;
                                   the_buffer : in out slow.device);

end IOC.slow;
