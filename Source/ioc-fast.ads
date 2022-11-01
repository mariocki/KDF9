-- Emulation of the common functionality of a KDF9 "fast", i.e. word-by-word, devices.
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

private with tracing;

package IOC.fast is

   --
   -- This is the root type for all fast I/O device types.
   --

   type device is abstract new IOC.device with private;

private

   use tracing; pragma Warnings(Off, tracing);

   type device is abstract new IOC.device with
      record
         switch_time,
         latency_time,
         elapsed_time  : KDF9.us := 0;
         word_count,
         switch_count,
         latency_count : KDF9.word := 0;
      end record;

   overriding
   function is_open (the_buffer : fast.device)
   return Boolean;

   overriding
   procedure add_in_the_IO_CPU_time (the_buffer  : in fast.device;
                                     bytes_moved : in KDF9.word);

end IOC.fast;
