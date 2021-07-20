-- Emulation of the common functionality of a KDF9 "fast", i.e. word-by-word, devices.
--
-- This file is part of ee9 (8.0k), the GNU Ada emulator of the English Electric KDF9.
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

package body IOC.fast is

   overriding
   function is_open (the_buffer : fast.device)
   return Boolean
   is (the_buffer.stream.is_open);

   overriding
   procedure add_in_the_IO_CPU_time (the_buffer  : in fast.device;
                                     bytes_moved : in KDF9.word) is
      pragma Unreferenced(the_buffer);
   begin
      the_CPU_delta := the_CPU_delta + KDF9.us(bytes_moved + 7) / 8 * 6; -- 6µs/word
   end add_in_the_IO_CPU_time;

end IOC.fast;
