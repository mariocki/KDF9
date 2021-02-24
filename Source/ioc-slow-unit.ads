-- Emulation of the common functionality of "unit record" (i.e. LP, CP or CR) devices.
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

package IOC.slow.unit is

   --
   -- This is the root type for all unit-record I/O device types.
   --

   type device is abstract new IOC.slow.device with private;

private

   type device is abstract new IOC.slow.device with
      record
         unit_count : KDF9.word := 0;
      end record;

   overriding
   function IO_elapsed_time_total (the_buffer : unit.device)
   return KDF9.us;

   overriding
   function atomic_item_count (the_buffer : unit.device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word;

end IOC.slow.unit;
