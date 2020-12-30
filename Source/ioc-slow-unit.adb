-- ioc-slow-unit.adb
--
-- Emulation of the common functionality of "unit record" (i.e. LP, CP or CR) devices.
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

package body IOC.slow.unit is

   overriding
   function IO_elapsed_time_total (the_buffer : unit.device)
   return KDF9.us is
   begin
      return IO_elapsed_time(the_buffer, the_buffer.unit_count);
   end IO_elapsed_time_total;

   overriding
   function atomic_item_count (the_buffer : unit.device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word is
      pragma Unreferenced(the_buffer);
      pragma Unreferenced(Q_operand);
   begin
      return 1;
   end atomic_item_count;

end IOC.slow.unit;
