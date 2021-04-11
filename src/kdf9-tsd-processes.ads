-- Implement OUTs 0.. 2 and 5..7 of the EE Time Sharing Directors.
--
-- This file is part of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
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

package KDF9.TSD.processes is

   procedure do_OUT_0
      with No_Return, Inline => False;

   procedure do_OUT_1
      with No_Return, Inline => False;

   procedure do_OUT_2
      with No_Return, Inline => False;

   procedure complete_TSD_OUT_2
      with Inline => False;

   procedure do_OUT_97
      with Inline => False;

end KDF9.TSD.processes;
