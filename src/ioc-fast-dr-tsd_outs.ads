-- Implement the drum API (OUTs) of the EE Time Sharing Director.
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

package IOC.fast.DR.TSD_OUTs is

   procedure do_OUT_11;

   procedure do_OUT_12;

   procedure do_OUT_13;

   procedure do_OUT_14;

   procedure free_any_reserved_drum_space;

end IOC.fast.DR.TSD_OUTs;
