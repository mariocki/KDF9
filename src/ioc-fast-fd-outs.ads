-- ioc.fast-fd-outs.ads
--
-- Implement the fixed disc API (OUTs) of the EE Time Sharing Director.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.fast.FD.OUTs is

   -- See the Manual, Appendix 6, �2 for the TSD FD OUTs.

   procedure do_TSD_OUT_41;

   procedure do_TSD_OUT_42;

   procedure do_TSD_OUT_43;

   procedure do_TSD_OUT_44;

   procedure do_TSD_OUT_45;

end IOC.fast.FD.OUTs;
