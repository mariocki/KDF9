-- ioc-magtape-st.adb
--
-- Emulation of a 7-track IBM-compatible magnetic tape buffer.
--
-- This file is part of ee9 (V2.0r), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2015, W. Findlay; all rights reserved.
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

package body IOC.magtape.ST is

   pragma Unsuppress(All_Checks);

   overriding
   procedure PMK (the_IBM     : in out ST.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_IBM.PMA(Q_operand, set_offline);
   end PMK;

   overriding
   procedure PML (the_IBM     : in out ST.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_IBM.PME(Q_operand, set_offline);
   end PML;

end IOC.magtape.ST;
