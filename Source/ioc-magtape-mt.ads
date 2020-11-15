-- ioc-magtape-mt.adb
--
-- Emulation of a KDF9-native magnetic tape buffer.
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

with IOC.magtape; pragma Elaborate_All(IOC.magtape);


package IOC.magtape.MT is

   pragma Unsuppress(All_Checks);


   type deck is new magtape.deck with private;

private

   type deck is new magtape.deck with null record;

   MT_quantum : constant := 1E6 / 40E3;  -- 40_000 characters per second.

   MT0 : aliased deck (number  => MT0_number+0,
                       kind    => MT_kind,
                       unit    => 0,
                       quantum => MT_quantum,
                       is_slow => False);

   MT1 : aliased deck (number  => MT0_number+1,
                       kind    => MT_kind,
                       unit    => 1,
                       quantum => MT_quantum,
                       is_slow => False);

   MT2 : aliased deck (number  => MT0_number+2,
                       kind    => MT_kind,
                       unit    => 2,
                       quantum => MT_quantum,
                       is_slow => False);

   MT3 : aliased deck (number  => MT0_number+3,
                       kind    => MT_kind,
                       unit    => 3,
                       quantum => MT_quantum,
                       is_slow => False);

   MT4 : aliased deck (number  => MT0_number+4,
                       kind    => MT_kind,
                       unit    => 4,
                       quantum => MT_quantum,
                       is_slow => False);

   MT5 : aliased deck (number  => MT0_number+5,
                       kind    => MT_kind,
                       unit    => 5,
                       quantum => MT_quantum,
                       is_slow => False);

   MT6 : aliased deck (number  => MT0_number+6,
                       kind    => MT_kind,
                       unit    => 6,
                       quantum => MT_quantum,
                       is_slow => False);

end IOC.magtape.MT;
