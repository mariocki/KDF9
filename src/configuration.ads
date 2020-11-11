-- configuration.ads
--
-- IOC components needing elaboration: these are the devices included in the KDF9 configuration.
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

with IOC.two_shift.FW;
with IOC.two_shift.TR;
with IOC.two_shift.TP;
with IOC.two_shift.GP;
with IOC.LP;
with IOC.CR;
with IOC.CP;
with IOC.magtape.MT; pragma Elaborate_All(IOC.magtape.MT);
with IOC.magtape.ST; pragma Elaborate_All(IOC.magtape.ST);
--with IOC.DR;
with IOC.FD;

pragma Unreferenced(IOC.two_shift.FW);

pragma Unreferenced(IOC.two_shift.TR);

pragma Unreferenced(IOC.two_shift.TP);

pragma Unreferenced(IOC.two_shift.GP);

pragma Unreferenced(IOC.LP);

pragma Unreferenced(IOC.CR);

pragma Unreferenced(IOC.CP);

pragma Unreferenced(IOC.magtape.MT);

pragma Unreferenced(IOC.magtape.ST);

--pragma Unreferenced(IOC.DR);

pragma Unreferenced(IOC.FD);

package configuration is

   pragma Unsuppress(All_Checks);

end configuration;
