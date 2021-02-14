-- IOC.equipment.adb
--
-- Data supporting the definition of a KDF9 I/O equipment configuration.
--
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

with IOC.absent;
with IOC.fast.DR;
with IOC.fast.FD;
with IOC.fast.MT;
with IOC.slow.shift.FW;
with IOC.slow.shift.GP;
with IOC.slow.shift.SI;
with IOC.slow.shift.TP;
with IOC.slow.shift.TR;
with IOC.slow.unit.CP;
with IOC.slow.unit.CR;
with IOC.slow.unit.LP;
with settings;

package body IOC.equipment is

   procedure configure is
   begin
      for b in KDF9.buffer_number loop
         case equipment.choice(b) is
            when CP => IOC.slow.unit.CP.enable(b);
            when CR => IOC.slow.unit.CR.enable(b);
            when DR => IOC.fast.DR.enable(b);
            when FD => IOC.fast.FD.enable(b);
            when FW => IOC.slow.shift.FW.enable(b);
            when GP => IOC.slow.shift.GP.enable(b);
            when LP => IOC.slow.unit.LP.enable(b);
            when MT => IOC.fast.MT.enable_MT_deck(b);
            when NA => IOC.absent.enable(b);
            when SI => IOC.slow.shift.SI.enable(b);
            when ST => IOC.fast.MT.enable_ST_deck(b);
            when TP => IOC.slow.shift.TP.enable(b);
            when TR => IOC.slow.shift.TR.enable(b);
         end case;
      end loop;
      if IOC.buffer(0) = null              or else
            IOC.buffer(0).kind /= IOC.FW_kind then
         trap_operator_error("buffer #00", "is not a FW");
      end if;
      if IOC.buffer(1) = null              or else
            IOC.buffer(1).kind /= IOC.TR_kind then
         trap_operator_error("buffer #01", "is not a TR");
      end if;
      if the_graph_plotter_is_enabled then
         install_GP0;
      end if;
      for b in IOC.equipment.choices'Range loop
         if IOC.buffer(b) = null then
            IOC.absent.enable(b);
         end if;
      end loop;
   end configure;

   procedure re_configure is
   begin
      for b in KDF9.buffer_number loop
         case equipment.choice(b) is
            when DR => IOC.fast.DR.re_enable(b);
            when FD => IOC.fast.FD.re_enable(b);
            when SI => IOC.slow.shift.SI.re_enable(b);
            when others => null;
         end case;
      end loop;
   end re_configure;

   procedure install_GP0 is
      b : KDF9.buffer_number;
   begin
      if the_graph_plotter_is_enabled then
         IOC.slow.shift.TP.disable_TP1(b);
         IOC.slow.shift.GP.enable(b);
      end if;
   end install_GP0;

end IOC.equipment;
