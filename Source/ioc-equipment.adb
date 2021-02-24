-- Data supporting the definition of a KDF9 I/O equipment configuration.
--
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

with IOC.absent;
with IOC.fast.DR;
with IOC.fast.FD;
with IOC.fast.tape;
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

   procedure configure_the_IOC is
   begin
      for b in KDF9.buffer_number loop
         case equipment.choice(b) is
            when AD => IOC.absent.enable(b);
            when CP => IOC.slow.unit.CP.enable(b);
            when CR => IOC.slow.unit.CR.enable(b);
            when DR => IOC.fast.DR.enable(b);
            when FD => IOC.fast.FD.enable(b);
            when FW => IOC.slow.shift.FW.enable(b);
            when GP => IOC.slow.shift.TP.remove_from_buffer(b);
                       IOC.slow.shift.GP.enable(b);
            when LP => IOC.slow.unit.LP.enable(b);
            when MT => IOC.fast.tape.enable_MT_deck(b);
            when SI => IOC.slow.shift.SI.enable(b);
            when ST => IOC.fast.tape.enable_ST_deck(b);
            when TP => IOC.slow.shift.TP.enable(b);
            when TR => IOC.slow.shift.TR.enable(b);
         end case;
         if buffer(b) = null then
            IOC.absent.enable(b);
         end if;
      end loop;
      -- By this point every buffer must have an attached device.
      if IOC.buffer(0).kind /= IOC.FW_kind then
         trap_operator_error("buffer #00 must be a FW");
      end if;
      if IOC.buffer(1).kind /= IOC.TR_kind then
         trap_operator_error("buffer #01 must be a TR");
      end if;
   end configure_the_IOC;

   procedure revise_the_configuration is
   begin
      -- By this point every buffer has a device which must be removed before it is replaced.
      for b in equipment.choice'Range loop
         case equipment.choice(b) is
            when DR => IOC.fast.FD.remove_from_buffer(b);
                       IOC.fast.DR.replace_on_buffer(b);
            when FD => IOC.fast.DR.remove_from_buffer(b);
                       IOC.fast.FD.replace_on_buffer(b);
            when GP => if buffer(b).device_name = "TP1" then
                          IOC.slow.shift.TP.remove_from_buffer(b);
                          IOC.slow.shift.GP.replace_on_buffer(b);
                       end if;
            when SI => IOC.slow.shift.SI.replace_on_buffer(b);
            when others => null;
         end case;
      end loop;
   end revise_the_configuration;

end IOC.equipment;
