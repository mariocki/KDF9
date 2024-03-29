-- Emulation of a lineprinter buffer.
-- Lineprinters are "unit record" devices: they cannot transfer less than a whole line.
--
-- This file is part of ee9 (9.0p), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2022, W. Findlay; all rights reserved.
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

package IOC.slow.unit.LP is

   type device is new IOC.slow.unit.device with private;

   -- LPQq
   overriding
   procedure POA (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- LPEQq
   overriding
   procedure POB (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Character write ??
   overriding
   procedure POC (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Character write to End_Message ??
   overriding
   procedure POD (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

private

   type device is new IOC.slow.unit.device with null record;

   overriding
   procedure Initialize (the_LP : in out LP.device);

   overriding
   procedure Finalize (the_LP : in out LP.device);

   overriding
   function kind (the_LP : LP.device)
   return IOC.device_kind
   is (LP_kind);

   overriding
   function quantum (the_LP : LP.device)
   return KDF9.us
   is (1E6 / (900 / 60)); -- 900 lines per minute.

end IOC.slow.unit.LP;
