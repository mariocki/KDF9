-- Emulation of a card punch buffer.
-- Card punches are "unit record" devices: they cannot transfer less than a whole card.
--
-- This file is part of ee9 (6.2r), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.slow.unit.CP is

   type device is new IOC.slow.unit.device with private;

   -- Punch binary mode.
   overriding
   procedure POA (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Punch binary mode to End Message.
   overriding
   procedure POB (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Punch binary character mode.
   overriding
   procedure POC (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Punch binary character mode to End Message.
   overriding
   procedure POD (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- As POC.
   overriding
   procedure POE (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- As POA.
   overriding
   procedure POF (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Punch alphanumeric mode.
   overriding
   procedure POG (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Punch alphanumeric mode to End Message.
   overriding
   procedure POH (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Punch alphanumeric character mode to End Message.
   overriding
   procedure POK (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Punch alphanumeric character mode.
   overriding
   procedure POL (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

private

   type device is new IOC.slow.unit.device with null record;

   overriding
   procedure Initialize (the_CP : in out CP.device);

   overriding
   procedure Finalize (the_CP : in out CP.device);

   overriding
   function kind (the_CP : CP.device)
   return IOC.device_kind
   is (CP_kind);

   overriding
   function quantum (the_CP : CP.device)
   return KDF9.us
   is (1E6 / (300 / 60)); -- 300 cards per minute.

end IOC.slow.unit.CP;
