-- ioc-cp.ads
--
-- Emulation of a card punch buffer.
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

package IOC.CP is

   pragma Unsuppress(All_Checks);

   type device is new IOC.unit_record_device with private;

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

private

   type device is new IOC.unit_record_device with null record;

   overriding
   procedure Initialize (the_CP : in out CP.device);

   overriding
   procedure Finalize (the_CP : in out CP.device);

end IOC.CP;
