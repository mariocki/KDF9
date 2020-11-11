-- ioc-shift_devices-tp.ads
--
-- Emulation of a tape punch buffer.
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

package IOC.two_shift.TP is

   pragma Unsuppress(All_Checks);

   type device is new IOC.two_shift.device with private;

   -- PWQq
   overriding
   procedure POA (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- PWEQq
   overriding
   procedure POB (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- PWCQq
   overriding
   procedure POC (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- PWCEQq
   overriding
   procedure POD (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- PGAPQq
   overriding
   procedure POE (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- ??
   -- This is called "word gap" in the Manual, but never defined.
   -- Assume this acts exactly like POE.
   overriding
   procedure POF (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TR := (the buffer has been switched from a tape punch to a graph plotter)
   overriding
   procedure PMB (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure switch_the_shared_buffer_from_TP1;

private

   type device is new IOC.two_shift.device with null record;

   overriding
   procedure Initialize (the_TP : in out TP.device);

   overriding
   procedure Finalize (the_TP : in out TP.device);

end IOC.two_shift.TP;
