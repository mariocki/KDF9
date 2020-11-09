-- ioc-two_shift-tr.ads
--
-- Emulation of a paper tape reader buffer.
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

package IOC.two_shift.TR is

   pragma Unsuppress(All_Checks);

   type device is new IOC.two_shift.device with private;

   -- PRQq
   overriding
   procedure PIA (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- PREQq
   overriding
   procedure PIB (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   --PRCQq
   overriding
   procedure PIC (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- PRCEQq
   overriding
   procedure PID (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PIA
   overriding
   procedure PIE (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PIB
   overriding
   procedure PIF (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PIC
   overriding
   procedure PIG (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PID
   overriding
   procedure PIH (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TR := (the reader is set to 8-track mode)
   overriding
   procedure PMB (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Reattach the reader to another file and set CASE NORMAL.
   overriding
   procedure reattach (the_buffer  : in out TR.device;
                       to_the_file : in String);

   -- Reattach TR0 to the specified binary program file.
   procedure reattach_TR0 (to_the_file : in String);

   procedure bootstrap_the_KDF9;

   procedure load_a_program;

private

   type device is new IOC.two_shift.device with null record;

   overriding
   procedure Initialize (the_TR : in out TR.device);

   overriding
   procedure Finalize (the_TR : in out TR.device);

end IOC.two_shift.TR;
