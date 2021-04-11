-- Emulation of a Calcomp 564 graph plotter, switched to a tape punch buffer.
--
-- This file is part of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.slow.shift.GP is

   type device is new IOC.slow.shift.device with private;

   overriding
   procedure POA (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POB (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POC (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POD (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- the_T_bit_is_set (buffer is switched to graph plotter)
   overriding
   procedure PMB (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

   procedure replace_on_buffer (b : in KDF9.buffer_number);

   procedure notify_invalid_movement (from_x, from_y, step_x, step_y : in Integer)
      with Inline => False;

private

   type device is new IOC.slow.shift.device with null record;

   overriding
   procedure Initialize (the_GP : in out GP.device);

   overriding
   procedure Finalize (the_GP : in out GP.device);

   overriding
   function kind (the_GP : GP.device)
   return IOC.device_kind
   is (GP_kind);

   overriding
   function quantum (the_GP : GP.device)
   return KDF9.us
   is (1E6 / 200);

   overriding
   procedure do_output_housekeeping (the_GP      : in out GP.device;
                                     size, lifts : in     KDF9.word);

end IOC.slow.shift.GP;
