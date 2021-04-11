-- Emulation of a tape punch buffer.
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

package IOC.slow.shift.TP is

   type device is new IOC.slow.shift.device with private;

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

   -- This is called "word gap" in the Manual, but never defined.
   overriding
   procedure POF (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- the_T_bit_is_set (the buffer has been switched from a tape punch to a graph plotter)
   overriding
   procedure PMB (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

   -- Finalize TP1 if necessary and remove it from the configuration, to allow GP0 to be attached.
   procedure remove_from_buffer (b : in KDF9.buffer_number);

   -- Set the character code to be used by the TP unit.
   procedure set_unit_code(unit : in Natural; is_transcribing : in Boolean);

private

   type device is new IOC.slow.shift.device with null record;

   overriding
   procedure Initialize (the_TP : in out TP.device);

   overriding
   procedure Finalize (the_TP : in out TP.device);

   overriding
   function kind (the_TP : TP.device)
   return IOC.device_kind
   is (TP_kind);

   overriding
   function quantum (the_TP : TP.device)
   return KDF9.us
   is (1E6 / 110);

end IOC.slow.shift.TP;
