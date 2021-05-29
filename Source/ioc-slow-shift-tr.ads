-- Emulation of a paper tape reader buffer.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.slow.shift.TR is

   type device is new IOC.slow.shift.device with private;

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

   -- the_T_bit_is_set (the reader is set to 8-track mode)
   overriding
   procedure PMB (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

   -- Reattach the designated TR to the specified file and set CASE NORMAL (may be irrelevant).
   -- This is done after loading a binary program, to allow access any TR data file(s);
   --    also to access a binary program for loading as an overlay.
   procedure reattach (unit : in Natural; next_file_name : in String);

   -- Read the 9-word bootstrap.
   procedure boot_the_KDF9 (program_file_name : in String);

   -- Read a binary program.
   procedure load_a_program (program_file_name : in String);

   -- This emulates action of a Director call program, including:
   --    1. Moving the JP0 order from E0U to E2U.
   --    2. Inserting the interrupt handling code into E0 and E1, and
   --    3. setting NIA to (4, 0) instead of (0, 0).
   procedure load_a_bare_Director (program_file_name : in String);

   -- Set the character code to be used by the TR unit.
   procedure set_unit_code(unit : in Natural; is_transcribing : in Boolean);

private

   type device is new IOC.slow.shift.device with null record;

   overriding
   procedure Initialize (the_TR : in out TR.device);

   overriding
   procedure Finalize (the_TR : in out TR.device);

   overriding
   function kind (the_TR : TR.device)
   return IOC.device_kind
   is (TR_kind);

   overriding
   function quantum (the_TR : TR.device)
   return KDF9.us
   is (1_000);

end IOC.slow.shift.TR;
