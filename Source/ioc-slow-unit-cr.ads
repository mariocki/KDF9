-- Emulation of a card reader buffer.
-- Card readers are "unit record" devices: they cannot transfer less than a whole card.
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.slow.unit.CR is

   type device is new IOC.slow.unit.device with private;

   -- Binary (undecoded) read
   overriding
   procedure PIA (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Binary (undecoded) read to End_Message
   overriding
   procedure PIB (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Binary (undecoded) character read
   overriding
   procedure PIC (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Binary (undecoded) character read to End_Message
   overriding
   procedure PID (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Alphanumeric (decoded) read
   overriding
   procedure PIE (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Alphanumeric (decoded) read to End_Message
   overriding
   procedure PIF (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Alphanumeric (decoded) character read
   overriding
   procedure PIG (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Alphanumeric (decoded) character read to End_Message
   overriding
   procedure PIH (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- the_T_bit_is_set := (RECHECK switch is OFF)
   overriding
   procedure PMB (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

private

   subtype max_card_columns is Positive range 1 .. 160;

   type device is new IOC.slow.unit.device with
      record
         card_image : String(max_card_columns);
      end record;

   overriding
   procedure Initialize (the_CR : in out CR.device);

   overriding
   procedure Finalize (the_CR : in out CR.device);

   overriding
   function kind (the_CR : CR.device)
   return IOC.device_kind
   is (CR_kind);

   overriding
   function quantum (the_CR : CR.device)
   return KDF9.us
   is (1E6 / (600 / 60)); -- 600 cards per minute.

end IOC.slow.unit.CR;
