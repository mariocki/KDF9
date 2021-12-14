-- Emulation of a standard interface buffer.
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

package IOC.slow.shift.SI is

   -- The Standard Interface Buffer is the KDF9 device about which least is presently known.
   -- Anecdotal evidence suggests it is an implementation of the British Standard Interface (BSIF).
   -- ee9 implements a best guess as to its functionality, based on the following considerations.

   -- According to the Manual, Appendix 6.1, p.296,
   --    the Standard Interface Buffer has orders that look very like the union of a TR and a TP.

   -- However:

   -- (a) PIE and PIF do a read with "parity off".
   --     I think this relates to a feature of the BSIF,
   --        whereby a source device can omit parity if it de-asserts its "parity valid" signal.
   --     With such a device the KDF9 would need a way of ignoring spurious parity errors.
   -- PIE/PIF act in exactly the same way as PIA/PIB as there will be no such error under ee9.

   -- (b) PMB and PMC set the Test Register "if 8 channel set".  The BSIF is 8 data bits wide.
   --     I think this signals that the KDF9 should use "character" orders to access all 8 bits,
   --        and I think that the other orders access only the low-order 6 bits of the interface.
   -- ee9 always asserts "8 channel set", as it is always capable of providing 8-bit bytes.

   -- In any case, 6-bit transfers work in the same way as for paper tape readers and punches.

   type device is new IOC.slow.shift.device with private;

   overriding
   procedure PIA (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIB (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIC (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PID (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIE (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIF (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIG (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIH (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMB (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMC (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POA (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POB (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POC (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POD (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POE (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POF (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

   procedure replace_on_buffer (b : in KDF9.buffer_number);

   function SI0_is_enabled
   return Boolean;

private

   type device is new IOC.slow.shift.device with null record;

   overriding
   procedure Initialize (the_SI : in out SI.device);

   overriding
   procedure Finalize (the_SI : in out SI.device);

   overriding
   function kind (the_SI : SI.device)
   return IOC.device_kind
   is (SI_kind);

   overriding
   function quantum (the_SI : SI.device)
   return KDF9.us
   is (1E6 / 50E3); -- I am guessing 50_000 characters per second.

end IOC.slow.shift.SI;
