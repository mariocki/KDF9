-- Handle attempted usage of a buffer with No Device attached.
--
-- This file is part of ee9 (8.2a), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.absent is

   type device is new IOC.device with null record;

   -- All the operations of an absent device terminate the run.

   overriding
   procedure PIA (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIB (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIC (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PID (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIE (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIF (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIG (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIH (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMA (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMB (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMC (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PMD (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PME (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMF (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMG (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMK (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PML (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POA (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POB (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POC (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POD (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POE (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POF (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POG (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POH (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POK (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POL (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- Register the absence of a device in case of future attempted access to buffer b.
   procedure enable (b : in KDF9.buffer_number);

private

   overriding
   procedure Initialize (the_device : in out absent.device);

   overriding
   function is_open (the_device : absent.device)
   return Boolean
   is (False);

   overriding
   function kind (the_device : absent.device)
   return IOC.device_kind
   is (AD_kind);

   overriding
   function quantum (the_device : absent.device)
   return KDF9.us
   is (0);

   overriding
   procedure add_in_the_IO_CPU_time (the_device  : in absent.device;
                                     bytes_moved : in KDF9.word)
   is null;

end IOC.absent;
