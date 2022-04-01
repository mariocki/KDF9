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

package body IOC.absent is

   procedure disavow (the_device  : in out absent.device;
                      Q_operand   : in KDF9.Q_register;
                      set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_operator_error("there is no device on buffer #" & oct_of(the_device.number, 2));
   end disavow;

   overriding
   procedure PIA (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PIA;

   overriding
   procedure PIB (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PIB;

   overriding
   procedure PIC (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PIC;

   overriding
   procedure PID (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PID;

   overriding
   procedure PIE (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PIE;

   overriding
   procedure PIF (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PIF;

   overriding
   procedure PIG (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PIG;

   overriding
   procedure PIH (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PIH;

   overriding
   procedure PMA (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PMA;

   overriding
   procedure PMB (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PMB;

   overriding
   procedure PMC (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PMC;
   overriding
   procedure PMD (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PMD;

   overriding
   procedure PME (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PME;

   overriding
   procedure PMF (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PMF;


   overriding
   procedure PMG (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PMG;

   overriding
   procedure PMK (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PMK;

   overriding
   procedure PML (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end PML;

   overriding
   procedure POA (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POA;

   overriding
   procedure POB (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POB;

   overriding
   procedure POC (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POC;

   overriding
   procedure POD (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POD;

   overriding
   procedure POE (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POE;

   overriding
   procedure POF (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POF;

   overriding
   procedure POG (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POG;

   overriding
   procedure POH (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POH;

   overriding
   procedure POK (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POK;

   overriding
   procedure POL (the_device  : in out absent.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin disavow(the_device, Q_operand, set_offline); end POL;

   overriding
   procedure Initialize (the_device : in out absent.device) is
   begin
      install(the_device);
   end Initialize;

   type AD_access is access absent.device;
   AD_list : array (IOC.unit_number range 0..14) of AD_access with Warnings => Off;

    -- This cannot overflow, because there must be at least 2 non-AD buffers: FW0 and TR0.
   unit : IOC.unit_number := 0;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      AD_list(unit) := new absent.device(number => b, unit => unit);
       unit := unit + 1;
   end enable;

end IOC.absent;

