-- ioc-dr.adb
--
-- Emulation of a drum store buffer.
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

package body IOC.DR is

   pragma Unsuppress(All_Checks);

   overriding
   procedure Initialize (the_DR   : in out DR.device) is
   begin
      open(the_DR, rd_wr_mode, attaching => False);
   end Initialize;

   overriding
   procedure PIA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PIA;

   overriding
   procedure PIB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PIB;

   overriding
   procedure PIC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PIC;

   overriding
   procedure PID (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PID;

   overriding
   procedure PIE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PIE;

   overriding
   procedure PIF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PIF;

   overriding
   procedure PIG (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PIG;

   overriding
   procedure PIH (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end PIH;

   overriding
   procedure PMB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_DR, Q_operand);
      validate_parity(the_DR);
      raise NYI_trap;
   end PMB;

   overriding
   procedure POA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end POA;

   overriding
   procedure POB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end POB;

   overriding
   procedure POC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end POC;

   overriding
   procedure POD (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end POD;

   overriding
   procedure POE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end POE;

   overriding
   procedure POF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      raise NYI_trap;
   end POF;

   DR_quantum : constant := 1E6 / 500E3;  -- 500_000 characters per second.

   DR0 : aliased DR.device (number => ND0_number, kind => DR_kind, unit => 0,
                            quantum => DR_quantum,
                            is_slow => False);
   pragma Unreferenced(DR0);


end IOC.DR;
