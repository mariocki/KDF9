-- CPU I/O orders are dispatched here to device-specific handlers within the IOC type hierarchy.
--
-- This file is part of ee9 (8.1a), the GNU Ada emulator of the English Electric KDF9.
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

with KDF9.PHU_store;
with tracing;

use  tracing;

package body IOC.dispatcher is

   --
   --
   -- CLO, SLO and TLO do not operate on a buffer, and so can be fully implemented here.
   --
   --

   procedure CLO (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
      use  KDF9.PHU_store;
   begin
      -- This is a Director-only instruction.
      take_note_of_test("   ", Q_operand, False);
      unlock_absolute_addresses(Q_operand);
      -- CLO also clears PHU[CPL].
      PHU(CPL) := idle_PHU;
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end CLO;

   procedure SLO (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      -- This is a Director-only instruction.
      take_note_of_test("   ", Q_operand, False);
      lock_out_absolute_addresses(Q_operand);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end SLO;

   procedure TLO (Q_operand   : in KDF9.Q_register;
                  result      : out Boolean) is
   begin
      -- This is NOT Director-only.
      result := there_are_locks_in_relative_addresses(Q_operand);
      take_note_of_test("   ", Q_operand, result);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end TLO;

   --
   --
   -- All other I/O orders do access a buffer, and so dispatch to the relevant device driver.
   --
   --

   procedure BUSY (Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).BUSY(Q_operand, set_offline, result);
   end BUSY;

   procedure PAR (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean;
                  result      : out Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PAR(Q_operand, set_offline, result);
   end PAR;

   procedure MANUAL_CT (Q_operand   : in KDF9.Q_register;
                        set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).MANUAL_CT(Q_operand, set_offline);
   end MANUAL_CT;

   procedure INT (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).INT(Q_operand, set_offline);
   end INT;

   procedure PIA (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PIA(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PIA;

   procedure PIB (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PIB(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PIB;

   procedure PIC (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PIC(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PIC;

   procedure PID (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PID(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PID;

   procedure PIE (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PIE(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PIE;

   procedure PIF (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PIF(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PIF;

   procedure PIG (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PIG(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PIG;

   procedure PIH (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PIH(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end PIH;

   procedure PMA (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PMA(Q_operand, set_offline);
   end PMA;

   procedure PMB (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PMB(Q_operand, set_offline);
   end PMB;

   procedure PMC (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PMC(Q_operand, set_offline);
   end PMC;

   procedure PMD (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PMD(Q_operand, set_offline);
   end PMD;

   procedure PME (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PME(Q_operand, set_offline);
   end PME;

   procedure PMF (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PMF(Q_operand, set_offline);
   end PMF;

   procedure PMG (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PMG(Q_operand, set_offline);
   end PMG;

   procedure PMK (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PMK(Q_operand, set_offline);
   end PMK;

   procedure PML (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).PML(Q_operand, set_offline);
   end PML;

   procedure POA (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POA(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end POA;

   procedure POB (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POB(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end POB;

   procedure POC (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POC(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end POC;

   procedure POD (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POD(Q_operand, set_offline);
      add_in_the_IO_lockout_CPU_time(Q_operand);
   end POD;

   procedure POE (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POE(Q_operand, set_offline);
   end POE;

   procedure POF (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POF(Q_operand, set_offline);
   end POF;

   procedure POG (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POG(Q_operand, set_offline);
   end POG;

   procedure POH (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POH(Q_operand, set_offline);
   end POH;

   procedure POK (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POK(Q_operand, set_offline);
   end POK;

   procedure POL (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      buffer(Q_operand.C and buffer_number_mask).POL(Q_operand, set_offline);
   end POL;

end IOC.dispatcher;
