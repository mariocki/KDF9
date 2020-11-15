-- ioc-assignment.ads
--
-- CPU I/O orders are assigned here to device-specific buffers within IOC.
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

package IOC.assignment is

   pragma Unsuppress(All_Checks);

   procedure BUSY (Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out KDF9.word);

   procedure PAR (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean;
                  result      : out KDF9.word);

   procedure TLO (Q_operand   : in KDF9.Q_register;
                  result      : out KDF9.word);

   procedure CTQ (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure CLO (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure SLO (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure INT (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIA (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIB (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIC (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PID (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIE (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIF (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIG (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIH (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMA (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMB (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMC (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMD (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PME (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMF (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMG (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMH (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMK (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PML (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POA (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POB (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POC (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POD (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POE (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POF (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POG (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POH (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POK (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POL (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

end IOC.assignment;

