-- CPU I/O orders are dispatched here to device-specific handlers within the IOC type hierarchy.
--
-- This file is part of ee9 (6.0a), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.dispatcher is

   procedure MANUAL_CT (Q_operand   : in KDF9.Q_register;
                        set_offline : in Boolean);

   procedure BUSY (Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out Boolean);

   procedure PAR (Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean;
                  result      : out Boolean);

   procedure TLO (Q_operand   : in KDF9.Q_register;
                  result      : out Boolean);

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

end IOC.dispatcher;

