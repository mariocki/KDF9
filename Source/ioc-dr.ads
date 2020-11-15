-- ioc-dr.ads
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

package IOC.DR is

   pragma Unsuppress(All_Checks);

   type device is new IOC.device with private;

   overriding
   procedure PIA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PID (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIG (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIH (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POD (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

private

   sector_size : constant := 128;

   type sector_data is array(KDF9.address range 0 .. sector_size-1) of KDF9.word;

   drum_size : constant := 320;

   type drum_data is array (KDF9.word range 0 .. drum_size-1) of sector_data;

   type device is new IOC.device with
      record
         data : drum_data;
      end record;

   overriding
   procedure Initialize (the_DR : in out DR.device);

end IOC.DR;
