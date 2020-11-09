-- ioc-fd.ads
--
-- Emulation of a fixed disc drive buffer.
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

private with FD_layout;

package IOC.FD is

   pragma Unsuppress(All_Checks);

   type device is new IOC.device with private;

   subtype disc is FD.device;

   overriding
   procedure PIA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PID (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIE (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIG (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PIH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PMA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PMB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PMC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PMD (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PMF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PMG (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure PMH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POD (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POE (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POG (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POK (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POL (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

private

   use FD_layout;

   -- The disc storage is actually implemented in an external file.
   -- The comb and locus variables shadow the physical state of the drive.
   -- They are used to derive a file address from the position established
   --    by seeks and transfer operations.

   type comb_data is array (FD_layout.platter_number)
                  of FD_layout.seek_area_number;
   type disc_data is array (FD_layout.platter_number, FD_layout.seek_area_number)
                  of FD_layout.track_set;

   type device is new IOC.device with
      record
         comb               : FD.comb_data := (others => 0);
         locus,
         target             : FD_layout.locus;
         switch_time,
         seek_time,
         latency_time,
         data_time,
         last_time,
         elapsed_time_total : KDF9.microseconds := 0;
         seek_count,
         latency_count,
         sector_count,
         byte_count         : KDF9.word := 0;
      end record;

   overriding
   procedure Initialize (the_FD : in out FD.device);

   overriding
   procedure Finalize (the_FD : in out FD.device);

   overriding
   function usage (the_FD : FD.device)
   return KDF9.word;

   overriding
   function IO_elapsed_time_total (the_FD : FD.device)
   return KDF9.microseconds;

end IOC.FD;
