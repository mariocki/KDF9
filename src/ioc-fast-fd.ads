-- Emulation of a fixed disc drive.
--
-- This file is part of ee9 (9.0p), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.fast.FD is

   type device is new fast.device with private;

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

   FD0_is_enabled : Boolean := False;

   procedure enable (b : in KDF9.buffer_number);

   procedure replace_on_buffer (b : in KDF9.buffer_number);

   procedure remove_from_buffer (b : in KDF9.buffer_number);

   function as_FD_command (Q_operand : KDF9.Q_register; for_seek, for_FH : Boolean := False)
   return String;

private

   words_per_sector      : constant := 40;
   bytes_per_sector      : constant := 8 * words_per_sector;

   type sector_data      is array (KDF9.address range 0 .. bytes_per_sector-1)
                         of KDF9_char_sets.symbol;

   sectors_per_seek_area  : constant := 96;
   sectors_in_outer_zone  : constant := 64;

   subtype sector_range  is KDF9.Q_part range 0 .. sectors_per_seek_area-1;

   type sector_array     is array (KDF9.Q_part range <>) of FD.sector_data;

   subtype head_range    is KDF9.Q_part range 0 .. 7;

   subtype inner_track   is FD.sector_array(FD.sector_range range 0 ..  7);
   subtype outer_track   is FD.sector_array(FD.sector_range range 0 .. 15);

   last_sector_for_head  : constant array (FD.head_range) of FD.sector_range
                         := (0 .. 3 => FD.outer_track'Last,
                             4 .. 7 => FD.inner_track'Last);

   type outer_data       is array (FD.head_range range 0 .. 3) of FD.outer_track;
   type inner_data       is array (FD.head_range range 4 .. 7) of FD.inner_track;

   -- These rates come from the Manual, §6.1.
   outer_rate : constant := 84_800;          -- bytes per second in the outer zone
   inner_rate : constant := outer_rate / 2;  -- bytes per second in the outer zone

   type track_set is
      record
         outer_zone : FD.outer_data;
         inner_zone : FD.inner_data;
      end record;

   seek_areas_per_platter  : constant := 64;
   subtype seek_area_range is KDF9.Q_part range 0 .. seek_areas_per_platter-1;

   main_discs_per_drive    : constant := 16;
   the_fixed_head_platter  : constant := 16;
   platters_per_drive      : constant := main_discs_per_drive + 1;

   subtype platter_range   is KDF9.Q_part range 0 .. platters_per_drive-1;

   -- The Eldon 2 KDF9 at Leeds University had a 2-drive disc system.
   -- This allows for the maximum number of drives possible.
   number_of_drives : constant := 4;

   subtype drive_range is KDF9.Q_part range 0 .. number_of_drives-1;

   type locus is
      record
         drive_number      : FD.drive_range     := 0;
         platter_number    : FD.platter_range   := 0;
         seek_area_number  : FD.seek_area_range := 0;
         sector_number     : FD.sector_range    := 0;
         has_fixed_heads,
         is_at_end_of_area : Boolean            := False;
      end record;

   -- The disc storage is actually implemented in an external file.
   -- The comb and locus variables shadow the physical state of the drives.
   -- They are used to derive a file address from the position established
   --    by seek and transfer operations.

   type comb_data is array (FD.drive_range, FD.platter_range) of FD.seek_area_range;

   type device is new fast.device with
      record
         comb         : FD.comb_data := (others => (others => 0));
         locus,
         target       : FD.locus;
         data_time,
         seek_time    : KDF9.us := 0;
         seek_count,
         sector_count : KDF9.word := 0;
      end record;

   overriding
   procedure Initialize (the_FD : in out FD.device);

   overriding
   procedure Finalize (the_FD : in out FD.device);

   overriding
   function kind (the_FD : FD.device)
   return IOC.device_kind
   is (FD_kind);

   overriding
   function quantum (the_FD : FD.device)
   return KDF9.us
   is ((1E6 + outer_rate - 1) / outer_rate);

   FD0_number : KDF9.buffer_number := 0;

end IOC.fast.FD;
