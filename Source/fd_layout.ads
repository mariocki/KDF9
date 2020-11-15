-- fd_layout.ads
--
-- Storage format of a fixed disc drive system.
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

with KDF9;

use  KDF9;

package FD_layout is

   pragma Unsuppress(All_Checks);

   package disc renames FD_layout;

   sector_size           : constant := 40;
   bytes_per_sector      : constant := 8 * disc.sector_size;

   type sector_data      is array (KDF9.address range 0 .. disc.bytes_per_sector-1)
                         of KDF9.symbol;

   sectors_per_seek_area : constant := 96;

   subtype sector_number is KDF9.Q_part range 0 .. disc.sectors_per_seek_area-1;

   type sector_array     is array (KDF9.Q_part range <>) of disc.sector_data;

   subtype head_number   is KDF9.Q_part range 0 .. 7;

   subtype inner_track   is disc.sector_array(disc.sector_number range 0 .. 7);
   subtype outer_track   is disc.sector_array(disc.sector_number range 0  .. 15);

   last_sector_for_head  : constant array (disc.head_number) of disc.sector_number
                         := (0 .. 3 => disc.outer_track'Last,
                             4 .. 7 => disc.inner_track'Last);

   type outer_data       is array (disc.head_number range 0 .. 3) of disc.outer_track;
   type inner_data       is array (disc.head_number range 4 .. 7) of disc.inner_track;

   type track_set is
      record
         outer_zone : disc.outer_data;
         inner_zone : disc.inner_data;
      end record;

   seek_areas_per_platter   : constant := 64;

   subtype seek_area_number is KDF9.Q_part range 0 .. disc.seek_areas_per_platter-1;

   platters_per_drive       : constant := 16;

   subtype platter_number   is KDF9.Q_part range 0 .. disc.platters_per_drive-1;

   -- The Eldon 2 KDF9 at Leeds University had a 2-drive disc system.
   -- This allows for the maximum number of drives possible.
   number_of_drives : constant := 4;

   subtype drive_number is KDF9.Q_part range 0 .. disc.number_of_drives-1;

   type locus is
      record
         drive_number      : disc.drive_number     := 0;
         platter_number    : disc.platter_number   := 0;
         seek_area_number  : disc.seek_area_number := 0;
         sector_number     : disc.sector_number    := 0;
         is_at_end_of_area : KDF9.word := 0;
      end record;

   function locus_from (Q_operand : KDF9.Q_register)
   return disc.locus;

   function formatted_as_FD_command (Q_operand : KDF9.Q_register)
   return String;

end FD_layout;
