-- disc.ads
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

package body FD_layout is

   -- Hypothesis:
   -- Where a specification of the Fixed Disc subsystem cannot be inferred from extant
   -- software, such as the Eldon 2 Director, or the EE KDF9 Programming Manual,
   -- then it is reasonable to extrapolate from the document:
   --    "GENERAL INFORMATION MANUAL dp/f-5022 DISCfILE (sic) STORAGE SYSTEM",
   -- by Data Products Corporation, dated March 1965; which describes a similar model.
   -- This document is referred to here as "GIM".
   -- Confirmation of much of this material has been gained from the ICT document:
   --    "Data Disc Store 1956:, dated September 1964"
   -- which describes the same device offered as the first disc drive for the 1900 Series.
   -- All three depict the drive as having a different division of tracks into sectors.

   disc_addressing_error : exception;

   -- Hypothesis:
   -- The format of the disc address assumed here follows that given in GIM.
   function locus_from (Q_operand : KDF9.Q_register)
   return disc.locus is
      parameter : constant KDF9.Q_part := Q_operand.C / 16; -- remove the buffer number
      seek_area : constant KDF9.Q_part := parameter mod seek_areas_per_platter;
      platter   : constant KDF9.Q_part
                := parameter / seek_areas_per_platter mod platters_per_drive;
      drive     : constant KDF9.Q_part
                := parameter / seek_areas_per_platter / platters_per_drive;
   begin
      if drive > disc.drive_number'Last then
         raise disc_addressing_error with "invalid unit number " & KDF9.Q_part'Image(drive);
      end if;
      -- Hypothesis:
      -- Seeking to a new locus zeroizes the sector number and clears the end-of-area flag.
      return (
              drive_number      => drive,
              platter_number    => platter,
              seek_area_number  => seek_area,
              sector_number     => 0,
              is_at_end_of_area => 0
             );
   end locus_from;

   -- These functions are used to avoid a cyclical dependency on the formatting package.

   subtype decimal is KDF9.Q_part range 0 .. 9;

   function digit (N : decimal)
   return Character is
   begin
      return Character'Val(N + Character'Pos('0'));
   end digit;

   subtype centennial is KDF9.Q_part range 0 .. 99;

   -- Return N as 2 decimal digits.
   function dec_of (N : centennial)
   return String is
   begin
      return (1 => digit(N/10), 2 => digit(N mod 10));
   end dec_of;

   function formatted_as_FD_command (Q_operand : in KDF9.Q_register)
   return String is
      locus : constant disc.locus := locus_from(Q_operand);
   begin
      return "D" & dec_of(locus.drive_number)(2)
           & "P" & dec_of(locus.platter_number)
           & "S" & dec_of(locus.seek_area_number);
   end formatted_as_FD_command;

end FD_layout;
