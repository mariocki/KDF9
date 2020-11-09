-- kdf9.directors.ads
--
-- Implement the APIs  (OUTs) of the supported KDF9 Directors.
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

package KDF9.Directors is

   pragma Unsuppress(All_Checks);

   -- Emulate a subset of the EGDON Director's OUT API.
   procedure do_an_EGDON_OUT (OUT_number : in KDF9.word);

   -- Implement a subset of the Time Sharing Director's OUT 8 API.
   procedure do_an_OUT_8;

   -- Emulate a subset of the Time Sharing Director's OUT API.
   procedure do_a_TSD_OUT (OUT_number : in KDF9.word);

   -- These are the device-type codes to be given when requesting
   --    the allocation of a peripheral with TSD OUT 5,
   --       according to the Manual and the document
   --          "Order Code Notes 18-Further OUTs".

   FW_OUT5_code : constant := 0;
   TP_OUT5_code : constant := 1;
   TR_OUT5_code : constant := 2;
   LP_OUT5_code : constant := 3;
   CR_OUT5_code : constant := 4;
   CP_OUT5_code : constant := 7;
   GP_OUT5_code : constant := 8#20#;

   -- Set the base for virtual elapsed time reckoning.
   procedure set_the_time_of_loading (the_time : in KDF9.microseconds);

end KDF9.Directors;
