-- kdf9.directors.ads
--
-- Implement the APIs (OUTs) of KDF9 Directors.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

package KDF9.Directors is

   -- Emulate a subset of the EGDON Director's OUT API.
   procedure do_an_EGDON_OUT (OUT_number : in KDF9.word);

   -- Emulate a subset of the Time Sharing Director's OUT API.
   procedure do_a_TSD_OUT (OUT_number : in KDF9.word);

   -- Effect a Time Sharing Director OUT 2 restart.
   procedure complete_TSD_OUT_2 (time_limit : in KDF9.word);

   -- Emulate a subset of some other Director's OUT API.
   procedure do_some_other_OUT (OUT_number : in KDF9.word);

   -- Set the base for virtual elapsed time reckoning.
   procedure set_the_time_of_loading (the_time : in KDF9.us);

end KDF9.Directors;
