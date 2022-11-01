-- Implement the API (OUTs) of the EGDON Director.
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

package body KDF9.EGDON is

   procedure do_an_EGDON_OUT (OUT_number : in KDF9.word) is
   begin
      trap_unimplemented_feature("EGDON OUT" & OUT_number'Image);
   end do_an_EGDON_OUT;

end KDF9.EGDON;
