-- Implement the API (OUTs) of the EE Time Sharing Directors.
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

package KDF9.TSD is

   procedure do_a_TSD_OUT (OUT_number : in KDF9.word);

   -- Put the parameters of an I/O OUT back into the NEST in case the I/O order causes a lockout.
   procedure restore_the_IO_OUT_operands (OUT_number, parameter : KDF9.word);

   -- Remove the OUT parameters from the NEST after the I/O order completes without interrupting.
   procedure remove_the_IO_OUT_operands;

end KDF9.TSD;
