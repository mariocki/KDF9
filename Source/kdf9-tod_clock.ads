-- functions that implement timing for Director emulation.
--
-- This file is part of ee9 (6.2r), the GNU Ada emulator of the English Electric KDF9.
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

package KDF9.TOD_clock is

   -- The date a multiple of 28 years ago has the same day/date correspondence as today.
   -- To avoid exposing KDF9's lack of Y2K compliance, ee9 uses such a date before 2000.
   -- 8-)
   -- todays_date_28n_years_ago returns a word of 8 KDF9 characters in the format DD/MM/YY.

   function todays_date_28n_years_ago
   return KDF9.word;

   -- The time in microseconds since midnight.
   function the_time_of_day
   return KDF9.us;

end KDF9.TOD_clock;
