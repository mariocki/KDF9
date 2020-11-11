-- kdf9-tod_clock.adb
--
-- functions that implement timing for Director emulation.
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

with Ada.Calendar;
with Ada.Calendar.Formatting;

use  Ada.Calendar;
use  Ada.Calendar.Formatting;

package body KDF9.TOD_clock is

   pragma Unsuppress(All_Checks);

   function todays_date_28n_years_ago
   return KDF9.word is

      -- For values of i in 0..99, return two 6-bit KDF9 decimal digits.
      function as_2_digits (i : KDF9.word)
      return KDF9.word is
         zero : constant KDF9.word := 8#20#;  -- KDF9 code for '0'.
      begin
         return (i/10 + zero)*64 or (i mod 10 + zero);
      end as_2_digits;

      slash : constant KDF9.word := 8#17#;  -- The result is in the form DD/MM/YY.
      today : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      year, month, day, hour, minute, second, sub_second : KDF9.word;

   begin  -- todays_date_28n_years_ago
      Split(today,
            Year_Number(year),
            Month_Number(month),
            Day_Number(day),
            Hour_Number(hour),
            Minute_Number(minute),
            Second_Number(second),
            Second_Duration(sub_second)
           );
      loop  -- Repeat n > 0 times, assuming no time travel into the past!
         year := year - 28;
      exit when year < 2000;
      end loop;
      return (as_2_digits(day)*64   or slash) * 64**5  -- DD/.....
          or (as_2_digits(month)*64 or slash) * 64**2  --    MM/..
          or (as_2_digits((year) mod 100));            --       YY
   end todays_date_28n_years_ago;

   function the_time_of_day
   return KDF9.microseconds is
      today : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      year, month, day, hour, minute, second, sub_second : KDF9.word;
   begin
      Split(today,
            Year_Number(year),
            Month_Number(month),
            Day_Number(day),
            Hour_Number(hour),
            Minute_Number(minute),
            Second_Number(second),
            Second_Duration(sub_second)
           );
      return KDF9.microseconds(hour*3600 + minute*60 + second) * 1_000_000;
   end the_time_of_day;


end KDF9.TOD_clock;
