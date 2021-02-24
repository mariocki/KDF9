-- Implement the timing OUTs of the EE Time Sharing Directors.
--
-- This file is part of ee9 (6.1a), the GNU Ada emulator of the English Electric KDF9.
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

package body KDF9.TSD.timing is

   -- This is the actual wall clock time at which the program was loaded.
   -- If signature hashing is enabled, it stays at zero to get a repeatable hash.
   the_time_of_loading : KDF9.us := 0;

   -- Set the base for virtual elapsed time reckoning.
   procedure set_the_time_of_loading (the_time : in KDF9.us) is
   begin
      the_time_of_loading := the_time;
   end set_the_time_of_loading;

   -- Return a time in µs as 48-bit seconds to 23 integral places.
   function OUT_time (microseconds : KDF9.us)
   return KDF9.word is
      -- The time was recorded by the hardware in units of 32 us, not 1 us.
      truncated_time : constant KDF9.us := microseconds and not 31;
   begin
      if truncated_time < 2**23 * 1E6 then
         -- 2**18 / 15625 = 2**24 / 1E6, with no risk of overflow in 64 bits.
         return KDF9.word(truncated_time * 2**18 / 15625);
      else
         -- The virtual elapsed time overflows the 23-bit seconds field.
         -- This would never have happened to a real KDF9, as 2**23 seconds is over three months.
         -- No KDF9 could stay up that long!
         -- However 2**23 KDF9 seconds pass in about 5 hours of ee9 real time,
         --    so precautions have to be taken.
         raise program_exit with "the KDF9 has been running too long, time > 2**23 seconds";
      end if;
   end OUT_time;

   procedure do_OUT_3 is
   begin
      push(OUT_time(the_CPU_time));
      the_trace_operand := read_top;
   end do_OUT_3;

   procedure do_OUT_9 is
   begin
      -- A TOD clock is simulated using the real TOD at which the program was
      --    loaded, and the virtual time that has elapsed since.
      push(OUT_time(the_time_of_loading + the_clock_time));
      the_trace_operand := read_top;
   end do_OUT_9;

   procedure do_OUT_17 is
   begin
      ensure_that_the_NEST_has_room_for_2_results;
      -- In program mode, the Elapsed Time is the same thing as the_clock_time.
      push(OUT_time(the_clock_time));
      push(OUT_time(the_CPU_time));
      the_trace_operand := read_top;
   end do_OUT_17;

end KDF9.TSD.timing;
