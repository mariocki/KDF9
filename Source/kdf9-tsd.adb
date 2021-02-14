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

with IOC.fast.DR.TSD_OUTs;
with IOC.fast.FD.TSD_OUTs;
with IOC.fast.MT.TSD_OUTs;
with KDF9.TSD.peripherals;
with KDF9.TSD.processes;
with KDF9.TSD.spooling;
with KDF9.TSD.timing;
with settings;
with tracing;

use  IOC.fast.DR.TSD_OUTs;
use  IOC.fast.FD.TSD_OUTs;
use  IOC.fast.MT.TSD_OUTs;
use  KDF9.TSD.peripherals;
use  KDF9.TSD.processes;
use  KDF9.TSD.spooling;
use  KDF9.TSD.timing;
use  settings;
use  tracing;

package body KDF9.TSD is

   procedure restore_the_IO_OUT_operands (OUT_number, parameter : KDF9.word) is
   begin
      push(parameter);
      push(OUT_number);
   end restore_the_IO_OUT_operands;

   procedure remove_the_IO_OUT_operands renames pop_pair;

   -- Emulate a subset of the Time Sharing Director's OUT API.
   procedure do_a_TSD_OUT (OUT_number : in KDF9.word) is
   begin
      -- Dismiss the OUT number in N1, allowing for an empty NEST, treated as OUT 0.
      if the_nest_depth > 0 then
         pop;
      end if;

      case OUT_number is

         when 0 =>
            do_OUT_0;

         when 1 =>
            do_OUT_1;

         when 2 =>
            do_OUT_2;

         when 3 =>
            do_OUT_3;

         when 4 =>
            do_OUT_4;

         when 5 =>
            do_OUT_5;

         when 6 =>
            do_OUT_6;

         when 7 =>
            do_OUT_7;

         when 8 =>
            do_OUT_8;

         when 9 =>
            do_OUT_9;

         when 10 =>
            do_OUT_10;

         when 11 =>
            do_OUT_11;

         when 12 =>
            do_OUT_12;

         when 13 =>
            do_OUT_13;

         when 14 =>
            do_OUT_14;

         when 16 =>
            do_OUT_16;

         when 17 =>
            do_OUT_17;

         when 41 =>
            do_OUT_41;

         when 42 =>
            do_OUT_42;

         when 43 =>
            do_OUT_43;

         when 44 =>
            do_OUT_44;

         when 45 =>
            do_OUT_45;

         when 47 =>
            do_OUT_47;

         when 70 =>
            -- This is not a genuine TSD OUT, it is an expedient for debugging KAlgol,
            --   so ee9 simply erases its parameters from N1 and N2.
            ensure_that_the_nest_holds_2_operands;
            pop_pair;

         when 97 =>
            -- This is not a genuine TSD OUT, it gets an integer value from the command line.
            -- The operand is the name of an environment variable.
            -- The result is the numerical value of that variable.
            ensure_that_the_nest_holds_an_operand;
            do_OUT_97;

         when 98 =>
            -- This is not a genuine TSD OUT, it is an ee9 'OUT' for setting FW output format.
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            realistic_FW_output_is_wanted := the_trace_operand /= 0;

         when 99 =>
            -- This is not a genuine TSD OUT, it is an ee9 'OUT' for program instrumentation.
            -- Get present value of the Instruction Count Register (ICR) from within ee9.
            push(KDF9.word(ICR));
            the_trace_operand := KDF9.word(ICR);

         when others =>
            push(OUT_number);
            trap_failing_OUT(OUT_number, "is not yet implemented");

      end case;
   end do_a_TSD_OUT;

end KDF9.TSD;
