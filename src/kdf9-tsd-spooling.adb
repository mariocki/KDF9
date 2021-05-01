-- Implement a subset of the Time Sharing Director's OUT 8 / OUT 16 spooling API.
--
-- This file is part of ee9 (6.3b), the GNU Ada emulator of the English Electric KDF9.
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

with IOC;
with IOC.dispatcher;
with IOC.slow.shift.FW;
with KDF9.store;
with KDF9.TSD.peripherals;

use  IOC;
use  IOC.dispatcher;
use  IOC.slow.shift.FW;
use  KDF9.store;
use  KDF9.TSD.peripherals;

package body KDF9.TSD.spooling is

   procedure do_IO (this_OUT : in KDF9.word) is

      function destination_device_for (the_stream : KDF9.word)
      return IOC.device_number is
         data_word  : constant String(1..16) := oct_of(the_stream);
         the_device : IOC.device_number;
         the_type   : String(1..2);
      begin
         case the_stream is
            when 8#00# =>
               return 0;
            when 8#10# |8#12# |8#14# | 8#16# =>
               the_device := TP0_number; the_type := "TP";
            when 8#11# |8#13# |8#15# | 8#17# =>
               the_device := TP1_number; the_type := "TP";
            when 8#30#..8#37# =>
               the_device := LP0_number; the_type := "LP";
            when 8#50#..8#57# =>
               the_device := TP1_number; the_type := "TP";
            when 8#70#..8#77# =>
               the_device := LP0_number; the_type := "LP";
            when others =>
               trap_failing_OUT(8, "invalid stream #" & data_word);
         end case;
         if the_device = 0 then
            trap_operator_error("OUT 8 needs a " & the_type & " for stream #" & data_word(15..16));
         end if;
         return the_device;
      end destination_device_for;

      W, the_stream : KDF9.word;
      Q, G    : KDF9.Q_register;

      procedure prepare_output_to_FW0 is
         message_prefix  : constant KDF9.word := 8#02_06_21_55_22_00_07_77#; -- LS CS [ m ] SP CN Ø
         prompt_prefix   : constant KDF9.word := 8#02_06_21_61_22_00_07_77#; -- LS CS [ q ] SP CN Ø
         OUT_16_prefix   : constant KDF9.word := 8#02_07_56_33_00_77_77_77#; -- LS CN N º SP Ø  Ø Ø
         filler_in_D0_D5 : constant KDF9.word := 8#77_00_00_00_00_00_00_00#; -- Ø
         S               : KDF9_char_sets.symbol := KDF9_char_sets.Word_Filler;
         the_prefix      : KDF9.word;
      begin  -- prepare_output_to_FW0
         -- The logic of FW streams is rather complex, to preserve the layout of the typescript.
         -- There are three significant aspects.

         -- 1. The message is truncated if longer than 8 words.
         if Q.M - Q.I > 8 then
            Q.M := Q.I + 8;
         end if;

         -- 2. It must not contain LS or HT;
         --       nor ';' in the last word;
         --          nor ';' other than in character position 7;
         --    but anything after an End Message can safely be ignored.
         word_loop: for w in Q.I+1 .. Q.M loop
             for c in KDF9_char_sets.symbol_index'Range loop
                S := fetch_symbol(w, c);
                if S = KDF9_char_sets.Line_Shift                                 or else
                      S = KDF9_char_sets.Tabulation                              or else
                         ((S = KDF9_char_sets.Semi_Colon) and (c /= 7 or  w = Q.M)) then
                   trap_failing_OUT(this_OUT, "invalid data for OUT 8 to FW");
                end if;
         exit word_loop when S in KDF9_char_sets.Semi_Colon | KDF9_char_sets.End_Message;
             end loop;
         end loop word_loop;

         the_prefix := (if S = KDF9_char_sets.Semi_Colon then prompt_prefix else message_prefix);
         the_prefix := (if this_OUT = 16                 then OUT_16_prefix else the_prefix);

         -- 3. Take a new line for each OUT 8 message to the FW.
         --    Set up the format effector(s) in the first word of the OUT 8 area.
         declare
            package FW renames IOC.slow.shift.FW;
            the_FW : FW.device renames FW.device(buffer(0).all);
         begin
            if a_LF_was_just_read(the_FW) then
               -- Replace the redundant Line Shift with a Word Filler character.
               store_word(the_prefix or filler_in_D0_D5, Q.I);
            else
               -- The initial Line Shift is needed.
               store_word(the_prefix, Q.I);
            end if;
         end;
      end prepare_output_to_FW0;

      page_change : constant := 8#77_77_77_77_77_77_77_03#;  --  LP Page Change character

   begin  -- do_IO
      ensure_that_the_NEST_holds_an_operand;
      W := pop;
      Q := as_Q(W);
      the_trace_operand := W;

      -- A FW query has (only) D0 of the control word set.
      if (Q.C and 8#1_00000#) /= 0 then
         Q.C := 0;
      end if;

      if Q.C = Q.I and Q.I = Q.M then
         -- The N2 parameter specifies stream closure.
         flush(buffer(destination_device_for(KDF9.word(Q.C))).all);
         return;
      end if;

      --
      -- This must precede anything that might LOV, e.g. store accesses and I/O orders.
      --
      restore_the_IO_OUT_operands(this_OUT, W);
      --
      --
      --

      -- The (Q, from N2) parameter specifies a block starting with the stream number.
      check_address_and_lockout(Q.I);
      the_stream := fetch_word(Q.I);

      Q.C := destination_device_for(the_stream);
      let_OUT_8_use_the_device_on_buffer(Q.C);

      check_address_and_lockout(Q.I+1);
      G := as_Q(fetch_word(Q.I+1));

      -- See the Manual, §12.6.1.
      if G.C = 4095 and then G.I = 8#177777# then
         -- The G parameter specifies output of a 'gap' suitable for the device.
         if G.M = 0 then
            -- Do nothing.
            the_trace_operand := as_word(G);
         elsif destination_device_for(the_stream) in TP0_number | TP1_number then
            -- Write runout according to the character set mode of the punch.
            G.M := (if G.M in 1 .. 511 then G.M else 120);
            Q := (Q.C, 0, G.M);
            the_trace_operand := as_word(Q);
            POE(Q, False);
         elsif destination_device_for(the_stream) = LP0_number then
             -- Write a Page Change.
            store_word(page_change, Q.I);
            Q := (Q.C, Q.I, Q.I);
            the_trace_operand := as_word(Q);
            POA(Q, False);
         end if;

         --
         --
         remove_the_IO_OUT_operands;
         --
         --

         return;
      end if;

      if Q.M <= Q.I then
         trap_failing_OUT(8, "invalid end-address #" & oct_of(Q.M));
      end if;

      if Q.C = 0 then
         -- The logic for FW streams is more complex, to preserve the layout of the typescript.
         prepare_output_to_FW0;
      else
         -- For non-FW streams, the first word of the OUT 8 area is not transferred.
         Q.I := Q.I + 1;
      end if;
      the_trace_operand := as_word(Q);

      POB(Q, False);

      --
      --
      remove_the_IO_OUT_operands;
      --
      --

   end do_IO;

   procedure do_OUT_8 is
   begin
      do_IO(this_OUT => 8);
   end do_OUT_8;

   procedure do_OUT_16 is
   begin
      do_IO(this_OUT => 16);
   end do_OUT_16;

end KDF9.TSD.spooling;
