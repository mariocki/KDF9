-- Emulation of magnetic tape decks and buffers.
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

package body IOC.fast.tape.TSD_OUTs is

   type short_label is new String(1 .. 8);
   type long_label  is new String(1 .. 16);

   function needs_rewinding (b : KDF9.buffer_number)
   return Boolean is
      the_deck : tape.deck renames tape.deck(buffer(b).all);
   begin
      return the_deck.is_open and then the_deck.tape_file.position > 0;
   end needs_rewinding;

   procedure do_OUT_4 is
      B : KDF9.Q_part;
      S : KDF9.word;
      W : KDF9.word;
   begin
      ensure_that_the_NEST_holds_an_operand;
      W := pop;
      declare
         label : constant short_label := short_label(to_string(W));
      begin
         find_tape(tape.data_storage(label), B, S);
         push(KDF9.word(B));
         the_trace_operand := KDF9.word(B);
         if W = 0 then
            log_API_message("OUT 4: requested a ZERO tape  and got "
                          & device_name_of(buffer(B).all)
                          & " with TSN �"
                          & to_string(S)
                          & "�"
                           );
         else
            log_API_message("OUT 4: requested  �"
                          & String(label)
                          & "�  and got "
                          & device_name_of(buffer(B).all)
                          & " with TSN �"
                          & to_string(S)
                          & "�"
                           );
         end if;
      end;
      set_state_of(buffer(B), allocated => True);
   end do_OUT_4;

   procedure do_OUT_10 is
      B : KDF9.Q_part;
      P : KDF9.pair;
      S : KDF9.word;
   begin
      ensure_that_the_NEST_holds_2_operands;
      P := pop;
      declare
         label : constant long_label := long_label(to_string(P));
      begin
         find_tape(tape.data_storage(label), B, S);
         push(S);
         push(KDF9.word(B));
         the_trace_operand := KDF9.word(B);
         log_API_message("OUT 10: requested �"
                       & String(label)
                       & "� and got "
                       & device_name_of(buffer(B).all)
                       & " with TSN �"
                       & to_string(S)
                       & "�"
                        );
      end;
      set_state_of(buffer(B), allocated => True);
   end do_OUT_10;

end IOC.fast.tape.TSD_OUTs;
