-- Implement the drum API (OUTs) of the EE Time Sharing Director.
--
-- This file is part of ee9 (8.2a), the GNU Ada emulator of the English Electric KDF9.
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

with IOC.dispatcher;
with KDF9.TSD;

use  IOC.dispatcher;
use  KDF9.TSD;

package body IOC.fast.DR.TSD_OUTs is

   OUT13_was_done          : Boolean   := False;
   last_reserved_DR_sector : KDF9.word := -1;

   procedure free_any_reserved_drum_space is
   begin
      OUT13_was_done := False;
      last_reserved_DR_sector := -1;
   end free_any_reserved_drum_space;

   Q : KDF9.Q_register;
   W : KDF9.word;

   procedure access_the_OUT_operand is
   begin
      ensure_that_the_NEST_holds_an_operand;
      W := pop;
      Q := as_Q(W);
      the_trace_operand := W;
    end access_the_OUT_operand;

   procedure ensure_that_DR0_is_enabled (OUT_number : in KDF9.word) is
   begin
      if not DR0_is_enabled then
         trap_failing_OUT(OUT_number, "there is no drum in this configuration");
      end if;
   end ensure_that_DR0_is_enabled;

   procedure formulate_the_drum_transfer_operand (OUT_number : in KDF9.word) is
      S : KDF9.word;
   begin
      ensure_that_DR0_is_enabled(OUT_number);
      if not OUT13_was_done then
         trap_failing_OUT(OUT_number, "obeyed before OUT 13");
      end if;
      S := KDF9.word(Q.C + (Q.M - Q.I + bytes_per_sector/2) / bytes_per_sector);
      if S > last_reserved_DR_sector then
         trap_failing_OUT(OUT_number, "too many drum sectors would be transferred");
      end if;
      Q := (Q.C*16 + DR0_number, Q.I, Q.M);
      the_trace_operand := as_word(Q);
   end formulate_the_drum_transfer_operand;

   procedure do_OUT_11 is
   begin
      access_the_OUT_operand;
      formulate_the_drum_transfer_operand(11);
      restore_the_IO_OUT_operands(11, W);
         POA(Q, False);
      remove_the_IO_OUT_operands;
   end do_OUT_11;

   procedure do_OUT_12 is
   begin
      access_the_OUT_operand;
      formulate_the_drum_transfer_operand(12);
      restore_the_IO_OUT_operands(12, W);
         PIA(Q, False);
      remove_the_IO_OUT_operands;
   end do_OUT_12;

   procedure do_OUT_13 is
   begin
      access_the_OUT_operand;
      ensure_that_DR0_is_enabled(13);
      if OUT13_was_done then
         trap_failing_OUT(13, "obeyed a second time");
      end if;
      if W > sectors_per_system or else
            W = 0                  then
         trap_failing_OUT(13, "demands an impossible number of drum sectors");
      end if;
      last_reserved_DR_sector := W - 1;
      OUT13_was_done := True;
      set_state_of(buffer(DR0_number), allocated => True);
      log_API_message("OUT 13: allocated" & W'Image + "drum sectors");
   end do_OUT_13;

   procedure do_OUT_14 is
   begin
      -- I assume that the drum never experiences a parity error in ee9.
      ensure_that_DR0_is_enabled(14);
      if OUT13_was_done then
         the_trace_operand := (sectors_per_system - last_reserved_DR_sector - 1) or 2**47;
      else
         the_trace_operand := sectors_per_system;
      end if;
      push(the_trace_operand);
   end do_OUT_14;

end IOC.fast.DR.TSD_OUTs;
