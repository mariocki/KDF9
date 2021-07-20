-- ioc-fast-dr-outs.adb
--
-- Implement the drum API (OUTs) of the EE Time Sharing Director.
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

with HCI;
with IOC.dispatcher;
with tracing;

use  HCI;
use  IOC.dispatcher;
use  tracing;

package body IOC.fast.DR.OUTs is

   OUT13_was_done          : Boolean   := False;
   last_reserved_DR_sector : KDF9.word := -1;

   Q : KDF9.Q_register;
   W : KDF9.word;

   procedure check_DR_OUT (
                           OUT_number      : in KDF9.word;
                           it_is_erroneous : in Boolean;
                           error_message   : in String
                          ) is
   begin
      if not is_enabled then
         fail_OUT(OUT_number, "there is no drum in this configuration");
      end if;
      if OUT_number /= 14 then
         ensure_that_the_nest_holds_an_operand;
         the_trace_operand := pop;
      end if;
      if it_is_erroneous then
         fail_OUT(OUT_number, error_message);
      end if;
   end check_DR_OUT;

   procedure prepare_drum_transfer (OUT_number : in KDF9.word) is
   begin
      check_DR_OUT(OUT_number, not OUT13_was_done, "obeyed before drum store reservation");
      Q := as_Q(the_trace_operand);
      W := KDF9.word(Q.C + (Q.M - Q.I + bytes_per_sector/2) / bytes_per_sector);
      if W > last_reserved_DR_sector then
         fail_OUT(OUT_number, "obeyed with wrong parameters that exceed the reservation");
      end if;
      Q := (Q.C*16 + DR0_number, Q.I, Q.M);
   end prepare_drum_transfer;

   procedure do_TSD_OUT_11 is
   begin
      prepare_drum_transfer(11);
      POA(Q, False);
   end do_TSD_OUT_11;

   procedure do_TSD_OUT_12 is
   begin
      prepare_drum_transfer(12);
      PIA(Q, False);
   end do_TSD_OUT_12;

   procedure do_TSD_OUT_13 is
   begin
      check_DR_OUT(13, OUT13_was_done, "obeyed a second time");
      W := the_trace_operand;
      if W > sectors_per_system or else
            W = 0                       then
         fail_OUT(13, "asks for an impossible number of sectors");
      end if;
      last_reserved_DR_sector := W - 1;
      OUT13_was_done := True;
      set_state_of(buffer(DR0_number), allocated => True);
      log_API_message("OUT 13: allocated" & W'Image & " drum sectors");
   end do_TSD_OUT_13;

   procedure do_TSD_OUT_14 is
   begin
      check_DR_OUT(14, False, "");
      if OUT13_was_done then
         push((sectors_per_system - last_reserved_DR_sector - 1) or 2**47);
      else
         push(sectors_per_system);
      end if;
      -- I assume that the drum never experiences a parity error in ee9.
   end do_TSD_OUT_14;

end IOC.fast.DR.OUTs;
