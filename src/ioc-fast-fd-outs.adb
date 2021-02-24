-- ioc.fast-fd-outs.adb
--
-- Implement the fixed disc API (OUTs) of the EE Time Sharing Director.
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

package body IOC.fast.FD.OUTs is

   -- See the Manual, Appendix 6, §2 for the TSD FD OUTs.

   claimable_discs              : constant := 8;
   sectors_per_logical_block    : constant := 16;
   logical_blocks_per_seek_area : constant := sectors_per_seek_area / sectors_per_logical_block;
   logical_blocks_per_platter   : constant := seek_areas_per_platter * logical_blocks_per_seek_area;
   sectors_per_platter          : constant := sectors_per_logical_block * logical_blocks_per_platter;

   type reserved_disc_set is
      record
         claimed : Boolean := False;
         size    : KDF9.Q_part range 0 .. claimable_discs;
         sectors : KDF9.Q_part range 0 .. claimable_discs * sectors_per_platter;
         start   : KDF9.Q_part range 0 .. claimable_discs;
      end record;

   disc_set             : array (KDF9.Q_part range 1 .. 2) of reserved_disc_set;
   disc_sets_claimed    : KDF9.Q_part range 0 .. 2 := 0;
   current_disc_set     : KDF9.Q_part range 1 .. 2;
   discs_claimed_so_far : KDF9.Q_part := 0;

   function FD_seek_parameter (OUT_number: KDF9.word; Q : KDF9.Q_register)
   return KDF9.Q_register is
      current_set : reserved_disc_set renames disc_set(current_disc_set);
      block       : constant KDF9.Q_part := Q.C / sectors_per_logical_block;
      seek_area   : constant KDF9.Q_part := block  /  (6 * current_set.size);
      residue     : constant KDF9.Q_part := block mod (6 * current_set.size);
      disc        : constant KDF9.Q_part := residue / 6 + current_set.start;
      drive       : constant KDF9.Q_part := 0;  -- ee9 will reserve discs on drive 0 only.
      parameter   : constant KDF9.Q_part
                  := drive * seek_areas_per_platter * main_discs_per_drive
                   + disc  * seek_areas_per_platter
                   + seek_area;
   begin
      if Q.C >= disc_set(current_disc_set).sectors then
         fail_OUT(OUT_number, Q.C'Image & " exceeds the sector capacity of the selected set");
      end if;
      return (parameter * 16 + FD0_number,  Q.I, Q.M);
   end FD_seek_parameter;

   function FD_xfer_parameter (Q : KDF9.Q_register)
   return KDF9.Q_register is
      sector : constant KDF9.Q_part := Q.C mod 96;
   begin
      return (sector * 16 + FD0_number, Q.I, Q.M);
   end FD_xfer_parameter;

   F : KDF9.Q_part;
   Q : KDF9.Q_register;
   W : KDF9.word;

   procedure check_FD_OUT (
                           OUT_number       : in KDF9.word;
                           operand_message  : in String;
                           it_is_erroneous  : in Boolean;
                           second_message   : in String;
                           second_test_only : in Boolean := False
                          ) is
   begin
      if not is_enabled then
         fail_OUT(OUT_number, "FD0 is not included in this configuration");
      end if;
      ensure_that_the_nest_holds_an_operand;
      the_trace_operand := pop;
      if the_trace_operand not in 0 | -1 and not second_test_only then
         fail_OUT(OUT_number, operand_message);
      end if;
      if it_is_erroneous then
         fail_OUT(OUT_number, second_message);
      end if;
      W := the_trace_operand;
      Q := as_Q(W);
   end check_FD_OUT;

   procedure do_TSD_OUT_41 is
   begin
      check_FD_OUT(
                   41,
                   "",
                   disc_sets_claimed = 0,
                   "tries to write to unreserved FD0 discs",
                   second_test_only => True
                  );
      W := KDF9.word(Q.C) / 16;
      F := Q.C mod 16 + (Q.M - Q.I + words_per_sector - 1)/words_per_sector;
      if F >= sectors_per_logical_block then
         fail_OUT(41, "tries to write past the end of a logical block on FD0");
      end if;
      PMA(FD_seek_parameter(41, Q), False);
      POA(FD_xfer_parameter(Q), False);
   end do_TSD_OUT_41;

   procedure do_TSD_OUT_42 is
   begin
      check_FD_OUT(
                   42,
                   "",
                   disc_sets_claimed = 0,
                   "tries to read from unreserved FD0 discs",
                   second_test_only => True
                  );
      W := KDF9.word(Q.C) / 16;
      F := Q.C mod 16 + (Q.M - Q.I + words_per_sector - 1)/words_per_sector;
      if F >= sectors_per_logical_block then
         fail_OUT(42, "tries to read past the end of a logical block on FD0");
      end if;
      PMA(FD_seek_parameter(42, Q), False);
      PIA(FD_xfer_parameter(Q), False);
   end do_TSD_OUT_42;

   procedure do_TSD_OUT_43 is
   begin
      check_FD_OUT(
                   43,
                   "tries to select an impossible set of discs",
                   disc_sets_claimed = 0,
                   "no FD0 discs have been reserved yet"
                  );
      W := -the_trace_operand;
      F := as_Q(W).M + 1;
      if F > disc_sets_claimed then
         fail_OUT(45, "tries to select an unreserved FD0 disc set");
      end if;
      current_disc_set := as_Q(-the_trace_operand).M + 1;
   end do_TSD_OUT_43;

   procedure do_TSD_OUT_44 is
   begin
      check_FD_OUT(
                   44,
                   "",
                   disc_sets_claimed = 2,
                   "tries to reserve more than two sets of FD0 discs",
                   second_test_only => True
                  );
      F := as_Q(W).M;
      if W not in 1..claimable_discs                           or else
            discs_claimed_so_far + F not in 1..claimable_discs then
         fail_OUT(44, "tries to reserve more than 8 FD0 discs");
      end if;
      current_disc_set := disc_sets_claimed + 1;
      disc_set(current_disc_set).claimed := True;
      disc_set(current_disc_set).size := F;
      disc_set(current_disc_set).sectors := F * sectors_per_platter;
      disc_set(current_disc_set).start := discs_claimed_so_far;
      discs_claimed_so_far := discs_claimed_so_far + F;
      disc_sets_claimed := disc_sets_claimed + 1;
      log_API_message(
                      "OUT 44: reserved"
                    & F'Image
                    & " FD0 disc"
                    & plurality(KDF9.word(F))
                    & " in set"
                    & current_disc_set'Image
                     );
      set_state_of(buffer(FD0_number), allocated => True);
   end do_TSD_OUT_44;

   procedure do_TSD_OUT_45 is
   begin
      check_FD_OUT(
                   45,
                   "tries to release an unknown set of FD0 discs",
                   disc_sets_claimed = 0,
                   "tries to release an unreserved set of FD0 discs"
                  );
      W := -W;
      F := as_Q(W).M + 1;
      if F > disc_sets_claimed then
         fail_OUT(45, "tries to release more FD0 disc sets than are reserved");
      end if;
      log_API_message(
                      "OUT 45: released"
                    & disc_set(F).size'Image
                    & " FD0 disc"
                    & plurality(KDF9.word(disc_set(F).size))
                    & " from set"
                    & F'Image
                     );
      discs_claimed_so_far := discs_claimed_so_far - disc_set(F).size;
      current_disc_set := 1;
      disc_sets_claimed := disc_sets_claimed - 1;
      if F = 1 then
         disc_set(1) := disc_set(2);
         disc_set(2).claimed := False;
      end if;
   end do_TSD_OUT_45;

end IOC.fast.FD.OUTs;
