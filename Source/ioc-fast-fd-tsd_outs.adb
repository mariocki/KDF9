-- Implement the fixed disc API (OUTs) of the EE Time Sharing Director.
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

with IOC.dispatcher;
with KDF9.TSD;

use  IOC.dispatcher;
use  KDF9.TSD;

package body IOC.fast.FD.TSD_OUTs is

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

   procedure free_any_reserved_disc_space is
   begin
      disc_sets_claimed := 0;
      discs_claimed_so_far := 0;
   end free_any_reserved_disc_space;

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
         trap_failing_OUT(OUT_number, Q.C'Image & " exceeds the sector capacity of the selected set");
      end if;
      return (parameter * 16 + FD0_number,  Q.I, Q.M);
   end FD_seek_parameter;

   function FD_xfer_parameter (Q : KDF9.Q_register)
   return KDF9.Q_register is
      sector : constant KDF9.Q_part := Q.C mod 96;
   begin
      return (sector * 16 + FD0_number, Q.I, Q.M);
   end FD_xfer_parameter;

   Q : KDF9.Q_register;
   W : KDF9.word;

   procedure ensure_that_FD0_is_enabled (OUT_number : in KDF9.word) is
   begin
      if not FD0_is_enabled then
         trap_failing_OUT(OUT_number, "there is no disc in this configuration");
      end if;
   end ensure_that_FD0_is_enabled;

   procedure access_the_operand is
   begin
      ensure_that_the_NEST_holds_an_operand;
      W := pop;
      Q := as_Q(W);
      the_trace_operand := W;
   end access_the_operand;

   procedure do_OUT_41 is
      F       : KDF9.Q_part;
      seek_Q,
      write_Q : KDF9.Q_register;
   begin
      access_the_operand;
      ensure_that_FD0_is_enabled(41);
      if disc_sets_claimed = 0 then
         trap_failing_OUT(41, "tries to write to FD0 with no discs reserved");
      end if;
      W := KDF9.word(Q.C) / 16;
      F := Q.C mod 16 + (Q.M - Q.I + words_per_sector - 1)/words_per_sector;
      if F >= sectors_per_logical_block then
         trap_failing_OUT(41, "tries to write past the end of a logical block on FD0");
      end if;
      seek_Q  := FD_seek_parameter(41, Q);
      write_Q := FD_xfer_parameter(Q);
      the_trace_operand := as_word(write_Q);
      restore_the_IO_OUT_operands(42, W);
         PMA(seek_Q, False);
         POA(write_Q, False);
      remove_the_IO_OUT_operands;
   end do_OUT_41;

   procedure do_OUT_42 is
      F      : KDF9.Q_part;
      seek_Q,
      read_Q : KDF9.Q_register;
   begin
      access_the_operand;
      ensure_that_FD0_is_enabled(42);
      if disc_sets_claimed = 0 then
         trap_failing_OUT(42, "tries to read from FD0 with no discs reserved");
      end if;
      W := KDF9.word(Q.C) / 16;
      F := Q.C mod 16 + (Q.M - Q.I + words_per_sector - 1)/words_per_sector;
      if F >= sectors_per_logical_block then
         trap_failing_OUT(42, "tries to read past the end of a logical block on FD0");
      end if;
      seek_Q := FD_seek_parameter(41, Q);
      read_Q := FD_xfer_parameter(Q);
      the_trace_operand := as_word(read_Q);
      restore_the_IO_OUT_operands(42, W);
         PMA(seek_Q, False);
         PIA(read_Q, False);
      remove_the_IO_OUT_operands;
   end do_OUT_42;

   procedure do_OUT_43 is
      F : KDF9.Q_part;
   begin
      access_the_operand;
      ensure_that_FD0_is_enabled(43);
      if disc_sets_claimed = 0 then
         trap_failing_OUT(43, "no FD0 discs have been reserved yet");
      end if;
      if W not in 0 | -1 then
         trap_failing_OUT(43, "tries to select an impossible set of discs");
      end if;
      F := as_Q(-W).M + 1;
      if F > disc_sets_claimed then
         trap_failing_OUT(43, "tries to select an unreserved FD0 disc set");
      end if;
      current_disc_set := as_Q(-the_trace_operand).M + 1;
   end do_OUT_43;

   procedure do_OUT_44 is
      F : KDF9.Q_part;
   begin
      access_the_operand;
      ensure_that_FD0_is_enabled(44);
      if disc_sets_claimed = 2 then
         trap_failing_OUT(44, "tries to reserve more than two sets of FD0 discs");
      end if;
      F := as_Q(W).M;
      if W not in 1..claimable_discs                        or else
            discs_claimed_so_far + F not in 1..claimable_discs then
         trap_failing_OUT(44, "tries to reserve more than 8 FD0 discs");
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
   end do_OUT_44;

   procedure do_OUT_45 is
      F : KDF9.Q_part;
   begin
      access_the_operand;
      ensure_that_FD0_is_enabled(45);
      if disc_sets_claimed = 0 then
         trap_failing_OUT(45, "tries to release an unreserved set of FD0 discs");
      end if;
      if W not in 0 | -1 then
         trap_failing_OUT(45, "tries to release an unknown set of FD0 discs");
      end if;
      F := as_Q(-W).M + 1;
      if F > disc_sets_claimed then
         trap_failing_OUT(45, "tries to release more FD0 disc sets than are reserved");
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
   end do_OUT_45;

   procedure do_OUT_47 is
   begin
      -- I assume that disc transfer parity errors never occur under ee9.
      ensure_that_FD0_is_enabled(47);
      if disc_sets_claimed = 0 then
         trap_failing_OUT(47, "tries to check a transfer on unreserved FD0 discs");
      end if;
   end do_OUT_47;

end IOC.fast.FD.TSD_OUTs;
