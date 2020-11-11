-- kdf9-microcode.adb
--
-- KDF9 ISP emulation - CPU microcode routines.
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

with exceptions;
with IOC;
with IOC.assignment;
with KDF9.compressed_opcodes;
with KDF9.CPU;
with KDF9.Directors;
with KDF9.store;
with POSIX;
with settings;
with state_display;
with tracing;

use  exceptions;
use  IOC;
use  IOC.assignment;
use  KDF9.compressed_opcodes;
use  KDF9.CPU;
use  KDF9.Directors;
use  KDF9.store;
use  settings;
use  state_display;
use  tracing;

package body KDF9.microcode is

   procedure do_a_one_syllable_order is
      A, B, C, E : KDF9.word;
      AB, CD     : KDF9.pair;
      X, Y       : CPU.float;
      XY, ZT     : CPU.double;
   begin
      case INS.syndrome is

         when VR =>
            the_V_bit := 0;
            the_CPU_delta := the_CPU_delta + 1;

         when TO_TR =>
            ensure_that_the_nest_holds_an_operand;
            if resign(pop) < 0 then
               the_T_bit := 1;
            end if;
            the_CPU_delta := the_CPU_delta + 2;

         when BITS =>
            check_whether_the_nest_holds_an_operand;
            write_top(cardinality(read_top));
            the_CPU_delta := the_CPU_delta + 27;

         when XF =>
            ensure_that_the_nest_holds_2_operands;
            X := pop;
            Y := read_top;
            write_top(CPU.float'(Y * X));
            the_CPU_delta := the_CPU_delta + 15;

         when XDF =>
            ensure_that_the_nest_holds_2_operands;
            XY := read_top;
            ZT := XY.lsw * XY.msw;
            write_top(ZT);
            the_CPU_delta := the_CPU_delta + 16;

         when XPLUSF =>
            ensure_that_the_nest_holds(at_least => 4);
            XY := pop;
            ZT := XY.lsw * XY.msw;
            XY := read_top;
            write_top(XY + ZT);
            the_CPU_delta := the_CPU_delta + 18;

         when NEGD =>
            check_whether_the_nest_holds_2_operands;
            AB := read_top;
            write_top( - AB);
            the_CPU_delta := the_CPU_delta + 2;

         when OR_9 =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            write_top(read_top or A);
            the_CPU_delta := the_CPU_delta + 1;

         when PERM =>
            check_whether_the_nest_holds(at_least => 3);
            A := pop;
            CD := pop;
            push(A);
            push(CD);
            the_CPU_delta := the_CPU_delta + 2;

         when TOB =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            B := read_top;
            C := 0;
            for i in 1 .. 8 loop
               A := rotate_word_left(A, 6);
               B := rotate_word_left(B, 6);
               C := C*(B and 8#77#) + (A and 8#77#);
            end loop;
            write_top(C);
            the_CPU_delta := the_CPU_delta + 27;

         when ROUNDH =>
            check_whether_the_nest_holds_an_operand;
            A := read_top;
            write_top(resign(A) + 2**23);
            the_CPU_delta := the_CPU_delta + 22;

         when NEV =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            write_top(read_top xor A);
            the_CPU_delta := the_CPU_delta + 2;

         when ROUND =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            write_top(resign(A) + resign(shift_word_right(read_top, 46) and 1));
            the_CPU_delta := the_CPU_delta + 1;

         when DUMMY =>
            the_CPU_delta := the_CPU_delta + 1;

         when ROUNDF =>
            ensure_that_the_nest_holds_2_operands;
            XY := pop;
            ZT := XY;
            push(rounded(XY));
            the_CPU_delta := the_CPU_delta + 3;

         when ROUNDHF =>
            check_whether_the_nest_holds_an_operand;
            X := pop;
            push(rounded(X));
            the_CPU_delta := the_CPU_delta + 3;

         when MINUSDF =>
            ensure_that_the_nest_holds(at_least => 4);
            XY := pop;
            ZT := read_top;
            write_top(ZT - XY);
            the_CPU_delta := the_CPU_delta + 12;

         when PLUSDF =>
            ensure_that_the_nest_holds(at_least => 4);
            XY := pop;
            ZT := read_top;
            write_top(ZT + XY);
            the_CPU_delta := the_CPU_delta + 12;

         when FLOAT_9 =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            B := read_top;
            write_top(KDF9.word(normalized(full_fraction => B, scaler => A)));
            the_CPU_delta := the_CPU_delta + 7;

         when FLOATD =>
            ensure_that_the_nest_holds(at_least => 3);
            A := pop;
            CD := read_top;
            -- See §3.4 of Report K/GD.y.83, dated 6/12/1962.
            CD.lsw := CD.lsw and not 8#77#;  -- The 6 l.s.b. are lost.
            reconstruct(CD, scaler => A);
            write_top(CD);
            the_CPU_delta := the_CPU_delta + 8;

         when ABS_9 =>
            check_whether_the_nest_holds_an_operand;
            write_top( abs resign(read_top));
            the_CPU_delta := the_CPU_delta + 1;

         when NEG =>
            check_whether_the_nest_holds_an_operand;
            write_top( - resign(read_top));
            the_CPU_delta := the_CPU_delta + 1;

         when ABSF =>
            check_whether_the_nest_holds_an_operand;
            X := read_top;
            if resign(KDF9.word(X)) < 0 then
               write_top( - X);
               the_CPU_delta := the_CPU_delta + 4;
            else
               the_CPU_delta := the_CPU_delta + 1;
            end if;

         when NEGF =>
            check_whether_the_nest_holds_an_operand;
            X := read_top;
            write_top( - X);
            the_CPU_delta := the_CPU_delta + 3;

         when MAX =>
            check_whether_the_nest_holds_2_operands;
            AB := read_top;
            if resign(AB.lsw) >= resign(AB.msw) then
               write_top(KDF9.pair'(msw => AB.lsw, lsw =>AB.msw));
               the_V_bit := 1;
            end if;
            the_CPU_delta := the_CPU_delta + 4;

         when NOT_9 =>
            check_whether_the_nest_holds_an_operand;
            A := read_top;
            write_top(not A);
            the_CPU_delta := the_CPU_delta + 1;

         when XD =>
            AB := read_top;
            CD := AB.msw * AB.lsw;
            write_top(CD);
            the_CPU_delta := the_CPU_delta + 14;

         when X_frac =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            write_top(integral(CPU.fraction'(read_top * A)));
            the_CPU_delta := the_CPU_delta + 15;

         when MINUS =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            B := read_top;
            write_top(resign(B) - resign(A));
            the_CPU_delta := the_CPU_delta + 1;

         when SIGN =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            B := read_top;
            if B = A then
               write_top(KDF9.word'(0));
            elsif resign(B) > resign(A) then
               write_top(KDF9.word'(1));
            else
               write_top(all_one_bits);
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when ZERO =>
            ensure_that_the_nest_has_room_for_a_result;
            push(all_zero_bits);
            the_CPU_delta := the_CPU_delta + 2;

         when DUP =>
            check_whether_the_nest_holds_an_operand;
            ensure_that_the_nest_has_room_for_a_result;
            A := read_top;
            push(A);
            the_CPU_delta := the_CPU_delta + 2;

         when DUPD =>
            check_whether_the_nest_holds_2_operands;
            ensure_that_the_nest_has_room_for_2_results;
            AB := read_top;
            push(AB);
            the_CPU_delta := the_CPU_delta + 4;

         when DIVI =>
            check_whether_the_nest_holds_2_operands;
            AB := read_top;
            do_DIVI(L => AB.lsw,
                    R => AB.msw,
                    Quotient  => CD.lsw,
                    Remainder => CD.msw);
            write_top(CD);
            the_CPU_delta := the_CPU_delta + 36;

         when FIX =>
            ensure_that_the_nest_holds_an_operand;
            ensure_that_the_nest_has_room_for_a_result;
            X := read_top;
            write_top(fraction_word(X));
            push(scaler(X));
            the_CPU_delta := the_CPU_delta + 6;

         when STR =>
            check_whether_the_nest_holds_an_operand;
            ensure_that_the_nest_has_room_for_a_result;
            A := read_top;
            if resign(A) < 0 then
               write_top(A and not_sign_bit);
               push(all_one_bits);
            else
               push(all_zero_bits);
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when CONT =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            B := read_top;
            write_top(contracted(msw => A, lsw => B));
            the_CPU_delta := the_CPU_delta + 2;

         when REVD =>
            check_whether_the_nest_holds(at_least => 4);
            AB := pop;
            CD := pop;
            push(AB);
            push(CD);
            the_CPU_delta := the_CPU_delta + 4;

         when ERASE =>
            ensure_that_the_nest_holds_an_operand;
            pop;
            the_CPU_delta := the_CPU_delta + 1;

         when MINUSD =>
            ensure_that_the_nest_holds(at_least => 4);
            AB := pop;
            CD := read_top;
            write_top(CD - AB);
            the_CPU_delta := the_CPU_delta + 3;

         when AND_9 =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            write_top(read_top and A);
            the_CPU_delta := the_CPU_delta + 1;

         when PLUS =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            B := read_top;
            write_top(resign(B) + resign(A));
            the_CPU_delta := the_CPU_delta + 1;

         when PLUSD =>
            ensure_that_the_nest_holds(at_least => 4);
            AB := pop;
            CD := read_top;
            write_top(CD + AB);
            the_CPU_delta := the_CPU_delta + 3;

         when DIV =>
            ensure_that_the_nest_holds_2_operands;
            AB := pop;
            push(integral(CPU.fraction'(AB.lsw / AB.msw)));
            the_CPU_delta := the_CPU_delta + 36;

         when DIVD =>
            ensure_that_the_nest_holds(at_least => 3);
            A := pop;
            CD := pop;
            do_DIVD(L => CD,
                    R => A,
                    Q => E);
            push(E);
            the_CPU_delta := the_CPU_delta + 36;

         when DIVF =>
            check_whether_the_nest_holds_2_operands;
            X := pop;
            Y := read_top;
            write_top(Y / X);
            the_CPU_delta := the_CPU_delta + 36;

         when DIVDF =>
            ensure_that_the_nest_holds(at_least => 3);
            Y := pop;
            XY := pop;
            push(XY / Y);
            the_CPU_delta := the_CPU_delta + 35;

         when DIVR =>
            ensure_that_the_nest_holds(at_least => 3);
            A := pop;
            CD := read_top;
            do_DIVR(L => CD,
                    R => A,
                    Quotient  => AB.msw,
                    Remainder => AB.lsw);
            write_top(AB);
            the_CPU_delta := the_CPU_delta + 36;

         when REV =>
            check_whether_the_nest_holds_2_operands;
            AB := read_top;
            write_top(KDF9.pair'(msw => AB.lsw, lsw =>AB.msw));
            the_CPU_delta := the_CPU_delta + 1;

         when CAB =>
            check_whether_the_nest_holds(at_least => 3);
            AB := pop;
            C := pop;
            push(AB);
            push(C);
            the_CPU_delta := the_CPU_delta + 2;

         when FRB =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            B := read_top;
            C := 0;
            for i in 1 .. 8 loop
               C := C or (A mod (B and 8#77#));
               A := KDF9.word'(A / (B and 8#77#));
               B := shift_word_right(B, 6);
               C := rotate_word_right(C, 6);
            end loop;
            write_top(C);
            the_CPU_delta := the_CPU_delta + 30;

         when STAND =>
            check_whether_the_nest_holds_an_operand;
            X := read_top;
            write_top(normalized(X));
            the_CPU_delta := the_CPU_delta + 5;

         when NEGDF =>
            check_whether_the_nest_holds_2_operands;
            XY := read_top;
            write_top( - XY);
            the_CPU_delta := the_CPU_delta + 9;

         when MAXF =>
            check_whether_the_nest_holds_2_operands;
            XY := read_top;
            if XY.lsw >= XY.msw then
               write_top(CPU.double'(msw => XY.lsw, lsw =>XY.msw));
               the_V_bit := 1;
            end if;
            the_CPU_delta := the_CPU_delta + 6;

         when PLUSF =>
            ensure_that_the_nest_holds_2_operands;
            X := pop;
            Y := read_top;
            write_top(Y + X);
            the_CPU_delta := the_CPU_delta + 7;

         when MINUSF =>
            ensure_that_the_nest_holds_2_operands;
            X := pop;
            Y := read_top;
            write_top(Y - X);
            the_CPU_delta := the_CPU_delta + 7;

         when SIGNF =>
            ensure_that_the_nest_holds_2_operands;
            XY := pop;
            if KDF9.word(XY.lsw) = KDF9.word(XY.msw) then
               push(all_zero_bits);
            elsif XY.lsw < XY.msw then
               push(all_one_bits);
            else
               push(one_in_ls_bit);
            end if;
            the_CPU_delta := the_CPU_delta + 5;

         when others =>
            trap_invalid_instruction;

      end case;
   end do_a_one_syllable_order;

   procedure do_an_IO_order is
      set_offline : constant Boolean         := (INS.Qk and manual_bit) /= 0;
      IO_opcode   : constant KDF9.syndrome   := (INS.Qk and not manual_bit);
      IO_operand  : constant KDF9.Q_register := the_Q_store(INS.Qq);
   begin
      case INS.syndrome is

         when PARQq =>
            the_CPU_delta := the_CPU_delta + 11;
            PAR(IO_operand, set_offline, the_T_bit);
            the_CPU_delta := the_CPU_delta + 3;

         when PIAQq_PICQq_CLOQq_TLOQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIAQq_bits =>
                  PIA(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PICQq_bits =>
                  PIC(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when CLOQq_bits =>
                  LIV_if_user_mode;
                  CLO(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 1;
               when TLOQq_bits =>
                  TLO(IO_operand, the_T_bit);
               when others =>
                  trap_invalid_instruction;
            end case;

         when PIBQq_PIDQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIBQq_bits =>
                  PIB(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PIDQq_bits =>
                  PID(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_invalid_instruction;
            end case;

         when PIEQq_PIGQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIEQq_bits =>
                  PIE(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PIGQq_bits =>
                  PIG(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_invalid_instruction;
            end case;

         when PIFQq_PIHQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIFQq_bits =>
                  PIF(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PIHQq_bits =>
                  PIH(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_invalid_instruction;
            end case;

         when PMAQq_PMKQq_INTQq =>
            the_CPU_delta := the_CPU_delta + 11;
            case IO_opcode is
               when PMAQq_bits =>
                  PMA(IO_operand, set_offline);
               when PMKQq_bits =>
                  PMK(IO_operand, set_offline);
               when INTQq_bits =>
                  INT(IO_operand, set_offline);
               when others =>
                  trap_invalid_instruction;
            end case;

         when CTQq_PMBQq_PMCQq_BUSYQq =>
            the_CPU_delta := the_CPU_delta + 11;
            case IO_opcode is
               when CTQq_bits =>
                  LIV_if_user_mode;
                  CTQ(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 2;
               when PMBQq_bits =>
                  PMB(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 3;
               when PMCQq_bits =>
                  PMC(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 3;
               when BUSYQq_bits =>
                  BUSY(IO_operand, set_offline, the_T_bit);
                  the_CPU_delta := the_CPU_delta + 2;
               when others =>
                  trap_invalid_instruction;
            end case;

         when PMDQq_PMEQq_PMLQq =>
            the_CPU_delta := the_CPU_delta + 14;
            case IO_opcode is
               when PMDQq_bits =>
                  PMD(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 5;
               when PMEQq_bits =>
                  PME(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 5;
               when PMLQq_bits =>
                  PML(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 5;
               when others =>
                  trap_invalid_instruction;
            end case;

         when PMFQq =>
            the_CPU_delta := the_CPU_delta + 11;
            PMF(IO_operand, set_offline);
            the_CPU_delta := the_CPU_delta + 3;

         when PMGQq =>
            LIV_if_user_mode;
            the_CPU_delta := the_CPU_delta + 14;  -- ??
            PMG(IO_operand, set_offline);

         when PMHQq =>
            LIV_if_user_mode;
            the_CPU_delta := the_CPU_delta + 16;  -- ??
            PMH(IO_operand, set_offline);

         when POAQq_POCQq_POEQq_POFQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POAQq_bits =>
                  POA(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POCQq_bits =>
                  POC(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POEQq_bits =>
                  POE(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 4;
               when POFQq_bits =>
                  POF(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 4;
               when others =>
                  trap_invalid_instruction;
            end case;

         when POBQq_PODQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POBQq_bits =>
                  POB(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PODQq_bits =>
                  POD(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_invalid_instruction;
            end case;

         when POGQq_POLQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POGQq_bits =>
                  POG(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POLQq_bits =>
                  POL(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_invalid_instruction;
            end case;

         when POHQq_POKQq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POHQq_bits =>
                  POH(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POKQq_bits =>
                  POK(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_invalid_instruction;
            end case;

         when others =>
            trap_invalid_instruction;
      end case;
   end do_an_IO_order;

   all_zero_Q_store : constant KDF9.Q_register := (C | I | M => 0);

   procedure ensure_that_Q0_contains_zero (suspect : KDF9.Q_number);
   pragma Inline(ensure_that_Q0_contains_zero);

   procedure ensure_that_Q0_contains_zero (suspect : KDF9.Q_number) is
   begin
      if suspect /= 0 then
         return;  -- There cannot be a problem.
      end if;
      -- Q0 was updated, so deal with the possibility of a non-zero result.
      if the_Q_store(0) = all_zero_Q_store then
         return;  -- All is well.
      end if;
      if the_authenticity_mode = lax_mode or the_CPU_state = Director_state then
         the_Q_store(0) := all_zero_Q_store;  -- Suppress the assignment to Q0.
      else
         trap_invalid_instruction;
      end if;
   end ensure_that_Q0_contains_zero;

   procedure auto_increment;
   pragma Inline(auto_increment);

   procedure auto_increment is
   begin
      the_Q_store(INS.Qq).M := the_Q_store(INS.Qq).M + the_Q_store(INS.Qq).I;
      the_Q_store(INS.Qq).C := the_Q_store(INS.Qq).C - 1;
      ensure_that_Q0_contains_zero(suspect => INS.Qq);
   end auto_increment;

   function shift_count return CPU.signed_Q_part;
   pragma Inline(shift_count);

   function shift_count return CPU.signed_Q_part is
      constant_flag : constant := 1;  -- D15 of order = 1 => fixed amount
   begin
      if (INS.order.syllable_1 and constant_flag) /= 0  then
         return resign(KDF9.Q_part(INS.order.syllable_1/2 xor 64)) - 64;
      else
         return resign((the_Q_store(INS.Qq).C and 255) xor 128) - 128;
      end if;
   end shift_count;

   procedure do_a_two_syllable_order is
      A  : KDF9.word;
      AB : KDF9.pair;
      CD : KDF9.pair;
   begin
      case INS.syndrome is

         when JCqNZS =>
            if the_Q_store(INS.Qq).C /= 0 then
               if fetching_normally then
                  set_IWB0_and_IWB1_for_a_JCqNZS_loop;
                  the_CPU_delta := the_CPU_delta + 7;  -- Takes 11µs the first time it jumps.
               end if;
               -- The IWBs now contain the loop, so go to syllable 0 of IWB0.
               go_back_to_the_start_of_IWB0;
            else
               continue_after_JCqNZS;
            end if;
            the_CPU_delta := the_CPU_delta + 4;

         when MkMq =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQ =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when MkMqH =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M/2);
            validate_access(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_nest_has_room_for_a_result;
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQH =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M/2);
            validate_access(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_nest_has_room_for_a_result;
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when MkMqN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when MkMqHN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M/2);
            validate_access(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_nest_has_room_for_a_result;
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQHN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M/2);
            validate_access(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_nest_has_room_for_a_result;
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMq =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQ =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMqH =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M/2);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQH =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M/2);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMqN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMqHN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQHN =>
            the_trace_address := validated_sum(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when M_PLUS_Iq =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qq).M := the_Q_store(INS.Qq).M + the_Q_store(INS.Qq).I;

         when M_MINUS_Iq =>
            the_CPU_delta := the_CPU_delta + 5;
            the_Q_store(INS.Qq).M := the_Q_store(INS.Qq).M - the_Q_store(INS.Qq).I;

         when NCq =>
            the_CPU_delta := the_CPU_delta + 5;
            the_Q_store(INS.Qq).C := - the_Q_store(INS.Qq).C;

         when DCq =>
            the_CPU_delta := the_CPU_delta + 3;
            the_Q_store(INS.Qq).C := the_Q_store(INS.Qq).C - 1;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when POS1_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            the_Q_store(INS.Qq).I := + 1;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when NEG1_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            the_Q_store(INS.Qq).I := - 1;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when POS2_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            the_Q_store(INS.Qq).I := + 2;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when NEG2_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            the_Q_store(INS.Qq).I := - 2;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when CqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
            ensure_that_Q0_contains_zero(suspect => INS.Qk);

         when IqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
            ensure_that_Q0_contains_zero(suspect => INS.Qk);

         when MqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
            ensure_that_Q0_contains_zero(suspect => INS.Qk);

         when QqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qk) := the_Q_store(INS.Qq);
            ensure_that_Q0_contains_zero(suspect => INS.Qk);

         when CIqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
            the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
            ensure_that_Q0_contains_zero(suspect => INS.Qk);

         when IMqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
            the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
            ensure_that_Q0_contains_zero(suspect => INS.Qk);

         when CMqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
            the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
            ensure_that_Q0_contains_zero(suspect => INS.Qk);

         when QCIMq =>
            ensure_that_the_nest_has_room_for_a_result;
            if (INS.Qk and all_Q_choice) = all_Q_choice then -- Qq
               push(as_word(the_Q_store(INS.Qq)));
               the_CPU_delta := the_CPU_delta + 4;
            elsif (INS.Qk and M_part_choice) /= 0 then       -- Mq
               push(sign_extended(the_Q_store(INS.Qq).M));
               the_CPU_delta := the_CPU_delta + 4;
            elsif (INS.Qk and C_part_choice) /= 0 then       -- Cq
               push(sign_extended(the_Q_store(INS.Qq).C));
               the_CPU_delta := the_CPU_delta + 5;
            elsif (INS.Qk and I_part_choice) /= 0 then       -- Iq
               push(sign_extended(the_Q_store(INS.Qq).I));
               the_CPU_delta := the_CPU_delta + 6;
            else
               trap_invalid_instruction;
            end if;

         when TO_RCIMq =>
            ensure_that_the_nest_holds_an_operand;
            if (INS.Qk and all_Q_choice) = all_Q_choice then -- =Qq
               the_Q_store(INS.Qq) := as_Q(pop);
               the_CPU_delta := the_CPU_delta + 2;
            elsif (INS.Qk and M_part_choice) /= 0 then       -- =[R]Mq
               the_Q_store(INS.Qq).M := KDF9.Q_part(pop and Q_part_mask);
               if (INS.Qk and reset_choice) /= 0 then
                  the_Q_store(INS.Qq).C := 0;
                  the_Q_store(INS.Qq).I := 1;
                  the_CPU_delta := the_CPU_delta + 3;
               else
                  the_CPU_delta := the_CPU_delta + 2;
               end if;
            elsif (INS.Qk and C_part_choice) /= 0 then       -- =[R]Cq
               the_Q_store(INS.Qq).C := KDF9.Q_part(pop and Q_part_mask);
               if (INS.Qk and reset_choice) /= 0 then
                  the_Q_store(INS.Qq).I := 1;
                  the_Q_store(INS.Qq).M := 0;
                  the_CPU_delta := the_CPU_delta + 3;
               else
                  the_CPU_delta := the_CPU_delta + 2;
               end if;
            elsif (INS.Qk and I_part_choice) /= 0 then       -- =[R]Iq
               the_Q_store(INS.Qq).I := KDF9.Q_part(pop and Q_part_mask);
               if (INS.Qk and reset_choice) /= 0 then
                  the_Q_store(INS.Qq).C := 0;
                  the_Q_store(INS.Qq).M := 0;
                  the_CPU_delta := the_CPU_delta + 3;
               else
                  the_CPU_delta := the_CPU_delta + 2;
               end if;
            else
               trap_invalid_instruction;
            end if;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when ADD_TO_QCIMq =>
            ensure_that_the_nest_has_room_for_a_result;
            ensure_that_the_nest_holds_an_operand;
            if (INS.Qk and all_Q_choice) = all_Q_choice then -- =+Qq
               the_Q_store(INS.Qq) := as_Q(as_word(the_Q_store(INS.Qq)) + pop);
               the_CPU_delta := the_CPU_delta + 5;
            elsif (INS.Qk and M_part_choice) /= 0 then       -- =+Mq
               the_Q_store(INS.Qq).M := KDF9.Q_part(Q_part_mask and
                                             (sign_extended(the_Q_store(INS.Qq).M) + pop));
               the_CPU_delta := the_CPU_delta + 5;
            elsif (INS.Qk and C_part_choice) /= 0 then       -- =+Cq
               the_Q_store(INS.Qq).C := KDF9.Q_part(Q_part_mask and
                                             (sign_extended(the_Q_store(INS.Qq).C) + pop));
               the_CPU_delta := the_CPU_delta + 6;
            elsif (INS.Qk and I_part_choice) /= 0 then       -- =+Iq
               the_Q_store(INS.Qq).I := KDF9.Q_part(Q_part_mask and
                                             (sign_extended(the_Q_store(INS.Qq).I) + pop));
               the_CPU_delta := the_CPU_delta + 7;
            else
               trap_invalid_instruction;
            end if;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when SHA =>
            check_whether_the_nest_holds_an_operand;
            A := read_top;
            write_top(KDF9.word'(shift_arithmetic(A, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when SHAD =>
            check_whether_the_nest_holds_2_operands;
            AB := read_top;
            write_top(KDF9.pair'(shift_arithmetic(AB, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when MACC =>
            ensure_that_the_nest_holds(at_least => 4);
            AB := pop;
            AB := AB.msw * AB.lsw;
            CD := read_top;
            write_top(CD + shift_arithmetic(AB, shift_count));
            the_CPU_delta := the_CPU_delta + 15 + shift_time(Natural(abs shift_count));

         when SHL =>
            check_whether_the_nest_holds_an_operand;
            write_top(KDF9.word'(shift_logical(read_top, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when SHLD =>
            check_whether_the_nest_holds_2_operands;
            write_top(KDF9.pair'(shift_logical(read_top, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when SHC =>
            check_whether_the_nest_holds_an_operand;
            write_top(shift_circular(read_top, shift_count));
            the_CPU_delta := the_CPU_delta + 3 + shift_time(Natural(abs shift_count));

         when TO_Kk =>
            LIV_if_user_mode;
            ensure_that_the_nest_holds_an_operand;
            case INS.Qq is
               when K0 =>
                  if read_top /= all_zero_bits then
                     for w in all_zero_bits .. read_top mod 8 loop
                        POSIX.output_line("HOOT!");
                     end loop;
                     delay 10.0;
                  end if;
               when K1 =>
                  set_K1_register(read_top);
               when K2 =>
                  set_K2_register(read_top);
               when K3 =>
                  set_K3_register(read_top);
               when others =>
                  trap_invalid_instruction;
            end case;
            the_CPU_delta := the_CPU_delta + 3;

         when Kk =>
            LIV_if_user_mode;
            ensure_that_the_nest_has_room_for_a_result;
            case INS.Qk is
               when K4 =>
                  push(get_K4_operand);
                  the_RFIR := (others => False);
               when K5 =>
                  push(get_K5_operand);
               when K7 =>
                  push(get_K7_operand);
               when others =>
                  trap_invalid_instruction;
            end case;
            the_CPU_delta := the_CPU_delta + 3;

         when LINK =>
            if the_CPU_state = Director_state and the_sjns_depth = 0 then -- clear out JB
               push(all_zero_bits);
               the_sjns_depth := 0 - 1;
            else
               ensure_that_the_nest_has_room_for_a_result;
               ensure_that_the_sjns_is_not_empty;
               push(as_word(KDF9.code_link(KDF9.code_point'(pop))));
            end if;
            the_CPU_delta := the_CPU_delta + 4;

         when TO_LINK =>
            ensure_that_the_sjns_is_not_full;
            ensure_that_the_nest_holds_an_operand;
            push(KDF9.code_point(as_link(pop)));
            the_CPU_delta := the_CPU_delta + 3;

         when others =>
            do_an_IO_order;
      end case;
   end do_a_two_syllable_order;

   procedure do_a_jump_order is
      RA        : KDF9.code_point;
      A         : KDF9.word;
   begin
      case INS.syndrome is

         when Jr =>
            set_NIA_to_the_INS_target_address;
            fetching_normally := True;
            the_CPU_delta := the_CPU_delta + 8;

         when JSr =>
            ensure_that_the_sjns_is_not_full;
            push(CIA);
            set_NIA_to_the_INS_target_address;
            fetching_normally := True;
            the_CPU_delta := the_CPU_delta + 11;

         when JrEQ =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            if A = read_top then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 12;
            else
               the_CPU_delta := the_CPU_delta + 5;
            end if;

         when JrNE =>
            ensure_that_the_nest_holds_2_operands;
            A := pop;
            if A /= read_top then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 12;
            else
               the_CPU_delta := the_CPU_delta + 5;
            end if;

         when JrGTZ =>
            ensure_that_the_nest_holds_an_operand;
            if resign(pop) > 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrLTZ =>
            ensure_that_the_nest_holds_an_operand;
            A := pop;
            if resign(A) < 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrEQZ =>
            ensure_that_the_nest_holds_an_operand;
             if pop = all_zero_bits then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrLEZ =>
            ensure_that_the_nest_holds_an_operand;
            if resign(pop) <= 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrGEZ =>
            ensure_that_the_nest_holds_an_operand;
            if resign(pop) >= 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrNEZ =>
            ensure_that_the_nest_holds_an_operand;
            if pop /= all_zero_bits then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrV =>
            the_trace_operand := the_V_bit;
            if the_V_bit /= 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;

         when JrNV =>
            the_trace_operand := the_V_bit;
            if the_V_bit = 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;
            the_V_bit := 0;

         when JrEN =>
            the_trace_operand := KDF9.word(the_nest_depth);
            if the_nest_depth = 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;

         when JrNEN =>
            the_trace_operand := KDF9.word(the_nest_depth);
            if the_nest_depth /= 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;

         when JrEJ =>
            the_trace_operand := KDF9.word(the_sjns_depth);
            if the_sjns_depth = 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when JrNEJ =>
            the_trace_operand := KDF9.word(the_sjns_depth);
            if the_sjns_depth /= 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when JrTR =>
            the_trace_operand := the_T_bit;
            if the_T_bit /= 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;
            the_T_bit := 0;

         when JrNTR =>
            the_trace_operand := the_T_bit;
            if the_T_bit = 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;
            the_T_bit := 0;

         when EXIT_9 =>
            ensure_that_the_sjns_is_not_empty;
            RA := pop;
            if INS.target.syllable_number = 3 then  -- c.f. decode_a_jump_order.
               increment_by_3(RA);
            end if;
            RA.word_number := RA.word_number+INS.target.word_number;
            set_NIA_to(RA);
            fetching_normally := True;
            the_CPU_delta := the_CPU_delta + 12 + KDF9.microseconds(INS.target.syllable_number mod 2);

         when EXITD =>  -- STUB
            LIV_if_user_mode;
            ensure_that_the_sjns_is_not_empty;
            RA := pop;
            change_to_user_state_at(RA);
            fetching_normally := True;
            the_CPU_delta := the_CPU_delta + 11;
            raise program_exit;  -- STUB for now

         when JrCqZ =>
            if the_Q_store(INS.Qq).C = 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrCqNZ =>
            if the_Q_store(INS.Qq).C /= 0 then
               set_NIA_to_the_INS_target_address;
               fetching_normally := True;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when OUT_9 =>
            fetching_normally := True;
            the_CPU_delta := the_CPU_delta + 13;
            if the_execution_mode = boot_mode then
               -- Emulate the hardware behaviour.
               signal_interrupt(OUT_flag);
               return;  -- we get here only in Director state, so the OUT does not interrupt.
            end if;
            if the_nest_depth = 0 then
               push(all_zero_bits);
            end if;
            A := read_top;
            -- Emulate a subset of the appropriate Director's API.
            if A <= 47 then
               do_a_TSD_OUT(OUT_number => A);
            elsif A > 99 then
               do_an_EGDON_OUT(OUT_number => A);
            else
               -- Other Directors are not handled yet.
               trap_invalid_instruction("invalid OUT number");
            end if;

         when others =>
            trap_invalid_instruction;

      end case;
   end do_a_jump_order;

   procedure do_a_data_access_order is
   begin
      case (INS.syndrome) is
         when EaMq =>
            the_trace_address := validated_sum(the_Q_store(INS.Qq).M, INS.operand);
            validate_access(the_trace_address);
            ensure_that_the_nest_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 6;

         when TO_EaMq =>
            the_trace_address := validated_sum(the_Q_store(INS.Qq).M, INS.operand);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            the_CPU_delta := the_CPU_delta + 6;

         when EaMqQ =>
            the_trace_address := validated_sum(the_Q_store(INS.Qq).M, INS.operand);
            validate_access(the_trace_address);
            ensure_that_the_nest_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 7;

         when TO_EaMqQ =>
            the_trace_address := validated_sum(the_Q_store(INS.Qq).M, INS.operand);
            validate_access(the_trace_address);
            ensure_that_the_nest_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 7;

         when SET =>
            ensure_that_the_nest_has_room_for_a_result;
            the_trace_operand := sign_extended(INS.operand);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 4;

         when others =>
            trap_invalid_instruction;
      end case;
   end do_a_data_access_order;

   procedure update_the_virtual_clocks;
   pragma Inline(update_the_virtual_clocks);

   procedure update_the_virtual_clocks is
   begin
      the_CPU_time := the_CPU_time + the_CPU_delta;
      the_elapsed_time := the_elapsed_time + the_CPU_delta;
      if the_CPU_time > the_elapsed_time then
         the_elapsed_time := the_CPU_time;
      end if;
      ICR := ICR + 1;
   end update_the_virtual_clocks;

   procedure do_a_fast_time_slice is
   begin
      for i in 1 .. time_slice loop

         the_CPU_delta := 0;

         process_syllable_0_of_INS;
         case INS.kind is
            when one_syllable_order =>
               do_a_one_syllable_order;
            when two_syllable_order =>
               process_syllable_1_of_INS;
               do_a_two_syllable_order;
            when normal_jump_order =>
               process_syllables_1_and_2_of_a_jump_order;
               do_a_jump_order;
            when data_access_order =>
               process_syllables_1_and_2_of_a_data_access_order;
               do_a_data_access_order;
         end case;

         update_the_virtual_clocks;

         if the_elapsed_time > the_next_interrupt_time       and then
               (INS.syndrome /= EXITD and INS.syndrome /= OUT_9) then
            act_on_pending_interrupts;
         end if;
      end loop;

   exception

      when program_exit =>
         complete_all_extant_transfers;
         update_the_virtual_clocks;
         synchronize_the_real_and_virtual_times;
         raise;

   end do_a_fast_time_slice;

   procedure do_a_traced_instruction_cycle is
      use tracing.order_flags;

      procedure finalize_traced_instruction_execution is
      begin
         update_the_virtual_clocks;
         synchronize_the_real_and_virtual_times;

         if ICR in low_count .. high_count            and then
               NIA_word_number in low_bound .. high_bound then
            take_note_of(the_trace_operand);
            if the_signature_is_enabled then
               update_the_digital_signature;
            end if;
            if the_histogram_is_enabled then
               add_INS_to_the_histogram;
            end if;
            if the_external_trace_is_enabled then
               log_to_external_trace;
            end if;
            case INS.kind is
               when two_syllable_order =>
                  act_on_any_two_syllable_order_watchpoints;
               when data_access_order =>
                  act_on_any_data_access_order_watchpoints;
               when others =>
                  null;
            end case;
         end if;
      end finalize_traced_instruction_execution;

   begin
      the_trace_operand := 0;
      the_trace_address := 0;
      the_CPU_delta := 0;

      process_syllable_0_of_INS;
      case INS.kind is
         when one_syllable_order =>
            preview_a_one_syllable_order;
               do_a_one_syllable_order;
            look_back_at_a_one_syllable_order;
         when two_syllable_order =>
            process_syllable_1_of_INS;
            preview_a_two_syllable_order;
               do_a_two_syllable_order;
            look_back_at_a_two_syllable_order;
         when normal_jump_order =>
            process_syllables_1_and_2_of_a_jump_order;
            preview_a_jump_order;
               do_a_jump_order;
            look_back_at_a_jump_order;
         when data_access_order =>
            process_syllables_1_and_2_of_a_data_access_order;
            preview_a_data_access_order;
               do_a_data_access_order;
            look_back_at_a_data_access_order;
      end case;

      finalize_traced_instruction_execution;

      if the_Q_store(0) /= all_zero_Q_store then
         raise emulation_failure with "Q0 is not zero";
      end if;

      if the_elapsed_time > the_next_interrupt_time       and then
            (INS.syndrome /= EXITD and INS.syndrome /= OUT_9) then
         act_on_pending_interrupts;
      end if;

      if ICR >= time_limit then
         raise time_expired;
      end if;

      if (NIA_word_number/is_a_breakpoint    and then
             ICR in low_count .. high_count)  or else
                the_diagnostic_mode = pause_mode then
         handle_breakpoint;
      end if;

   exception

      when program_exit =>
         case INS.kind is
            when one_syllable_order =>
               look_back_at_a_one_syllable_order;
            when two_syllable_order =>
               look_back_at_a_two_syllable_order;
            when normal_jump_order =>
               look_back_at_a_jump_order;
            when data_access_order =>
               look_back_at_a_data_access_order;
         end case;

         complete_all_extant_transfers;
         finalize_traced_instruction_execution;

         raise;

   end do_a_traced_instruction_cycle;

end KDF9.microcode;
