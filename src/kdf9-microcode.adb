-- KDF9 ISP emulation - CPU microcode routines.
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

with break_in;
with exceptions;
with IOC;
with IOC.dispatcher;
with KDF9.CPU;
with KDF9.decoding;
with KDF9.EGDON;
with KDF9.store;
with KDF9.TSD;
with KDF9.TSD.processes;
with POSIX;
with settings;
with state_display;
with tracing;

use  exceptions;
use  IOC;
use  IOC.dispatcher;
use  KDF9.CPU;
use  KDF9.decoding;
use  KDF9.EGDON;
use  KDF9.store;
use  KDF9.TSD;
use  KDF9.TSD.processes;
use  settings;
use  state_display;
use  tracing;

package body KDF9.microcode is

   procedure do_a_one_syllable_order is
      A, B, C, E : KDF9.word;
      bit_count  : KDF9.word;
      AB, CD     : KDF9.pair;
      X, Y       : CPU.f48;
      XY, ZT     : CPU.f96;
   begin
      case INS.compressed_opcode is

         when 0 =>
            -- The DUMMY order originally had code 0, before being changed to #17.
            -- The Kidsgrove compiler continued to use code 0, so we must assume that,
            --    despite the Manual, 0 continued to be a valid no-op order.
            the_CPU_delta := the_CPU_delta + 1;

         when VR =>
            the_V_bit_is_set := False;
            the_CPU_delta := the_CPU_delta + 1;

         when TO_TR =>
            ensure_that_the_NEST_holds_an_operand;
            if resign(pop) < 0 then
               the_T_bit_is_set := True;
            end if;
            the_CPU_delta := the_CPU_delta + 2;

         when BITS =>
            write_top(number_of_1_bits_in(read_top));
            the_CPU_delta := the_CPU_delta + 27;

         when XF =>
            ensure_that_the_NEST_holds_2_operands;
            X := pop;
            Y := read_top;
            write_top(CPU.f48'(Y * X));
            the_CPU_delta := the_CPU_delta + 15;

         when XDF =>
            ensure_that_the_NEST_holds_2_operands;
            XY := read_top;
            ZT := XY.lsw * XY.msw;
            write_top(ZT);
            the_CPU_delta := the_CPU_delta + 16;

         when XPLUSF =>
            ensure_that_the_NEST_holds(at_least => 4);
            XY := pop;
            ZT := XY.lsw * XY.msw;
            XY := read_top;
            write_top(XY + ZT);
            the_CPU_delta := the_CPU_delta + 18;

         when NEGD =>
            AB := read_top;
            write_top( - AB);
            the_CPU_delta := the_CPU_delta + 2;

         when OR_9 =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            write_top(read_top or A);
            the_CPU_delta := the_CPU_delta + 1;

         when PERM =>
            A := pop;
            CD := pop;
            push(A);
            push(CD);
            the_CPU_delta := the_CPU_delta + 2;

         when TOB =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;      -- the value
            bit_count := number_of_1_bits_in(A);
            B := read_top; -- the radixes
            C := 0;

            for i in 1 .. 8 loop
               A := rotate_word_left(A, 6);
               B := rotate_word_left(B, 6);
               E := B and 8#77#; -- this radix
               C := C*E + (A and 8#77#);
            end loop;

            write_top(C);
            the_CPU_delta := the_CPU_delta + 2 + 4*KDF9.us(bit_count);

         when ROUNDH =>
            A := read_top;
            write_top(resign(A) + 2**23);
            the_CPU_delta := the_CPU_delta + 22;

         when NEV =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            write_top(read_top xor A);
            the_CPU_delta := the_CPU_delta + 2;

         when ROUND =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            write_top(resign(A) + resign(shift_word_right(read_top, 46) and 1));
            the_CPU_delta := the_CPU_delta + 1;

         when DUMMY =>
            the_CPU_delta := the_CPU_delta + 1;

         when ROUNDF =>
            ensure_that_the_NEST_holds_2_operands;
            XY := pop;
            ZT := XY;
            push(narrowed(XY));
            the_CPU_delta := the_CPU_delta + 3;

         when ROUNDHF =>
            X := pop;
            push(narrowed(X));
            the_CPU_delta := the_CPU_delta + 3;

         when MINUSDF =>
            ensure_that_the_NEST_holds(at_least => 4);
            XY := pop;
            ZT := read_top;
            write_top(ZT - XY);
            the_CPU_delta := the_CPU_delta + 12;

         when PLUSDF =>
            ensure_that_the_NEST_holds(at_least => 4);
            XY := pop;
            ZT := read_top;
            write_top(ZT + XY);
            the_CPU_delta := the_CPU_delta + 12;

         when FLOAT_9 =>
            ensure_that_the_NEST_holds_2_operands;
            -- There is great uncertainty as to how the FLOAT/FLOATD orders handled scale factors
            --   in N1 that lie outside the range allowed by the Manual, namely -128 <= N1 <= +127.
            -- ee9 here takes a safety-first approach which is consistent with the
            --    behaviour of the Kidsgrove sqrt function with arguments < 0.5º0.
            A := shift_arithmetic(shift_logical(pop, +40), -40);
            B := read_top;
            write_top(KDF9.word(normalized(full_fraction => B, scaler => A)));

         when FLOATD =>
            ensure_that_the_NEST_holds(at_least => 3);
            A := shift_arithmetic(shift_logical(pop, +40), -40);
            CD := read_top;
            -- See §3.4 of Report K/GD.y.83, dated 6/12/1962.  It would seem to require this:
            -- CD.lsw := CD.lsw and not 8#77#;  -- The 6 l.s.b. are lost.
            -- The above is commented out because it gives the wrong answer with KAA01.
            -- A post-document hardware modification is suspected.
            reconstruct(CD, scaler => A);
            write_top(CD);
            the_CPU_delta := the_CPU_delta + 8;

         when ABS_9 =>
            write_top( abs resign(read_top));
            the_CPU_delta := the_CPU_delta + 1;

         when NEG =>
            write_top( - resign(read_top));
            the_CPU_delta := the_CPU_delta + 1;

         when ABSF =>
            X := read_top;
            if resign(KDF9.word(X)) < 0 then
               write_top( - X);
               the_CPU_delta := the_CPU_delta + 4;
            else
               the_CPU_delta := the_CPU_delta + 1;
            end if;

         when NEGF =>
            X := read_top;
            write_top( - X);
            the_CPU_delta := the_CPU_delta + 3;

         when MAX =>
            AB := read_top;
            if resign(AB.lsw) >= resign(AB.msw) then
               write_top(KDF9.pair'(msw => AB.lsw, lsw =>AB.msw));
               the_V_bit_is_set := True;
            end if;
            the_CPU_delta := the_CPU_delta + 4;

         when NOT_9 =>
            A := read_top;
            write_top(not A);
            the_CPU_delta := the_CPU_delta + 1;

         when XD =>
            AB := read_top;
            CD := AB.msw * AB.lsw;
            write_top(CD);
            the_CPU_delta := the_CPU_delta + 14;

         when X_frac =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            write_top(as_word(CPU.fraction'(read_top * A)));
            the_CPU_delta := the_CPU_delta + 15;

         when MINUS =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            B := read_top;
            write_top(resign(B) - resign(A));
            the_CPU_delta := the_CPU_delta + 1;

         when SIGN =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            B := read_top;
            if B = A then
               write_top(KDF9.word'(0));
            elsif resign(B) > resign(A) then
               write_top(KDF9.word'(1));
            else
               write_top(KDF9.word'(all_one_bits));
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when ZERO =>
            ensure_that_the_NEST_has_room_for_a_result;
            push(KDF9.word'(all_zero_bits));
            the_CPU_delta := the_CPU_delta + 2;

         when DUP =>
            ensure_that_the_NEST_has_room_for_a_result;
            A := read_top;
            push(A);
            the_CPU_delta := the_CPU_delta + 2;

         when DUPD =>
            ensure_that_the_NEST_has_room_for_2_results;
            AB := read_top;
            push(AB);
            the_CPU_delta := the_CPU_delta + 4;

         when DIVI =>
            AB := read_top;
            do_DIVI(L => AB.lsw,
                    R => AB.msw,
                    Quotient  => CD.lsw,
                    Remainder => CD.msw);
            write_top(CD);
            the_CPU_delta := the_CPU_delta + 36;

         when FIX =>
            ensure_that_the_NEST_holds_an_operand;
            ensure_that_the_NEST_has_room_for_a_result;
            X := read_top;
            write_top(fraction_word(X));
            push(scaler(X));
            the_CPU_delta := the_CPU_delta + 6;

         when STR =>
            ensure_that_the_NEST_has_room_for_a_result;
            A := read_top;
            if resign(A) < 0 then
               write_top(A and not_sign_bit);
               push(KDF9.word'(all_one_bits));
            else
               push(KDF9.word'(all_zero_bits));
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when CONT =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            B := read_top;
            write_top(contracted(KDF9.pair'(msw => A, lsw => B)));
            the_CPU_delta := the_CPU_delta + 2;

         when REVD =>
            AB := pop;
            CD := pop;
            push(AB);
            push(CD);
            the_CPU_delta := the_CPU_delta + 4;

         when ERASE =>
            ensure_that_the_NEST_holds_an_operand;
            pop;
            the_CPU_delta := the_CPU_delta + 1;

         when MINUSD =>
            ensure_that_the_NEST_holds(at_least => 4);
            AB := pop;
            CD := read_top;
            write_top(CD - AB);
            the_CPU_delta := the_CPU_delta + 3;

         when AND_9 =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            write_top(read_top and A);
            the_CPU_delta := the_CPU_delta + 1;

         when PLUS =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            B := read_top;
            write_top(resign(B) + resign(A));
            the_CPU_delta := the_CPU_delta + 1;

         when PLUSD =>
            ensure_that_the_NEST_holds(at_least => 4);
            AB := pop;
            CD := read_top;
            write_top(CD + AB);
            the_CPU_delta := the_CPU_delta + 3;

         when DIV =>
            ensure_that_the_NEST_holds_2_operands;
            AB := pop;
            push(as_word(CPU.fraction'(AB.lsw / AB.msw)));
            the_CPU_delta := the_CPU_delta + 36;

         when DIVD =>
            ensure_that_the_NEST_holds(at_least => 3);
            A := pop;
            CD := pop;
            do_DIVD(L => CD,
                    R => A,
                    Q => E);
            push(E);
            the_CPU_delta := the_CPU_delta + 36;

         when DIVF =>
            X := pop;
            Y := read_top;
            write_top(Y / X);
            the_CPU_delta := the_CPU_delta + 36;

         when DIVDF =>
            ensure_that_the_NEST_holds(at_least => 3);
            Y := pop;
            XY := pop;
            push(XY / Y);
            the_CPU_delta := the_CPU_delta + 35;

         when DIVR =>
            ensure_that_the_NEST_holds(at_least => 3);
            A := pop;
            CD := read_top;
            do_DIVR(L => CD,
                    R => A,
                    Quotient  => AB.msw,
                    Remainder => AB.lsw);
            write_top(AB);
            the_CPU_delta := the_CPU_delta + 36;

         when REV =>
            AB := read_top;
            write_top(KDF9.pair'(msw => AB.lsw, lsw =>AB.msw));
            the_CPU_delta := the_CPU_delta + 1;

         when CAB =>
            AB := pop;
            C := pop;
            push(AB);
            push(C);
            the_CPU_delta := the_CPU_delta + 2;

         when FRB =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;      -- the value
            bit_count := number_of_1_bits_in(A);
            B := read_top; -- the radixes
            C := 0;

            for i in 1 .. 8 loop
               E := B and 8#77#;
               if E /= 0 then
                  C := C or (A mod E);
                  A := A / E;
               else
                  if A /= 0 then the_V_bit_is_set := True; end if;
               end if;
               B := shift_word_right(B, 6);
               C := rotate_word_right(C, 6);
            end loop;

            if A /= 0 then
               -- The value was too big for the representation; see Manual.
               the_V_bit_is_set := True;
            end if;
            write_top(C);
            the_CPU_delta := the_CPU_delta + 8 + 3*KDF9.us(bit_count);

         when STAND =>
            X := read_top;
            write_top(normalized(X));
            the_CPU_delta := the_CPU_delta + 5;

         when NEGDF =>
            XY := read_top;
            write_top( - XY);
            the_CPU_delta := the_CPU_delta + 9;

         when MAXF =>
            XY := read_top;
            if XY.lsw >= XY.msw then
               write_top(CPU.f96'(msw => XY.lsw, lsw =>XY.msw));
               the_V_bit_is_set := True;
            end if;
            the_CPU_delta := the_CPU_delta + 6;

         when PLUSF =>
            ensure_that_the_NEST_holds_2_operands;
            X := pop;
            Y := read_top;
            write_top(Y + X);
            the_CPU_delta := the_CPU_delta + 7;

         when MINUSF =>
            ensure_that_the_NEST_holds_2_operands;
            X := pop;
            Y := read_top;
            write_top(Y - X);
            the_CPU_delta := the_CPU_delta + 7;

         when SIGNF =>
            ensure_that_the_NEST_holds_2_operands;
            XY := pop;
            if KDF9.word(XY.lsw) = KDF9.word(XY.msw) then
               push(KDF9.word'(all_zero_bits));
            elsif XY.lsw < XY.msw then
               push(KDF9.word'(all_one_bits));
            else
               push(KDF9.word(1));
            end if;
            the_CPU_delta := the_CPU_delta + 5;

         when others =>
            trap_illegal_instruction;

      end case;
   end do_a_one_syllable_order;


   procedure do_an_IO_order is
      IO_opcode   : constant KDF9.compressed_opcode := (INS.Qk and not manual_bit);
      IO_operand  : constant KDF9.Q_register := the_Q_store(INS.Qq);
      set_offline : constant Boolean         := (INS.Qk and manual_bit) /= 0;
   begin
      case INS.compressed_opcode is

         when PAR_Qq =>
            the_CPU_delta := the_CPU_delta + 11;
            PAR(IO_operand, set_offline, the_T_bit_is_set);
            the_CPU_delta := the_CPU_delta + 3;

         when PIA_PIC_CLO_TLO_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIA_bits =>
                  PIA(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PIC_bits =>
                  PIC(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when CLO_bits =>
                  fail_in_problem_program_state;
                  CLO(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 1;
               when TLO_bits =>
                  TLO(IO_operand, the_T_bit_is_set);
               when others =>
                  trap_illegal_instruction;
            end case;

         when PIB_PID_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIB_bits =>
                  PIB(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PID_bits =>
                  PID(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_illegal_instruction;
            end case;

         when PIE_PIG_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIE_bits =>
                  PIE(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PIG_bits =>
                  PIG(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_illegal_instruction;
            end case;

         when PIF_PIH_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when PIF_bits =>
                  PIF(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when PIH_bits =>
                  PIH(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_illegal_instruction;
            end case;

         when PMA_PMK_INT_Qq =>
            the_CPU_delta := the_CPU_delta + 11;
            case IO_opcode is
               when PMA_bits =>
                  PMA(IO_operand, set_offline);
               when PMK_bits =>
                  PMK(IO_operand, set_offline);
               when INT_bits =>
                  INT(IO_operand, set_offline);
               when others =>
                  trap_illegal_instruction;
            end case;

         when CT_PMB_PMC_BUSY_Qq =>
            the_CPU_delta := the_CPU_delta + 11;
            case IO_opcode is
               when CTQ_bits =>
                  -- if set_offline then MANUALQq else CTQq
                  if set_offline                      or else
                        the_CPU_state = Director_state   then
                     MANUAL_CT(IO_operand, set_offline);
                     the_CPU_delta := the_CPU_delta + 2;
                  else
                     trap_illegal_instruction; -- This will always LIV, as we are not in Director.
                  end if;
               when PMB_bits =>
                  PMB(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 3;
               when PMC_bits =>
                  PMC(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 3;
               when BUSY_bits =>
                  BUSY(IO_operand, set_offline, the_T_bit_is_set);
                  the_CPU_delta := the_CPU_delta + 2;
               when others =>
                  trap_illegal_instruction;
            end case;

         when PMD_PME_PML_Qq =>
            the_CPU_delta := the_CPU_delta + 14;
            case IO_opcode is
               when PMD_bits =>
                  PMD(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 5;
               when PME_bits =>
                  PME(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 5;
               when PML_bits =>
                  PML(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 5;
               when others =>
                  trap_illegal_instruction;
            end case;

         when PMF_PMG_Qq =>
            the_CPU_delta := the_CPU_delta + 11;
            case IO_opcode is
               when PMF_bits =>
                  PMF(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 3;
               when PMG_bits =>
                  fail_in_problem_program_state;
                  the_CPU_delta := the_CPU_delta + 14;  -- ??
                  PMG(IO_operand, set_offline);
                when others =>
                  trap_illegal_instruction;
            end case;

         when POA_POC_POE_POF_PMH_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POA_bits =>
                  POA(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POC_bits =>
                  POC(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POE_bits =>
                  POE(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 4;
               when POF_bits =>
                  POF(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 4;
               when PMH_bits =>
                  fail_in_problem_program_state;
                  SLO(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 1;
               when others =>
                  trap_illegal_instruction;
            end case;

         when POB_POD_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POB_bits =>
                  POB(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POD_bits =>
                  POD(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_illegal_instruction;
            end case;

         when POG_POL_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POG_bits =>
                  POG(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POL_bits =>
                  POL(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_illegal_instruction;
            end case;

         when POH_POK_Qq =>
            the_CPU_delta := the_CPU_delta + 15;
            case IO_opcode is
               when POH_bits =>
                  POH(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when POK_bits =>
                  POK(IO_operand, set_offline);
                  the_CPU_delta := the_CPU_delta + 7;
               when others =>
                  trap_illegal_instruction;
            end case;

         when others =>
            trap_illegal_instruction;

      end case;
   end do_an_IO_order;


   all_zero_Q_store : constant KDF9.Q_register := (C | I | M => 0);

   procedure ensure_that_Q0_contains_zero (suspect : KDF9.Q_number)
      with Inline;

   procedure ensure_that_Q0_contains_zero (suspect : KDF9.Q_number) is
   begin
      if suspect = 0 then
         the_Q_store(0) := all_zero_Q_store;  -- Override any assignment to Q0.
      end if;
   end ensure_that_Q0_contains_zero;

   procedure auto_increment
      with Inline;

   procedure auto_increment is
   begin
      if INS.Qq /= 0 then
         the_Q_store(INS.Qq).M := the_Q_store(INS.Qq).M + the_Q_store(INS.Qq).I;
         the_Q_store(INS.Qq).C := the_Q_store(INS.Qq).C - 1;
      end if;
   end auto_increment;

   function shift_count
   return CPU.signed_Q_part
      with Inline;

   function shift_count
   return CPU.signed_Q_part
   is (
       if (INS.order.syllable_1 and constant_bit) /= 0  then
          resign(KDF9.Q_part(INS.order.syllable_1/2 xor 64)) - 64
       else
          resign((the_Q_store(INS.Qq).C and 255) xor 128) - 128
      );


   procedure do_a_two_syllable_order is
      A  : KDF9.word;
      AB : KDF9.pair;
      CD : KDF9.pair;
   begin
      case INS.compressed_opcode is

         when JCqNZS =>
            if CIA.syllable_index = 5 then
               -- KDF9 did not actually detect this error, and the JCqNZS instruction would work
               --    until broken-into by an interrupt, which returned to the word following that
               --       containing the first syllable of the JCqNZS instruction.
               -- I see no case for reproducing this behaviour.
               trap_illegal_instruction ("JCqNZS instruction at syllable 5");
            end if;
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
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQ =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when MkMqH =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_NEST_has_room_for_a_result;
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQH =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_NEST_has_room_for_a_result;
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when MkMqN =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQN =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when MkMqHN =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_NEST_has_room_for_a_result;
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 7;

         when MkMqQHN =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
            ensure_that_the_NEST_has_room_for_a_result;
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMq =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQ =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMqH =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQH =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMqN =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQN =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 8;

         when TO_MkMqHN =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
            the_CPU_delta := the_CPU_delta + 7;

         when TO_MkMqQHN =>
            the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
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
            if INS.Qq /= 0 then
               the_Q_store(INS.Qq).C := the_Q_store(INS.Qq).C - 1;
            end if;

         when POS1_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            if INS.Qq /= 0 then
               the_Q_store(INS.Qq).I := + 1;
            end if;

         when NEG1_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            if INS.Qq /= 0 then
               the_Q_store(INS.Qq).I := - 1;
            end if;

         when POS2_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            if INS.Qq /= 0 then
               the_Q_store(INS.Qq).I := + 2;
            end if;

         when NEG2_TO_Iq =>
            the_CPU_delta := the_CPU_delta + 3;
            if INS.Qq /= 0 then
               the_Q_store(INS.Qq).I := - 2;
            end if;

         when CqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            if INS.Qk /= 0 then
               the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
            end if;

         when IqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            if INS.Qk /= 0 then
               the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
            end if;

         when MqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            if INS.Qk /= 0 then
               the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
            end if;

         when QqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            if INS.Qk /= 0 then
               the_Q_store(INS.Qk) := the_Q_store(INS.Qq);
            end if;

         when CIqTOQk =>
           the_CPU_delta := the_CPU_delta + 4;
           if INS.Qk /= 0 then
              the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
              the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
           end if;

         when IMqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            if INS.Qk /= 0 then
               the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
               the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
            end if;

         when CMqTOQk =>
            the_CPU_delta := the_CPU_delta + 4;
            if INS.Qk /= 0 then
               the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
               the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
            end if;

         when QCIMq =>
            ensure_that_the_NEST_has_room_for_a_result;
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
               trap_illegal_instruction;
            end if;

         when TO_RCIMq =>
            ensure_that_the_NEST_holds_an_operand;
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
               trap_illegal_instruction;
            end if;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when ADD_TO_QCIMq =>
            ensure_that_the_NEST_has_room_for_a_result;
            ensure_that_the_NEST_holds_an_operand;

            -- Because the following does not push the Q operand on to the NEST,
            --   it will not leave an authentic bit pattern in the NEST core stack,
            --      in the event of a subsequent NOUV.
            -- I take this to be of no importance.

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
               trap_illegal_instruction;
            end if;
            ensure_that_Q0_contains_zero(suspect => INS.Qq);

         when SHA =>
            A := read_top;
            write_top(KDF9.word'(shift_arithmetic(A, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when SHAD =>
            AB := read_top;
            write_top(KDF9.pair'(shift_arithmetic(AB, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when MACC =>
            ensure_that_the_NEST_holds(at_least => 4);
            AB := pop;
            AB := AB.msw * AB.lsw;
            CD := read_top;
            write_top(CD + shift_arithmetic(AB, shift_count));
            the_CPU_delta := the_CPU_delta + 15 + shift_time(Natural(abs shift_count));

         when SHL =>
            write_top(KDF9.word'(shift_logical(read_top, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when SHLD =>
            write_top(KDF9.pair'(shift_logical(read_top, shift_count)));
            the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));

         when SHC =>
            write_top(shift_circular(read_top, shift_count));
            the_CPU_delta := the_CPU_delta + 3 + shift_time(Natural(abs shift_count));

         when TO_Kq =>
            fail_in_problem_program_state;
            case INS.Qq is
               when K0 =>
                  if read_top /= KDF9.word'(all_zero_bits) then
                     for w in all_zero_bits .. read_top mod 8 loop
                        POSIX.output_line("BLEEP!");
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
                  trap_illegal_instruction;
            end case;
            the_CPU_delta := the_CPU_delta + 3;

         when Kk =>
            fail_in_problem_program_state;
            ensure_that_the_NEST_has_room_for_a_result;
            case INS.Qk is
               when K4 =>
                  push(get_K4_operand);
                  the_RFIR := (others => False);
               when K5 =>
                  push(get_K5_operand);
               when K7 =>
                  push(get_K7_operand);
               when others =>
                  trap_illegal_instruction;
            end case;
            the_CPU_delta := the_CPU_delta + 3;

         when LINK =>
            if the_CPU_state = Director_state and the_SJNS_depth = 0 then -- clear out JB
               push(KDF9.word'(all_zero_bits));
               the_SJNS_depth := 0 - 1;
            else
               ensure_that_the_NEST_has_room_for_a_result;
               ensure_that_the_SJNS_is_not_empty;
               push(as_word(KDF9.SJNS_link(KDF9.syllable_address'(pop))));
            end if;
            the_CPU_delta := the_CPU_delta + 4;

         when TO_LINK =>
            ensure_that_the_SJNS_is_not_full;
            ensure_that_the_NEST_holds_an_operand;
            push(KDF9.syllable_address(as_link(pop)));
            the_CPU_delta := the_CPU_delta + 3;

         when others =>
            do_an_IO_order;

      end case;
   end do_a_two_syllable_order;

   procedure do_a_jump_order is
      RA        : KDF9.syllable_address;
      A         : KDF9.word;
   begin
      fetching_normally := True;

      case INS.compressed_opcode is

         when Jr =>
            set_NIA_to_the_INS_target_address;
            the_CPU_delta := the_CPU_delta + 8;

         when JSr =>
            ensure_that_the_SJNS_is_not_full;
            push(CIA);
            set_NIA_to_the_INS_target_address;
            the_CPU_delta := the_CPU_delta + 11;

         when JrEQ =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            if A = read_top then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 12;
            else
               the_CPU_delta := the_CPU_delta + 5;
            end if;

         when JrNE =>
            ensure_that_the_NEST_holds_2_operands;
            A := pop;
            if A /= read_top then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 12;
            else
               the_CPU_delta := the_CPU_delta + 5;
            end if;

         when JrGTZ =>
            ensure_that_the_NEST_holds_an_operand;
            if resign(pop) > 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrLTZ =>
            ensure_that_the_NEST_holds_an_operand;
            A := pop;
            if resign(A) < 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrEQZ =>
            ensure_that_the_NEST_holds_an_operand;
             if pop = KDF9.word'(all_zero_bits) then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrLEZ =>
            ensure_that_the_NEST_holds_an_operand;
            if resign(pop) <= 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrGEZ =>
            ensure_that_the_NEST_holds_an_operand;
            if resign(pop) >= 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrNEZ =>
            ensure_that_the_NEST_holds_an_operand;
            if pop /= KDF9.word'(all_zero_bits) then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrV =>
            if the_V_bit_is_set then
               the_V_bit_is_set := False;
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;

         when JrNV =>
            if the_V_bit_is_set then
               the_V_bit_is_set := False;
               the_CPU_delta := the_CPU_delta + 3;
            else
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            end if;

         when JrEN =>
            the_trace_operand := KDF9.word(the_NEST_depth);
            if the_NEST_depth = 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;

         when JrNEN =>
            the_trace_operand := KDF9.word(the_NEST_depth);
            if the_NEST_depth /= 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;

         when JrEJ =>
            the_trace_operand := KDF9.word(the_SJNS_depth);
            if the_SJNS_depth = 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when JrNEJ =>
            the_trace_operand := KDF9.word(the_SJNS_depth);
            if the_SJNS_depth /= 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            end if;
            the_CPU_delta := the_CPU_delta + 3;

         when JrTR =>
            if the_T_bit_is_set then
               the_T_bit_is_set := False;
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            else
               the_CPU_delta := the_CPU_delta + 3;
            end if;

         when JrNTR =>
            if the_T_bit_is_set then
               the_T_bit_is_set := False;
               the_CPU_delta := the_CPU_delta + 3;
            else
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 10;
            end if;

         when EXIT_n =>
            ensure_that_the_SJNS_is_not_empty;
            RA := pop;
            if INS.target.syllable_index = 3 then  -- c.f. decode_a_jump_order.
               increment_by_3(RA);
            end if;
            RA.order_word_number := RA.order_word_number+INS.target.order_word_number;
            set_NIA_to(RA);
            the_CPU_delta := the_CPU_delta + 12 + KDF9.us(INS.target.syllable_index mod 2);

         when EXITD =>
            fail_in_problem_program_state;
            if the_SJNS_depth = 0 then
               -- This indicates a serious failure in Director; best to abandon it at once.
               trap_illegal_instruction("empty SJNS in Director");
            end if;
            RA := pop;
            the_CPU_delta := the_CPU_delta + 11;
            return_from_Director_to(RA);

         when JrCqZ =>
            if the_Q_store(INS.Qq).C = 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when JrCqNZ =>
            if the_Q_store(INS.Qq).C /= 0 then
               set_NIA_to_the_INS_target_address;
               the_CPU_delta := the_CPU_delta + 11;
            else
               the_CPU_delta := the_CPU_delta + 4;
            end if;

         when OS_OUT =>
            the_CPU_delta := the_CPU_delta + 13;
            ensure_that_the_SJNS_is_not_full;
            A := (if the_NEST_depth = 0 then 0 else read_top);
            if the_execution_mode = boot_mode then
               effect_interrupt(caused_by_OUT, A'Image);
               -- We get here only in Director state, when the OUT does not interrupt.
               -- Arguably, this should be notified as an error.
               return; -- OUT has the effect of a no-op in Director state.
            end if;
            -- Emulate a subset of the appropriate Director's API.
            if A < 100 then
               do_a_TSD_OUT(OUT_number => A);
            elsif A < 200 then
               do_an_EGDON_OUT(OUT_number => A);
            else
               trap_failing_OUT(A, "is unknown, or not yet implemented");
            end if;

         when others =>
            trap_illegal_instruction;

      end case;
   end do_a_jump_order;

   procedure do_a_data_access_order is
   begin
      case INS.compressed_opcode is

         when EaMq =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 6;

         when TO_EaMq =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            the_CPU_delta := the_CPU_delta + 6;

         when EaMqQ =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_has_room_for_a_result;
            the_trace_operand := fetch_word(the_trace_address);
            push(the_trace_operand);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 7;

         when TO_EaMqQ =>
            the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
            check_address_and_lockout(the_trace_address);
            ensure_that_the_NEST_holds_an_operand;
            the_trace_operand := pop;
            store_word(the_trace_operand, the_trace_address);
            auto_increment;
            the_CPU_delta := the_CPU_delta + 7;

         when SET =>
            ensure_that_the_NEST_has_room_for_a_result;
            the_trace_operand := sign_extended(INS.operand);
            push(the_trace_operand);
            the_CPU_delta := the_CPU_delta + 4;

         when others =>
            trap_illegal_instruction;

      end case;
   end do_a_data_access_order;

   procedure update_the_virtual_clocks
      with Inline;

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

      if break_in.has_been_requested then
         break_in.handler;
      end if;

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
         check_for_a_clock_interrupt;
         if the_elapsed_time > the_next_interrupt_time then
            act_on_pending_interrupts;
         end if;

      end loop;

   exception

      when program_exit =>
         complete_all_extant_transfers;
         update_the_virtual_clocks;
         synchronize_the_real_and_virtual_times;
         raise;

      when OUT_2_restart =>
         complete_all_extant_transfers;
         update_the_virtual_clocks;
         synchronize_the_real_and_virtual_times;
         complete_TSD_OUT_2;

   end do_a_fast_time_slice;

   procedure do_a_traced_instruction_cycle is
      use tracing.order_flags;

      procedure finalize_the_traced_instruction_execution is
      begin
         update_the_virtual_clocks;
         synchronize_the_real_and_virtual_times;

         if ICR in low_count .. high_count            and then
               NIA_word_number in low_bound .. high_bound then
            take_note_of(the_trace_operand);
            if the_signature_is_enabled then
               update_the_digital_signature;
            end if;
            if histogramming_is_enabled then
               add_INS_to_the_histogram;
               add_CIA_to_the_profile;
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
      end finalize_the_traced_instruction_execution;

   begin  -- do_a_traced_instruction_cycle

      if break_in.has_been_requested then
         break_in.handler;
      end if;

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

      finalize_the_traced_instruction_execution;

      if ICR >= time_limit then
         raise time_expired;
      end if;

      if (breakpoints/NIA_word_number        and then
             ICR in low_count .. high_count)  or else
                the_diagnostic_mode = pause_mode then
         handle_breakpoint;
      end if;

      check_for_a_clock_interrupt;
      if the_elapsed_time > the_next_interrupt_time then
         act_on_pending_interrupts;
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
         finalize_the_traced_instruction_execution;
         raise;

      when OUT_2_restart =>
         complete_all_extant_transfers;
         finalize_the_traced_instruction_execution;
         complete_TSD_OUT_2;

   end do_a_traced_instruction_cycle;

end KDF9.microcode;
