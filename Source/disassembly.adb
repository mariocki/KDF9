-- disassembly.adb
--
-- Produce dis-assembled instructions in an approximation to KDF9 Usercode.
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

with formatting;
with KDF9.CPU;
with KDF9.compressed_opcodes;

use  formatting;
use  KDF9.CPU;
use  KDF9.compressed_opcodes;

package body disassembly is

   pragma Unsuppress(All_Checks);

   function machine_code (decoded : KDF9.decoded_order)
   return String is
   begin
      case decoded.kind is
         when one_syllable_order =>
            return "#" & oct_of(decoded.order.syllable_0);
         when two_syllable_order =>
            return "#" & oct_of(decoded.order.syllable_0)
                 & ":" & oct_of(decoded.order.syllable_1);
         when normal_jump_order | data_access_order=>
            return "#" & oct_of(decoded.order.syllable_0)
                 & ":" & oct_of(decoded.order.syllable_1)
                 & ":" & oct_of(decoded.order.syllable_2);
      end case;
   end machine_code;

   function one_syllable_order_name (decoded : KDF9.decoded_order)
   return String is
   begin
      case decoded.syndrome is
         when VR =>
            return "VR";
         when TO_TR =>
            return "=TR";
         when BITS =>
            return "BITS";
         when XF =>
            return "×F";
         when XDF =>
            return "×DF";
         when XPLUSF =>
            return "×+F";
         when NEGD =>
            return "NEGD";
         when OR_9 =>
            return "OR";
         when PERM =>
            return "PERM";
         when TOB =>
            return "TOB";
         when ROUNDH =>
            return "ROUNDH";
         when NEV =>
            return "NEV";
         when ROUND =>
            return "ROUND";
         when DUMMY =>
            return "DUMMY";
         when ROUNDF =>
            return "ROUNDF";
         when ROUNDHF =>
            return "ROUNDHF";
         when MINUSDF =>
            return "-DF";
         when PLUSDF =>
            return "+DF";
         when FLOAT_9 =>
            return "FLOAT";
         when FLOATD =>
            return "FLOATD";
         when ABS_9 =>
            return "ABS";
         when NEG =>
            return "NEG";
         when ABSF =>
            return "ABSF";
         when NEGF =>
            return "NEGF";
         when MAX =>
            return "MAX";
         when NOT_9 =>
            return "NOT";
         when XD =>
            return "×D";
         when X_frac =>
            return "×";
         when MINUS =>
            return "-";
         when SIGN =>
            return "SIGN";
         when ZERO =>
            return "ZERO";
         when DUP =>
            return "DUP";
         when DUPD =>
            return "DUPD";
         when DIVI =>
            return "DIVI";
         when FIX =>
            return "FIX";
         when STR =>
            return "STR";
         when CONT =>
            return "CONT";
         when REVD =>
            return "REVD";
         when ERASE =>
            return "ERASE";
         when MINUSD =>
            return "-D";
         when AND_9 =>
            return "AND";
         when PLUS =>
            return "+";
         when PLUSD =>
            return "+D";
         when DIV =>
            return "DIV";
         when DIVD =>
            return "DIVD";
         when DIVF =>
            return "DIVF";
         when DIVDF =>
            return "DIVDF";
         when DIVR =>
            return "DIVR";
         when REV =>
            return "REV";
         when CAB =>
            return "CAB";
         when FRB =>
            return "FRB";
         when STAND =>
            return "STAND";
         when NEGDF =>
            return "NEGDF";
         when MAXF =>
            return "MAXF";
         when PLUSF =>
            return "+F";
         when MINUSF =>
            return "-F";
         when SIGNF =>
            return "SIGNF";
         when others =>
            return machine_code(decoded);
      end case;
   end one_syllable_order_name;

   function two_syllable_order_name (decoded : KDF9.decoded_order)
   return String is

      k : constant String := trimmed(KDF9.Q_number'Image(decoded.Qk));
      q : constant String := trimmed(KDF9.Q_number'Image(decoded.Qq));

      function shift_count return String is
         constant_flag : constant := 1;
         fixed_shift   : CPU.signed_Q_part;
      begin
         if (decoded.order.syllable_1 and constant_flag) /= 0  then
            fixed_shift := resign(KDF9.Q_part(decoded.order.syllable_1/2));
            if fixed_shift > 63 then
               fixed_shift := fixed_shift - 128;
            end if;
            return optional(fixed_shift<0, "", "+")
                 & trimmed(CPU.signed_Q_part'Image(fixed_shift));
         else
            return "C" & q;
         end if;
      end shift_count;

      function IO_order_name (decoded : KDF9.decoded_order)
      return String is
         IO_opcode : constant KDF9.syndrome := (decoded.Qk and not manual_bit);
      begin
         case decoded.syndrome is

            when PARQq =>
               return "PARQ" & q;

            when PIAQq_PICQq_CLOQq_TLOQq =>
               case IO_opcode is
                  when PIAQq_bits =>
                     return "PIAQ" & q;
                  when PICQq_bits =>
                     return "PICQ" & q;
                  when CLOQq_bits =>
                     return "CLOQ" & q;
                  when TLOQq_bits =>
                     return "TLOQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when PIBQq_PIDQq =>
               case IO_opcode is
                  when PIBQq_bits =>
                     return "PIBQ" & q;
                  when PIDQq_bits =>
                     return "PIDQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when PIEQq_PIGQq =>
               case IO_opcode is
                  when PIEQq_bits =>
                     return "PIEQ" & q;
                  when PIGQq_bits =>
                     return "PIGQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when PIFQq_PIHQq =>
               case IO_opcode is
                  when PIFQq_bits =>
                     return "PIFQ" & q;
                  when PIHQq_bits =>
                     return "PIHQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when PMAQq_PMKQq_INTQq =>
               case IO_opcode is
                  when PMAQq_bits =>
                     return "PMAQ" & q;
                  when PMKQq_bits =>
                     return "PMKQ" & q;
                  when INTQq_bits =>
                     return "INTQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when CTQq_PMBQq_PMCQq_BUSYQq =>
               case IO_opcode is
                  when CTQq_bits =>
                     if (decoded.Qk and manual_bit) /= 0 then
                        return "MANUALQ" & q;
                     else
                        return "CTQ" & q;
                     end if;
                  when PMBQq_bits =>
                     return "PMBQ" & q;
                  when PMCQq_bits =>
                     return "PMCQ" & q;
                  when BUSYQq_bits =>
                     return "BUSYQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when PMDQq_PMEQq_PMLQq =>
               case IO_opcode is
                  when PMDQq_bits =>
                     return "PMDQ" & q;
                  when PMEQq_bits =>
                     return "PMEQ" & q;
                  when PMLQq_bits =>
                     return "PMLQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when PMFQq =>
               return "PMFQ" & q;

            when PMGQq =>
               return "PMGQ" & q;

            when PMHQq =>
               return "PMHQ" & q;

            when POAQq_POCQq_POEQq_POFQq =>
               case IO_opcode is
                  when POAQq_bits =>
                     return "POAQ" & q;
                  when POCQq_bits =>
                     return "POCQ" & q;
                  when POEQq_bits =>
                     return "POEQ" & q;
                  when POFQq_bits =>
                     return "POFQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when POBQq_PODQq =>
               case IO_opcode is
                  when POBQq_bits =>
                     return "POBQ" & q;
                  when PODQq_bits =>
                     return "PODQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when POGQq_POLQq =>
               case IO_opcode is
                  when POGQq_bits =>
                     return "POGQ" & q;
                  when POLQq_bits =>
                     return "POLQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when POHQq_POKQq =>
               case IO_opcode is
                  when POHQq_bits =>
                     return "POHQ" & q;
                  when POLQq_bits =>
                     return "POKQ" & q;
                  when others =>
                     return machine_code(decoded);
               end case;

            when others =>
               return machine_code(decoded);
         end case;
      end IO_order_name;

   begin
      case decoded.syndrome is
         when JCqNZS =>
            return "JC" & q & "NZS";
         when MkMq =>
            return "M" & k & "M" & q;
         when MkMqQ =>
            return "M" & k & "M" & q & "Q";
         when MkMqH =>
            return "M" & k & "M" & q & "H";
         when MkMqQH =>
            return "M" & k & "M" & q & "QH";
         when MkMqN =>
            return "M" & k & "M" & q & "N";
         when MkMqQN =>
            return "M" & k & "M" & q & "QN";
         when MkMqHN =>
            return "M" & k & "M" & q & "HN";
         when MkMqQHN =>
            return "M" & k & "M" & q & "QHN";
         when TO_MkMq =>
            return "=M" & k & "M" & q;
         when TO_MkMqQ =>
            return "=M" & k & "M" & q & "Q";
         when TO_MkMqH =>
            return "=M" & k & "M" & q & "H";
         when TO_MkMqQH =>
            return "=M" & k & "M" & q & "QH";
         when TO_MkMqN =>
            return "=M" & k & "M" & q & "N";
         when TO_MkMqQN =>
            return "=M" & k & "M" & q & "QN";
         when TO_MkMqHN =>
            return "=M" & k & "M" & q & "HN";
         when TO_MkMqQHN =>
            return "=M" & k & "M" & q & "QHN";
         when M_PLUS_Iq =>
            return "M+I" & q;
         when M_MINUS_Iq =>
            return "M-I" & q;
         when NCq =>
            return "NC" & q;
         when DCq =>
            return "DC" & q;
         when POS1_TO_Iq =>
            return "I" & q & "=+1";
         when NEG1_TO_Iq =>
            return "I" & q & "=-1";
         when POS2_TO_Iq =>
            return "I" & q & "=+2";
         when NEG2_TO_Iq =>
            return "I" & q & "=-2";
         when MqTOQk =>
            return "M"  & q & "TOQ" & k;
         when IqTOQk =>
            return "I"  & q & "TOQ" & k;
         when IMqTOQk =>
            return "IM" & q & "TOQ" & k;
         when CqTOQk =>
            return "C"  & q & "TOQ" & k;
         when CMqTOQk =>
            return "CM" & q & "TOQ" & k;
         when CIqTOQk =>
            return "CI" & q & "TOQ" & k;
         when QqTOQk =>
            return "Q"  & q & "TOQ" & k;
         when QCIMq =>
            if (decoded.Qk and all_Q_choice) = all_Q_choice then
               return "Q" & q;
            else
               if (decoded.Qk and M_part_choice) /= 0 then
                  return "M" & q;
               elsif (decoded.Qk and C_part_choice) /= 0 then
                  return "C" & q;
               elsif (decoded.Qk and I_part_choice) /= 0 then
                  return "I" & q;
               else
                  return machine_code(decoded);
               end if;
            end if;
         when TO_RCIMq =>
            if (decoded.Qk and all_Q_choice) = all_Q_choice then
               return "=Q" & q;
            else
               if (decoded.Qk and M_part_choice) /= 0 then
                  return optional((decoded.Qk and reset_choice) /= 0, "=RM" & q, "=M" & q);
               elsif (decoded.Qk and C_part_choice) /= 0 then
                  return optional((decoded.Qk and reset_choice) /= 0, "=RC" & q, "=C" & q);
               elsif (decoded.Qk and I_part_choice) /= 0 then
                  return optional((decoded.Qk and reset_choice) /= 0, "=RI" & q, "=I" & q);
               else
                  return machine_code(decoded);
               end if;
            end if;
         when ADD_TO_QCIMq =>
            if (decoded.Qk and all_Q_choice) = all_Q_choice then
               return "=+Q" & q;
            else
               if (decoded.Qk and M_part_choice) /= 0 then
                  return "=+M" & q;
               elsif (decoded.Qk and C_part_choice) /= 0 then
                  return "=+C" & q;
               elsif (decoded.Qk and I_part_choice) /= 0 then
                  return "=+I" & q;
               else
                  return machine_code(decoded);
               end if;
            end if;
         when SHA =>
            return "SHA"  & shift_count;
         when SHAD =>
            return "SHAD" & shift_count;
         when MACC =>
            return "×+"   & shift_count;
         when SHL =>
            return "SHL"  & shift_count;
         when SHLD =>
            return "SHLD" & shift_count;
         when SHC =>
            return "SHC"  & shift_count;
         when TO_Kk =>
            case decoded.Qq is
               when K0 =>
                  return "=K0";
               when K1 =>
                  return "=K1";
               when K2 =>
                  return "=K2";
               when K3 =>
                  return "=K3";
               when others =>
                  return machine_code(decoded);
            end case;
         when Kk =>
            case decoded.Qk is
               when K4 =>
                  return "K4";
               when K5 =>
                  return "K5";
               when K7 =>
                  return "K7";
               when others =>
                  return machine_code(decoded);
            end case;
         when LINK =>
            return "LINK";
         when TO_LINK =>
            return "=LINK";
         when others =>
            return IO_order_name(decoded);
      end case;
   end two_syllable_order_name;

   function normal_jump_order_name (decoded      : KDF9.decoded_order;
                                    octal_option : Boolean)
   return String is
      the_target  : code_point renames decoded.target;
      the_address : constant String := oct_or_dec_of(the_target, octal_option);
   begin
      case decoded.syndrome is
         when JrEQ =>
            return "JE" & the_address & "EQ";
         when JrGTZ =>
            return "JE" & the_address & "GTZ";
         when JrLTZ =>
            return "JE" & the_address & "LTZ";
         when JrEQZ =>
            return "JE" & the_address & "EQZ";
         when JrV =>
            return "JE" & the_address & "V";
         when JrEN =>
            return "JE" & the_address & "EN";
         when Jr =>
            return "JE" & the_address;
         when JrEJ =>
            return "JE" & the_address & "EJ";
         when JSr =>
            return "JSE" & the_address;
         when JrTR =>
            return "JE" & the_address & "TR";
         when EXIT_9 =>
            if the_target.syllable_number = 0 then  -- c.f. decode_a_jump_order.
               -- No halfword offset applies.
               if the_target.word_number < 8 then
                  if the_target.word_number = 0 then
                     return "EXIT";
                  else
                     return "EXIT " & digit_map(KDF9.halfword(2*the_target.word_number));
                  end if;
               else
                  return "EXITAE" & oct_or_dec_of((0, the_target.word_number), octal_option);
               end if;
            elsif the_target.word_number < 8 then
               return "EXIT " & digit_map(KDF9.halfword(2*the_target.word_number + 1));
            else
               return "EXITAE" & oct_or_dec_of((3, the_target.word_number), octal_option);
            end if;
         when JrNE =>
            return "JE" & the_address & "NE";
         when JrLEZ =>
            return "JE" & the_address & "LEZ";
         when JrGEZ =>
            return "JE" & the_address & "GEZ";
         when JrNEZ =>
            return "JE" & the_address & "NEZ";
         when JrNV =>
            return "JE" & the_address & "NV";
         when JrNEN =>
            return "JE" & the_address & "NEN";
         when JrNEJ =>
            return "JE" & the_address & "NEJ";
         when JrNTR =>
            return "JE" & the_address & "NTR";
         when OUT_9 =>
            return "OUT";
         when EXITD =>
            return "EXITD";
         when JrCqZ =>
            return "JE" & the_address
                       & "C" & trimmed(KDF9.Q_number'Image(decoded.Qq)) & "Z";
         when JrCqNZ =>
            return "JE" & the_address
                       & "C" & trimmed(KDF9.Q_number'Image(decoded.Qq)) & "NZ";
         when others =>
            return machine_code(decoded);
      end case;
   end normal_jump_order_name;

   function data_access_order_name (decoded      : KDF9.decoded_order;
                                    octal_option : Boolean)
   return String is
      operand     : KDF9.Q_part   renames decoded.operand;
      Qq          : KDF9.Q_number renames decoded.Qq;
      the_address : constant String
                  := optional(octal_option,
                              "#" & oct_of(operand),
                              trimmed(KDF9.Q_part'Image(operand)));
      the_Q_store : constant String
                  := optional(Qq /= 0,
                              "M" & trimmed(KDF9.Q_number'Image(Qq)));
   begin
      case decoded.syndrome is
         when EaMq =>
            return "E"  & the_address & the_Q_store;
         when TO_EaMq =>
            return "=E" & the_address & the_Q_store;
         when EaMqQ =>
            return "E"  & the_address & the_Q_store & "Q" ;
         when TO_EaMqQ =>
            return "=E" & the_address & the_Q_store & "Q" ;
         when SET =>
            return "SET " & optional(octal_option,"B" & oct_of(operand, 2), dec_of(operand));
         when others =>
            return machine_code(decoded);
      end case;
   end data_access_order_name;

   function the_name_of (order : KDF9.decoded_order; octal_option : Boolean := True)
   return String is
   begin
      case order.kind is
         when one_syllable_order =>
            return one_syllable_order_name(order);
         when two_syllable_order =>
            return two_syllable_order_name(order);
         when normal_jump_order =>
            return normal_jump_order_name(order, octal_option);
         when data_access_order =>
            return data_access_order_name(order, octal_option);
      end case;
   end the_name_of;

   function the_order (order : KDF9.syllable_group; octal_option : Boolean)
   return String is
      its_INS : KDF9.decoded_order;
   begin
      its_INS.order := order;
      decode(its_INS);
      return the_name_of(its_INS, octal_option);
   end the_order;

   function the_order_at (address : KDF9.code_point; octal_option : Boolean)
   return String is
      saved_CIA : constant KDF9.code_point := CIA;
      saved_NIA : constant KDF9.code_point := NIA;
      saved_INS : constant KDF9.decoded_order := INS;
   begin
      set_NIA_to(address);
      decode_the_next_order;
      return result : constant String := the_name_of(INS, octal_option) do
         CIA := saved_CIA;
         set_NIA_to(saved_NIA);
         INS := saved_INS;
      end return;
   end the_order_at;


   function two_syllable_skeleton (encoding : KDF9.syllable)
   return String is

      function IO_skeleton (encoding : KDF9.syllable)
      return String is
      begin
         case encoding and 8#77# is
            when PARQq =>
               return "PARQq";
            when PIAQq_PICQq_CLOQq_TLOQq =>
               return "{PIA|PIC|CLO|TLO}Qq";
            when PIBQq_PIDQq =>
               return "{PIB|PID}Qq";
            when PIEQq_PIGQq =>
               return "{PIE|PIG}Qq";
            when PIFQq_PIHQq =>
               return "{PIF|PIH}Qq";
            when PMAQq_PMKQq_INTQq =>
               return "{INT|PMA|PMK}Qq";
            when CTQq_PMBQq_PMCQq_BUSYQq =>
               return "{BUSY|CTQ|PMB|PMC}Qq";
            when PMDQq_PMEQq_PMLQq =>
               return "{PMD|PME}Qq";
            when PMFQq =>
               return "PMFQq";
            when PMGQq =>
               return "PMGQq";
            when PMHQq =>
               return "PMHQq";
            when POAQq_POCQq_POEQq_POFQq =>
               return "{POA|POC|POE|POF}Qq";
            when POBQq_PODQq =>
               return "{POB|POD}Qq";
            when POGQq_POLQq =>
               return "{POG|POL}Qq";
            when POHQq_POKQq =>
               return "{POH|POK}Qq";
            when others =>
               return "P??Qq";
         end case;
      end IO_skeleton;

   begin
      case encoding and 8#77# is
         when JCqNZS =>
            return "JCqNZS";
         when MkMq =>
            return "MkMq";
         when MkMqQ =>
            return "MkMqQ";
         when MkMqH =>
            return "MkMqH";
         when MkMqQH =>
            return "MkMqQH";
         when MkMqN =>
            return "MkMqN";
         when MkMqQN =>
            return "MkMqQN";
         when MkMqHN =>
            return "MkMqHN";
         when MkMqQHN =>
            return "MkMqQHN";
         when TO_MkMq =>
            return "=MkMq";
         when TO_MkMqQ =>
            return "=MkMqQ";
         when TO_MkMqH =>
            return "=MkMqH";
         when TO_MkMqQH =>
            return "=MkMqQH";
         when TO_MkMqN =>
            return "=MkMqN";
         when TO_MkMqQN =>
            return "=MkMqQN";
         when TO_MkMqHN =>
            return "=MkMqHN";
         when TO_MkMqQHN =>
            return "=MkMqQHN";
         when M_PLUS_Iq =>
            return "M+Iq";
         when M_MINUS_Iq =>
            return "M-Iq";
         when NCq =>
            return "NCq";
         when DCq =>
            return "DCq";
         when POS1_TO_Iq =>
            return "Iq=+1";
         when NEG1_TO_Iq =>
            return "Iq=-1";
         when POS2_TO_Iq =>
            return "Iq=+2";
         when NEG2_TO_Iq =>
            return "Iq=-2";
         when MqTOQk =>
            return "MqTOQk";
         when IqTOQk =>
            return "IqTOQk";
         when IMqTOQk =>
            return "IMqTOQk";
         when CqTOQk =>
            return "CqTOQk";
         when CMqTOQk =>
            return "CMqTOQk";
         when CIqTOQk =>
            return "CIqTOQk";
         when QqTOQk =>
            return "QqTOQk";
         when QCIMq =>
            return "{Q|C|I|M}q";
         when TO_RCIMq =>
            return "=[R]{Q|C|I|M}q";
         when ADD_TO_QCIMq =>
            return "=+{Q|C|I|M}q";
         when SHA =>
            return "SHA";
         when SHAD =>
            return "SHAD";
         when MACC =>
            return "×+";
         when SHL =>
            return "SHL";
         when SHLD =>
            return "SHLD";
         when SHC =>
            return "SHC";
         when TO_Kk =>
            case encoding mod 16 is
               when K0 =>
                  return "=K0";
               when K1 =>
                  return "=K1";
               when K2 =>
                  return "=K2";
               when K3 =>
                  return "=K3";
               when others =>
                  return "=K?";
            end case;
         when Kk =>
            case encoding mod 16 is
               when K4 =>
                  return "K4";
               when K5 =>
                  return "K5";
               when K7 =>
                  return "K7";
               when others =>
                  return "K?";
            end case;
         when LINK =>
            return "LINK";
         when TO_LINK =>
            return "=LINK";
         when others =>
            return IO_skeleton (encoding);
      end case;
   end two_syllable_skeleton;

   function normal_jump_skeleton (encoding : KDF9.syllable)
   return String is
   begin
      case encoding and 8#77# is
         when JrEQ =>
            return "JrEQ";
         when JrGTZ =>
            return "JrGTZ";
         when JrLTZ =>
            return "JrLTZ";
         when JrEQZ =>
            return "JrEQZ";
         when JrV =>
            return "JrV";
         when JrEN =>
            return "JrEN";
         when Jr =>
            return "Jr";
         when JrEJ =>
            return "JrEJ";
         when JSr =>
            return "JSr";
         when JrTR =>
            return "JrTR";
         when EXIT_9 =>
            return "EXIT";
         when JrNE =>
            return "JrNE";
         when JrLEZ =>
            return "JrLEZ";
         when JrGEZ =>
            return "JrGEZ";
         when JrNEZ =>
            return "JrNEZ";
         when JrNV =>
            return "JrNV";
         when JrNEN =>
            return "JrNEN";
         when JrNEJ =>
            return "JrNEJ";
         when JrNTR =>
            return "JrNTR";
         when OUT_9 =>
            return "OUT";
         when EXITD =>
            return "EXITD";
         when JrCqZ .. JrCqZ+2#1111# =>
            return "JrCqZ";
         when JrCqNZ .. JrCqNZ+2#1111# =>
            return "JrCqNZ";
         when others =>
            return "Jr?";
      end case;
   end normal_jump_skeleton;

   function data_access_skeleton (decoded : KDF9.decoded_order)
   return String is
   begin
      case decoded.syndrome is
         when EaMq =>
            return "EeMq";
         when TO_EaMq =>
            return "=EeMq";
         when EaMqQ =>
            return "EeMqQ";
         when TO_EaMqQ =>
            return "=EeMqQ";
         when SET =>
            return "SET";
         when others =>
            return machine_code(decoded);
      end case;
   end data_access_skeleton;

   function the_skeleton_order (syllable_0 : KDF9.syllable)
   return String is
      its_INS : KDF9.decoded_order;
   begin
      its_INS.order := (syllable_0, 0, 0);
      case KDF9.INS_kind(syllable_0 / 2**6) is
         when one_syllable_order =>
            decode(its_INS);
            return one_syllable_order_name(its_INS);
         when two_syllable_order =>
            return two_syllable_skeleton(syllable_0);
         when normal_jump_order =>
            return normal_jump_skeleton(syllable_0);
         when data_access_order =>
            decode(its_INS);
            return data_access_skeleton(its_INS);
      end case;
   end the_skeleton_order;

end disassembly;
