-- Produce dis-assembled instructions in an approximation to KDF9 Usercode.
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

with KDF9.imaging;
with disassembly.symbols;
with KDF9.CPU;
with KDF9.decoding;
with string_editing;

use  KDF9.imaging;
use  disassembly.symbols;
use  KDF9.CPU;
use  KDF9.decoding;
use  string_editing;

package body disassembly is

   function flagged (flag : String; s : KDF9.syllable)
   return String
   is (flag & oct_of(s));

   function machine_code (decoded : KDF9.decoded_order)
   return String
   is (
       case decoded.kind is
          when one_syllable_order => flagged("#", decoded.order.syllable_0),

          when two_syllable_order => flagged("#", decoded.order.syllable_0)
                                   & flagged(":", decoded.order.syllable_1),
          when normal_jump_order
             | data_access_order  => flagged("#", decoded.order.syllable_0)
                                   & flagged(":", decoded.order.syllable_1)
                                   & flagged(":", decoded.order.syllable_2)
      );

   function one_syllable_name (decoded : KDF9.decoded_order)
   return String
   is (
       case decoded.compressed_opcode is
          when ABS_9   => "ABS",
          when ABSF    => "ABSF",
          when AND_9   => "AND",
          when BITS    => "BITS",
          when CAB     => "CAB",
          when CONT    => "CONT",
          when DIV     => "/",
          when DIVD    => "/D",
          when DIVDF   => "/DF",
          when DIVF    => "/F",
          when DIVI    => "/I",
          when DIVR    => "/R",
          when DUMMY   => "DUMMY",
          when DUP     => "DUP",
          when DUPD    => "DUPD",
          when ERASE   => "ERASE",
          when FIX     => "FIX",
          when FLOAT_9 => "FLOAT",
          when FLOATD  => "FLOATD",
          when FRB     => "FRB",
          when MAX     => "MAX",
          when MAXF    => "MAXF",
          when MINUS   => "-",
          when MINUSD  => "-D",
          when MINUSDF => "-DF",
          when MINUSF  => "-F",
          when NEG     => "NEG",
          when NEGD    => "NEGD",
          when NEGDF   => "NEGDF",
          when NEGF    => "NEGF",
          when NEV     => "NEV",
          when NOT_9   => "NOT",
          when OR_9    => "OR",
          when PERM    => "PERM",
          when PLUS    => "+",
          when PLUSD   => "+D",
          when PLUSDF  => "+DF",
          when PLUSF   => "+F",
          when REV     => "REV",
          when REVD    => "REVD",
          when ROUND   => "ROUND",
          when ROUNDF  => "ROUNDF",
          when ROUNDH  => "ROUNDH",
          when ROUNDHF => "ROUNDHF",
          when SIGN    => "SIGN",
          when SIGNF   => "SIGNF",
          when STAND   => "STAND",
          when STR     => "STR",
          when TO_TR   => "=TR",
          when TOB     => "TOB",
          when VR      => "VR",
          when X_frac  => "×",
          when XD      => "×D",
          when XDF     => "×DF",
          when XF      => "×F",
          when XPLUSF  => "×+F",
          when ZERO    => "ZERO",
          when 0       => "DUMMY0",
          when others  =>  machine_code(decoded)
       );

   function two_syllable_name (decoded : KDF9.decoded_order)
   return String is

      default : constant String := machine_code(decoded);
      invalid : constant String := "";
      k       : constant String := trimmed(decoded.Qk'Image);
      q       : constant String := trimmed(decoded.Qq'Image);
      opcode  : constant KDF9.compressed_opcode := (decoded.Qk and not manual_bit);
      CT      : constant Boolean := (decoded.Qk and manual_bit) = 0;

      function IO_order (stem : String)
      return String
      is (if stem = invalid then default else stem & "Q" & q);

      function IO_name
      return String
      is (
          case decoded.compressed_opcode is
               when PIA_PIC_CLO_TLO_Qq =>
                                   IO_order(case opcode is
                                               when PIA_bits => "PIA",
                                               when PIC_bits => "PIC",
                                               when CLO_bits => "CLO",
                                               when TLO_bits => "TLO",
                                               when others   => invalid),
               when PIB_PID_Qq =>
                                   IO_order(case opcode is
                                               when PIB_bits => "PIB",
                                               when PID_bits => "PID",
                                               when others   => invalid),
               when PIE_PIG_Qq =>
                                   IO_order(case opcode is
                                               when PIE_bits => "PIE",
                                               when PIG_bits => "PIG",
                                               when others   => invalid),
               when PIF_PIH_Qq =>
                                   IO_order(case opcode is
                                               when PIF_bits => "PIF",
                                               when PIH_bits => "PIH",
                                               when others   => invalid),
               when PMA_PMK_INT_Qq =>
                                   IO_order(case opcode is
                                               when PMA_bits => "PMA",
                                               when PMK_bits => "PMK",
                                               when INT_bits => "INT",
                                               when others   => invalid),
               when CT_PMB_PMC_BUSY_Qq =>
                                   IO_order(case opcode is
                                               when PMB_bits  => "PMB",
                                               when PMC_bits  => "PMC",
                                               when BUSY_bits => "BUSY",
                                               when CTQ_bits => (if CT then "CT" else "MANUAL"),
                                               when others    => invalid),
               when PMD_PME_PML_Qq =>
                                   IO_order(case opcode is
                                               when PMD_bits => "PMD",
                                               when PME_bits => "PME",
                                               when PML_bits => "PML",
                                               when others   => invalid),
               when PMF_PMG_Qq =>
                                   IO_order(case opcode is
                                               when PMF_bits => "PMF",
                                               when PMG_bits => "PMG",
                                               when others   => invalid),
               when POA_POC_POE_POF_PMH_Qq =>
                                   IO_order(case opcode is
                                               when POA_bits => "POA",
                                               when POC_bits => "POC",
                                               when POE_bits => "POE",
                                               when POF_bits => "POF",
                                               when PMH_bits => "PMH",
                                               when others   => invalid),
               when POB_POD_Qq =>
                                   IO_order(case opcode is
                                               when POB_bits => "POB",
                                               when POD_bits => "POD",
                                               when others   => invalid),
               when POG_POL_Qq =>
                                   IO_order(case opcode is
                                               when POG_bits => "POG",
                                               when POL_bits => "POL",
                                               when others   => invalid),
               when POH_POK_Qq =>
                                   IO_order(case opcode is
                                               when POH_bits => "POH",
                                               when POL_bits => "POK",
                                               when others   => invalid),
               when PAR_Qq =>      IO_order("PAR"),
               when others =>      IO_order(invalid)
         );

      function indirect_store_name (suffix : String := "")
      return String
      is ("=M" & k & "M" & q & suffix);

      function indirect_fetch_name (suffix : String := "")
      return String
      is ("M" & k & "M" & q & suffix);

      function Qq_to_Qk_name (part : String)
      return String
      is (part & q & " TO Q" & k);

      function Qq_name (action : String; suffix : String := "")
      return String
      is (action & q & suffix);

      function shift_count
      return String is
         constant_flag : constant := 1;
         fixed_shift   : CPU.signed_Q_part;
      begin
         if (decoded.order.syllable_1 and constant_flag) /= 0  then
            fixed_shift := resign(KDF9.Q_part(decoded.order.syllable_1/2));
            if fixed_shift > 63 then
               fixed_shift := fixed_shift - 128;
            end if;
            return (if fixed_shift < 0 then "" else "+") & trimmed(fixed_shift'Image);
         else
            return "C" & q;
         end if;
      end shift_count;

      function shift_name (action : String)
      return String
      is (action & shift_count);

   begin -- two_syllable_name
      return
         (
          case decoded.compressed_opcode is
             when MkMq       => indirect_fetch_name,
             when MkMqQ      => indirect_fetch_name(suffix => "Q"),
             when MkMqH      => indirect_fetch_name(suffix => "H"),
             when MkMqQH     => indirect_fetch_name(suffix => "QH"),
             when MkMqN      => indirect_fetch_name(suffix => "N"),
             when MkMqQN     => indirect_fetch_name(suffix => "QN"),
             when MkMqHN     => indirect_fetch_name(suffix => "HN"),
             when MkMqQHN    => indirect_fetch_name(suffix => "QHN"),

             when TO_MkMq    => indirect_store_name,
             when TO_MkMqQ   => indirect_store_name(suffix => "Q"),
             when TO_MkMqH   => indirect_store_name(suffix => "H"),
             when TO_MkMqQH  => indirect_store_name(suffix => "QH"),
             when TO_MkMqN   => indirect_store_name(suffix => "N"),
             when TO_MkMqQN  => indirect_store_name(suffix => "QN"),
             when TO_MkMqHN  => indirect_store_name(suffix => "HN"),
             when TO_MkMqQHN => indirect_store_name(suffix => "QHN"),

             when M_PLUS_Iq  => Qq_name("M+I"),
             when M_MINUS_Iq => Qq_name("M-I"),
             when NCq        => Qq_name("NC"),
             when DCq        => Qq_name("DC"),
             when POS1_TO_Iq => Qq_name("I",  suffix => "=+1"),
             when NEG1_TO_Iq => Qq_name("I",  suffix => "=-1"),
             when POS2_TO_Iq => Qq_name("I",  suffix => "=+2"),
             when NEG2_TO_Iq => Qq_name("I",  suffix => "=+2"),
             when JCqNZS     => Qq_name("JC", suffix => "NZS"),

             when MqTOQk     => Qq_to_Qk_name("M"),
             when IqTOQk     => Qq_to_Qk_name("I"),
             when IMqTOQk    => Qq_to_Qk_name("IM"),
             when CqTOQk     => Qq_to_Qk_name("C"),
             when CMqTOQk    => Qq_to_Qk_name("CM"),
             when CIqTOQk    => Qq_to_Qk_name("CI"),
             when QqTOQk     => Qq_to_Qk_name("Q"),
             when QCIMq =>
                (
                 if (decoded.Qk and all_Q_choice) = all_Q_choice then  Qq_name("Q")
                 elsif (decoded.Qk and M_part_choice) /= 0       then  Qq_name("M")
                 elsif (decoded.Qk and C_part_choice) /= 0       then  Qq_name("C")
                 elsif (decoded.Qk and I_part_choice) /= 0       then  Qq_name("I")
                 else  default
                ),
             when TO_RCIMq =>
                (
                 if (decoded.Qk and all_Q_choice) = all_Q_choice then Qq_name("=Q")
                 elsif (decoded.Qk and M_part_choice) /= 0 then
                    Qq_name(if (decoded.Qk and reset_choice) /= 0 then "=RM" else "=M")
                 elsif (decoded.Qk and C_part_choice) /= 0 then
                    Qq_name(if (decoded.Qk and reset_choice) /= 0 then "=RC" else "=C")
                 elsif (decoded.Qk and I_part_choice) /= 0 then
                    Qq_name(if (decoded.Qk and reset_choice) /= 0 then "=RI" else "=I")
                 else default
                ),
             when ADD_TO_QCIMq =>
                (
                 if (decoded.Qk and all_Q_choice) = all_Q_choice then Qq_name("=+Q")
                 elsif (decoded.Qk and M_part_choice) /= 0       then Qq_name("=+M")
                 elsif (decoded.Qk and C_part_choice) /= 0       then Qq_name("=+C")
                 elsif (decoded.Qk and I_part_choice) /= 0       then Qq_name("=+I")
                 else  default
                ),

             when SHA   => shift_name("SHA"),
             when SHAD  => shift_name("SHAD"),
             when MACC  => shift_name("×+"),
             when SHL   => shift_name("SHL"),
             when SHLD  => shift_name("SHLD"),
             when SHC   => shift_name("SHC"),

             when TO_Kq =>
                (
                 case decoded.Qq is
                    when K0 => "=K0",
                    when K1 => "=K1",
                    when K2 => "=K2",
                    when K3 => "=K3",
                    when others => default
                ),
             when Kk =>
                (
                 case decoded.Qk is
                   when K4 => "K4",
                   when K5 => "K5",
                   when K7 => "K7",
                   when others => default
                ),

             when LINK    => "LINK",
             when TO_LINK => "=LINK",

             when others  => IO_name
          );
   end two_syllable_name;

   function closer (
                    decoded  : KDF9.decoded_order;
                    address  : KDF9.syllable_address := (0, 0);
                    in_octal : Boolean := True
                   )   return String
   is (
       if decoded.kind = normal_jump_order and decoded.compressed_opcode = JSr
       then ";(LINK=" & oct_or_dec_of(address, in_octal) & "); "
       else "; "
      );

   function normal_jump_name (
                              decoded   : KDF9.decoded_order;
                              in_octal  : Boolean := True
                             )
   return String is

      the_target  : constant KDF9.syllable_address  := decoded.target;
      the_symbol  : constant String := code_operand(the_target, in_octal);
      num_remark  : constant String
                  := (
                      if   in_octal
                      then ";("  & dec_of(KDF9.Q_part(the_target.code_address))
                      else ";(#" & oct_of(the_target.code_address)
                     )
                  & ")";
      sym_remark  : constant String
                  := (
                      if   in_octal
                      then ";(#" & oct_of(the_target.code_address)
                      else ";("  & dec_of(KDF9.Q_part(the_target.code_address))
                     )
                   & ")";
      remark      : constant String
                  := (if the_symbol(the_symbol'First) = 'E' then num_remark else sym_remark);

      function jump (condition : String; name : String := "J")
      return String
      is (name & code_operand(the_target, in_octal) & condition & remark);

      function leave (and_how : String)
      return String
      is ("EXIT" & and_how);

   begin  -- normal_jump_name
      return (
              case decoded.compressed_opcode is
                 when JrEQ   => jump("="),
                 when JrGTZ  => jump("GTZ"),
                 when JrLTZ  => jump("LTZ"),
                 when JrEQZ  => jump("=Z"),
                 when JrV    => jump("V"),
                 when JrEN   => jump("EN"),
                 when Jr     => jump(""),
                 when JrEJ   => jump("EJ"),
                 when JrTR   => jump("TR"),
                 when JrNE   => jump("±"),
                 when JrLEZ  => jump("LEZ"),
                 when JrGEZ  => jump("GEZ"),
                 when JrNEZ  => jump("±Z"),
                 when JrNV   => jump("NV"),
                 when JrNEN  => jump("NEN"),
                 when JrNEJ  => jump("NEJ"),
                 when JrNTR  => jump("NTR"),
                 when JrCqZ  => jump("C" & trimmed(decoded.Qq'Image) & "Z"),
                 when JrCqNZ => jump("C" & trimmed(decoded.Qq'Image) & "NZ"),
                 when JSr    => jump("", name => "JS"),
                 when OS_OUT => "OUT",
                 when EXITD  => leave("D"),
                 when EXIT_n =>
                    -- Try to give the most helpful interpretation of the operand.
                    (
                     if the_target.syllable_index = 0 then  -- c.f. decode_a_jump_order.
                        -- No halfword offset applies.
                        (
                         if the_target.code_address < 4 then
                           leave(
                                 if the_target.code_address = 0
                                 then ""
                                 else oct_of(KDF9.Q_part(2*the_target.code_address), 1)
                                )
                         else
                           leave("AE" & oct_or_dec_of((the_target.code_address, 0), in_octal))
                        )
                     elsif the_target.code_address < 4 then
                        leave(oct_of(KDF9.Q_part(2*the_target.code_address + 1), 1))
                     else
                        leave("AE" & oct_or_dec_of((the_target.code_address, 3), in_octal))
                    ),

                 when others =>  machine_code(decoded)
             );
   end normal_jump_name;

   function data_access_name (
                              decoded       : KDF9.decoded_order;
                              in_octal      : Boolean
                             )
   return String is
      opcode        : constant KDF9.compressed_opcode := decoded.compressed_opcode;
      operand       : KDF9.Q_part renames decoded.operand;
      number        : constant String := oct_or_dec_of(operand, in_octal);
      the_bare_name : constant String := (
                                          if operand not in KDF9.address
                                          then "E" & number
                                          else data_operand(operand, in_octal)
                                         );

      Qq        : KDF9.Q_number    renames decoded.Qq;
      M_suffix  : constant String  := (if Qq /= 0 then "M" & trimmed(Qq'Image) else "");
      Q_suffix  : constant String  := (if opcode in EaMqQ | TO_EaMqQ then "Q" else "");
      modifier  : constant String  := M_suffix & Q_suffix;
      remark    : constant String
                := (
                    if operand < 8 or the_bare_name(1) = 'E'
                    then ""
                    else ";(" & number & ")"
                   );

      the_name : constant String := the_bare_name & modifier & remark;

   begin
      return
             (
              case opcode is
                 when EaMq
                    | EaMqQ    => the_name,
                 when TO_EaMq
                    | TO_EaMqQ => "=" & the_name,
                 when SET      => "SET" & SET_operand(operand, in_octal),
                 when others   => "?"
             );
   end data_access_name;

   function the_full_name_of (order : KDF9.decoded_order; in_octal : Boolean)
   return String is
      result : constant String
         := (
             case order.kind is
                when one_syllable_order => one_syllable_name(order),
                when two_syllable_order => two_syllable_name(order),
                when normal_jump_order  => normal_jump_name(order, in_octal),
                when data_access_order  => data_access_name(order, in_octal)
            );
   begin
      return (if result(1) /= '?' then result else "an INVALID order");
   end the_full_name_of;

   function the_code_and_name_of_INS
   return String
   is (machine_code(INS) & ", i.e. " & the_full_name_of(INS, in_octal => True));

   function two_syllable_skeleton (encoding : KDF9.syllable)
   return String is

      function IO_skeleton
      return String
      is (
          case encoding and 8#77# is
             when POA_POC_POE_POF_PMH_Qq => "{POA|POC|POE|POF|PMH}Qq",
             when PIA_PIC_CLO_TLO_Qq     => "{PIA|PIC|CLO|TLO}Qq",
             when CT_PMB_PMC_BUSY_Qq     => "{BUSY|CT|MANUAL|PMB|PMC}Qq",
             when PAR_Qq                 => "PARQq",
             when PIB_PID_Qq             => "{PIB|PID}Qq",
             when PIE_PIG_Qq             => "{PIE|PIG}Qq",
             when PIF_PIH_Qq             => "{PIF|PIH}Qq",
             when PMA_PMK_INT_Qq         => "{INT|PMA|PMK}Qq",
             when PMD_PME_PML_Qq         => "{PMD|PME}Qq",
             when PMF_PMG_Qq             => "{PMF|PMG}Qq",
             when POB_POD_Qq             => "{POB|POD}Qq",
             when POG_POL_Qq             => "{POG|POL}Qq",
             when POH_POK_Qq             => "{POH|POK}Qq",
             when others                 => "invalid IO group syllable #" & oct_of(encoding)
         );

   begin  -- two_syllable_skeleton
      return
         (
          case encoding and 8#77# is
             when MkMq         => "MkMq",
             when MkMqQ        => "MkMqQ",
             when MkMqH        => "MkMqH",
             when MkMqQH       => "MkMqQH",
             when MkMqN        => "MkMqN",
             when MkMqQN       => "MkMqQN",
             when MkMqHN       => "MkMqHN",
             when MkMqQHN      => "MkMqQHN",

             when TO_MkMq      => "=MkMq",
             when TO_MkMqQ     => "=MkMqQ",
             when TO_MkMqH     => "=MkMqH",
             when TO_MkMqQH    => "=MkMqQH",
             when TO_MkMqN     => "=MkMqN",
             when TO_MkMqQN    => "=MkMqQN",
             when TO_MkMqHN    => "=MkMqHN",
             when TO_MkMqQHN   => "=MkMqQHN",

             when JCqNZS       => "JCqNZS",
             when M_PLUS_Iq    => "M+Iq",
             when M_MINUS_Iq   => "M-Iq",
             when NCq          => "NCq",
             when DCq          => "DCq",
             when POS1_TO_Iq   => "Iq=+1",
             when NEG1_TO_Iq   => "Iq=-1",
             when POS2_TO_Iq   => "Iq=+2",
             when NEG2_TO_Iq   => "Iq=-2",

             when MqTOQk       => "MqTOQk",
             when IqTOQk       => "IqTOQk",
             when IMqTOQk      => "IMqTOQk",
             when CqTOQk       => "CqTOQk",
             when CMqTOQk      => "CMqTOQk",
             when CIqTOQk      => "CIqTOQk",
             when QqTOQk       => "QqTOQk",

             when QCIMq        => "{Q|C|I|M}q",
             when TO_RCIMq     => "=[R]{Q|C|I|M}q",
             when ADD_TO_QCIMq => "=+{Q|C|I|M}q",

             when SHA          => "SHA",
             when SHAD         => "SHAD",
             when MACC         => "×+",
             when SHL          => "SHL",
             when SHLD         => "SHLD",
             when SHC          => "SHC",

             when TO_Kq =>
                (
                 case encoding / 16 mod 16 is
                    when K0 => "=K0",
                    when K1 => "=K1",
                    when K2 => "=K2",
                    when K3 => "=K3",
                    when others => "=K?"
                ),
             when Kk =>
                (
                 case encoding mod 16 is
                    when K4 => "K4",
                    when K5 => "K5",
                    when K7 => "K7",
                    when others => "K?"
                ),

             when LINK =>    "LINK",
             when TO_LINK => "=LINK",

             when others =>  IO_skeleton
         );
   end two_syllable_skeleton;

   function normal_jump_skeleton (encoding : KDF9.syllable)
   return String
   is (
       case encoding and 8#77# is
          when JrCqZ  .. JrCqZ+2#1111#  => "JrCqZ",
          when JrCqNZ .. JrCqNZ+2#1111# => "JrCqNZ",
          when JrEQ   => "Jr=",
          when JrGTZ  => "JrGTZ",
          when JrLTZ  => "JrLTZ",
          when JrEQZ  => "Jr=Z",
          when JrV    => "JrV",
          when JrEN   => "JrEN",
          when Jr     => "Jr",
          when JrEJ   => "JrEJ",
          when JSr    => "JSr",
          when JrTR   => "JrTR",
          when EXIT_n => "EXIT",
          when JrNE   => "Jr±",
          when JrLEZ  => "JrLEZ",
          when JrGEZ  => "JrGEZ",
          when JrNEZ  => "Jr±Z",
          when JrNV   => "JrNV",
          when JrNEN  => "JrNEN",
          when JrNEJ  => "JrNEJ",
          when JrNTR  => "JrNTR",
          when OS_OUT => "OUT",
          when EXITD  => "EXITD",
          when others => "invalid jump group syllable #" & oct_of(encoding)
      );

   function data_access_skeleton (compressed_opcode : KDF9.compressed_opcode)
   return String
   is (
       case compressed_opcode is
          when EaMq     => "EeMq",
          when TO_EaMq  => "=EeMq",
          when EaMqQ    => "EeMqQ",
          when TO_EaMqQ => "=EeMqQ",
          when SET      => "SET",
          when others   => "invalid data access compressed opcode #" & oct_of(compressed_opcode)
      );

   function the_short_name_of (syllable_0 : KDF9.syllable)
   return String is
      its_INS : KDF9.decoded_order := (order => (syllable_0, 0, 0), others => <>);
   begin
      decode(its_INS);
      return
         (
          case KDF9.INS_kind(syllable_0 / 2**6) is
             when one_syllable_order   => one_syllable_name(its_INS),
             when two_syllable_order   => two_syllable_skeleton(syllable_0),
             when normal_jump_order    => normal_jump_skeleton(syllable_0),
             when data_access_order    => data_access_skeleton(its_INS.compressed_opcode)
         );
   end the_short_name_of;

end disassembly;
