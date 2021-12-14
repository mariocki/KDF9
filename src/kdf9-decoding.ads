-- The "compressed_opcode" values are effective opcodes, partially decoded from the first syllable,
--   and combined with opcode bits of the second syllable, where appropriate (e.g. in jumps).
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

package KDF9.decoding is

   -- The compressed_opcode values for 1-syllable orders are equal to their full codes.

   ALL_0    : constant KDF9.compressed_opcode := 2#000_000#;
   VR       : constant KDF9.compressed_opcode := 2#000_001#;
   TO_TR    : constant KDF9.compressed_opcode := 2#000_010#;
   BITS     : constant KDF9.compressed_opcode := 2#000_011#;
   XF       : constant KDF9.compressed_opcode := 2#000_100#;
   XDF      : constant KDF9.compressed_opcode := 2#000_101#;
   INV006   : constant KDF9.compressed_opcode := 2#000_110#;
   XPLUSF   : constant KDF9.compressed_opcode := 2#000_111#;
   NEGD     : constant KDF9.compressed_opcode := 2#001_000#;
   OR_9     : constant KDF9.compressed_opcode := 2#001_001#;
   PERM     : constant KDF9.compressed_opcode := 2#001_010#;
   TOB      : constant KDF9.compressed_opcode := 2#001_011#;
   ROUNDH   : constant KDF9.compressed_opcode := 2#001_100#;
   NEV      : constant KDF9.compressed_opcode := 2#001_101#;
   ROUND    : constant KDF9.compressed_opcode := 2#001_110#;
   DUMMY    : constant KDF9.compressed_opcode := 2#001_111#;
   ROUNDF   : constant KDF9.compressed_opcode := 2#010_000#;
   ROUNDHF  : constant KDF9.compressed_opcode := 2#010_001#;
   MINUSDF  : constant KDF9.compressed_opcode := 2#010_010#;
   PLUSDF   : constant KDF9.compressed_opcode := 2#010_011#;
   FLOAT_9  : constant KDF9.compressed_opcode := 2#010_100#;
   FLOATD   : constant KDF9.compressed_opcode := 2#010_101#;
   ABS_9    : constant KDF9.compressed_opcode := 2#010_110#;
   NEG      : constant KDF9.compressed_opcode := 2#010_111#;
   ABSF     : constant KDF9.compressed_opcode := 2#011_000#;
   NEGF     : constant KDF9.compressed_opcode := 2#011_001#;
   MAX      : constant KDF9.compressed_opcode := 2#011_010#;
   NOT_9    : constant KDF9.compressed_opcode := 2#011_011#;
   XD       : constant KDF9.compressed_opcode := 2#011_100#;
   X_frac   : constant KDF9.compressed_opcode := 2#011_101#;
   MINUS    : constant KDF9.compressed_opcode := 2#011_110#;
   SIGN     : constant KDF9.compressed_opcode := 2#011_111#;
   INV040   : constant KDF9.compressed_opcode := 2#100_000#;
   ZERO     : constant KDF9.compressed_opcode := 2#100_001#;
   DUP      : constant KDF9.compressed_opcode := 2#100_010#;
   DUPD     : constant KDF9.compressed_opcode := 2#100_011#;
   DIVI     : constant KDF9.compressed_opcode := 2#100_100#;
   FIX      : constant KDF9.compressed_opcode := 2#100_101#;
   INV046   : constant KDF9.compressed_opcode := 2#100_110#;
   STR      : constant KDF9.compressed_opcode := 2#100_111#;
   CONT     : constant KDF9.compressed_opcode := 2#101_000#;
   REVD     : constant KDF9.compressed_opcode := 2#101_001#;
   ERASE    : constant KDF9.compressed_opcode := 2#101_010#;
   MINUSD   : constant KDF9.compressed_opcode := 2#101_011#;
   AND_9    : constant KDF9.compressed_opcode := 2#101_100#;
   INV055   : constant KDF9.compressed_opcode := 2#101_101#;
   PLUS     : constant KDF9.compressed_opcode := 2#101_110#;
   PLUSD    : constant KDF9.compressed_opcode := 2#101_111#;
   DIV      : constant KDF9.compressed_opcode := 2#110_000#;
   DIVD     : constant KDF9.compressed_opcode := 2#110_001#;
   DIVF     : constant KDF9.compressed_opcode := 2#110_010#;
   DIVDF    : constant KDF9.compressed_opcode := 2#110_011#;
   DIVR     : constant KDF9.compressed_opcode := 2#110_100#;
   REV      : constant KDF9.compressed_opcode := 2#110_101#;
   CAB      : constant KDF9.compressed_opcode := 2#110_110#;
   FRB      : constant KDF9.compressed_opcode := 2#110_111#;
   STAND    : constant KDF9.compressed_opcode := 2#111_000#;
   NEGDF    : constant KDF9.compressed_opcode := 2#111_001#;
   MAXF     : constant KDF9.compressed_opcode := 2#111_010#;
   INV073   : constant KDF9.compressed_opcode := 2#111_011#;
   PLUSF    : constant KDF9.compressed_opcode := 2#111_100#;
   MINUSF   : constant KDF9.compressed_opcode := 2#111_101#;
   INV076   : constant KDF9.compressed_opcode := 2#111_110#;
   SIGNF    : constant KDF9.compressed_opcode := 2#111_111#;


   -- compressed_opcode values for 2-syllable indirect fetch and store orders

   MkMq       : constant KDF9.compressed_opcode := 2#000_000#;
   MkMqQ      : constant KDF9.compressed_opcode := 2#000_010#;
   MkMqH      : constant KDF9.compressed_opcode := 2#000_100#;
   MkMqQH     : constant KDF9.compressed_opcode := 2#000_110#;
   MkMqN      : constant KDF9.compressed_opcode := 2#001_000#;
   MkMqQN     : constant KDF9.compressed_opcode := 2#001_010#;
   MkMqHN     : constant KDF9.compressed_opcode := 2#001_100#;
   MkMqQHN    : constant KDF9.compressed_opcode := 2#001_110#;

   TO_MkMq    : constant KDF9.compressed_opcode := 2#000_001#;
   TO_MkMqQ   : constant KDF9.compressed_opcode := 2#000_011#;
   TO_MkMqH   : constant KDF9.compressed_opcode := 2#000_101#;
   TO_MkMqQH  : constant KDF9.compressed_opcode := 2#000_111#;
   TO_MkMqN   : constant KDF9.compressed_opcode := 2#001_001#;
   TO_MkMqQN  : constant KDF9.compressed_opcode := 2#001_011#;
   TO_MkMqHN  : constant KDF9.compressed_opcode := 2#001_101#;
   TO_MkMqQHN : constant KDF9.compressed_opcode := 2#001_111#;


   -- compressed_opcode values for 2-syllable Q store orders

   M_PLUS_Iq    : constant KDF9.compressed_opcode := 2#100_000#;
   M_MINUS_Iq   : constant KDF9.compressed_opcode := 2#100_001#;
   NCq          : constant KDF9.compressed_opcode := 2#100_010#;
   DCq          : constant KDF9.compressed_opcode := 2#100_011#;
   POS1_TO_Iq   : constant KDF9.compressed_opcode := 2#100_100#;
   NEG1_TO_Iq   : constant KDF9.compressed_opcode := 2#100_101#;
   POS2_TO_Iq   : constant KDF9.compressed_opcode := 2#100_110#;
   NEG2_TO_Iq   : constant KDF9.compressed_opcode := 2#100_111#;

   MqTOQk       : constant KDF9.compressed_opcode := 2#101_001#;
   IqTOQk       : constant KDF9.compressed_opcode := 2#101_010#;
   IMqTOQk      : constant KDF9.compressed_opcode := 2#101_011#;
   CqTOQk       : constant KDF9.compressed_opcode := 2#101_100#;
   CMqTOQk      : constant KDF9.compressed_opcode := 2#101_101#;
   CIqTOQk      : constant KDF9.compressed_opcode := 2#101_110#;
   QqTOQk       : constant KDF9.compressed_opcode := 2#101_111#;

   SHA          : constant KDF9.compressed_opcode := 2#110_001#;
   SHAD         : constant KDF9.compressed_opcode := 2#110_010#;
   MACC         : constant KDF9.compressed_opcode := 2#110_011#;
   SHL          : constant KDF9.compressed_opcode := 2#110_100#;
   SHLD         : constant KDF9.compressed_opcode := 2#110_110#;
   SHC          : constant KDF9.compressed_opcode := 2#110_111#;
   constant_bit : constant := 1;

   TO_RCIMq     : constant KDF9.compressed_opcode := 2#111_000#;
   QCIMq        : constant KDF9.compressed_opcode := 2#111_001#;
   ADD_TO_QCIMq : constant KDF9.compressed_opcode := 2#111_010#;

   -- masks for Q store Qk bits

   reset_choice  : constant := 2#0001#;
   C_part_choice : constant := 2#1000#;
   I_part_choice : constant := 2#0100#;
   M_part_choice : constant := 2#0010#;
   all_Q_choice  : constant := C_part_choice + I_part_choice + M_part_choice;


   -- compressed_opcode values for 2-syllable SJNS orders

   LINK    : constant KDF9.compressed_opcode := 2#111_011#;
   TO_LINK : constant KDF9.compressed_opcode := 2#111_100#;


   -- compressed_opcode values for 2-syllable Director-only orders

   TO_Kq : constant KDF9.compressed_opcode := 2#111_101#;
   K0    : constant := 2#1000#;
   K1    : constant := 2#0100#;
   K2    : constant := 2#0010#;
   K3    : constant := 2#0001#;
   Kk    : constant KDF9.compressed_opcode := 2#111_110#;
   K4    : constant := 2#1000#;
   K5    : constant := 2#0100#;
   K7    : constant := 2#0001#;


   -- compressed_opcode value for 2-syllable short-loop jump order

   JCqNZS : constant KDF9.compressed_opcode := 2#111_111#;


   -- compressed_opcode values for 2-syllable I/O orders

   CT_PMB_PMC_BUSY_Qq     : constant KDF9.compressed_opcode := 2#010_000#;
   PAR_Qq                 : constant KDF9.compressed_opcode := 2#010_001#;
   PMF_PMG_Qq             : constant KDF9.compressed_opcode := 2#010_010#;
   PIA_PIC_CLO_TLO_Qq     : constant KDF9.compressed_opcode := 2#010_100#;
   PIB_PID_Qq             : constant KDF9.compressed_opcode := 2#010_101#;
   PIE_PIG_Qq             : constant KDF9.compressed_opcode := 2#010_110#;
   PIF_PIH_Qq             : constant KDF9.compressed_opcode := 2#010_111#;

   POA_POC_POE_POF_PMH_Qq : constant KDF9.compressed_opcode := 2#011_000#;
   POB_POD_Qq             : constant KDF9.compressed_opcode := 2#011_001#;
   POG_POL_Qq             : constant KDF9.compressed_opcode := 2#011_010#;
   POH_POK_Qq             : constant KDF9.compressed_opcode := 2#011_011#;
   PMA_PMK_INT_Qq         : constant KDF9.compressed_opcode := 2#011_100#;
   PMD_PME_PML_Qq         : constant KDF9.compressed_opcode := 2#011_110#;

   -- masks for I/O opcode extension bits (Qk field)

   PAR_bits  : constant := 2#0000#;

   -- PIA_PIC_CLO_TLO_Qq:
   PIA_bits  : constant := 2#0000#;
   PIC_bits  : constant := 2#1000#;
   CLO_bits  : constant := 2#0010#;
   TLO_bits  : constant := 2#0100#;

   -- PIB_PID_Qq:
   PIB_bits  : constant := 2#0000#;
   PID_bits  : constant := 2#1000#;

   -- PIE_PIG_Qq:
   PIE_bits  : constant := 2#0000#;
   PIG_bits  : constant := 2#1000#;

   -- PIF_PIH_Qq:
   PIF_bits  : constant := 2#0000#;
   PIH_bits  : constant := 2#1000#;

   -- PMA_PMK_INT_Qq:
   PMA_bits  : constant := 2#0000#;
   PMK_bits  : constant := 2#0100#;
   INT_bits  : constant := 2#0010#;

   -- CT_PMB_PMC_BUSY_Qq:
   CTQ_bits   : constant := 2#0000#;
   PMB_bits   : constant := 2#1000#;
   PMC_bits   : constant := 2#0100#;
   BUSY_bits  : constant := 2#0010#;
   manual_bit : constant := 2#0001#;

   -- PMD_PME_PML_Qq:
   PME_bits  : constant := 2#0000#;
   PMD_bits  : constant := 2#1000#;
   PML_bits  : constant := 2#0100#;

   -- PMF_PMG_Qq:
   PMF_bits  : constant := 2#0000#;
   PMG_bits  : constant := 2#0100#;

   -- POA_POC_POE_POF_PMH_Qq:
   POA_bits  : constant := 2#0000#;
   POC_bits  : constant := 2#1000#;
   POE_bits  : constant := 2#1100#;
   POF_bits  : constant := 2#0100#;
   PMH_bits  : constant := 2#0010#;

   -- POB_POD_Qq:
   POB_bits  : constant := 2#0000#;
   POD_bits  : constant := 2#1000#;

   -- POG_POL_Qq:
   POG_bits  : constant := 2#0000#;
   POL_bits  : constant := 2#1000#;

   -- POH_POK_Qq:
   POH_bits  : constant := 2#0000#;
   POK_bits  : constant := 2#1000#;


   -- compressed_opcode values for normal jump orders

   JrNE   : constant KDF9.compressed_opcode := 2#000_001#;
   JrGEZ  : constant KDF9.compressed_opcode := 2#000_010#;
   JrLEZ  : constant KDF9.compressed_opcode := 2#000_100#;
   JrNEZ  : constant KDF9.compressed_opcode := 2#000_110#;
   JrNV   : constant KDF9.compressed_opcode := 2#001_000#;
   OS_OUT : constant KDF9.compressed_opcode := 2#001_001#;
   JrNEN  : constant KDF9.compressed_opcode := 2#001_010#;
   Jr     : constant KDF9.compressed_opcode := 2#001_011#;
   JrNEJ  : constant KDF9.compressed_opcode := 2#001_100#;
   JSr    : constant KDF9.compressed_opcode := 2#001_101#;
   JrNTR  : constant KDF9.compressed_opcode := 2#001_110#;
   EXIT_n : constant KDF9.compressed_opcode := 2#001_111#;  -- 0h0 in bits 5-7
   JrEQ   : constant KDF9.compressed_opcode := 2#010_001#;
   JrLTZ  : constant KDF9.compressed_opcode := 2#010_010#;
   JrGTZ  : constant KDF9.compressed_opcode := 2#010_100#;
   JrEQZ  : constant KDF9.compressed_opcode := 2#010_110#;
   JrV    : constant KDF9.compressed_opcode := 2#011_000#;
   JrEN   : constant KDF9.compressed_opcode := 2#011_010#;
   JrEJ   : constant KDF9.compressed_opcode := 2#011_100#;
   JrTR   : constant KDF9.compressed_opcode := 2#011_110#;
   EXITD  : constant KDF9.compressed_opcode := 2#011_111#;  -- 010 in bits 5-7
   JrCqZ  : constant KDF9.compressed_opcode := 2#100_000#;
   JrCqNZ : constant KDF9.compressed_opcode := 2#110_000#;

   EXIT_1_bit : constant := 2#010#;  -- 0h0 in bits 5-7 of EXIT syllable_0


   -- compressed_opcode values for directly-addressed data access orders

   EaMq     : constant KDF9.compressed_opcode := 2#000_000#;
   TO_EaMq  : constant KDF9.compressed_opcode := 2#000_001#;
   EaMqQ    : constant KDF9.compressed_opcode := 2#000_010#;
   TO_EaMqQ : constant KDF9.compressed_opcode := 2#000_011#;
   SET      : constant KDF9.compressed_opcode := 2#000_100#;


end KDF9.decoding;
