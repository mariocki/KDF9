-- Support for KDF9 CPU/ALU operations that are not automatically inherited from
--   Ada types; and for types used in the internal functioning of the microcode.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Unchecked_Conversion;

package KDF9.CPU is

--
--
   --
   -- 48-bit integer and fractional ALU types and operations
   --
--
--

   type signed is range -2**47 .. +2**47 - 1 with Size => KDF9.word'Size;

   function unsign is new Ada.Unchecked_Conversion (CPU.signed, KDF9.word);

   function resign is new Ada.Unchecked_Conversion (KDF9.word, CPU.signed);

   function "-" (I : CPU.signed)
   return KDF9.word
      with Inline;

   function "abs" (I : CPU.signed)
   return KDF9.word
      with Inline;

   function "+" (L, R : CPU.signed)
   return KDF9.word
      with Inline;

   function "-" (L, R : CPU.signed)
   return KDF9.word
      with Inline;

   function "*" (L, R : CPU.signed)
   return KDF9.word;

   -- Determine the Quotient and Remainder of L/R, where:
   --    sign(Remainder) = sign(R) and |Remainder| < |R|, i.e. Remainder = L mod R;
   --    Quotient = (L - Remainder) / R.

   procedure do_DIVI (
                      L         : in KDF9.word;
                      R         : in KDF9.word;
                      Quotient,
                      Remainder : out KDF9.word
                     );

   -- Inherited signed single-length integer division is removed from the type.

   function "/" (L, R : CPU.signed)
   return KDF9.word is abstract;

   function "mod" (L, R : CPU.signed)
   return KDF9.word is abstract;

   -- Contract a double-word, setting the V bit if necessary.

   function contracted (P : KDF9.pair)
   return KDF9.word
      with Inline;

--
--
   -- Shifting operations with KDF9 semantics.
--
--

   type signed_Q_part is range  -2**15 .. +2**15 - 1 with Size => KDF9.Q_part'Size;

   function resign is new Ada.Unchecked_Conversion (KDF9.Q_part, CPU.signed_Q_part);

   -- L>0 for left-shift, L<0 for right-shift.

   function shift_logical (W : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word
      with Inline;

   function shift_circular (W : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word
      with Inline;

   -- shift_arithmetic rounds the result correctly.
   function shift_arithmetic (I : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word
      with Inline;

   -- number_of_1_bits_in counts the number of bits in W with value 1.
   function number_of_1_bits_in (W : KDF9.word)
   return KDF9.word
      with Inline;

--
--
   -- A fraction is a word W interpreted as the value W / 2**47;
--
--

   KDF9_small : constant := 2.0**(-47);

   type fraction is delta KDF9_small range -1.0 .. +1.0 - KDF9_small with Size => KDF9.word'Size;

   function as_fraction is new Ada.Unchecked_Conversion (KDF9.word, CPU.fraction);

   function as_word     is new Ada.Unchecked_Conversion (CPU.fraction, KDF9.word);

   -- These operations treat the KDF9.word operands as full-word fractions,

   function "*" (L, R : KDF9.word)
   return CPU.fraction;

   function "/" (L, R : KDF9.word)
   return CPU.fraction;


--
--
   --
   -- 48-bit integer and fractional ALU operations
   --
--
--

   function "+" (L, R : KDF9.pair)
   return KDF9.pair
      with Inline;

   function "-" (J : KDF9.pair)
   return KDF9.pair
      with Inline;

   function "-" (L, R : KDF9.pair)
   return KDF9.pair
      with Inline;

   -- 48 * 48 -> 96-bit, for XD, etc.

   function "*" (L, R : KDF9.word)
   return KDF9.pair;

   procedure do_DIVD (
                      L : in KDF9.pair;
                      R : in KDF9.word;
                      Q : out KDF9.word
                     );

   procedure do_DIVR (
                      L         : in KDF9.pair;
                      R         : in KDF9.word;
                      Quotient,
                      Remainder : out KDF9.word
                     );

   function shift_logical (P : KDF9.pair; L : CPU.signed_Q_part)
   return KDF9.pair
      with Inline;

   function shift_arithmetic (P : KDF9.pair; L : CPU.signed_Q_part)
   return KDF9.pair
      with Inline;


--
--
   --
   -- 48-bit floating point ALU types and operations
   --
--
--

   -- This is a substrate for KDF9 floating point, not an Ada f.p. type.

   type f48 is mod 2**48 with Size => KDF9.word'Size;

   -- Remove useless substrate modular operations not, and, or, xor and mod.

   overriding
   function "not" (R : CPU.f48)
   return CPU.f48 is abstract;

   overriding
   function "and" (L, R : CPU.f48)
   return CPU.f48 is abstract;

   overriding
   function "or" (L, R : CPU.f48)
   return CPU.f48 is abstract;

   overriding
   function "xor" (L, R : CPU.f48)
   return CPU.f48 is abstract;

   overriding
   function "mod" (L, R : CPU.f48)
   return CPU.f48 is abstract;

   function as_word is new Ada.Unchecked_Conversion (CPU.f48, KDF9.word);

   function as_f48  is new Ada.Unchecked_Conversion (KDF9.word, CPU.f48);

   procedure push (F : in CPU.f48);

   function pop
   return CPU.f48
      with Inline;

   procedure write_top (F : in CPU.f48)
      with Inline;

   function read_top
   return CPU.f48
      with Inline;

    -- Standardize a (possibly) non-normalized floating-point number.

   function normalized  (R : CPU.f48)
   return CPU.f48;

   -- Convert a 47-bit fraction to a rounded, standardized 39-bit mantissa,
   --    and adjust its exponent accordingly, setting overflow when necessary.

   procedure normalize (fraction, exponent : in out KDF9.word)
      with Inline;

   -- Convert a 39-bit mantissa to a 47-bit fraction, preserving the sign.

   function fraction_word (mantissa : CPU.f48)
   return KDF9.word
      with Inline;

   -- The floating-point number with the exponent field set to 0.

   function masked_mantissa (F : CPU.f48)
   return CPU.f48
      with Inline;

   -- The algebraic scale-factor, not the hardware exponent, -128 <= scaler < +128.

   function scaler (F : CPU.f48)
   return KDF9.word
      with Inline;

    -- Synthesize a normalized floating-point number from its components.

   function normalized (full_fraction, scaler : KDF9.word)
   return CPU.f48
      with Inline;

   -- Round a 48-bit floating-point number to 24-bit format.

   function narrowed (R : CPU.f48)
   return CPU.f48;

   overriding
   function "-" (R : CPU.f48)
   return CPU.f48;

   overriding
   function "abs" (R : CPU.f48)
   return CPU.f48;

   overriding
   function "+" (L, R : CPU.f48)
   return CPU.f48;

   overriding
   function "-" (L, R : CPU.f48)
   return CPU.f48;

   overriding
   function "*" (L, R : CPU.f48)
   return CPU.f48;

   overriding
   function "/" (L, R : CPU.f48)
   return CPU.f48;

   overriding
   function "<" (L, R : CPU.f48)
   return Boolean;

   function host_float (X : CPU.f48)
   return Long_Float;

   exponent_mask : constant KDF9.word := KDF9.word'(2#11_111_111#) * 2**39;
   mantissa_mask : constant KDF9.word := not exponent_mask;
   frac_msb_mask : constant KDF9.word := 2**46;  -- M.S.B. of a 47-bit fraction
   mant_msb_mask : constant KDF9.word := 2**38;  -- M.S.B. of a 39-bit mantissa
   overflow_mask : constant KDF9.word := 2**39;  -- bit set on rounding overflow


--
--
   --
   -- 96-bit floating point ALU types and operations
   --
--
--

   type f96 is
      record
         msw, lsw : CPU.f48;
      end record;

   function as_pair is new Ada.Unchecked_Conversion (CPU.f96, KDF9.pair);

   function as_f96  is new Ada.Unchecked_Conversion (KDF9.pair, CPU.f96);

   procedure push (DF : in CPU.f96)
      with Inline,
           Pre => the_NEST_depth < 15
               or else the_CPU_state = Director_state;

   function pop
   return CPU.f96
      with Inline;

   procedure write_top (DF : in CPU.f96)
      with Inline;

   function read_top
   return CPU.f96
      with Inline;

   -- The algebraic scale-factor, not the hardware exponent, -128 <= scaler < +128.

   function scaler (DF : CPU.f96)
   return KDF9.word
      with Inline;

   -- Round a 96-bit double-precision floating-point number to 48 bit format.

   function narrowed (DF : CPU.f96)
   return CPU.f48
      with Inline;

   -- Derive a 96-bit fraction from the double-precision floating-point number,
   --    with the mantissa bits in D9-D47 and D49-D87,
   --       and with D1-D8 copies of the sign, D48 zero, and D87-D95 zero.

   function fraction_pair (DF : CPU.f96)
   return KDF9.pair
      with Inline;

   -- Convert 96-bit fraction, and an algebraic scale-factor exponent,
   --    into a 96-bit floating point number, setting overflow when necessary.

   procedure reconstruct (frac   : in out KDF9.pair;
                          scaler : in KDF9.word);

   function "-" (R : CPU.f96)
   return CPU.f96;

   function "+" (L, R : CPU.f96)
   return CPU.f96;

   function "-" (L, R : CPU.f96)
   return CPU.f96;

   function "*" (L, R : CPU.f48)
   return CPU.f96;

   function "/" (L : CPU.f96;
                 R : CPU.f48)
   return CPU.f48;

------------------------------------------------------------------------------------------------

--
--
   --
   -- These are the emulation host's register types and their operations.
   --
--
--

   type u_64 is mod 2**64 with Size => 64;

   pragma Provide_Shift_Operators (u_64);

   function as_word (u : CPU.u_64)
   return KDF9.word
      with Inline;

   type s_64 is range -2**63 .. +2**63-1 with Size => 64;

   -- The signed as_word sets the V bit if necessary.

   function as_word (s : CPU.s_64)
   return KDF9.word
      with Inline;

   function unsign is new Ada.Unchecked_Conversion(CPU.s_64, CPU.u_64);

   function resign is new Ada.Unchecked_Conversion(CPU.u_64, CPU.s_64);

--
--
   --
   -- These are the 48-bit primitive, fixed-direction, shift operations.
   --
--
--

   function shift_time (amount : Natural)
   return KDF9.us
      with Inline;

   subtype word_shift_length is Natural range 0..48;

   function shift_word_left (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
      with Inline;

   function shift_word_right (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
      with Inline;

   function rotate_word_left (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
      with Inline;

   function rotate_word_right (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
      with Inline;

end KDF9.CPU;
