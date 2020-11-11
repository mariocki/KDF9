-- kdf9.ads
--
-- The architecturally-defined data formats of the KDF9 computer.
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

with System;
--
with Latin_1;

use System;
--
use Latin_1;

package KDF9 is

--
   -- The following register types are available in user and Director states.
--

   --
   -- The fundamental 48-bit storage unit is the 48-bit word.
   --

   --
   -- The 48-bit word, considered as an unsigned integer.
   --
   type word is mod 2**48;

   word_mask : constant := 8#7777777777777777#;
   min_word  : constant := 8#4000000000000000#;
   max_word  : constant := 8#3777777777777777#;

   all_zero_bits : constant KDF9.word := 0;
   one_in_ls_bit : constant KDF9.word := 1;
   sign_bit      : constant KDF9.word := KDF9.min_word;
   not_sign_bit  : constant KDF9.word := KDF9.max_word;
   all_one_bits  : constant KDF9.word := KDF9.word_mask;

   --
   -- The 96-bit double word, considered as a pair of words.
   --
   type pair is
      record
         msw, lsw : KDF9.word;
      end record;

   --
   -- The fundamental 16-bit storage unit.
   --
   type field_of_16_bits is mod 2**16;

   --
   -- The 16-bit word, considered as a field of a Q register.
   --
   type Q_part is new KDF9.field_of_16_bits;

   Q_part_mask : constant := KDF9.Q_part'Last;

   function sign_extended (Q : KDF9.Q_part)
   return KDF9.word;
   pragma Inline(sign_extended);

   --
   -- The 16-bit word, considered as a buffer (DMA channel) number.
   --
   subtype buffer_number is KDF9.Q_part range 0 .. 15;

   buffer_number_mask : constant := buffer_number'Last;

   --
   -- The 16-bit word, considered as a core-store address.
   --
   subtype address is KDF9.Q_part range 0 .. 8#77777#;

   --
   -- The Q-store element.
   --
   type Q_register is
      record
         C, I, M : KDF9.Q_part;
      end record;

   function as_Q (the_word : KDF9.word)
   return KDF9.Q_register;
   pragma Inline(as_Q);

   function as_word (the_Q : KDF9.Q_register)
   return KDF9.word;
   pragma Inline(as_word);

   --
   -- The 8-bit instruction syllable and its components.
   --
   type syllable is mod 2**8;

   subtype syndrome is KDF9.syllable range 0 .. 63;
   subtype Q_number is KDF9.syllable range 0 .. 15;

   type syllable_group is
      record
         syllable_0, syllable_1, syllable_2 : KDF9.syllable := 0;
      end record;

   --
   -- An instruction address.
   --
   -- N.B. 5 is the hardware's largest valid syllable address.
   -- The values 6 and 7 are used as diagnostic flags by ee9.
   type syllable_code is mod 2**3;
   type code_location is mod 2**13;

   type code_point is
      record
         syllable_number : KDF9.syllable_code;
         word_number     : KDF9.code_location;
      end record;

   --
   -- An instruction address, in the packed hardware format.
   --
   type code_link is new KDF9.code_point;
   for code_link'Size use 16;
   for code_link'Bit_Order use Low_Order_First;
   for code_link use
      record
         syllable_number at 0 range 13 .. 15;
         word_number     at 0 range  0 .. 12;
      end record;

   function as_word (the_link : KDF9.code_link)
   return KDF9.word;

   function as_link (the_word : KDF9.word)
   return KDF9.code_link;

   procedure increment_by_1 (the_link : in out KDF9.code_point);
   pragma Inline(increment_by_1);

   procedure increment_by_2 (the_link : in out KDF9.code_point);
   pragma Inline(increment_by_2);

   procedure increment_by_3 (the_link : in out KDF9.code_point);
   pragma Inline(increment_by_3);

   --
   -- The KDF9 halfword. Each occupies 24 bits, packed 2 per word.
   --
   type halfword is mod 2**24;
   halfword_mask : constant := 8#77_77_77_77#;

   subtype halfword_number is KDF9.address range 0 .. 1;

   --
   -- The KDF9 character. Each symbol occupies six bits, packed 8 per word.
   --
   type symbol is mod 2**6;

   Blank_Space : constant KDF9.symbol := 8#00#;
   Line_Shift  : constant KDF9.symbol := 8#02#;
   Page_Change : constant KDF9.symbol := 8#03#;
   Tabulation  : constant KDF9.symbol := 8#04#;
   Case_Shift  : constant KDF9.symbol := 8#06#;
   Case_Normal : constant KDF9.symbol := 8#07#;
   Semi_Colon  : constant KDF9.symbol := 8#34#;
   End_Message : constant KDF9.symbol := 8#75#;
   Word_Filler : constant KDF9.symbol := 8#77#;

   type symbol_number is mod 8;

   --
   -- KDF9 <=> ISO Latin_1 character code inter-relationaships.
   --

   type output_code_table is array (KDF9.symbol) of Character;
   type input_code_table  is array (Character)   of KDF9.symbol;

   C_N : constant Character := 'ñ';  -- Models KDF9's Case_Normal in Latin_1.
   C_S : constant Character := 'ß';  -- Models KDF9's Case_Shift  in Latin_1.
   E_M : constant Character := '|';  -- Models KDF9's End_Message in Latin_1.
   W_F : constant Character := 'Ø';  -- Models KDF9's Word Filler in Latin_1.

   -- The Line Printer code:
   --    W_F is used for values that have no printable representation.
   to_LP : constant output_code_table
         :=  (' ',  W_F,   LF,   FF,   HT,  W_F,  '%',  ''',
              ':',  '=',  '(',  ')',  '£',  '*',  ',',  '/',
              '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
              '8',  '9',  W_F,  'º',  ';',  '+',  '-',  '.',
              W_F,  'A',  'B',  'C',  'D',  'E',  'F',  'G',
              'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
              'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
              'X',  'Y',  'Z',  W_F,  W_F,  E_M,  W_F,  W_F
             );

   -- The Card Reader code:
   --    W_F is used for external characters that have no assigned punching.
   CR_in : constant input_code_table
         := (' ' => 8#00#,  '"' => 8#01#,   LF => 8#02#,   FF => 8#03#,
              HT => 8#04#,  '#' => 8#05#,  '%' => 8#06#,  ''' => 8#07#,
             ':' => 8#10#,  '=' => 8#11#,  '(' => 8#12#,  ')' => 8#13#,
             '£' => 8#14#,  '*' => 8#15#,  ',' => 8#16#,  '/' => 8#17#,
             '0' => 8#20#,  '1' => 8#21#,  '2' => 8#22#,  '3' => 8#23#,
             '4' => 8#24#,  '5' => 8#25#,  '6' => 8#26#,  '7' => 8#27#,
             '8' => 8#30#,  '9' => 8#31#,  '_' => 8#32#,  'º' => 8#33#,
             ';' => 8#34#,  '+' => 8#35#,  '-' => 8#36#,  '.' => 8#37#,

             '@' => 8#40#,  'A' => 8#41#,  'B' => 8#42#,  'C' => 8#43#,
             'D' => 8#44#,  'E' => 8#45#,  'F' => 8#46#,  'G' => 8#47#,
             'H' => 8#50#,  'I' => 8#51#,  'J' => 8#52#,  'K' => 8#53#,
             'L' => 8#54#,  'M' => 8#55#,  'N' => 8#56#,  'O' => 8#57#,
             'P' => 8#60#,  'Q' => 8#61#,  'R' => 8#62#,  'S' => 8#63#,
             'T' => 8#64#,  'U' => 8#65#,  'V' => 8#66#,  'W' => 8#67#,
             'X' => 8#70#,  'Y' => 8#71#,  'Z' => 8#72#,  '{' => 8#73#,
             '}' => 8#74#,  E_M => 8#75#,  '\' => 8#76#,  W_F => 8#77#,

                            'a' => 8#41#,  'b' => 8#42#,  'c' => 8#43#,
             'd' => 8#44#,  'e' => 8#45#,  'f' => 8#46#,  'g' => 8#47#,
             'h' => 8#50#,  'i' => 8#51#,  'j' => 8#52#,  'k' => 8#53#,
             'l' => 8#54#,  'm' => 8#55#,  'n' => 8#56#,  'o' => 8#57#,
             'p' => 8#60#,  'q' => 8#61#,  'r' => 8#62#,  's' => 8#63#,
             't' => 8#64#,  'u' => 8#65#,  'v' => 8#66#,  'w' => 8#67#,
             'x' => 8#70#,  'y' => 8#71#,  'z' => 8#72#,
             others => Word_Filler
            );

   -- The Card Punch code:
   to_CP : constant output_code_table
         := (' ',  '"',   LF,   FF,   HT,  '#',  '%',  ''',
             ':',  '=',  '(',  ')',  '£',  '*',  ',',  '/',
             '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
             '8',  '9',  '_',  'º',  ';',  '+',  '-',  '.',
             '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
             'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
             'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
             'X',  'Y',  'Z',  '{',  '}',  E_M,  '\',  W_F
            );

   -- Two-shift devices expand the code by adopting alternative representations
   --    depending on the current "shift".
   -- The Flexowriter type cage really did shift up and down
   --    to bring the appropriate glyph set into position.

   subtype letter_case is KDF9.symbol range KDF9.Blank_Space .. KDF9.Case_Normal;
   both   : constant KDF9.symbol := KDF9.Blank_Space;
   normal : constant KDF9.symbol := KDF9.Case_Normal;
   shift  : constant KDF9.symbol := KDF9.Case_Shift;

   case_of : constant input_code_table
           := (' ' =>  both,  '"' =>  both,   LF =>  both,   FF =>  both,
                HT =>  both,  '#' =>  both,  C_S =>  both,  C_N =>  both,
               '&' =>  both,  '?' =>  both,  '!' =>  both,  '%' =>  both,
               ''' =>  both,  '$' =>  both,  '~' =>  both,  ':' => shift,
               '^' => shift,  '[' => shift,  ']' => shift,  '<' => shift,
               '>' => shift,  '=' => shift,  '×' => shift,  '÷' => shift,
               '(' => shift,  ')' => shift,  '_' =>  both,  '£' => shift,
               ';' =>  both,  '±' => shift,  '*' => shift,  ',' => shift,

               '@' =>  both,  'a' => shift,  'b' => shift,  'c' => shift,
               'd' => shift,  'e' => shift,  'f' => shift,  'g' => shift,
               'h' => shift,  'i' => shift,  'j' => shift,  'k' => shift,
               'l' => shift,  'm' => shift,  'n' => shift,  'o' => shift,
               'p' => shift,  'q' => shift,  'r' => shift,  's' => shift,
               't' => shift,  'u' => shift,  'v' => shift,  'w' => shift,
               'x' => shift,  'y' => shift,  'z' => shift,  '{' =>  both,
               '}' =>  both,  E_M =>  both,  '\' =>  both,  W_F =>  both,
               others => normal
              );

   next_case : constant array (shift .. normal) of Character := (normal => C_S, shift => C_N);

   -- The Case Normal shift paper tape code:
   TP_CN : constant output_code_table
         := (' ',  '"',   LF,   FF,   HT,  '#',  C_S,  C_N,
             '&',  '?',  '!',  '%',  ''',  '$',  '~',  '/',
             '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
             '8',  '9',  '_',  'º',  ';',  '+',  '-',  '.',
             '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
             'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
             'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
             'X',  'Y',  'Z',  '{',  '}',  E_M,  '\',  W_F
            );

   CN_TR : constant input_code_table
         := (' ' => 8#00#,  '"' => 8#01#,   LF => 8#02#,   FF => 8#03#,
              HT => 8#04#,  '#' => 8#05#,  C_S => 8#06#,  C_N => 8#07#,
             '&' => 8#10#,  '?' => 8#11#,  '!' => 8#12#,  '%' => 8#13#,
             ''' => 8#14#,  '$' => 8#15#,  '~' => 8#16#,  '/' => 8#17#,
             '0' => 8#20#,  '1' => 8#21#,  '2' => 8#22#,  '3' => 8#23#,
             '4' => 8#24#,  '5' => 8#25#,  '6' => 8#26#,  '7' => 8#27#,
             '8' => 8#30#,  '9' => 8#31#,  '_' => 8#32#,  'º' => 8#33#,
             ';' => 8#34#,  '+' => 8#35#,  '-' => 8#36#,  '.' => 8#37#,
             '@' => 8#40#,  'A' => 8#41#,  'B' => 8#42#,  'C' => 8#43#,
             'D' => 8#44#,  'E' => 8#45#,  'F' => 8#46#,  'G' => 8#47#,
             'H' => 8#50#,  'I' => 8#51#,  'J' => 8#52#,  'K' => 8#53#,
             'L' => 8#54#,  'M' => 8#55#,  'N' => 8#56#,  'O' => 8#57#,
             'P' => 8#60#,  'Q' => 8#61#,  'R' => 8#62#,  'S' => 8#63#,
             'T' => 8#64#,  'U' => 8#65#,  'V' => 8#66#,  'W' => 8#67#,
             'X' => 8#70#,  'Y' => 8#71#,  'Z' => 8#72#,  '{' => 8#73#,
             '}' => 8#74#,  E_M => 8#75#,  '\' => 8#76#,  W_F => 8#77#,
             others => 0  -- This must be zero.
            );

   -- The Case Shift paper tape code:
   TP_CS : constant output_code_table
         := (' ',  '"',   LF,   FF,   HT,  '#',  C_S,  C_N,
             '&',  '?',  '!',  '%',  ''',  '$',  '~',  ':',
             '^',  '[',  ']',  '<',  '>',  '=',  '×',  '÷',
             '(',  ')',  '_',  '£',  ';',  '±',  '*',  ',',
             '@',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
             'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
             'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
             'x',  'y',  'z',  '{',  '}',  E_M,  '\',  W_F
            );

   CS_TR : constant input_code_table
         := (' ' => 8#00#,  '"' => 8#01#,   LF => 8#02#,   FF => 8#03#,
              HT => 8#04#,  '#' => 8#05#,  C_S => 8#06#,  C_N => 8#07#,
             '&' => 8#10#,  '?' => 8#11#,  '!' => 8#12#,  '%' => 8#13#,
             ''' => 8#14#,  '$' => 8#15#,  '~' => 8#16#,  ':' => 8#17#,
             '^' => 8#20#,  '[' => 8#21#,  ']' => 8#22#,  '<' => 8#23#,
             '>' => 8#24#,  '=' => 8#25#,  '×' => 8#26#,  '÷' => 8#27#,
             '(' => 8#30#,  ')' => 8#31#,  '_' => 8#32#,  '£' => 8#33#,
             ';' => 8#34#,  '±' => 8#35#,  '*' => 8#36#,  ',' => 8#37#,
             '@' => 8#40#,  'a' => 8#41#,  'b' => 8#42#,  'c' => 8#43#,
             'd' => 8#44#,  'e' => 8#45#,  'f' => 8#46#,  'g' => 8#47#,
             'h' => 8#50#,  'i' => 8#51#,  'j' => 8#52#,  'k' => 8#53#,
             'l' => 8#54#,  'm' => 8#55#,  'n' => 8#56#,  'o' => 8#57#,
             'p' => 8#60#,  'q' => 8#61#,  'r' => 8#62#,  's' => 8#63#,
             't' => 8#64#,  'u' => 8#65#,  'v' => 8#66#,  'w' => 8#67#,
             'x' => 8#70#,  'y' => 8#71#,  'z' => 8#72#,  '{' => 8#73#,
             '}' => 8#74#,  E_M => 8#75#,  '\' => 8#76#,  W_F => 8#77#,
             others => 0  -- This must be zero.
            );

   --
   -- These types define the structure of the KDF9's programmable registers.
   --
   type nest_depth is mod 19;
   for  nest_depth'Size use 32;
   type nest    is array (KDF9.nest_depth) of KDF9.word;

   type sjns_depth is mod 17;
   for  sjns_depth'Size use 32;
   type sjns    is array (KDF9.sjns_depth) of KDF9.code_link;

   type Q_store is array (KDF9.Q_number)  of KDF9.Q_register;

--
   -- The following variables (the_nest, the_sjns and the_Q_store) are
   --    the emulation microcode's working set (unlike the real KDF9).
--

   --
   -- The NEST.
   --

   the_nest       : KDF9.nest;
   the_nest_depth : KDF9.nest_depth  := 0;

   -- check_whether_the_nest_holds* does not cause NOUV if the_authenticity_mode = lax_mode.
   procedure check_whether_the_nest_holds (at_least : in KDF9.nest_depth);
   pragma Inline(check_whether_the_nest_holds);

   procedure check_whether_the_nest_holds_an_operand;
   pragma Inline(check_whether_the_nest_holds_an_operand);

   procedure check_whether_the_nest_holds_2_operands;
   pragma Inline(check_whether_the_nest_holds_2_operands);

   -- ensure_that_the_nest_holds* ignores the_authenticity_mode.
   procedure ensure_that_the_nest_holds (at_least : in KDF9.nest_depth);
   pragma Inline(ensure_that_the_nest_holds);

   procedure ensure_that_the_nest_holds_an_operand;
   pragma Inline(ensure_that_the_nest_holds_an_operand);

   procedure ensure_that_the_nest_holds_2_operands;
   pragma Inline(ensure_that_the_nest_holds_2_operands);

   procedure ensure_that_the_nest_has_room_for (at_least : in KDF9.nest_depth);
   pragma Inline(ensure_that_the_nest_has_room_for);

   procedure ensure_that_the_nest_has_room_for_a_result;
   pragma Inline(ensure_that_the_nest_has_room_for_a_result);

   procedure ensure_that_the_nest_has_room_for_2_results;
   pragma Inline(ensure_that_the_nest_has_room_for_2_results);

   procedure push (the_word : in KDF9.word);

   function pop
   return KDF9.word;

   procedure pop;

   procedure write_top (the_word : in KDF9.word);

   function read_top
   return KDF9.word;

   procedure push (the_pair : in KDF9.pair);

   function pop
   return KDF9.pair;

   procedure write_top (the_pair : in KDF9.pair);

   function read_top
   return KDF9.pair;

   pragma Inline(push);
   pragma Inline(pop);
   pragma Inline(write_top);
   pragma Inline(read_top);

   --
   -- The SJNS.
   --

   the_sjns       : KDF9.sjns;
   the_sjns_depth : KDF9.sjns_depth := 0;

   procedure ensure_that_the_sjns_is_not_empty;
   pragma Inline(ensure_that_the_sjns_is_not_empty);

   procedure ensure_that_the_sjns_is_not_full;
   pragma Inline(ensure_that_the_sjns_is_not_full);

   procedure push (the_link : in KDF9.code_point);
   pragma Inline(push);

   function pop
   return KDF9.code_point;
   pragma Inline(pop);

   function sjns_top
   return KDF9.code_link;


   --
   -- The Q Store.
   --

   -- Q0 is set to zero after every Q store updating order, to keep it permanently zeroised.
   the_Q_store : KDF9.Q_store;

   --
   -- The Boolean registers.
   --

   the_V_bit : KDF9.word := 0;
   the_T_bit : KDF9.word := 0;


--
   -- The following are to do with maintaining the virtual time.
--

   -- The emulation clocks tick in microseconds (unlike KDF9's clock).
   type microseconds is mod 2**64;

   -- The virtual processor time.
   the_CPU_time      : KDF9.microseconds := 0;

   -- The amount by which the_CPU_time is increased by an instruction execution.
   the_CPU_delta     : KDF9.microseconds := 0;

   -- The virtual elapsed time.
   -- Cap the result to prevent a spurious double-clock (RESET) interrupt.
   function the_clock_time return KDF9.microseconds;
   pragma Inline(the_clock_time);

   -- Advance to the max of the_CPU_time, the_elapsed_time, the_last_delay_time, and this_time.
   -- Cap the increase to prevent a spurious double-clock (RESET) interrupt in Director.
   -- If necessary, pause execution until the real time equals the virtual elapsed time.
   procedure advance_the_clock_past (this_time : in KDF9.microseconds);

   -- The virtual clock time at which the next IO interrupt is expected.
   the_next_interrupt_time : KDF9.microseconds := KDF9.microseconds'Last;

   -- Pause execution for the_delay_time in virtual microseconds.
   procedure delay_by (the_delay_time : in KDF9.microseconds);

   -- If necessary, pause execution until the real time equals the virtual elapsed time.
   procedure synchronize_the_real_and_virtual_times;


--
   -- The following register types are used only in Director state.
--

   --
   -- The following are to do with the K1 order.
   --
   type priority is mod 2**2;

   -- This is the priority level of the currently-executing problem program.
   CPL : KDF9.priority;

   -- BA = word address of first allocated word (NOT group number as in the KDF9).
   BA  : KDF9.address;

   -- NOL = word address of last allocated word (NOT group number as in the KDF9).
   NOL : KDF9.address;

   -- Set BA (setting bits D38:47), CPL (D34:35) and NOL (D24:33).
   procedure set_K1_register (setting : in KDF9.word);

   --
   -- The following are to do with the =K2 order.
   --
   type one_bit is mod 2;

   -- The Current Peripheral Device Allocation Register.
   type CPDAR is array (KDF9.buffer_number) of KDF9.one_bit;
   pragma Convention(C, CPDAR);

   the_CPDAR : KDF9.CPDAR;

   -- Set CPDAR (setting bits D32 .. D47).
   procedure set_K2_register (setting : in KDF9.word);

   --
   -- The following are to do with the =K3 and K7 orders.
   --
   type user_register_set is
      record
         nest     : KDF9.nest;
         sjns     : KDF9.sjns;
         Q_store  : KDF9.Q_store;
      end record;

   -- There are 4 sets of user registers.
   -- The execution context is the number of the register set in active use.
   type context is mod 2**2;

   -- register_bank holds the currently inactive register sets.
   register_bank : array(KDF9.context) of KDF9.user_register_set;

   -- KDF9 actually indexed the register bank with the value of the_context,
   --   but the emulator swaps register sets between register_bank and
   --      the_nest, the_sjns, and the_Q_store (q.v.).

   the_context : KDF9.context := 0;

   -- Set context (bits D46:47), nest_depth (D41:45) and sjns_depth (D36:41).
   procedure set_K3_register (setting : in KDF9.word);

   -- Get BA (bits D0 .. D9), CPL (D12 .. D13) and NOL (D14 .. D23).
   function get_K7_operand
   return KDF9.word;

   --
   -- The following are to do with the K4 order.
   --
   type interrupt_number is range 22 .. 31;

   -- higher PRiority unblocked by end of I/O, or INTQq on busy device
   PR_flag    : constant KDF9.interrupt_number := 22;
   PR_trap    : exception;

   -- FLEXowriter interrupt from operator
   FLEX_flag  : constant KDF9.interrupt_number := 23;
   FLEX_trap  : exception;

   -- Lock-In Violation (attempt at a disallowed operation)
   LIV_flag   : constant KDF9.interrupt_number := 24;
   LIV_trap   : exception;

   -- Nest (or SJNS) Over/Underflow Violation
   NOUV_flag  : constant KDF9.interrupt_number := 25;
   NOUV_trap  : exception;

   -- End of Director Transfer, or I/O priority inversion
   EDT_flag   : constant KDF9.interrupt_number := 26;
   EDT_trap   : exception;

   -- OUT system call
   OUT_flag   : constant KDF9.interrupt_number := 27;
   OUT_trap   : exception;

   -- Lock-Out Violation
   LOV_flag   : constant KDF9.interrupt_number := 28;
   LOV_trap   : exception;

   -- invalid syllable number or 'double-clock'
   RESET_flag : constant KDF9.interrupt_number := 29;
   RESET_trap : exception;

   type RFIR is array (KDF9.interrupt_number) of Boolean;

   the_RFIR : KDF9.RFIR := (others => False);

    -- The time at which the last K4 order was executed.
   the_last_K4_time : KDF9.microseconds := 0;

   -- Get clock (bits D0:15) and RFIR (D16:31), clearing both.
   function get_K4_operand
   return KDF9.word;

   --
   -- The following are to do with the K5 order.
   --

   -- The Program Hold-Up register is internal to I/O Control.
   -- Get PHUi (bits D6i .. 6i+5), i = 0 .. 3.
   function get_K5_operand
   return KDF9.word;


--
   -- The following are to do with management of the CPU's internal state.
--
   type CPU_state is (Director_state, program_state);

   the_CPU_state : KDF9.CPU_state;

   procedure reset_the_CPU_state;

   procedure reset_the_internal_registers (the_new_state : in CPU_state := Director_state);

   procedure LIV_if_user_mode (the_reason : in String := "Director-only instruction");

   procedure LOV_if_user_mode;

   procedure change_to_user_state_at (new_IAR : in KDF9.code_point);

   procedure signal_interrupt (the_reason : in KDF9.interrupt_number);

   procedure trap_invalid_instruction (the_message : in String := "invalid instruction");

   procedure reset_the_program_state;

--
   -- Instruction fetch and decode.
--

   -- These Instruction Address Registers are the nearest KDF9 has
   --    to a conventional 'Program Counter' register.
   -- NIA is significant only after an instruction has been decoded.

   function NIA return KDF9.code_point;  -- the Next Instruction Address
   pragma Inline(NIA);

   function NIA_word_number return KDF9.code_location;
   pragma Inline(NIA_word_number);

   CIA : KDF9.code_point;                -- the Current Instruction Address

   -- IWB0 and IWB1 in KDF9 contained the current 2 instruction words.
   -- A 'short' loop, initiated by the JCqNZS instruction, ran entirely
   --    inside the IWBs, obviating repeated instruction-fetch overhead.
   -- Director exploits this in a loop that zeroizes the whole of core,
   --    including that loop, which runs, immune to overwriting, in the IWBs.

   procedure set_NIA_to (new_NIA : in KDF9.code_point);
   pragma Inline(set_NIA_to);

   procedure set_NIA_to_the_INS_target_address;

   procedure set_IWB0_and_IWB1_for_a_JCqNZS_loop;
   pragma Inline(set_IWB0_and_IWB1_for_a_JCqNZS_loop);

   procedure go_back_to_the_start_of_IWB0;
   pragma Inline(go_back_to_the_start_of_IWB0);

   procedure continue_after_JCqNZS;
   pragma Inline(continue_after_JCqNZS);

   -- Bits 0-1 of every order indicates its type as follows.
   type INS_kind is mod 2**2;

   one_syllable_order : constant := 0;
   two_syllable_order : constant := 1;
   normal_jump_order  : constant := 2;
   data_access_order  : constant := 3;

   type decoded_order is
      record
         order : KDF9.syllable_group := (0, 0, 0);
         kind  : KDF9.INS_kind := 0;

         -- The syndrome is:
         --    bits 2-7 of 1- and 2-syllable orders
         --    bits 2-3|8-11 of normal jumps
         --    bits 5-7 of SET and directly-addressed store access orders.
         -- See the compressed_opcodes package.
         syndrome : KDF9.syndrome := 0;

          -- Qq is bits 8-11, Qk is bits 12-15.
         Qq, Qk : KDF9.Q_number := 0;

         -- For an jump instruction, syllable_number is bits 5-7.
         target : code_point;

         -- For a data address or value (SET), operand is bits 2-4|12-23.
         operand : KDF9.Q_part := 0;
      end record;

   INS : KDF9.decoded_order;  -- analogous to the INS register in Main Control

   -- After decode_the_next_order:
   --    INS contains the whole instruction at the address given by CIA,
   --       with its components unpacked.
   procedure decode_the_next_order;
   pragma Inline(decode_the_next_order);

   procedure decode (the_order : in out KDF9.decoded_order);
   pragma Inline(decode);

   procedure process_syllable_0_of_INS;
   pragma Inline(process_syllable_0_of_INS);

   procedure process_syllable_1_of_INS;
   pragma Inline(process_syllable_1_of_INS);

   procedure process_syllables_1_and_2_of_a_jump_order;
   pragma Inline(process_syllables_1_and_2_of_a_jump_order);

   procedure process_syllables_1_and_2_of_a_data_access_order;
   pragma Inline(process_syllables_1_and_2_of_a_data_access_order);

   -- the_order_at_NIA gets three syllables starting at [NIA].  It is FOR DIAGNOSTIC USE ONLY!
   -- It does NOT update the CPU time properly and must not be used inside an instruxtion cycle.
   function the_order_at_NIA
   return KDF9.syllable_group;
   pragma Inline(the_order_at_NIA);

   -- Save E0U, lest the initial jump in E0 be corrupted during the run.
   procedure save_the_initial_jump;

   -- Restore E0U to its saved value.
   procedure restore_the_initial_jump;

   -- Check whether E0U has changed.
   function the_initial_jump_was_corrupted
   return Boolean;

   -- True if the parameter is not a valid KDF9 instruction.
   function is_an_invalid_order (decoded : KDF9.decoded_order)
   return Boolean;


--
   -- The Instruction Counter Register (N.B. NOT a 'PROGRAM counter')
   --   indicates the number of instructions executed by the KDF9.
--

   type order_counter is mod 2**64;

   ICR : KDF9.order_counter := 0;


--
   -- The following support hashed execution-signature checking,
   --    mainly for self-checking of new versions and ports.
--

   function the_digital_signature
   return KDF9.word;

   procedure update_the_digital_signature;
   pragma Inline(update_the_digital_signature);


--
   -- These should be in IOC, but are put here to break cyclic dependencies.
--

   subtype logical_device_name is String(1 .. 3);

   FD0_number : constant := 14; -- Fixed Disc buffer number, used in several places.


private

   the_elapsed_time    : KDF9.microseconds := 0;
   the_last_delay_time : KDF9.microseconds := 0;

   fetching_normally   : Boolean := True;

end KDF9;
