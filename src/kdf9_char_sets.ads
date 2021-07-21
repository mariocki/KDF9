-- The architecturally-defined character codes of the KDF9 computer.
--
-- This file is part of ee9 (8.0k), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Characters.Latin_1;

use  Ada.Characters.Latin_1;

package KDF9_char_sets is -- NB NOT a child of KDF9!

   --
   -- The KDF9 character. Each symbol occupies six bits, and they are packed 8 per word.
   --

   type symbol is mod 2**6;

   type symbol_index is mod 8;

   Blank_Space  : constant KDF9_char_sets.symbol := 8#00#;
   Line_Shift   : constant KDF9_char_sets.symbol := 8#02#;
   Page_Change  : constant KDF9_char_sets.symbol := 8#03#;
   Tabulation   : constant KDF9_char_sets.symbol := 8#04#;
   Case_Shift   : constant KDF9_char_sets.symbol := 8#06#;
   Case_Normal  : constant KDF9_char_sets.symbol := 8#07#;
   Tape_Mark    : constant KDF9_char_sets.symbol := 8#17#;
   Semi_Colon   : constant KDF9_char_sets.symbol := 8#34#;
   Upper_Case_D : constant KDF9_char_sets.symbol := 8#44#;
   Upper_Case_M : constant KDF9_char_sets.symbol := 8#55#;
   Upper_Case_P : constant KDF9_char_sets.symbol := 8#60#;
   End_Message  : constant KDF9_char_sets.symbol := 8#75#;
   Word_Filler  : constant KDF9_char_sets.symbol := 8#77#;
   Group_Mark   : constant KDF9_char_sets.symbol := 8#77#;

   --
   -- These are the 8 bits of a paper tape frame containing these characters.
   --
   Semi_Colon_tape_bits  : constant := 8#074#;
   End_Message_tape_bits : constant := 8#175#;

   --
   -- KDF9 <=> ISO Latin-1 character code inter-relationships.
   --

   type output_code_table is array (KDF9_char_sets.symbol) of Character;
   type input_code_table  is array (Character)   of KDF9_char_sets.symbol;

   C_N : constant Character := 'ñ';  -- Models KDF9's Case_Normal in legible Latin-1.
   C_S : constant Character := 'ß';  -- Models KDF9's Case_Shift  in legible Latin-1.
   E_M : constant Character := '|';  -- Models KDF9's End_Message in legible Latin-1.
   W_F : constant Character := 'Ø';  -- Models KDF9's Word Filler in legible Latin-1.

   -- The Line Printer code:
   --    W_F is used for values that have no printable representation.
   to_LP : constant output_code_table
         :=  (' ',  W_F,   LF,   FF,  W_F,  W_F,  '%',  ''',
              ':',  '=',  '(',  ')',  '£',  '*',  ',',  '/',
              '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
              '8',  '9',  W_F,  'º',  ';',  '+',  '-',  '.',
              W_F,  'A',  'B',  'C',  'D',  'E',  'F',  'G',
              'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
              'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
              'X',  'Y',  'Z',  W_F,  W_F,  W_F,  W_F,  W_F
             );

   -- The Card Reader code:
   --    W_F is used for external characters that have no assigned punching.
   CR_in : constant input_code_table
         := (' ' => 8#00#,  '"' => 8#01#,  '®' => 8#02#,  '©' => 8#03#,
             '¬' => 8#04#,  '#' => 8#05#,  '%' => 8#06#,  ''' => 8#07#,
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
         := (' ',  '"',  '®',  '©',  '¬',  '#',  '%',  ''',
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
   -- The Flexowriter type cage really did shift up and down to bring the
   --    appropriate glyph set into position.

   subtype letter_case is KDF9_char_sets.symbol range Blank_Space .. Case_Normal;
   both   : constant KDF9_char_sets.symbol := Blank_Space;
   normal : constant KDF9_char_sets.symbol := Case_Normal;
   shift  : constant KDF9_char_sets.symbol := Case_Shift;

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

   function glyph_for (char : Character)
   return Character
   is (case char is
           when LF     => '®',
           when FF     => '©',
           when HT     => '¬',
           when others => char
      );

   --
   -- Used when (un)packing 8-bit bytes for raw data I/O.
   --

   type octet_index is mod 6;

   type octet is mod 2**8;

end KDF9_char_sets;
