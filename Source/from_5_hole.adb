-- Convert a Ferranti 5-hole paper tape code to Latin-1.
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

with Ada.Characters.Latin_1;

use  Ada.Characters.Latin_1;

function from_5_hole (char : in Character; Letters_Case : in out Boolean; legible : out Boolean)
return Character is
   shifted : constant array (Character) of Character
           := (
               'A' => '1', 'B' => '2', 'C' => '*', 'D' => '4',
               'E' => '(', 'F' => ')', 'G' => '7', 'H' => '8',
               'I' => '±', 'J' => '=', 'K' => '-', 'L' => 'v',
               'M' =>  LF, 'N' => ' ', 'O' => ',', 'P' => '0',
               'Q' => '>', 'R' => ']', 'S' => '3', 'T' => '»',  -- ] for >=, » for right arrow
               'U' => '5', 'V' => '6', 'W' => '/', 'X' => '×',
               'Y' => '9', 'Z' => '+', '?' => 'n', '£' => CR,
               others => '¿'
              );
   result : Character := char;
begin -- from_5_hole
   legible := True;
   case char is
      when '@' | NUL =>  -- #00 in Ferranti code
         Letters_Case := False; legible := False;
      when 'A'..'Z' | '?' | '£' =>
         result := (if Letters_Case then char else shifted(char));
      when 'º' | '{' =>  -- #33 in Ferranti code
         Letters_Case := True; legible := False;
      when '}'       =>  -- #34 in Ferranti code
         result := '.';
      when '+'       =>  -- #35 in Ferranti code, given for EM by OUT8
         result := (if Letters_Case then '?' else 'n');
      when '\'       =>  -- #36 in Ferranti code, #76 in ee9 KDF9 code
         result := (if Letters_Case then '£' else CR);
      when '|'       =>  -- #35 in Ferranti code, given for EM by OUT8
         result := (if Letters_Case then '?' else 'n');
      when 'Ø'       =>  -- #37 in Ferranti code
         legible := False;
      when others    => -- Leave it alone.
         null;
   end case;
   return result;
end from_5_hole;
