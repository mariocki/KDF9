-- Perform peephole optimizations of KDF9 Kidsgrove Algol object programs in Usercode.
--
-- This file is an auxiliary of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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
with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Unbounded;
--
with simple_IO;
with string_editing;


use  Ada.Strings;
use  Ada.Strings.Unbounded;
--
use  simple_IO;
use  string_editing;

procedure glance is

   pragma Unsuppress(All_Checks);

   package CLI renames Ada.Command_Line;

   function "+" (text : Unbounded_String)
   return Character
   is (To_String(text)(1));

   type fragment is access String;

   function "-" (text : String)
   return fragment
   is (new String'(text));

   function "+" (text : String)
   return fragment
   is (new String'(text));

   type constant_substitution  is
      record
         poor, good : fragment;
      end record;

   -- Substitutions are performed on each line in the order listed.

   constant_substitutions  : constant array (Positive range <>) of constant_substitution
      := (
          (-"JP233V; ",                            +""),
          (-"JSP244; ",                            +""),
          (-"JSP203;",                             +"SET47; FLOAT;"),

          (-"M+I2; M+I2; M+I2; M+I2; M+I2; M+I2;", +"SET6; =+M2;"),
          (-"M+I2; M+I2; M+I2; M+I2; M+I2;",       +"SET5; =+M2;"),
          (-"M+I2; M+I2; M+I2; M+I2;",             +"SET4; =+M2;"),
          (-"M+I2; M+I2; M+I2;",                   +"SET3; =+M2;"),

          (-"REV; +;",                             +"+;"),
          (-"REV; +F;",                            +"+F;"),
          (-"REV; REV;",                           +""),
          (-"REV; XD;",                            +"XD;"),
          (-"REV; XF;",                            +"XF;"),

          (-"SET1; NEG;",                          +"ZERO; NOT;"),
          (-"SET-1;",                              +"ZERO; NOT;"),
          (-"SET1; -;",                            +"NEG; NOT;"),
          (-"SET1; +;",                            +"NOT; NEG;"),
          (-"SET1; XD; CONT;",                     +""),

          (-"SET0;",                               +"ZERO;"),
          (-"SETB0;",                              +"ZERO;"),
          (-"SET1;",                               +"ZERO; NOT; NEG;"),
          (-"SET+1;",                              +"ZERO; NOT; NEG;"),
          (-"SETB1;",                              +"ZERO; NOT; NEG;"),

          (-"SET1; SET47; FLOAT;",                 +"SETB403; SHC-10;"),

          (-"SET2; -;",                            +"NEG; NOT; NEG; NOT;"),
          (-"SET2; +;",                            +"NOT; NEG; NOT; NEG;"),
          (-"SET2; XD; CONT;",                     +"DUP; +;"),

          (-"SET2; JS1P231;",                      +"DUP; XF;"),
          (-"SET2; JS4P231;",                      +"DUP; XD; CONT; SET47; FLOAT;"),

          (-"SET2; SET47; FLOAT;",                 +"SETB405; SHC-10;"),
          (-"SET3; SET47; FLOAT;",                 +"SETB407; SHC-10;"),
          (-"SET3; JS1P231;",                      +"DUP; DUP; XF; XF;"),
          (-"SET3; XD; CONT;",                     +"DUP; DUP; +; +;"),
          (-"SET4; XD; CONT;",                     +"SHA+2;"),
          (-"SET5; SET47; FLOAT;",                 +"SETB413; SHC-10;"),
          (-"SET5; XD; CONT;",                     +"DUP; SHA+2; +;"),
          (-"SET8; XD; CONT;",                     +"SHA+3;"),
          (-"SET10; XD; CONT;",                    +"DUP; SHA+3; REV; DUP; +; +;"),
          (-"SET10; SET47; FLOAT;",                +"SETB425; SHC-10;"),

          (-"ZERO; SET47; FLOAT;",                 +"ZERO;"),
          (-"ZERO; AND;",                          +"ZERO;"),
          (-"ZERO; REV; =M3;",                     +"=M3; ZERO;"),

          (-"ZERO; OR; ",                          +""),
          (-"ZERO; NEV; ",                         +""),

          (-"ZERO; NOT; NEG; -;",                  +"NEG; NOT;"),
          (-"ZERO; NOT; NEG; +;",                  +"NOT; NEG;"),
          (-"ZERO; NOT; REV; =M3;",                +"=M3; ZERO; NOT;"),
          (-"ZERO; NOT; NEG; REV; =M3;",           +"=M3; ZERO; NOT; NEG;"),
          -- The following are last to pick up any trailing NEGs or NOTs created by the above.
          (-"NEG; NEG;",                           +""),
          (-"NOT; NOT;",                           +"")
         );

   type one_field_optimization  is
      record
         old_left,
         old_right,
         new_left,
         new_right : fragment;
      end record;

   one_field_optimizations : constant array (Positive range <>) of one_field_optimization
      := (
          (-"ZERO; SIGNF; STR; REV; ERASE; NOT; J", -"=Z;",      +"J",		   +"<Z;"),
          (-"ZERO; SIGNF; STR; REV; ERASE; J",		 -"=Z;",      +"J",		   +"GEZ;"),
          (-"ZERO; SIGNF; SHA-1; NEG; NOT; J",   	 -"=Z;",      +"J",        +">Z;"),
          (-"ZERO; SIGNF; SHA-1; NEG; J",        	 -"=Z;",      +"J",        +"LEZ;"),
          (-"ZERO; SIGNF; SHA-1; NEG; J",           -"LEZ;",     +"J",        +"GEZ;"),
          (-"ZERO; SIGNF; J",                    	 -"#Z;",      +"J",        +"#Z;"),
          (-"ZERO; SIGNF; J",                    	 -"=Z;",      +"J",        +"=Z;"),
          (-"ZERO; SIGNF; J",                       -"<Z;",      +"J",        +"<Z;"),
          (-"ZERO; SIGNF; J",                       -">Z;",      +"J",        +">Z;"),
          (-"ZERO; SIGNF; J",                       -"GEZ;",     +"J",        +"GEZ;"),
          (-"ZERO; SIGNF; J",                       -"LEZ;",     +"J",        +"LEZ;"),
          (-"ZERO; SIGNF; ABS; NEG; NOT; J",     	 -"#Z;",		  +"J",        +"=Z;"),
          (-"ZERO; SIGNF; ABS; NEG; NOT; J",     	 -"=Z;",      +"J",        +"#Z;"),
          (-"ZERO; SIGNF; ABS; NEG; J",          	 -"#Z;",		  +"J",        +"#Z;"),
          (-"ZERO; SIGNF; ABS; NEG; J",          	 -"=Z;",		  +"J",        +"=Z;"),
          (-"ZERO; SIGN; STR; REV; ERASE; NOT; J",  -"=Z;",      +"J",		   +"<Z;"),
          (-"ZERO; SIGN; STR; REV; ERASE; J",		 -"=Z;",      +"J",		   +"GEZ;"),
          (-"ZERO; SIGN; SHA-1; NEG; NOT; J",    	 -"=Z;",      +"J",        +">Z;"),
          (-"ZERO; SIGN; SHA-1; NEG; J",         	 -"=Z;",      +"J",        +"LEZ;"),
          (-"ZERO; SIGN; SHA-1; NEG; J",            -"LEZ;",     +"J",        +"GEZ;"),
          (-"ZERO; SIGN; J",                     	 -"#Z;",      +"J",        +"#Z;"),
          (-"ZERO; SIGN; J",                     	 -"=Z;",      +"J",        +"=Z;"),
          (-"ZERO; SIGN; J",                        -"<Z;",      +"J",        +"<Z;"),
          (-"ZERO; SIGN; J",                        -">Z;",      +"!!!J",     +">Z;"),
          (-"ZERO; SIGN; J",                        -"GEZ;",     +"J",        +"GEZ;"),
          (-"ZERO; SIGN; J",                        -"LEZ;",     +"J",        +"LEZ;"),
          (-"ZERO; SIGN; ABS; NEG; NOT; J",         -"#Z;",		  +"J",        +"=Z;"),
          (-"ZERO; SIGN; ABS; NEG; NOT; J",         -"=Z;",		  +"J",        +"#Z;"),
          (-"ZERO; SIGN; ABS; NEG; J",              -"#Z;",      +"J",        +"#Z;"),
          (-"ZERO; SIGN; ABS; NEG; J",              -"=Z;",      +"J",        +"=Z;"),
          (-"SIGNF; STR; REV; ERASE; NOT; J",       -"=Z;",      +"SIGNF; J", +"<Z;"),
          (-"SIGNF; STR; REV; ERASE; J",            -"=Z;",      +"SIGNF; J", +"GEZ;"),
          (-"SIGNF; SHA-1; NEG; NOT; J",            -"=Z;",      +"SIGNF; J", +">Z;"),
          (-"SIGNF; SHA-1; NEG; J",                 -"=Z;",      +"SIGNF; J", +"LEZ;"),
          (-"SIGNF; SHA-1; NEG; J",                 -"LEZ;",     +"SIGNF; J", +"GEZ;"),
          (-"SIGNF; ABS; NEG; NOT; J",           	 -"#Z;",      +"NEV; J",   +"=Z;"),
          (-"SIGNF; ABS; NEG; NOT; J",           	 -"=Z;",      +"NEV; J",   +"#Z;"),
          (-"SIGNF; ABS; NEG; J",                	 -"#Z;",      +"NEV; J",   +"#Z;"),
          (-"SIGNF; ABS; NEG; J",                	 -"=Z;",      +"NEV; J",   +"=Z;"),
          (-"SIGN; STR; REV; ERASE; NOT; J",			 -"=Z;",		  +"SIGN; J",	+"<Z;"),
          (-"SIGN; STR; REV; ERASE; J",		       -"=Z;",		  +"SIGN; J",	+"GEZ;"),
          (-"SIGN; SHA-1; NEG; NOT; J",          	 -"=Z;",      +"SIGN; J",  +">Z;"),
          (-"SIGN; SHA-1; NEG; J",               	 -"=Z;",      +"SIGN; J",  +"LEZ;"),
          (-"SIGN; SHA-1; NEG; J",                  -"LEZ;",     +"SIGN; J",  +"GEZ;"),
          (-"SIGN; ABS; NEG; NOT; J",            	 -"#Z;",      +"NEV; J",   +"=Z;"),
          (-"SIGN; ABS; NEG; NOT; J",            	 -"=Z;",      +"NEV; J",   +"#Z;"),
          (-"SIGN; ABS; NEG; J",                 	 -"#Z;",      +"NEV; J",   +"#Z;"),
          (-"SIGN; ABS; NEG; J",                 	 -"=Z;",      +"NEV; J",   +"=Z;"),
          (-"REV; -; J",                            -"#Z",       +"NEV; J",   +"#Z"),
          (-"REV; -; J",                            -"=Z",       +"NEV; J",   +"=Z"),
          (-"REV; -; J",                            -"<Z",       +"-; J",     +"GTZ"),
          (-"REV; -; J",                            -">Z",       +"-; J",     +"LTZ"),
          (-"REV; -; J",                            -"GEZ",      +"-; J",     +"LEZ"),
          (-"REV; -; J",                            -"LEZ",      +"-; J",     +"GEZ")
         );

   type first_field_optimization is
      record
         old_left,
         old_middle,
         old_right,
         new_left,
         new_right : fragment;
      end record;

   first_field_optimizations : constant array (Positive range <>) of first_field_optimization
      := (
          1 => (-"XD; CONT; J", -"LEZ; J", -";",      +"XD; CONT; J", +">Z")
         );

   type cross_jump_optimization is
      record
         old_left,
         old_middle,
         old_right,
         new_left,
         new_right : fragment;
      end record;

   cross_jump_optimizations : constant array (Positive range <>) of cross_jump_optimization
      := (
          (-"J",     -"#Z; J",   -";",    +"J", +"=Z;"),
          (-"J",     -"<Z; J",   -";",    +"J", +"GEZ;"),
          (-"J",     -"=Z; J",   -";",    +"J", +"#Z;"),
          (-"J",     -">Z; J",   -";",    +"J", +"LEZ;"),
          (-"J",     -"LEZ; J",  -";",    +"J", +">Z;"),
          (-"J",     -"GEZ; J",  -";",    +"J", +"<Z;"),
          (-"J",     -"; J",     -";",    null, null)
         );

   special_optimizations : constant array (Positive range <>) of one_field_optimization
      := (
          (-" SET",             -"; =Z0;",                       null, null),
          (-"JS",               -"; EXIT1;",                     +"J", +";"),
          (-"=Y0M2Q; ",         -"; M-I2; Y0M2; +;",             +"",  +"; +;"),
          (-"=Y0M2Q; ",         -"; M-I2; Y0M2; +F;",            +"",  +"; +F;"),
          (-"=Y0M2Q; ",         -"; M-I2; Y0M2; XD; CONT;",      +"",  +"XD; CONT;"),
          (-"=Y0M2Q; ",         -"; M-I2; Y0M2; XF;",            +"",  +"; XF;"),
          (-"=Y0M2Q; ",         -"; M-I2; Y0M2; -;",             +"",  +"; REV; -;"),
          (-"=Y0M2Q; ",         -"; M-I2; Y0M2; -F;",            +"",  +"; REV; -F;"),
          (-"=Y0M2Q; ",         -"; M-I2; Y0M2; %F;",            +"",  +"; REV; %F;"),
          (-"SET1; ",           -"; +;",                         +"",  +"; NOT; NEG;"),
          (-"SET1; ",           -"; XD; CONT;",                  +"",  +";"),
          (-"SET2; ",           -"; +;",                         +"",  +"; NOT; NEG; NOT; NEG;"),
          (-"SET2; ",           -"; XD; CONT;",                  +"",  +"; DUP; +;"),
          (-"SET3; ",           -"; XD; CONT;",                  +"",  +"; DUP; DUP; +; +;"),
          (-"SET4; ",           -"; XD; CONT;",                  +"",  +"; SHA+2;"),
          (-"SET5; ",           -"; XD; CONT;",                  +"",  +"; DUP; SHA+2; +;"),
          (-"SET8; ",           -"; XD; CONT;",                  +"",  +"; SHA+3;"),
          (-"ZERO; NOT; NEG; ", -"; +;",                         +"",  +"; NOT; NEG;"),
          (-"ZERO; NOT; NEG; ", -"; -;",                         +"",  +"; NEG; NOT;"),
          (-"ZERO; NOT; NEG; ", -"; XD; CONT;",                  +"",  +";")
         );

   function is_semicolon_free (data : Unbounded_String)
   return Boolean is
      info : constant String := To_String(data);
   begin
      for c of info loop
         if c = ';' then
            return False;
         end if;
      end loop;
      return True;
   end is_semicolon_free;

   data : Unbounded_String;

   max_line_length : constant := 4096;

   subtype line_length_range is Natural range 0 .. max_line_length;

   subtype source_code_line  is String(line_length_range range 1..max_line_length);

   procedure do_special_substitutions is
      l_pos, r_pos : line_length_range;
   begin
      if Length(data) = 0 then
         return;
      end if;
i_th: for i in special_optimizations'Range loop
         declare
            this   : one_field_optimization renames special_optimizations(i);
            before : String  renames this.old_left.all;
            after  : String  renames this.old_right.all;
         begin
            r_pos := Length(data);
            loop
               r_pos := Index(data, after, r_pos, Backward);
            exit when r_pos = 0;
               l_pos := Index(data, before,  r_pos, Backward);
            exit when l_pos = 0;
               if is_semicolon_free(Unbounded_Slice(data, l_pos+before'Length, r_pos-1))  and then
                     Unbounded_Slice(data, l_pos+before'Length, r_pos-1) /= " "               then
                  if this.new_left = null and then this.new_right = null then
                     data := Unbounded_Slice(data, 1, l_pos-1)
                           & Unbounded_Slice(data, r_pos+after'Length, Length(data));
      exit i_th;
                  else
                     data := Unbounded_Slice(data, 1, l_pos-1)
                           & To_Unbounded_String(this.new_left.all)
                           & Unbounded_Slice(data, l_pos+before'Length, r_pos-1)
                           & To_Unbounded_String(this.new_right.all)
                           & Unbounded_Slice(data, r_pos+after'Length, Length(data));
                  end if;
               end if;
               r_pos := r_pos - after'Length;
            end loop;
         end;
      end loop i_th;
   exception
      when others =>
         report_line( "E|" & To_string(data) & r_pos'Image);
   end do_special_substitutions;

   procedure do_cross_jump_substitutions is
   begin
      if Length(data) = 0 then
         return;
      end if;
      for i in cross_jump_optimizations'Range loop
         declare
            this   : cross_jump_optimization renames cross_jump_optimizations(i);
            before : String renames this.old_left.all;
            middle : String renames this.old_middle.all;
            after  : String renames this.old_right.all;
            l_pos,
            m_pos,
            r_pos  : line_length_range;
         begin
            l_pos := 1;
    case_i: loop
               l_pos := Index(data, before, l_pos);
            exit case_i when l_pos = 0;
               m_pos := Index(data, middle, l_pos+before'Length);
            exit case_i when m_pos = 0;
               if is_semicolon_free(Unbounded_Slice(data, l_pos+before'Length, m_pos-1)) then
                  r_pos := Index(data, after,  m_pos+middle'Length);
               exit case_i when r_pos = 0;
                  if is_semicolon_free(Unbounded_Slice(data, m_pos+middle'Length, r_pos-1)) then
                     if (this.new_left = null and this.new_right = null)          and then
                        +Unbounded_Slice(data, l_pos+1, l_pos+1) /= 'S'           and then
                           +Unbounded_Slice(data, m_pos-2, m_pos-2) in '0'..'9'   and then
                              +Unbounded_Slice(data, m_pos+3, m_pos+3) /= 'S'     and then
                                 +Unbounded_Slice(data, r_pos-1, r_pos-1) in '0'..'9' then
                        -- Suppress and impossible jump.
                        data := Unbounded_Slice(data, 1, m_pos)
                              & Unbounded_Slice(data, r_pos+1, Length(data));
            exit case_i;
                     elsif (this.new_left /= null and this.new_right /= null) then
                        -- Convert conditional jump to positive logic.
                        data := Unbounded_Slice(data, 1, l_pos - 1)
                              & this.new_left.all
                              & Unbounded_Slice(data, m_pos+middle'Length, r_pos - 1)
                              & this.new_right.all
                              & Unbounded_Slice(data, r_pos+after'Length, Length(data));
                     end if;
                  end if;
               end if;
               l_pos := l_pos + before'Length;
            end loop case_i;
         end;
      end loop;
   end do_cross_jump_substitutions;

   procedure do_first_field_substitutions is
   begin
      if Length(data) = 0 then
         return;
      end if;
      for i in first_field_optimizations'Range loop
         declare
            this   : first_field_optimization renames first_field_optimizations(i);
            before : String                 renames this.old_left.all;
            middle : String                 renames this.old_middle.all;
            after  : String                 renames this.old_right.all;
            l_pos,
            m_pos,
            r_pos  : line_length_range;
         begin
    case_i: loop
               l_pos := 1;
               l_pos := Index(data, before, l_pos);
            exit case_i when l_pos = 0;
               m_pos := Index(data, middle, l_pos);
            exit case_i when m_pos = 0;
               r_pos := Index(data, after,  m_pos);
            exit case_i when r_pos = 0;
               data := Unbounded_Slice(data, 1, l_pos - 1)
                     & this.new_left.all
                     & Unbounded_Slice(data, m_pos+middle'Length, r_pos - 1)
                     & this.new_right.all
                     & Unbounded_Slice(data, r_pos+after'Length, Length(data));
            end loop case_i;
         end;
      end loop;
   end do_first_field_substitutions;

   procedure do_single_field_substitutions is
   begin
      if Length(data) = 0 then
         return;
      end if;
      for i in one_field_optimizations'Range loop
         declare
            this   : one_field_optimization renames one_field_optimizations(i);
            before : String                 renames this.old_left.all;
            after  : String                 renames this.old_right.all;
            l_pos,
            r_pos  : line_length_range;
         begin
    case_i: loop
               l_pos := 1;
               l_pos := Index(data, before, l_pos);
            exit case_i when l_pos = 0;
               r_pos := Index(data, after,  l_pos);
            exit case_i when r_pos = 0;
               data := Unbounded_Slice(data, 1, l_pos - 1)
                     & this.new_left.all
                     & Unbounded_Slice(data, l_pos+before'Length, r_pos - 1)
                     & this.new_right.all
                     & Unbounded_Slice(data, r_pos+after'Length, Length(data));
            end loop case_i;
         end;
      end loop;
   end do_single_field_substitutions;

   procedure do_constant_substitutions is
   begin
      for i in constant_substitutions'Range loop
         declare
            this   : constant_substitution renames constant_substitutions(i);
            before : String                renames this.poor.all;
            after  : String                renames this.good.all;
            pos    : line_length_range;
         begin
    case_i: loop
               pos := 1;
               pos := Index(data, before, pos);
            exit case_i when pos = 0;
               data := Unbounded_Slice(data, 1, pos - 1)
                     & after
                     & Unbounded_Slice(data, pos + before'Length, Length(data));
            end loop case_i;
         end;
      end loop;
   end do_constant_substitutions;

   SP : Character renames Ada.Characters.Latin_1.Space;
   HT : Character renames Ada.Characters.Latin_1.HT;
   gap : constant String(1..12) := (others => SP);

   -- Convert ";  +" to "; ".
   procedure normalize_spacing (data : in out Unbounded_String) is
      pos : line_length_range;
   begin
      for i in reverse Positive'(2) .. 8 loop
         pos := 1;
         loop
            pos := Index(data, ";" & gap(1..i), pos);
         exit when pos = 0;
            Replace_Slice(data, pos, pos+i-1, "; ");
            pos := pos + 2;
         end loop;
      end loop;
      pos := 1;
      loop
         pos := Index(data, ";", pos);
      exit when pos = 0;
         if pos < Length(data) and then Unbounded_Slice(data, pos+1, pos+1) /= " " then
            Replace_Slice(data, pos, pos, "; ");
         end if;
         pos := pos + 2;
      end loop;
   end normalize_spacing;

   line : source_code_line;
   last : line_length_range;

   -- Replace non-ASCII characters (× => X, º => ~, ÷ => %, ± => #) and tabs by a space.
   procedure do_simple_substitutions (line : in out String) is
      j : line_length_range;
   begin
      if line'Length = 0 then return; end if;
      for c of line loop
         if    c  = '×' then
               c := 'X';
         elsif c  = 'º' then
               c := '~';
         elsif c  = '÷' then
               c := '%';
         elsif c  = '±' then
               c := '#';
         elsif c  = HT then
               c := SP;
         end if;
      end loop;
      j := line'First;
      while j <= line'Last-3 loop
         if    line(j..j+2)  = "_>Z" then
               line(j..j+2) := "GEZ";
               j := j + 3;
         elsif line(j..j+2)  = "_<Z" then
               line(j..j+2) := "LEZ";
               j := j + 3;
         else
               j := j + 1;
         end if;
      end loop;
   end do_simple_substitutions;

   -- Print labels at the left margin, jumps 3 columns in, and the rest 6 columns in.
   procedure pretty_print (data : in String; indent : in Natural := 6) is

      function indented (s : String; indent : Natural := 6)
      return String
      is (gap(1..indent) & trimmed(s));

      line : constant String := trimmed(data);

      function next_semicolon_after (pos : Positive)
      return Natural is
         k : Natural := 0;
      begin -- next_semicolon_after
         for j in pos+1 ..line'Last loop
            k := j;
         exit when line(j) = ';';
         end loop;
         return k;
      end next_semicolon_after;

      function next_nonblank_after (pos : Positive)
      return Natural is
         k : Natural := 0;
      begin -- next_nonblank_after
         for j in pos+1 ..line'Last loop
            k := j;
         exit when line(j) /= SP;
         end loop;
         return k;
      end next_nonblank_after;

      j, k, m, p : Natural;

      function handle_assignments (to : in Character)
      return Boolean is
      begin -- handle_assignments
         m := index_forward(line, "=" & to, p);
         if m /= 0 then
            k := next_semicolon_after(m);
            if k /= 0 then
               print_line(indented(line(line'First..k)));
               pretty_print(line(k+1..line'Last));
               return True;
            end if;
         end if;
         return False;
      end handle_assignments;

      flag : Character;

   begin -- pretty_print

      if line = "" then
         return;
      end if;

      for i in line'First .. line'Last loop
         p := i;
      exit when line(p) not in  SP | HT;
      end loop;

      if line(p) = '(' then
         k := index_forward(line, ")", p);
         if k /= 0 then
            if k < line'Last and then line(k+1) = ';' then
               k := k + 1;
            end if;
          print_line(indented(line(p..k), indent => 0));
          pretty_print(line(k+1..line'Last));
          return;
         end if;
      end if;

      if line(p) in '*' | '$' then
         k := next_nonblank_after(p);
         if k /= 0 and then line(k) in '0' .. '9' then
            print(line(p) & "");
            pretty_print(line(k..line'Last));
            return;
         elsif k /= 0 and then line(k) = ';' then
            print(line(p) & ";");
            pretty_print(line(k+1..line'Last));
            return;
         elsif k /= 0 and then line(k) = 'J' then
            m := next_semicolon_after(k);
            print_line("*  " & line(k..m));
            pretty_print(line(m+1..line'Last));
            return;
         else
            print_line(line);
            return;
         end if;
      end if;

      if line(p) in '0' .. '9' then
         k := next_semicolon_after(p);
         if k /= 0 then
            print(indented(line(p..k), indent => 0));
            new_line;
            pretty_print(line(k+1..line'Last));
            return;
         else
            print_line(line);
            return;
         end if;
      end if;

      m := index_forward(line, "EXIT", p);
      if m /= 0 then
         k := next_semicolon_after(m);
         pretty_print(line(line'First..m-1));
         print_line(indented(line(m..k), 3));
         pretty_print(line(k+1..line'Last));
         return;
      end if;

      m := p;
      if line(m) = 'P' and then line'Length > 1 and then line(m+1) in '0'..'9' then
         k := next_semicolon_after(m);
         if k /= 0 then
            pretty_print(line(line'First..m-1));
            new_line(2);
            print_line(line(m..k));
            pretty_print(line(k+1..line'Last));
            return;
         end if;
      end if;

      -- Special case for short loop jumps.
      m := index_forward(line, "J", p);
      if m > 1 and then line(m-1) in '*' | '$' then
         flag := line(m-1);
         k := index_forward(line, "NZS;", m);
         if k /= 0 then
               pretty_print(line(line'First..m-2));
               m := index_backward(line, "J", k);
               print_line(flag & "  " & line(m..k+3));
               pretty_print(line(k+4..line'Last));
               return;
         end if;
      end if;

      m := index_forward(line, "J", p);
      if m /= 0 then
         k := next_semicolon_after(m);
         if k /= 0 then
            pretty_print(line(line'First..m-1));
            print_line(indented(line(m..k), 3));
            pretty_print(line(k+1..line'Last));
            return;
         end if;
      end if;

      m := index_forward(line, "V", p);
      if m /= 0 then
         k := index_forward(line, ";", m);
         j := index_forward(line, "=", m);
         if j in m+1 .. k-1 then
            pretty_print(line(line'First..m-1));
            print(indented(line(m..k)));
            j := next_nonblank_after(k);
            if j /= 0 then
               if line(j) /= '(' then
                  new_line;
                  pretty_print(line(j..line'Last));
                  return;
               else
                  m := index_forward(line, ");", j);
                  if m /= 0 then
                     print_line(line(k+1..m+1));
                     pretty_print(line(m+2..line'Last));
                     return;
                  else
                     m := index_forward(line, ")", j);
                     if m /= 0 then
                        print_line(line(k+1..m));
                        pretty_print(line(m+1..line'Last));
                        return;
                     else
                        pretty_print(line(k+1..line'Last));
                        return;
                     end if;
                  end if;
               end if;
            else
               new_line;
               return;
            end if;
         end if;
      end if;

      if handle_assignments('L')       or else
            handle_assignments('V')    or else
               handle_assignments('W') or else
                  handle_assignments('Y') then
         return;
      end if;

      print_line(indented(line, (if line = "FINISH;" then 0 else indent)));

   end pretty_print;

   procedure clarify (optimizing : in Boolean) is
   begin
      loop
         read_line(line, last);
         if last /= 0 then
            if line(1) = '|' then
               print_line("|");
               return;
            else
               do_simple_substitutions(line(1..last));
               data := To_Unbounded_String(line(1..last));
               normalize_spacing(data);
               if optimizing then
                  do_constant_substitutions;
                  do_special_substitutions;
                  do_first_field_substitutions;
                  do_single_field_substitutions;
                  do_cross_jump_substitutions;
                  do_special_substitutions;
               end if;
               declare
                  updated_line : constant String := To_String(data);
               begin
                  pretty_print(updated_line);
               end;
            end if;
         end if;
      end loop;
   end clarify;

   procedure complain (about : in String := "") is
   begin
      if about /= "" then
         report_line(about & ".");
      end if;
      report_line("usage: glance -o | -r ");
      CLI.Set_Exit_Status(CLI.Failure);
      raise program_error;
   end complain;

begin
   if CLI.Argument_Count /= 1 then
      complain;
   end if;
   if lower(CLI.Argument(1)) not in "-o" | "-r" then
      complain("invalid parameter: " & CLI.Argument(1));
   end if;
   loop
      read_line(line, last);
      print_line(line(1..last));
      last := index_forward(line(1..last), "PROGRAM;", 1);
   exit when last /= 0;
   end loop;
   clarify(optimizing => lower(CLI.Argument(1)) = "-o");
   flush_outputs;
exception
   when end_error =>
      print_line("|");
      flush_outputs;
   when others =>
      flush_outputs;
      CLI.Set_Exit_Status(CLI.Failure);
end glance;
