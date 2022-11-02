-- Perform peephole optimizations of KDF9 Kidsgrove Algol object programs in Usercode.
--
-- This file is an auxiliary of ee9 (9.0p), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded;
--
with simple_IO;
with string_editing;


use  Ada.Exceptions;
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
          (-"ZERO; NOT; NEG; SIGN; ABS; NEG; J",      -"#Z;",      +"NEG; NOT; J",  +"#Z;"),
          (-"ZERO; NOT; NEG; SIGN; ABS; NEG; J",      -"=Z;",      +"NEG; NOT; J",  +"=Z;"),
          (-"ZERO; NOT; NEG; SIGN; ABS; NEG; NOT; J", -"#Z;",      +"NEG; NOT; J",  +"=Z;"),
          (-"ZERO; NOT; NEG; SIGN; ABS; NEG; NOT; J", -"=Z;",      +"NEG; NOT; J",  +"#Z;"),
          (-"ZERO; NOT; NEG; SIGN; NEG; STR; REV; ERASE; NOT; J",
                                                      -"=Z;",      +"NEG; NOT; J",  +"GTZ;"),
          (-"ZERO; NOT; NEG; SIGN; NEG; STR; REV; ERASE; J",
                                                      -"=Z;",      +"NEG; NOT; J",  +"LEZ;"),
          (-"ZERO; NOT; NEG; SIGN; STR; REV; ERASE; NOT; J",
                                                      -"=Z;",      +"J",            +"LEZ;"),
          (-"ZERO; NOT; NEG; SIGN; STR; REV; ERASE; J",
                                                      -"=Z;",      +"J",            +">Z;"),
          (-"ZERO; NOT; SIGN; NEG; STR; REV; ERASE; NOT; J",
                                                      -"=Z;",      +"NOT; NEG; J",  +">Z;"),
          (-"ZERO; NOT; SIGN; NEG; STR; REV; ERASE; J",
                                                      -"=Z;",      +"NOT; NEG; J",  +"LEZ;"),
          (-"ZERO; NOT; SIGN; STR; REV; ERASE; NOT; J",
                                                      -"=Z;",      +"NOT; NEG; J",  +"<Z;"),
          (-"ZERO; NOT; SIGN; STR; REV; ERASE; J",
                                                      -"=Z;",      +"NOT; NEG; J",  +"GEZ;"),
          (-"ZERO; NOT; SIGN; ABS; NEG; J",           -"#Z;",      +"NOT; NEG; J",  +"#Z;"),
          (-"ZERO; NOT; SIGN; ABS; NEG; J",           -"=Z;",      +"NOT; NEG; J",  +"=Z;"),
          (-"ZERO; NOT; SIGN; ABS; NEG; NOT; J",      -"#Z;",      +"NOT; NEG; J",  +"=Z;"),
          (-"ZERO; NOT; SIGN; ABS; NEG; NOT; J",      -"=Z;",      +"NOT; NEG; J",  +"#Z;"),
          (-"ZERO; SIGN; ABS; NEG; J",                -"#Z;",      +"J",            +"#Z;"),
          (-"ZERO; SIGN; ABS; NEG; J",                -"=Z;",      +"J",            +"=Z;"),
          (-"ZERO; SIGN; ABS; NEG; NOT; J",           -"#Z;",		 +"J",            +"=Z;"),
          (-"ZERO; SIGN; ABS; NEG; NOT; J",           -"=Z;",		 +"J",            +"#Z;"),
          (-"ZERO; SIGN; J",                     	   -"#Z;",      +"J",            +"#Z;"),
          (-"ZERO; SIGN; J",                     	   -"=Z;",      +"J",            +"=Z;"),
          (-"ZERO; SIGN; J",                          -"<Z;",      +"J",            +"<Z;"),
          (-"ZERO; SIGN; J",                          -">Z;",      +"!!!J",         +">Z;"),
          (-"ZERO; SIGN; J",                          -"GEZ;",     +"J",            +"GEZ;"),
          (-"ZERO; SIGN; J",                          -"LEZ;",     +"J",            +"LEZ;"),
          (-"ZERO; SIGN; NEG; STR; REV; ERASE; NOT; J",
                                                      -"=Z;",	    +"J",	         +"GTZ;"),
          (-"ZERO; SIGN; NEG; STR; REV; ERASE; J",    -"=Z;",	   +"J",	            +"LEZ;"),
          (-"ZERO; SIGN; SHA-1; NEG; J",         	   -"=Z;",      +"J",            +"LEZ;"),
          (-"ZERO; SIGN; SHA-1; NEG; J",              -"LEZ;",     +"J",            +"GEZ;"),
          (-"ZERO; SIGN; SHA-1; NEG; NOT; J",    	   -"=Z;",      +"J",            +">Z;"),
          (-"ZERO; SIGN; STR; REV; ERASE; J",		   -"=Z;",      +"J",		      +"GEZ;"),
          (-"ZERO; SIGN; STR; REV; ERASE; NOT; J",    -"=Z;",      +"J",		      +"<Z;"),
          (-"ZERO; SIGNF; ABS; NEG; J",          	   -"#Z;",		 +"J",            +"#Z;"),
          (-"ZERO; SIGNF; ABS; NEG; J",          	   -"=Z;",		 +"J",            +"=Z;"),
          (-"ZERO; SIGNF; ABS; NEG; NOT; J",     	   -"#Z;",		 +"J",            +"=Z;"),
          (-"ZERO; SIGNF; ABS; NEG; NOT; J",     	   -"=Z;",      +"J",            +"#Z;"),
          (-"ZERO; SIGNF; J",                    	   -"#Z;",      +"J",            +"#Z;"),
          (-"ZERO; SIGNF; J",                    	   -"=Z;",      +"J",            +"=Z;"),
          (-"ZERO; SIGNF; J",                         -"<Z;",      +"J",            +"<Z;"),
          (-"ZERO; SIGNF; J",                         -">Z;",      +"J",            +">Z;"),
          (-"ZERO; SIGNF; J",                         -"GEZ;",     +"J",            +"GEZ;"),
          (-"ZERO; SIGNF; J",                         -"LEZ;",     +"J",            +"LEZ;"),
          (-"ZERO; SIGNF; NEG; STR; REV; ERASE; J",   -"=Z;",		  +"J",	         +"LEZ;"),
          (-"ZERO; SIGNF; SHA-1; NEG; J",        	   -"=Z;",      +"J",            +"LEZ;"),
          (-"ZERO; SIGNF; SHA-1; NEG; J",             -"LEZ;",     +"J",            +"GEZ;"),
          (-"ZERO; SIGNF; SHA-1; NEG; NOT; J",   	   -"=Z;",      +"J",            +">Z;"),
          (-"ZERO; SIGNF; STR; REV; ERASE; J",		   -"=Z;",      +"J",		      +"GEZ;"),
          (-"ZERO; SIGNF; STR; REV; ERASE; NOT; J",   -"=Z;",      +"J",		      +"<Z;"),
          (-"SIGN; ABS; NEG; J",                 	   -"#Z;",      +"NEV; J",       +"#Z;"),
          (-"SIGN; ABS; NEG; J",                 	   -"=Z;",      +"NEV; J",       +"=Z;"),
          (-"SIGN; ABS; NEG; NOT; J",            	   -"#Z;",      +"NEV; J",       +"=Z;"),
          (-"SIGN; ABS; NEG; NOT; J",            	   -"=Z;",      +"NEV; J",       +"#Z;"),
          (-"SIGN; NEG; STR; REV; ERASE; J",			   -"=Z;",		 +"SIGN; J",      +"LEZ;"),
          (-"SIGN; SHA-1; NEG; J",               	   -"=Z;",      +"SIGN; J",      +"LEZ;"),
          (-"SIGN; SHA-1; NEG; J",                    -"LEZ;",     +"SIGN; J",      +"GEZ;"),
          (-"SIGN; SHA-1; NEG; NOT; J",          	   -"=Z;",      +"SIGN; J",      +">Z;"),
          (-"SIGN; STR; REV; ERASE; J",		         -"=Z;",	    +"SIGN; J",      +"GEZ;"),
          (-"SIGN; STR; REV; ERASE; NOT; J",			   -"=Z;",		 +"SIGN; J",      +"<Z;"),
          (-"SIGNF; ABS; NEG; J",                	   -"#Z;",      +"NEV; J",       +"#Z;"),
          (-"SIGNF; ABS; NEG; J",                	   -"=Z;",      +"NEV; J",       +"=Z;"),
          (-"SIGNF; ABS; NEG; NOT; J",           	   -"#Z;",      +"NEV; J",       +"=Z;"),
          (-"SIGNF; ABS; NEG; NOT; J",           	   -"=Z;",      +"NEV; J",       +"#Z;"),
          (-"SIGNF; NEG; STR; REV; ERASE; J",		   -"=Z;",	    +"SIGNF; J",     +"LEZ;"),
          (-"SIGNF; SHA-1; NEG; J",                   -"=Z;",      +"SIGNF; J",     +"LEZ;"),
          (-"SIGNF; SHA-1; NEG; J",                   -"LEZ;",     +"SIGNF; J",     +"GEZ;"),
          (-"SIGNF; SHA-1; NEG; NOT; J",              -"=Z;",      +"SIGNF; J",     +">Z;"),
          (-"SIGNF; STR; REV; ERASE; J",              -"=Z;",      +"SIGNF; J",     +"GEZ;"),
          (-"SIGNF; STR; REV; ERASE; NOT; J",         -"=Z;",      +"SIGNF; J",     +"<Z;"),
          (-"REV; -; J",                              -"LEZ;",      +"-; J",         +"GEZ;"),
          (-"REV; -; J",                              -"#Z;",       +"NEV; J",       +"#Z;"),
          (-"REV; -; J",                              -"<Z;",       +"-; J",         +"GTZ;"),
          (-"REV; -; J",                              -"=Z;",       +"NEV; J",       +"=Z;"),
          (-"REV; -; J",                              -">Z;",       +"-; J",         +"LTZ;"),
          (-"REV; -; J",                              -"GEZ;",      +"-; J",         +"LEZ;")
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

   max_line_length : constant := 1024; -- The longest line observed had 282 characters.

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
            r_pos,
            t_pos: line_length_range;
         begin
            l_pos := 1;
    case_i: loop
               l_pos := Index(data, before, l_pos);
            exit case_i when l_pos = 0;
               m_pos := Index(data, middle, l_pos+before'Length);
            exit case_i when m_pos = 0;
               if is_semicolon_free(Unbounded_Slice(data, l_pos+before'Length, m_pos-1)) then
                  r_pos := Index(data, after,  m_pos+middle'Length);
                  t_pos := Index(data, ";",  r_pos);
               exit case_i when r_pos = 0
                             or t_pos = 0
                             or Unbounded_Slice(data, t_pos-1, t_pos) = "V;";
                  if is_semicolon_free(Unbounded_Slice(data, m_pos+middle'Length, r_pos-1)) then
                     if (this.new_left = null and this.new_right = null)          and then
                        +Unbounded_Slice(data, l_pos+1, l_pos+1) /= 'S'           and then
                           +Unbounded_Slice(data, m_pos-2, m_pos-2) in '0'..'9'   and then
                              +Unbounded_Slice(data, m_pos+3, m_pos+3) /= 'S'     and then
                                 +Unbounded_Slice(data, r_pos-1, r_pos-1) in '0'..'9' then
                        -- Suppress an impossible jump.
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
            before : String renames this.old_left.all;
            middle : String renames this.old_middle.all;
            after  : String renames this.old_right.all;
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
            before : String renames this.old_left.all;
            after  : String renames this.old_right.all;
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
            before : String renames this.poor.all;
            after  : String renames this.good.all;
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

   -- Replace non-ASCII characters (� => X, � => ~, � => %, � => #) and tabs by a space.
   procedure do_simple_substitutions (line : in out String) is
      j : line_length_range;
   begin
      if line'Length = 0 then return; end if;
      for c of line loop
         if    c  = '�' then
               c := 'X';
         elsif c  = '�' then
               c := '~';
         elsif c  = '�' then
               c := '%';
         elsif c  = '�' then
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

   procedure clarify_algol (optimizing : in Boolean) is
   begin
      loop
         read_line(line, last, True);
         if last /= 0 then
            if line(1) = '|' then
               print_line("|");
               return;
            elsif line(1) = '(' then
               new_line;
               print_line(line(1..last));
            else
               do_simple_substitutions(line(1..last));
               Set_Unbounded_String(data, line(1..last));
               normalize_spacing(data);
               if optimizing then
                  do_constant_substitutions;
                  do_special_substitutions;
                  do_first_field_substitutions;
                  do_single_field_substitutions;
                  do_cross_jump_substitutions;
                  do_special_substitutions;
               end if;
               pretty_print(To_String(data));
            end if;
         end if;
      end loop;
   end clarify_algol;

   max_source_length : constant := 2**14;  -- > 8KW / 1.5 words per line
   subtype source_index_range is Positive range 1 .. max_source_length;
   pascal_source : array (source_index_range) of Unbounded_String;
   pascal_index  : source_index_range;
   placeholder   : constant Unbounded_String := To_Unbounded_String("(REMOVED BY GLANCE);");
   dumps_enabled : Boolean;

   procedure get_attributes (
                             line : in Unbounded_String;
                             is_a_leaf, has_nonlocals : out Boolean;
                             last_formal, last_local, first_l_value : out Natural
                            )
   is
      l_value_key : constant String := "LVALUE@";
      locals_key  : constant String := "LOCALS=";
      formals_key : constant String := "FORMALS=";
      first, last : Natural;
   begin
      is_a_leaf := Index(line, "LEAF", 1, Forward) /= 0;
      has_nonlocals := Index(line, "NONLOCALS", 1, Forward) /= 0;
      first_l_value := 32767;
      first := Index(line, l_value_key, 1, Forward);
      if first /= 0 then
         last := Index(line, " ", first, Forward);
         first_l_value := Natural'Value(To_String(Unbounded_Slice(line, first+l_value_key'Length, last-1)));
      end if;
      last_local := 0;
      first := Index(line, locals_key, 1, Forward);
      if first /= 0 then
         last := Index(line, " ", first, Forward);
         last_local := Natural'Value(To_String(Unbounded_Slice(line, first+locals_key'Length, last-1)));
      end if;
      last_formal := 0;
      first := Index(line, formals_key, 1, Forward);
      if first /= 0 then
         last := Index(line, " ", first, Forward);
         last_formal := Natural'Value(To_String(Unbounded_Slice(line, first+formals_key'Length, last-1)));
      end if;
   end get_attributes;

  procedure map_locals_to_Q_store (
                                   initial, final : in Natural;
                                   is_a_leaf: in Boolean;
                                   last_local, first_l_value : in Natural
                                   )
   is
      first : Natural;
   begin
      if is_a_leaf and last_local > 0 then
         for s in initial .. final loop
            if Length(pascal_source(s)) /= 0 then
               if Index(pascal_source(s), "M1;", 1, Forward) /= 0 then
                  for q in 5 .. Natural'Min(first_l_value, 12) loop
                     declare
                        q_image  : constant String := q'Image;
                        q_number : constant String := q_image(2..q_image'Last);
                     begin
                        first := 1;
                        loop
                           first := Index(pascal_source(s), "E" & q_number & "M1;", first, Forward);
                        exit when first = 0;
                           Replace_Slice(pascal_source(s), first, first+q_number'Length+3, "Q" & q_number & ";");
                        end loop;
                     end;
                  end loop;
               end if;
            end if;
         end loop;
      end if;
   end map_locals_to_Q_store;


  procedure optimize_linkages (
                               prelude_index, postlude_index : in Natural;
                               is_a_leaf, has_nonlocals : in Boolean;
                               last_formal, last_local, first_l_value : in Natural
                              )
   is
   begin
      null;
   end optimize_linkages;

   procedure pascal_second_pass (start, stop : in source_index_range) is
      first, last : Natural;
      prelude_index, postlude_index : Natural;
      is_a_leaf, has_nonlocals : Boolean;
      last_formal, last_local, first_l_value : Natural;
   begin
   body_loop:
      for s in start .. stop loop
         if Length(pascal_source(s)) /= 0 then
      exit body_loop when Unbounded_Slice(pascal_source(s), 1, 1) = "|";
            first := Index(pascal_source(s), "(BODY OF ", 1, Forward);
            if first /= 0 and Index(pascal_source(s), " MAIN PROGRAM)", 1, Forward) = 0 then
       postlude_loop:
               for t in s+1 .. stop loop
                  first := Index(pascal_source(t), "(POSTLUDE OF ", 1, Forward);
                  if first /= 0 then
                     get_attributes(
                                    pascal_source(t),
                                    is_a_leaf, has_nonlocals,
                                    last_formal, last_local, first_l_value
                                   );
                     prelude_index := s+1;
                     postlude_index := t+1;
                     pascal_source(s+1) := pascal_source(t+4);
                     last := Length(pascal_source(s+1));
                     first := Index(pascal_source(s+1), " J", last, Backward);
                     if first /= 0 then
                        Replace_Slice(pascal_source(s+1), first, last, "");
                     end if;
                     pascal_source(t+2)   := placeholder;
                     pascal_source(t+3) := placeholder;
                     pascal_source(t+4) := placeholder;
               exit postlude_loop;
                  end if;
               end loop postlude_loop;
               map_locals_to_Q_store(prelude_index, postlude_index, is_a_leaf, last_local, first_l_value);
               optimize_linkages(prelude_index, postlude_index,
                                 is_a_leaf, has_nonlocals,
                                 last_formal, last_local, first_l_value
                                );
            end if;
         end if;
      end loop body_loop;
   end pascal_second_pass;

   procedure pascal_third_pass (start, stop : in source_index_range) is
   begin
      for s in start .. stop loop
         data := pascal_source(s);
         if Length(data) /= 0 then
            if Unbounded_Slice(data, 1, 1) = "|" then
               print_line("|");
               return;
            elsif data = placeholder then
               -- Ignore an unwanted line.
               null;
            elsif Unbounded_Slice(data, 1, 1) = "(" or else Unbounded_Slice(data, 1, 1) = "P" then
               new_line;
               print_line(To_String(data));
            else
               do_single_field_substitutions;
               -- Pascal is not pretty-printed, because the output from PASKAL is neat.
               print_line(To_String(data));
            end if;
         end if;
      end loop;
   end pascal_third_pass;

   procedure clarify_pascal is
   begin
      begin
         dumps_enabled := false;
         pascal_index := 1;
         loop
            read_line(line, last, True);
            Set_Unbounded_String(pascal_source(pascal_index), line(1..last));
            if Index(pascal_source(pascal_index), "DUMP ON", 1, Forward) /= 0 then
               dumps_enabled := true;
            end if;
            pascal_index := pascal_index + 1;
         end loop;
      exception
         when end_error =>
            pascal_index := pascal_index - 1;
      end;
      pascal_second_pass(1, pascal_index);
      pascal_third_pass(1, pascal_index);
   end clarify_pascal;

   procedure complain (about : in String := "") is
   begin
      if about /= "" then
         report_line(about & ".");
      end if;
      report_line("usage: glance -o | -p | -r ");
      CLI.Set_Exit_Status(CLI.Failure);
      raise program_error;
   end complain;

begin
   if CLI.Argument_Count /= 1 then
      complain;
   end if;
   if lower(CLI.Argument(1)) not in "-o" | "-p" | "-r" then
      complain("invalid parameter: " & CLI.Argument(1));
   end if;
   -- Merely copy the Usercode frontsheet.
   loop
      read_line(line, last, True);
      print_line(line(1..last));
      last := index_forward(line(1..last), "PROGRAM;", 1);
   exit when last /= 0;
   end loop;
   -- Process the rest of the text.
   if lower(CLI.Argument(1)) = "-p" then
      clarify_pascal;
   else
      clarify_algol(optimizing => lower(CLI.Argument(1)) = "-o");
   end if;
   flush_outputs;
exception
   when end_error =>
      print_line("|");
      flush_outputs;
   when EOL_error =>
      report_line("glance: input line too long!");
      flush_outputs;
      CLI.Set_Exit_Status(CLI.Failure);
   when error : others =>
      report_line("Failure in glance: " & Exception_Information(error) & ".");
      flush_outputs;
      CLI.Set_Exit_Status(CLI.Failure);

end glance;