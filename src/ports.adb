-- Convert KDF9 Algol programs between source code formats.
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
--
-- The ee9 program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option,
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Command_Line;
with Ada.Strings.Unbounded;
--
with simple_IO;
with string_editing;

use  Ada.Strings.Unbounded;
--
use  simple_IO;
use  string_editing;

procedure ports is

   package CLI renames Ada.Command_Line;

   type string_access is access String;

   type vocabulary is array (Positive range <>) of string_access;

   function "+" (s : String)
   return string_access
   is (new String'(s));

   de_flex  : constant vocabulary
         := (
             +"_c_o_m_m_e_n_t _l_i_b_r_a_r_y",
             +"_A_L_G_O_L",
             +"_a_n_d",
             +"_a_r_r_a_y",
             +"_b_e_g_i_n",
             +"_b_o_o_l_e_a_n",
             +"_B_o_o_l_e_a_n",
             +"_c_o_m_m_e_n_t",
             +"_d_o",
             +"_e_l_s_e",
             +"_e_n_d",
             +"_e_q_v",
             +"_E_X_I_T",
             +"_f_a_l_s_e",
             +"_f_o_r",
             +"_g_o_t_o",
             +"_t_o",
             +"_g_o",
             +"_i_f",
             +"_i_m_p",
             +"_i_n_t_e_g_e_r",
             +"_K_D_F_9",
             +"_l_a_b_e_l",
             +"_l_i_b_r_a_r_y",
             +"_n_o_t",
             +"_o_r",
             +"_o_w_n",
             +"_p_r_o_c_e_d_u_r_e",
             +"_r_e_a_l",
             +"_s_t_e_p",
             +"_s_t_r_i_n_g",
             +"_s_w_i_t_c_h",
             +"_t_h_e_n",
             +"_t_r_u_e",
             +"_u_n_t_i_l",
             +"_v_a_l_u_e",
             +"_w_h_i_l_e",
             +"_>",
             +"_<"
            );

   re_bangs : constant vocabulary
         := (
             +"!library",
             +"!ALGOL",
             +"!and",
             +"!array",
             +"!begin",
             +"!boolean",
             +"!Boolean",
             +"!comment",
             +"!do",
             +"!else",
             +"!end",
             +"!eqv",
             +"!EXIT",
             +"!false",
             +"!for",
             +"!goto",
             +"!to",
             +"!go",
             +"!if",
             +"!imp",
             +"!integer",
             +"!KDF9",
             +"!label",
             +"!library",
             +"!not",
             +"!or",
             +"!own",
             +"!procedure",
             +"!real",
             +"!step",
             +"!string",
             +"!switch",
             +"!then",
             +"!true",
             +"!until",
             +"!value",
             +"!while",
             +">=",
             +"<="
            );

   re_kwote : constant vocabulary
         := (
             +"'library'",
             +"'ALGOL'",
             +"'and'",
             +"'array'",
             +"'begin'",
             +"'boolean'",
             +"'Boolean'",
             +"'comment'",
             +"'do'",
             +"'else'",
             +"'end'",
             +"'eqv'",
             +"'EXIT'",
             +"'false'",
             +"'for'",
             +"'goto'",
             +"'to'",
             +"'go'",
             +"'if'",
             +"'imp'",
             +"'integer'",
             +"'KDF9'",
             +"'label'",
             +"'library'",
             +"'not'",
             +"'or'",
             +"'own'",
             +"'procedure'",
             +"'real'",
             +"'step'",
             +"'string'",
             +"'switch'",
             +"'then'",
             +"'true'",
             +"'until'",
             +"'value'",
             +"'while'",
             +">=",
             +"<="
            );

   re_flex  : constant vocabulary
         := (
             +"_c_o_m_m_e_n_t _l_i_b_r_a_r_y",
             +"_A_L_G_O_L",
             +"_a_n_d",
             +"_a_r_r_a_y",
             +"_b_e_g_i_n",
             +"_b_o_o_l_e_a_n",
             +"_B_o_o_l_e_a_n",
             +"_c_o_m_m_e_n_t",
             +"_d_o",
             +"_e_l_s_e",
             +"_e_n_d",
             +"_e_q_v",
             +"_E_X_I_T",
             +"_f_a_l_s_e",
             +"_f_o_r",
             +"_g_o_t_o",
             +"_t_o",
             +"_g_o",
             +"_i_f",
             +"_i_m_p",
             +"_i_n_t_e_g_e_r",
             +"_K_D_F_9",
             +"_l_a_b_e_l",
             +"_l_i_b_r_a_r_y",
             +"_n_o_t",
             +"_o_r",
             +"_o_w_n",
             +"_p_r_o_c_e_d_u_r_e",
             +"_r_e_a_l",
             +"_s_t_e_p",
             +"_s_t_r_i_n_g",
             +"_s_w_i_t_c_h",
             +"_t_h_e_n",
             +"_t_r_u_e",
             +"_u_n_t_i_l",
             +"_v_a_l_u_e",
             +"_w_h_i_l_e",
             +"_>",
             +"_<",
             +"±",
             +"±",
             +"^",
             +"÷",
             +"*",
             +"_[",
             +"_]"
            );


   de_bangs : constant vocabulary
         := (
             +"!library",
             +"!ALGOL",
             +"!and",
             +"!array",
             +"!begin",
             +"!boolean",
             +"!Boolean",
             +"!comment",
             +"!do",
             +"!else",
             +"!end",
             +"!eqv",
             +"!EXIT",
             +"!false",
             +"!for",
             +"!goto",
             +"!to",
             +"!go",
             +"!if",
             +"!imp",
             +"!integer",
             +"!KDF9",
             +"!label",
             +"!library",
             +"!not",
             +"!or",
             +"!own",
             +"!procedure",
             +"!real",
             +"!step",
             +"!string",
             +"!switch",
             +"!then",
             +"!true",
             +"!until",
             +"!value",
             +"!while",
             +"!ge",
             +"!le",
             +"!ne",
             +"!=",
             +"!up",
             +"!div",
             +"$ ",
             +"{",
             +"}"
            );

   de_kwote : constant vocabulary
         := (
             +"'library'",
             +"'ALGOL'",
             +"'and'",
             +"'array'",
             +"'begin'",
             +"'boolean'",
             +"'Boolean'",
             +"'comment'",
             +"'do'",
             +"'else'",
             +"'end'",
             +"'eqv'",
             +"'EXIT'",
             +"'false'",
             +"'for'",
             +"'goto'",
             +"'to'",
             +"'go'",
             +"'if'",
             +"'imp'",
             +"'integer'",
             +"'KDF9'",
             +"'label'",
             +"'library'",
             +"'not'",
             +"'or'",
             +"'own'",
             +"'procedure'",
             +"'real'",
             +"'step'",
             +"'string'",
             +"'switch'",
             +"'then'",
             +"'true'",
             +"'until'",
             +"'value'",
             +"'while'",
             +"'ge'",
             +"'le'",
             +"'ne'",
             +"'ne'",
             +"'up'",
             +"'div'",
             +"$ ",
             +"{",
             +"}"
            );

   FW_quotes : constant vocabulary := ( +"_[",  +"_]");

   braces    : constant vocabulary := ( +"{",   +"}");

   procedure convert (data : in out Unbounded_String; before, after : in vocabulary) is
      first : constant Natural := Index_Non_Blank(data);
      start : constant Natural := (if first = 0 then 1 else first);
      pos   : Natural;
   begin
      if Length(data) = 0 then
         return;
      end if;
      for i in before'Range loop
         pos := start;
         loop
            pos := Index(data, before(i).all, pos);
         exit when pos = 0;
            declare
               j    : constant Natural          := pos - 1;
               pre  : constant Unbounded_String := Unbounded_Slice(data, 1, j);
               k    : constant Natural          := pos + before(i).all'Length;
               post : constant Unbounded_String := Unbounded_Slice(data, k, Length(data));
            begin
               data := pre & after(i).all & post;
            end;
            pos := pos + before(i).all'Length;
         end loop;
      end loop;
   end convert;

   subtype source_code_line is String(1..256);

   procedure de_strop (program_name : in String) is

      procedure put_fixed_up (unfixed_line : in String) is
         fixed_up_line : String := unfixed_line;
      begin
         -- The following converts $ to a walgol quoted string space, i.e. *.
         for c of fixed_up_line loop
            if c  = '$' then
               c := '*';
            end if;
         end loop;
         print_line(fixed_up_line);
      end put_fixed_up;

      data : Unbounded_String;
      line : source_code_line;
      last : Natural range 0 .. source_code_line'Last;

      do_bangs, do_kwote, do_brace : Boolean;

   begin
      -- Add a program name line for WAlgol.
      print_line(program_name & "|");
      loop
         read_line(line, last);

         if last = 0 then
            New_Line;
         else
            -- * is a pain in the neck: used for multiply by kalgol, it is mapped to × for walgol.
            -- BUT, * is a "quoted string space" in walgol, so we cannot convert it willy nilly.
            -- These loops ensure that all kalgol quoted string spaces are reprsented by $
            --    and convert those special-character sequences that do not change the length.
            for i in 1..last-1 loop
               if    line(i..i+1)  = "_ " then
                     line(i..i+1) := "$ ";
               elsif line(i..i+1)  = ">=" then
                     line(i..i+1) := "_>";
               elsif line(i..i+1)  = "<=" then
                     line(i..i+1) := "_<";
               end if;
            end loop;
            for c of line(1..last) loop
               if    c  = '*' then
                     c := '×';
               elsif c  = '~' then
                     c := 'º';
               elsif c  = '%' then
                     c := '÷';
               elsif c  = '#' then
                     c := '±';
               end if;
            end loop;
            -- Convert reserved words to Flexowriter format.
            do_bangs := index_forward(line(1..last), "!", 1) > 0;
            do_kwote := index_forward(line(1..last), "'", 1) > 0;
            do_brace := index_forward(line(1..last), "{", 1) > 0;
            do_brace := do_brace or else index_forward(line(1..last), "}", 1) > 0;
            if not (do_bangs or do_kwote or do_brace) then
               -- There are no stropped basic symbols in this line.
               put_fixed_up(line(1..last));
            else
               -- Convert stropped symbols of either format.
               data := To_Unbounded_String(line(1..last));
               if do_bangs then
                  convert(data, de_bangs, re_flex);
               end if;
               if do_kwote then
                  convert(data, de_kwote, re_flex);
               end if;
               if do_brace then
                  convert(data, braces, FW_quotes);
               end if;
               put_fixed_up(To_String(data));
            end if;
         end if;

      end loop;
   end de_strop;

   procedure re_strop (re_strop : vocabulary) is
      line : source_code_line := (others => ' ');
      last : Natural range 0 .. source_code_line'Last;
      data : Unbounded_String;
   begin
      -- Suppress the WAlgol program name line for KAlgol.
      read_line(line, last);
      loop
         read_line(line, last);

         if last = 0 then
            New_Line;
         else
            -- Convert * used as a quoted string space to $ for kalgol.
            for c of line loop
               if    c  = '*' then
                     c := '$';
               end if;
            end loop;
            if index_forward(line(1..last), "_", 1) = 0 then
               -- No further changes are needed.
               print_line(line(1..last));
            else
               -- Convert to the re-strop format, leaving kalgol-valid Flexowriter symbols verbatim.
               data := To_Unbounded_String(line(1..last));
               convert(data, de_flex, re_strop);
               print_line(To_String(data));
            end if;
         end if;
      end loop;
   end re_strop;

   procedure complain (about : in String) is
   begin
      if about /= "" then
         report_line(about & "!");
      end if;
      report_line("usage: ports [ -b | -k | -f program_name ] ");
      raise Program_Error;
   end complain;

begin
   if CLI.Argument_Count = 0 then
      complain("Not enough parameters were given");
   end if;
   if upper(CLI.Argument(1)) in "-B" | "-K" then
      if CLI.Argument_Count > 1 then
         complain("Too many parameters were given");
      end if;
      re_strop(if upper(CLI.Argument(1)) = "-B" then re_bangs else re_kwote);
   elsif upper(CLI.Argument(1)) = "-F" then
      if CLI.Argument_Count < 2 then
         complain("No name parameter was given");
      elsif CLI.Argument_Count > 2 then
         complain("Too many parameters were given");
      end if;
      de_strop(upper(CLI.Argument(2)));
   else
      complain("'" & CLI.Argument(1) & "' is not a valid conversion flag");
   end if;
   flush_outputs;
exception
   when End_Error =>
      flush_outputs;
   when others =>
      flush_outputs;
      CLI.Set_Exit_Status(CLI.Failure);
end ports;
