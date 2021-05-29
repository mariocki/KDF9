-- Generate a formatted printout of a magnetic tape file.
--
-- This program is an auxiliary of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
--
-- The mtp program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
--
with from_5_hole;
with IOC_tape_data;
with KDF9_char_sets;
with OS_specifics;
with POSIX;

use  Ada.Characters.Handling;
use  Ada.Characters.Latin_1;
--
use  IOC_tape_data;
use  KDF9_char_sets;
use  OS_specifics;
use  POSIX;

procedure mtp is

   pragma Unsuppress(All_Checks);

   format_error, command_error, POSIX_error : exception;

   package CLI renames Ada.Command_Line;

   NL        : constant String :=  OS_specifics.EOL;

   bytes_out : Integer;

   procedure deal_with_an_output_error (where : in String; target : in Integer) is
   begin
      if bytes_out < target then
         raise POSIX_error
            with "POSIX.write error" & bytes_out'Image & " in " & where & "!";
      end if;
   end deal_with_an_output_error;

   procedure report (s : in String) is
   begin
      bytes_out := POSIX.write(2, s, s'Length);
      deal_with_an_output_error("report", s'Length);
   end report;

   procedure report_line is
   begin
      report(NL);
   end report_line;

   procedure report_line (s : String) is
   begin
      report(s);
      report_line;
   end report_line;

   procedure print (s : in String) is
   begin
      bytes_out := POSIX.write(1, s, s'Length);
      deal_with_an_output_error("print", s'Length);
   end print;

   procedure print_line is
   begin
      print(NL);
   end print_line;

   procedure print_line (s : String) is
   begin
      print(s);
      print_line;
   end print_line;

   procedure complain (about : in String) is
   begin
      if about /= "" then
         print_line(about & "!");
      end if;
      report_line("usage: mtp {MS}T{01234567}[T|{PQRS}{1357}{01234567}]");
      raise command_error;
   end complain;

   subtype slot_names is Character
      with Static_Predicate => slot_names in 'P' | 'Q' | 'R' | 'S';

   subtype option_flags is Character
      with Static_Predicate => option_flags in slot_names
                            or option_flags = 'T';

   type mtp_mode is (tape_analysis, plain_text_printing, OUT8_despooling);
   the_mode : mtp_mode := tape_analysis;

   slot_id   : Character := 'P';    -- default
   stream_id : String    := "30";   -- default

   no_header_has_been_printed : Boolean := True;
   an_LP_stream               : Boolean := False;
   a_5_track_TP               : Boolean := False;

   procedure process_the_parameter is
   begin
      if CLI.Argument_Count = 0 then
         complain("No parameter was given");
      end if;

      declare
         argument : constant String := To_Upper(CLI.Argument(1));
      begin
         -- Fail a too-short parameter.
         if argument'Length < 3 then
            complain("«" & argument & "» is too short");
         end if;

         -- Fail a too-long parameter.
         if argument'Length > 6 then
            complain("«" & argument & "» is too long");
         end if;

         -- Fail a non-MT parameter.
         if argument(1..2) not in "MT" | "ST" or else
               argument(3) not in '0' .. '7'     then
            complain("«" & argument & "» is not a valid MT unit");
         end if;

         -- Fail an impossible analysis suffix.
         if argument'Length >= 4 then
            if argument(4) not in option_flags then
               complain("«" & argument(4) & "» is not a valid option");
            end if;
            if argument(4) = 'T' then
               the_mode := plain_text_printing;
               if argument'Length > 4 then
                  complain("«" & argument & "» is not a valid parameter (too long)");
               end if;
               return;
            end if;
         end if;

         if argument'Length <= 4 then
            return;
         end if;

         -- Handle despooling parameter(s).
         if argument'Length < 6 then
            complain("«" & argument & "» is not a valid OUT8 despooling parameter (too short)");
         end if;

         the_mode := OUT8_despooling;
         if argument(4) not in slot_names then
               complain("«" & argument(5) & "» is not a valid TSD slot");
         end if;
         slot_id := argument(4);
         if argument(5) not in '1' | '3' | '5' | '7' then
            complain("«" & argument(5..6) & "» is not a valid OUT8 stream number");
         end if;
         if argument(6) not in '0' .. '7' then
            complain("«" & argument(5..6) & "» is not a valid OUT8 stream number");
         end if;
         stream_id := argument(5..6);
         an_LP_stream     := stream_id(1) in '3' | '7';
         a_5_track_TP := stream_id(1) = '5';
      end;
   end process_the_parameter;

   procedure configure_the_standard_outputs is
      fd : Integer with Warnings => Off;  -- only written, never read!
   begin
      ensure_UI_is_open;
      make_transparent(fd => 2);
      make_transparent(fd => 1);
   end configure_the_standard_outputs;

   procedure open_the_tape_file is
      fd : Integer with Warnings => Off;  -- only written, never read!
   begin
      fd := close(fd => 0);
      begin
         fd := open(CLI.Argument(1)(1..3), read_mode);
      exception
         when POSIX_IO_error =>
            report_line;
            report_line(CLI.Argument(1)(1..3) & " cannot be opened for reading!");
            raise command_error;
      end;
   end open_the_tape_file;

   the_slice      : String(1..MT_record_length);

   at_end_of_data : Boolean := False;
   slice_count    : Natural := 0;
   file_position  : Natural := 0;
   slice_size     : Natural := 0;
   slice_kind     : Character;
   slice_flags    : Character;
   bytes_read     : Integer;

   procedure report_an_invalid_tape is
      slice_flags_image : constant String := Natural'Image(Character'Pos(slice_flags));
   begin
      report_line;
      report_line
            (
             "This file is not formatted as a KDF9 tape for ee9: invalid metadata"
            & (if slice_kind  not in valid_slice_kinds   then ", kind " & slice_kind'Image  else "")
            & (if slice_flags not in valid_slice_flags   then ", flags" & slice_flags_image else "")
            & (if slice_size  not in 1..slice_size_limit then ", size"  & slice_size'Image  else "")
            & (if bytes_read /= MT_record_length         then ", read"  & bytes_read'Image  else "")
            & ", at file address"
            & file_position'Image
            & "!"
             );
      raise format_error;
   end report_an_invalid_tape;

   procedure read_a_slice is
   begin
      the_slice(1) := 'D';
      the_slice(2) := ' ';
      the_slice(3) := Character'Val(1);

      bytes_read := POSIX.read(0, the_slice, MT_record_length);

      if bytes_read < 0 then
         raise POSIX_error
            with "POSIX.read error code"
               & bytes_read'Image
               & " in read_a_slice!";
      end if;

      at_end_of_data := bytes_read = 0;
      if at_end_of_data then
         if slice_count = 0 then
            report_line;
            report_line("THIS IS AN EMPTY TAPE. IT MUST BE LABELLED FOR USE WITH KDF9.");
            report_line;
            raise format_error;
         end if;
         slice_kind  := 'Z'; -- Turn EOF into a slice.
         slice_flags := 'Z';
         slice_size  := 0;
         return;
      end if;

      slice_kind  := the_slice(1);
      slice_flags := the_slice(2);
      slice_size  := Character'Pos(the_slice(3));

      if slice_kind not in valid_slice_kinds           or else
            slice_flags not in valid_slice_flags       or else
               slice_size not in 1 .. slice_size_limit or else
                  bytes_read /= MT_record_length          then
        report_an_invalid_tape;
      end if;

      file_position := file_position + bytes_read;
      slice_count   := slice_count + 1;
   end read_a_slice;

   max_length  : constant := 32768*8;  -- This is the longest physically possible block.
   the_block   : String(1..max_length);
   block_size  : Natural range 0 .. max_length + 1;
   block_kind  : Character;
   block_flags : Character;

   procedure add_slice_to_block is
   begin
      the_block(block_size+1..block_size+slice_size) := the_slice(4..slice_size+3);
      block_size := block_size + slice_size;
   end add_slice_to_block;

   procedure read_the_next_block is
   begin
      block_size := 0;
      read_a_slice;
      block_kind  := slice_kind;
      block_flags := slice_flags;
      if at_end_of_data then
         return;
      end if;
      add_slice_to_block;
      loop
      exit when at_end_of_data or else slice_flags in final_slice_flags;
         read_a_slice;
         add_slice_to_block;
      end loop;
   end read_the_next_block;

   function decoded_stream_number (encoding : Character)
   return String is
      units_digit : constant Natural := Natural(CN_TR(encoding)) mod 8;
   begin
      return (
              case encoding is
                 when ' ' | '"' | '®' | '©' | '¬' | '#' | 'ß' | 'ñ' =>
                    Integer'Image(10+units_digit)(2..3),
                 when '&' | '?' | '!' | '%' | ''' | '$' | '~' | '/' =>
                    Integer'Image(30+units_digit)(2..3),
                 when '0' .. '7' =>
                    Integer'Image(50+units_digit)(2..3),
                 when '8' | '9' | '_' | 'o' | ';' | '+' | '-' | '.' =>
                    Integer'Image(70+units_digit)(2..3),
                 when others         =>
                    " ??"
             );
   end decoded_stream_number;

   function decoded_slot_name (encoding : Character)
   return Character is
   begin
      return (
              case encoding is
                 when '"'    => 'P',
                 when '#'    => 'Q',
                 when '_'    => 'R',
                 when '@'    => 'S',
                 when others => '?'
             );
   end decoded_slot_name;

   procedure verify_an_OUT8_block is
   begin
      if the_block(5..6) = "ØØ" then
         if the_block(1..4) = "0001" and then
                  the_block(7) = 'P' and then
                     the_block(8) = '"'  then
            return; -- This is the session identifier.
         elsif the_block(8) = '"' then
            return; -- This is another red tape block.
         else
            raise format_error;
         end if;
      elsif the_block(1..6) in "0001ØA" | "0001ØZ" | "}ØØØØØ" then
         return; -- This is a stream bracket or terminator.
      elsif the_block(8) in OUT8_selection_characters then
         return; -- This is a data block.
      else
         report_line;
         report_line(
                     "This is not an OUT 8 tape! BLOCK FOUND is «"
                   & the_block(1..block_size)
                   & "»!"
                    );
         raise format_error;
      end if;
   end verify_an_OUT8_block;

   --
   -- This flag is global as it must be preserved between calls to output plain text or OUT8 blocks.
   --
   Case_Normal  : Boolean := True;  -- EE KDF9  shift state
   Letters_Case : Boolean := False; -- Ferranti shift state

   -- If the data is in Ferranti 5-track code, convert it to KDF9 code
   function normalized (data : String; forced : Boolean := False)
   return String is
      force   : Boolean := True; -- Forces the use of letters case.
      result  : String(1..data'Length) := data;
      next    : Positive := 1;
      char    : Character;
      legible : Boolean;
   begin
      if not a_5_track_TP then
         return result;
      end if;
      for i in data'Range loop
         if forced then
            char := from_5_hole(data(i), force, legible);
         else
            char := from_5_hole(data(i), Letters_Case, legible);
         end if;
         if legible then
            result(next) := char; next := next + 1;
         end if;
      end loop;
      return result(1..next-1);
   end normalized;

   procedure print_the_OUT8_text (first : Positive := 9) is

      procedure despool_a_block_of_5_track_code is
      begin  -- despool_a_block_of_5_track_code
         print(normalized(the_block(first .. block_size)));
      end despool_a_block_of_5_track_code;

      procedure despool_a_block_of_LP_code is
         next : Positive := first;
         line : String(first..block_size);
      begin -- despool_a_block_of_LP_code
         for i in first .. block_size loop
            case the_block(i) is
               when '®' =>
                  line(next) := LF; next := next + 1;
               when '©' =>
                  line(next) := FF; next := next + 1;
               when '¬' =>
                  line(next) := HT; next := next + 1;
               when others =>
                  if to_LP(CN_TR(the_block(i))) not in E_M | W_F then
                     line(next) := to_LP(CN_TR(the_block(i))); next := next + 1;
                  end if;
            end case;
         end loop;
         print(line(first..next-1));
      end despool_a_block_of_LP_code;

      procedure despool_a_block_of_TP_code is
         next : Positive := first;
         line : String(first..block_size);
      begin -- despool_a_block_of_TP_code
         for i in first .. block_size loop
            case the_block(i) is
               when '®' =>
                  line(next) := LF; next := next + 1;
               when '©' =>
                  line(next) := FF; next := next + 1;
               when '¬' =>
                  line(next) := HT; next := next + 1;
               when 'ñ' =>
                  Case_Normal := True;
               when 'ß' =>
                  Case_Normal := False;
               when 'Ø' =>
                  null;
               when others =>
                  if Case_Normal then
                     line(next) := TP_CN(CN_TR(the_block(i))); next := next + 1;
                  else
                     line(next) := TP_CS(CN_TR(the_block(i))); next := next + 1;
                  end if;
            end case;
         end loop;
         print(line(first..next-1));
      end despool_a_block_of_TP_code;

   begin
      if a_5_track_TP then
         despool_a_block_of_5_track_code;
      elsif an_LP_stream then
         despool_a_block_of_LP_code;
      else
         despool_a_block_of_TP_code;
      end if;
   end print_the_OUT8_text;

   procedure despool_an_OUT8_block is
   begin
      -- Ignore irrelevant red tape blocks.
      if block_kind /= 'D' then return; end if;
      if slice_count = 1 then
         -- This is the tape label.
         return;
      end if;

      verify_an_OUT8_block;

      if block_flags in last_block_flags then
         return;  -- This is a "red tape" block.  Ignore it.
      end if;

      -- Analyse and print OUT 8 information.
      if the_block(8) in OUT8_selection_characters and then
            the_block(5..6) = "ØØ"                     then

         if the_block(1..4) = "0001" then
            if normalized(the_block(9..13+(if a_5_track_TP then 2 else 0))) = "  STR" then
               -- Redundant stream header.
               return;
            end if;
         end if;

         if the_block(1..4) = "0003"                           and then
               decoded_stream_number(the_block(7)) = stream_id and then
                  decoded_slot_name(the_block(8)) = slot_id        then
            if no_header_has_been_printed then
               -- I will say this only once.
               print(
                     "Despooling OUT 8 stream "
                   & stream_id
                   & " for slot "
                   & slot_id
                   & ", written on "
                    );
               declare
                  trim : constant Positive := (if a_5_track_TP then 2 else 1);
                  data : constant String   := normalized(the_block(9..block_size));
                  date : constant String   := normalized(data(16..data'Last-trim));
                  prog : constant String   := normalized(data(1..12), forced => True);
               begin
                  print_line(
                             date
                           & " by "
                           & prog
                            );
               end;
               print_line;
               no_header_has_been_printed := False;
            end if;
            return;
         end if;

         if the_block(9..13) = FF & "ØØØØ"    and then
               the_block(15..16) = FF & " "   and then
                  the_block(14) in slot_names     then
            -- This is a stream "bracket".  Ignore it.
            return;
         end if;

         if decoded_slot_name(the_block(8)) = slot_id         and then
                  decoded_stream_number(the_block(7)) = stream_id then
            print_the_OUT8_text;
         end if;

      end if;
   end despool_an_OUT8_block;

   procedure print_a_plain_text_block is
      line : String(1..block_size);
      next : Positive := 1;
   begin
      -- Ignore irrelevant red tape blocks.
      if block_kind /= 'D' then return; end if;
      if slice_count = 1 then
         -- This is the tape label.
         return;
      end if;

      -- from_5_hole and print printable text.
      for i in 1 .. block_size loop
         case the_block(i) is
            when '®' =>
               line(next) := LF; next := next + 1;
            when '©' =>
               line(next) := FF; next := next + 1;
            when '¬' =>
               line(next) := HT; next := next + 1;
            when 'ñ' =>
               Case_Normal := True;
            when 'ß' =>
               Case_Normal := False;
            when 'Ø' =>
               null;
            when others =>
               if Case_Normal then
                  line(next) := TP_CN(CN_TR(the_block(i))); next := next + 1;
               else
                  line(next) := TP_CS(CN_TR(the_block(i))); next := next + 1;
               end if;
         end case;
      end loop;
      print(line(1..next-1));
   end print_a_plain_text_block;

   the_data    : String(1..slice_size_limit);

   procedure get_the_next_slice is

      procedure from_5_hole (input : String; output : out String) is
         next : Positive := output'First;
      begin
         for i in input'Range loop
            output(next) :=
               (case input(i) is when LF => '®', when FF => '©', when HT => '¬', when others => input(i));
            next := next + 1;
         end loop;
      end from_5_hole;

   begin
      read_a_slice;
      from_5_hole(the_slice(4..MT_record_length), the_data);
   end get_the_next_slice;

   current_erasure_kind : Character := '?';
   current_erasure_size : Natural   := 0;

   procedure analyse_this_slice is
   begin
      case slice_kind is
         when 'D' =>
            print_line(
                       slice_count'Image
                     & HT
                     & (if slice_flags in last_block_flags then "* " else "  ")
                     & "«"
                     & the_data(1..slice_size)
                     & "»"
                      );
            if slice_flags in final_slice_flags then print_line; end if;

         when 'G' =>
            if current_erasure_kind /= 'G' then
               print(
                     slice_count'Image
                   & HT
                   & "  GAP  of"
                    );
               current_erasure_size := slice_size;
               current_erasure_kind := 'G';
            else
               current_erasure_size := current_erasure_size + slice_size;
            end if;

         when 'W' =>
            if current_erasure_kind /= 'W' then
               print(
                     slice_count'Image
                   & HT
                   & "  WIPE  of"
                    );
               current_erasure_size := slice_size;
               current_erasure_kind := 'W';
            else
               current_erasure_size := current_erasure_size + slice_size;
            end if;

         when 'e' =>
            print(slice_count'Image & HT);
            -- This is an even parity tape mark on a 7-track tape.
            print_line("  EVEN PARITY TAPE MARK");
            print_line;

         when 'o' =>
            print(slice_count'Image & HT);
            -- This is an odd parity tape mark on a 7-track tape.
            print_line("  ODD PARITY TAPE MARK");
            print_line;

         when others =>
            report_an_invalid_tape;
      end case;
   end analyse_this_slice;

   procedure finalize_any_erasure is
   begin
      if current_erasure_size /= 0 then
         print_line(current_erasure_size'Image & " erased characters");
         print_line;
         current_erasure_size := 0;
         current_erasure_kind := '?';
      end if;
   end finalize_any_erasure;

begin -- mtp
   process_the_parameter;

   configure_the_standard_outputs;

   open_the_tape_file;

   if the_mode = tape_analysis then
      -- Process the tape slice by slice.

      -- Deal specially with the first slice.
      get_the_next_slice;
      case slice_kind is
         when 'D' =>
            if the_data(1..8) = "00000000"          and then
                  the_data(9..slice_size) = "        "  then
               print_line("THIS IS A SCRATCH (ZERO) TAPE.");
               return;
            else
               print_line(
                          "TAPE LABEL TSN «"
                        & the_data(1..8)
                        & "», IDENTIFIER «"
                        & the_data(9..slice_size)
                        & "»"
                         );
               print_line;
            end if;
         when erasure_kinds =>
            print_line("THIS IS AN ERASED TAPE. IT MUST BE LABELLED FOR USE WITH KDF9.");
            raise format_error;
         when others =>
            report_an_invalid_tape;
      end case;

      -- Deal with any other slices.
      loop
         get_the_next_slice;
         if slice_kind /= current_erasure_kind then
            finalize_any_erasure;
         end if;
      exit when at_end_of_data;
         analyse_this_slice;
      end loop;
      print_line("EOF");

   else
      -- Process the tape complete block by complete block.
      loop
         read_the_next_block;
         if the_mode = plain_text_printing then
            print_a_plain_text_block;
         else
            despool_an_OUT8_block;
         end if;
      exit when at_end_of_data;
      end loop;

   end if;

exception

   when command_error | format_error =>
      CLI.Set_Exit_Status(CLI.Failure);

   when error : others =>
      report_line;
      report_line(Ada.Exceptions.Exception_Information(error));
      CLI.Set_Exit_Status(CLI.Failure);

end mtp;
