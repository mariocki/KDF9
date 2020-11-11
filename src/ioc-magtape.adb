-- ioc-magtape.adb
--
-- Emulation of magnetic tape buffer commonalities.
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

with exceptions;
with formatting;
with IOC;
with settings;

use  exceptions;
use  formatting;
use  settings;

package body IOC.magtape is

   pragma Unsuppress(All_Checks);

   use MT_brick_IO;

   function is_open (tape : in magtape.file)
   return Boolean is
   begin
      return Is_Open(tape.reel);
   end is_open;

   procedure open_RW (tape : in out magtape.file;
                      name : in String) is
   begin
      Open(tape.reel, Inout_File, name);
   exception
      when others =>
         Create(tape.reel, Inout_File, name);
   end open_RW;

   procedure open_RO (tape : in out magtape.file;
                      name : in String) is
   begin
      Open(tape.reel, In_File, name);
   end open_RO;

   procedure close (tape : in out magtape.file) is
   begin
      Close(tape.reel);
   end close;

   procedure write (tape  : in magtape.file;
                    index : in KDF9.word;
                    stuff : in IOC.magtape.brick) is
   begin
      Write(tape.reel, stuff, to => Positive_Count(index));
   exception
      when End_Error =>
         raise end_of_tape;
   end write;

   procedure read (tape  : in magtape.file;
                   index : in KDF9.word;
                   stuff : out IOC.magtape.brick) is
   begin
      Read(tape.reel, stuff, from => Positive_Count(index));
   exception
      when End_Error =>
         raise end_of_tape;
   end read;

   overriding
   procedure Initialize (the_deck : in out magtape.deck) is
   begin
      the_deck.device_name := logical_device_name_of(the_deck);
      open(the_deck);
      if not the_deck.is_open then
            output_line(the_deck.device_name
                      & " is on buffer #"
                      & oct_of(KDF9.Q_part(the_deck.number), 2)
                      & ", but no tape was mounted!");
         the_deck.is_abnormal := True;
         the_deck.is_offline  := True;
      elsif not the_deck.has_a_WP_ring then
            output_line(the_deck.device_name
                      & ", on buffer #"
                      & oct_of(KDF9.Q_part(the_deck.number), 2)
                      & ", does not have a Write Permit Ring.");
      end if;
      IOC.device(the_deck).Initialize;
   end Initialize;

   overriding
   procedure Finalize (the_deck : in out magtape.deck) is
   begin
      if the_deck.is_open then
         if the_deck.usage /= 0 and the_final_state_is_wanted then
            output_line(the_deck.device_name
                      & " on buffer #"
                      & oct_of(KDF9.Q_part(the_deck.number), 2)
                      & optional(the_deck.is_at_BOT,
                                 " is at BOT",
                                 optional(the_deck.is_at_ETW,
                                          " is at ETW",
                                          " is at gap" & KDF9.word'Image(the_deck.brick_number)
                                         )
                                )
                      & ", after"
                      & KDF9.word'Image(the_deck.gaps_crossed)
                      & " inter-block gap(s) and"
                      & KDF9.word'Image(the_deck.usage)
                      & " character(s).");
         end if;
         close(the_deck.tape);
      end if;
   exception
      when others =>
         output_line("Finalize error for buffer #"
                   & oct_of(KDF9.Q_part(the_deck.number))
                   & "; ");
         raise;
   end Finalize;

   function as_word (the_serial : String)
   return KDF9.word is
      word : KDF9.word := 0;
   begin
      for b in the_serial'Range loop
         word := (word * 2**6) or KDF9.word(CN_TR(the_serial(b)));
      end loop;
      return word;
   end as_word;

   procedure find_tape_labelled (the_label  : in magtape.short_label;
                                 its_number : out KDF9.buffer_number;
                                 its_serial : out KDF9.word) is
      the_brick : IOC.magtape.brick;
   begin
      for t in KDF9.buffer_number loop
         if is_unallocated(buffer(t))                and then
               buffer(t).kind = MT_kind              and then
                  is_at_BOT(magtape.deck(buffer(t).all)) then
            --read the label
            begin
               read(magtape.deck(buffer(t).all).tape, 1, the_brick);
               if the_brick.kind = MT_data_brick               and then
                    the_brick.data'Length >= 16                and then
                       the_brick.data(9 .. 16) = String(the_label) then
                  its_number := t;
                  its_serial := as_word(the_brick.data(1 .. 8));
                  return;
               end if;
            exception
               when end_of_tape =>
                  -- Treat empty tapes and zero labels specially for now!
                  if String(the_label) = (9 .. 16 => ' ') then
                     its_number := t;
                     its_serial := 0;
                     return;
                  end if;
                  null;
               when others =>
                  null;
            end;
         end if;
      end loop;
      trap_invalid_instruction("no such MT as '" & String(the_label) & "' is mounted");
   end find_tape_labelled;

   procedure find_tape_labelled (the_label  : in magtape.long_label;
                                 its_number : out KDF9.buffer_number;
                                 its_serial : out KDF9.word) is
      the_brick : IOC.magtape.brick;
   begin
      for t in KDF9.buffer_number loop
         if is_unallocated(buffer(t))                and then
               buffer(t).kind = MT_kind              and then
                  is_at_BOT(magtape.deck(buffer(t).all)) then
            --read the label
            begin
               read(magtape.deck(buffer(t).all).tape, 1, the_brick);
               if the_brick.kind = MT_data_brick               and then
                    the_brick.data'Length >= 24                and then
                       the_brick.data(9 .. 24) = String(the_label) then
                  its_number := t;
                  its_serial := as_word(the_brick.data(1 .. 8));
                  return;
               end if;
            exception
               when end_of_tape =>
                  -- Treat empty tapes and zero labels specially for now!
                  if String(the_label) = (9 .. 24 => ' ') then
                     its_number := t;
                     its_serial := 0;
                     return;
                  end if;
                  null;
               when others =>
                  null;
            end;
         end if;
      end loop;
      trap_invalid_instruction("no such MT as '" & String(the_label) & "' is mounted");
   end find_tape_labelled;

   procedure open (the_deck : in out magtape.deck) is
   begin
      the_deck.has_a_WP_ring := False;
      the_deck.tape.open_RW(the_deck.device_name);
      the_deck.has_a_WP_ring := True;
   exception
      when MT_brick_IO.Use_Error =>
         the_deck.tape.open_RO(the_deck.device_name);
   end open;

   overriding
   function is_open (the_deck : magtape.deck)
   return Boolean is
   begin
      return the_deck.tape.is_open;
   end is_open;

   overriding
   function usage (the_deck : magtape.deck)
   return KDF9.word is
   begin
      return the_deck.bytes_moved;
   end usage;

   overriding
   procedure close (the_deck : in out magtape.deck) is
   begin
      the_deck.tape.close;
   end close;

   not overriding
   function is_at_BOT (the_deck : magtape.deck)
   return Boolean is
   begin
      return the_deck.is_open and then
                the_deck.brick_number = 0;
   end is_at_BOT;

   the_ETW_position : constant := 10_000;

   not overriding
   function is_at_ETW (the_deck : magtape.deck)
   return Boolean is  -- STUB
   begin
      return the_deck.is_open and then
                the_deck.brick_number >= the_ETW_position;
   end is_at_ETW;

   not overriding
   function is_at_LBM (the_deck : magtape.deck)
   return Boolean is
   begin
      return the_deck.is_open and then
                the_deck.is_LBM_flagged;
   end is_at_LBM;

   procedure bump (word_address : in out KDF9.address; symbol_nr : in out KDF9.symbol_number) is
   begin
      if symbol_nr < 7 then
         symbol_nr := symbol_nr + 1;
      else
         symbol_nr := 0;
         word_address := word_address + 1;
      end if;
   end bump;

   -- See Manual §22.1.2, p.178, ¶2.
   gap_length  : constant KDF9.word := 140;

   function tape_traversal_time (the_deck : magtape.deck;
                                 gaps_crossed, erased_length : KDF9.word)
   return KDF9.microseconds is
   begin
      return the_deck.quantum
           * KDF9.microseconds(gaps_crossed * gap_length + erased_length);
   end tape_traversal_time;

   function data_transfer_time (the_deck   : magtape.deck;
                                byte_count : KDF9.word)
   return KDF9.microseconds is
   begin
      return the_deck.quantum * KDF9.microseconds(byte_count);
   end data_transfer_time;

   overriding
   function IO_elapsed_time_total (the_deck : magtape.deck)
   return KDF9.microseconds is
   begin
      return the_deck.elapsed_time_total;
   end IO_elapsed_time_total;

   procedure update_statistics (the_deck : in out magtape.deck;
                                gaps_crossed,
                                erased_length,
                                bytes_moved : in KDF9.word) is
      real_time : KDF9.microseconds;
   begin
      the_deck.gaps_crossed := the_deck.gaps_crossed + gaps_crossed;
      the_deck.erased_length := the_deck.erased_length + erased_length;
      the_deck.bytes_moved := the_deck.bytes_moved + bytes_moved;
      real_time := tape_traversal_time(the_deck, gaps_crossed, erased_length)
                 + data_transfer_time(the_deck, bytes_moved);
      the_deck.elapsed_time_total := the_deck.elapsed_time_total + real_time;
      add_in_the_IO_CPU_time(the_deck, bytes_moved);
      correct_transfer_time(the_deck, real_time);
   end update_statistics;

   procedure get_tape_brick (the_deck  : in out magtape.deck;
                             the_brick : out IOC.magtape.brick;
                             backwards : in Boolean := False) is
   begin
      the_deck.is_LBM_flagged := False;
      if not backwards then
         the_deck.brick_number := the_deck.brick_number + 1;
      end if;
      read(the_deck.tape, the_deck.brick_number, the_brick);
      if backwards then
         the_deck.brick_number := the_deck.brick_number - 1;
      end if;
      case the_brick.kind is
         when MT_data_brick =>
            update_statistics(the_deck, 1, 0, KDF9.word(the_brick.size));
         when MT_mark_brick =>
            update_statistics(the_deck, 1, 0, 0);
         when MT_erased_brick =>
            update_statistics(the_deck, 1, the_brick.erased_length, 0);
      end case;
   end get_tape_brick;

   procedure get_next_data_brick (the_deck  : in out magtape.deck;
                                  the_brick : out IOC.magtape.brick) is
   begin
      loop
         get_tape_brick(the_deck, the_brick);
      exit when the_brick.kind = MT_data_brick or the_deck.is_abnormal;
      end loop;
      the_deck.is_LBM_flagged := the_brick.is_LBM_flagged;
   end get_next_data_brick;

   procedure get_prev_data_brick (the_deck  : in out magtape.deck;
                                  the_brick : out IOC.magtape.brick) is
   begin
      loop
         get_tape_brick(the_deck, the_brick, backwards => True);
      exit when the_brick.kind = MT_data_brick;
      end loop;
      the_deck.is_LBM_flagged := the_brick.is_LBM_flagged;
   end get_prev_data_brick;

   procedure read_block (the_deck       : in out magtape.deck;
                         Q_operand      : in KDF9.Q_register;
                         reading_to_EM  : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      w         : KDF9.address := start_address;
      s         : KDF9.symbol_number := 0;
      the_brick : IOC.magtape.brick;
      the_last  : Positive;
   begin
      validate_range_access(start_address, end_address);
      get_next_data_brick(the_deck, the_brick);
      -- Disregard an incomplete final word; see Manual §22.1.5, p184, ¶2.
      if the_brick.data'Length mod 8 = 0 then
         the_last := the_brick.data'Last;
      else
         the_deck.is_abnormal := True;
         the_last := the_brick.data'Last - the_brick.data'Length mod 8;
      end if;
      -- Set the_last to the end of the earliest word containing End_Message,
      --    if such exists; else leave the_last unchanged.
      if reading_to_EM then
         to_locate_any_EM:
            for i in 1 .. the_last loop
               if the_brick.data(i) = KDF9.E_M then
                  the_last := i - i mod 8 + 8;
                  exit to_locate_any_EM;
               end if;
            end loop to_locate_any_EM;
      end if;
      -- Store the relevant words.
      for i in 1 .. the_last loop
         store_symbol(CN_TR(the_brick.data(i)), w, s);
      exit when (w = end_address) and (s = 7);
         bump(w, s);
      end loop;
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
         trap_invalid_instruction("attempt to read past ETW at brick"
                                & KDF9.word'Image(the_deck.brick_number)
                                & " of "
                                & the_deck.device_name);
   end read_block;

   procedure read_block_backwards (the_deck       : in out magtape.deck;
                                   Q_operand      : in KDF9.Q_register;
                                   reading_to_EM  : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      w         : KDF9.address := start_address;
      s         : KDF9.symbol_number := 0;
      the_brick : IOC.magtape.brick;
      the_first : Positive;
   begin
      validate_range_access(start_address, end_address);
      get_prev_data_brick(the_deck, the_brick);
      -- Disregard an incomplete initial word; see Manual §22.1.5, p184, ¶2.
      if the_brick.data'Length mod 8 = 0 then
         the_first := the_brick.data'First;
      else
         the_deck.is_abnormal := True;
         the_first := the_brick.data'First + the_brick.data'Length mod 8;
      end if;
      -- Set the_first to the start of the latest word containing End_Message,
      --    if such exists; else leave the_first unchanged.
      if reading_to_EM then
         to_locate_any_EM:
            for i in reverse the_first .. the_brick.data'Last loop
               if the_brick.data(i) = KDF9.E_M then
                  the_first := i - i mod 8 + 1;
                  exit to_locate_any_EM;
               end if;
            end loop to_locate_any_EM;
      end if;
      -- Store the relevant words.
      for i in the_first .. the_brick.data'Last loop
         store_symbol(CN_TR(the_brick.data(i)), w, s);
      exit when (w = end_address) and (s = 7);
         bump(w, s);
      end loop;
      -- And reverse them.
      mirror(start_address, w);
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
         raise end_of_tape with "when reading past brick"
                              & KDF9.word'Image(the_deck.brick_number)
                              & " of "
                              & the_deck.device_name;
   end read_block_backwards;

   procedure put_next_data_brick (the_deck  : in out magtape.deck;
                                  the_data  : in String;
                                  is_last   : in Boolean := False) is
      the_brick : constant IOC.magtape.brick := (kind => MT_data_brick,
                                                 size => the_data'Length,
                                                 data => the_data,
                                                 is_LBM_flagged => is_last);
   begin
      if not the_deck.has_a_WP_ring then
         trap_invalid_instruction("attempt to write a read-only tape");
      end if;
      the_deck.is_LBM_flagged := False;
      the_deck.brick_number := the_deck.brick_number + 1;
      write(the_deck.tape, the_deck.brick_number, the_brick);
      update_statistics(the_deck, 1, 0, KDF9.word(the_data'Length));
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
         trap_invalid_instruction("attempt to write past ETW at brick"
                                & KDF9.word'Image(the_deck.brick_number)
                                & " of "
                                & the_deck.device_name);
   end put_next_data_brick;

   procedure write (the_deck       : in out magtape.deck;
                    Q_operand      : in KDF9.Q_register;
                    is_LBM_flagged : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
   begin
      validate_range_access(start_address, end_address);
      declare
         next_byte : Positive := 1;
         the_data  : String(1 .. Positive(end_address-start_address+1)*8);
      begin
      word_loop:
         for w in start_address .. end_address loop
            for c in KDF9.symbol_number'Range loop
               the_data(next_byte) := TP_CN(fetch_symbol(w, c));
               next_byte := next_byte + 1;
            end loop;
         end loop word_loop;
         put_next_data_brick(the_deck, the_data, is_LBM_flagged);
      end;
   end write;

   procedure write_to_EM (the_deck       : in out magtape.deck;
                          Q_operand      : in KDF9.Q_register;
                          is_LBM_flagged : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
   begin
      validate_range_access(start_address, end_address);
      declare
         next_byte : Positive := 1;
         EM_found  : Boolean := False;
         the_data  : String(1 .. Positive(end_address-start_address+1)*8);
         symbol    : KDF9.symbol;
      begin
      word_loop:
         for w in start_address .. end_address loop
            for c in KDF9.symbol_number'Range loop
               symbol := fetch_symbol(w, c);
               EM_found := EM_found or (symbol = KDF9.End_Message);
               the_data(next_byte) := TP_CN(symbol);
               next_byte := next_byte + 1;
            end loop;
         exit word_loop when EM_found;
         end loop word_loop;
         put_next_data_brick(the_deck, the_data(1 .. next_byte-1), is_LBM_flagged);
      end;
   end write_to_EM;

   procedure skip_forwards (the_deck     : in out magtape.deck;
                            gaps_crossed : in KDF9.word) is
      the_brick  : IOC.magtape.brick;
   begin
      for i in 1 .. gaps_crossed loop
         get_next_data_brick(the_deck, the_brick);
         the_deck.is_abnormal := the_deck.is_abnormal or (the_brick.size mod 8 /= 0);
      exit when the_deck.is_LBM_flagged;
      end loop;
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
   end skip_forwards;

   procedure skip_backwards (the_deck     : in out magtape.deck;
                             gaps_crossed : in KDF9.word) is
      the_brick  : IOC.magtape.brick;
   begin
      for i in 1 .. gaps_crossed loop
         get_prev_data_brick(the_deck, the_brick);
         the_deck.is_abnormal := the_deck.is_abnormal or (the_brick.size mod 8 /= 0);
      exit when the_deck.is_at_BOT;  -- I.e., the tape is fully rewound.
      end loop;
   end skip_backwards;

   procedure erase_tape_gap (the_deck   : in out magtape.deck;
                             the_length : in KDF9.word;
                             gap_kind   : in tape_gap_kind := MGAP_gap) is
      new_erased_length : constant KDF9.word := the_length * 8;
      the_original : IOC.magtape.brick;
   begin
      if not the_deck.has_a_WP_ring then
         trap_invalid_instruction("attempt to erase a read-only tape");
      end if;
      the_deck.is_LBM_flagged := False;
      if gap_kind = MGAP_gap and
            the_deck.brick_number < KDF9.word(Size(the_deck.tape.reel)) then
         -- We are gapping; a wiped area must be under the write head.
         get_tape_brick(the_deck, the_original);
         if the_original.kind /= MT_erased_brick           or else
               new_erased_length > the_original.erased_length then
            trap_invalid_instruction("MGAPQq would overwrite data on " & the_deck.device_name);
         end if;
      end if;
      declare
         the_brick : constant IOC.magtape.brick
                   := (size => 0, kind => MT_erased_brick, erased_length => new_erased_length);
      begin
         the_deck.brick_number := the_deck.brick_number + 1;
         write(the_deck.tape, the_deck.brick_number, the_brick);
      end;
      update_statistics(the_deck, 1, new_erased_length, bytes_moved => 0);
   exception
      when IOC.magtape.end_of_tape =>
         the_deck.is_abnormal := True;
         raise;
   end erase_tape_gap;

   -- MRFQq
   overriding
   procedure PIA (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      read_block(the_deck, Q_operand, reading_to_EM => False);
      set_lockouts(Q_operand);
   end PIA;

   -- MFREQq
   overriding
   procedure PIB (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      read_block(the_deck, Q_operand, reading_to_EM => True);
      set_lockouts(Q_operand);
   end;

   -- as PIA
   overriding
   procedure PIC (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIA(Q_operand, set_offline);
   end PIC;

   -- as PIB
   overriding
   procedure PID (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIB(Q_operand, set_offline);
   end PID;

   -- MBRQq
   overriding
   procedure PIE (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if the_deck.is_at_BOT then
         trap_invalid_instruction("attempt to read backwards at BOT");
      end if;
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      read_block_backwards(the_deck, Q_operand, reading_to_EM => False);
      set_lockouts(Q_operand);
   end PIE;

   -- MBREQq
   overriding
   procedure PIF (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if the_deck.is_at_BOT then
         trap_invalid_instruction("attempt to read backwards to End_Message at BOT");
      end if;
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      read_block_backwards(the_deck, Q_operand, reading_to_EM => True);
      set_lockouts(Q_operand);
   end PIF;

   -- as PIE
   overriding
   procedure PIG (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIE(Q_operand, set_offline);
   end PIG;

   -- as PIF
   overriding
   procedure PIH (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIF(Q_operand, set_offline);
   end PIH;

   -- MFSKQq
   overriding
   procedure PMA (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0, is_DMAing => False);
      if Q_operand.M = 0 then
         skip_forwards(the_deck, 32768);  -- See Manual §22.1.9, p188, ¶1.
      else
         skip_forwards(the_deck, KDF9.word(Q_operand.M));
      end if;
   end PMA;

   -- MBTQq
   overriding
   procedure PMB (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_deck, Q_operand);
      validate_parity(the_deck);
      the_T_bit := KDF9.word(Boolean'Pos(is_at_BOT(the_deck)));
      take_note_of(Q_operand, the_deck.device_name, the_T_bit);
   end PMB;

   -- MLBQq
   overriding
   procedure PMC (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_deck, Q_operand);
      validate_parity(the_deck);
      the_T_bit := KDF9.word(Boolean'Pos(is_at_LBM(the_deck)));
      take_note_of(Q_operand, the_deck.device_name, the_T_bit);
   end PMC;

   -- MRWDQq
   overriding
   procedure PMD (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.is_abnormal := False;  -- See Manual §22.1.9, p.189, ¶-2.
      -- This is a STUB re timing: I don't know what the rewind speed was.
      if the_deck.is_at_BOT then
         -- No motion takes place; see Manual §22.1.9, p.190, ¶1.
         take_note_of(Q_operand,
                      the_deck.device_name,
                      KDF9.word(Boolean'Pos(the_deck.is_at_BOT))
                     );
      else
         the_deck.brick_number := the_deck.brick_number - 1;
         start_timed_transfer(the_deck, Q_operand, set_offline, 0, is_DMAing => False);
         skip_backwards(the_deck, the_deck.brick_number);
      end if;
      if not the_deck.is_at_BOT then
         trap_invalid_instruction("not at BOT after rewinding " & the_deck.device_name);
      end if;
   end PMD;

   -- MBSKQq
   overriding
   procedure PME (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if the_deck.is_at_BOT then
         trap_invalid_instruction("attempt to skip backwards at BOT");
      end if;
      start_timed_transfer(the_deck, Q_operand, set_offline, 0, is_DMAing => False);
      if Q_operand.M = 0 then
         skip_backwards(the_deck, 32768);  -- See Manual §22.1.9, p188, ¶1.
      else
         skip_backwards(the_deck, KDF9.word(Q_operand.M));
      end if;
   end PME;

   -- METQq
   overriding
   procedure PMF (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_deck, Q_operand);
      validate_parity(the_deck);
      the_T_bit := KDF9.word(Boolean'Pos(is_at_ETW(the_deck)));
      take_note_of(Q_operand, the_deck.device_name, the_T_bit);
   end PMF;

   -- MWQq
   overriding
   procedure POA (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      write(the_deck, Q_operand);
      set_lockouts(Q_operand);
   end POA;

   -- MWEQq
   overriding
   procedure POB (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      write_to_EM(the_deck, Q_operand);
      set_lockouts(Q_operand);
   end POB;

   -- MLWQq
   overriding
   procedure POC (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      write(the_deck, Q_operand, is_LBM_flagged => True);
      set_lockouts(Q_operand);
   end POC;

   -- MLWEQq
   overriding
   procedure POD (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0);
      write_to_EM(the_deck, Q_operand, is_LBM_flagged => True);
      set_lockouts(Q_operand);
   end POD;

   -- MGAPQq
   overriding
   procedure POE (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0, is_DMAing => False);
      require_positive_count(Q_operand.M);
      erase_tape_gap(the_deck, KDF9.word(Q_operand.M), gap_kind => MGAP_gap);
   end POE;

   -- MWIPEQq
   overriding
   procedure POF (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_timed_transfer(the_deck, Q_operand, set_offline, 0, is_DMAing => False);
      require_positive_count(Q_operand.M);
      erase_tape_gap(the_deck, KDF9.word(Q_operand.M), gap_kind => MWIPE_gap);
   end POF;

end IOC.magtape;
