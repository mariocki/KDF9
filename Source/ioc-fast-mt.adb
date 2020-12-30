-- ioc-fast-MT.adb
--
-- Emulation of magnetic tape decks and buffers.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Exceptions;
with Ada.IO_Exceptions;
--
with HCI;
with tracing;

use  HCI;
use  tracing;

package body IOC.fast.MT is

   --
   -- Ada direct-access file management.
   --

   procedure open_RO (the_tape : in out MT.file; name : in String) is
   begin
      MT_slice_IO.Open(the_tape.reel, In_File, name);
      the_tape.has_a_WP_ring := False;
   exception
      when others =>
         trap_operator_error(name, "cannot be opened for reading or writing");
   end open_RO;

   procedure open_RW (the_tape : in out MT.file; name : in String) is
   begin
      MT_slice_IO.Open(the_tape.reel, Inout_File, name);
      the_tape.has_a_WP_ring := True;
   exception
      when Ada.IO_Exceptions.Use_Error =>
         the_tape.has_a_WP_ring := False;
         open_RO(the_tape, name);
      when Ada.IO_Exceptions.Name_Error =>
         trap_operator_error(name, "cannot be found");
      when error : others =>
         trap_operator_error(name, "failed to open: " &  Ada.Exceptions.Exception_Message(error));
   end open_RW;

   procedure close (the_tape : in out MT.file) is
   begin
      if the_tape.has_a_WP_ring then
         MT_slice_IO.Flush(the_tape.reel);
      end if;
      MT_slice_IO.Close(the_tape.reel);
   end close;

   function is_open (the_tape : in MT.file)
   return Boolean
   is (Is_Open(the_tape.reel));

   -- Slice management.

   end_of_tape : exception;

   procedure write_slice (the_tape : in out MT.file;
                          slice    : in MT.slice) is
   begin
      the_tape.position := the_tape.position + 1;
      MT_slice_IO.Write(the_tape.reel, slice, to => the_tape.position);
      if slice.kind not in tape_gap_kind then
         the_tape.last_data_index := Count'Max(the_tape.last_data_index, the_tape.position);
      end if;
   exception
      when End_Error =>
         raise emulation_failure with "End_Error writing MT slice" & the_tape.position'Image;
   end write_slice;

   procedure read_next_slice (the_tape : in out MT.file;
                              slice    : out MT.slice) is
   begin
      if the_tape.last_data_index > 0 then
         the_tape.position := the_tape.position + 1;
         MT_slice_IO.Read(the_tape.reel, slice, from => the_tape.position);
      else
         raise end_of_tape with "read_next_slice";
      end if;
    exception
       when End_Error =>
          raise emulation_failure with "End_Error reading MT slice" & the_tape.position'Image;
   end read_next_slice;

   procedure read_prev_slice (the_tape : in out MT.file;
                              slice    : out MT.slice) is
   begin
      if the_tape.position > 0 then
         MT_slice_IO.Read(the_tape.reel, slice, from => the_tape.position);
         the_tape.position := the_tape.position - 1;
      else
         raise end_of_tape with "read_prev_slice";
      end if;
    exception
       when End_Error =>
          raise end_of_tape with "End_Error reading MT slice number" & the_tape.position'Image;
   end read_prev_slice;

   procedure bound_the_written_data (the_tape : in out MT.file) is
      the_slice : MT.slice;
   begin
      the_tape.position := Size(the_tape.reel);
      if the_tape.position = 0 then
         -- There is no data in the file.
         the_tape.last_data_index := 0;
         return;
      end if;
      -- Locate the last data slice (if any).
      while the_tape.position > 0 loop
         read_prev_slice(the_tape, the_slice);
      exit when the_slice.kind not in MT.tape_gap_kind;
      end loop;
      if the_slice.kind in MT.tape_gap_kind then
         the_tape.last_data_index := 0;
      else
         the_tape.last_data_index := the_tape.position + 1;
      end if;
      the_tape.position := 0;
   end bound_the_written_data;

   procedure reset (the_deck : in out MT.deck) is
    begin
      bound_the_written_data(the_deck.tape);
      the_deck.is_LBM_flagged := False;
      the_deck.is_abnormal := False;
      the_deck.unwound_frames := 0;
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
         the_deck.is_LBM_flagged := False;
         the_deck.unwound_frames := 0;
   end reset;

   -- Tape physical characteristics.

   -- The physical end of tape (PET) is signalled one maximum block length before the tape runs out.
   --  So PET is signalled at max_block_size before the absolute maximum position
   --    to avoid running past the end of the tape when a very large block is written.

   -- There could be as little as 60 inches of tape between the End of Tape Warning (ETW) and PET.
   -- See the Manual, §22.1.3, p.182.

   overriding
   procedure Initialize (the_deck : in out MT.deck) is
   begin
      the_deck.device_name := device_name_of(the_deck);
      open_RW(the_deck.tape, the_deck.device_name);
      Initialize(IOC.device(the_deck));
      if the_deck.kind = MT_kind then
         the_deck.terminator        := End_Message;
         the_deck.recording_density := max_bits_per_inch;  -- bits / inch
         the_deck.max_reel_length   := max_reel_length;    -- inches
      else
         the_deck.terminator        := Group_Mark;
         the_deck.recording_density := max_bits_per_inch/2;  -- bits / inch
         the_deck.max_reel_length   := max_reel_length;      -- inches
      end if;
      the_deck.inter_block_gap := the_deck.recording_density / 3;
      the_deck.tape_capacity   := the_deck.max_reel_length * the_deck.recording_density;
      the_deck.PET_position    := the_deck.tape_capacity - max_block_size;
      the_deck.ETW_position    := the_deck.PET_position - 60 * the_deck.recording_density;
      reset(the_deck);
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
   end Initialize;

   function is_at_BTW (the_deck : MT.deck)
   return Boolean
   is (the_deck.is_open and then the_deck.tape.position = 0);

   function holds_data (the_deck : MT.deck)
   return Boolean
   is (the_deck.is_open and then the_deck.tape.last_data_index > 0);

   function is_at_ETW (the_deck : MT.deck)
   return Boolean
   is (the_deck.is_open and then the_deck.unwound_frames >= the_deck.ETW_position);

   function is_at_PET (the_deck : MT.deck)
   return Boolean
   is (the_deck.is_open and then the_deck.unwound_frames >= the_deck.PET_position);

   procedure deal_with_trying_to_pass_PET (the_deck : in out MT.deck;
                                           do_this  : String) is
   begin
      if is_at_PET (the_deck) then
         trap_failing_IO_operation(the_deck, "an attempt was made to " & do_this & " past PET");
      end if;
   end deal_with_trying_to_pass_PET;

   -- There are cases that are invalid iff the tape is positioned beyond the last written block.
   function is_at_EOD (the_deck : MT.deck)
   return Boolean
   is (the_deck.is_open and then the_deck.tape.position > the_deck.tape.last_data_index);

   function tape_traversal_time (the_deck : MT.deck; tape_crossed : KDF9.word)
   return KDF9.us
   is (the_deck.quantum * KDF9.us(tape_crossed));

   function data_transfer_time (the_deck   : MT.deck;
                                byte_count : KDF9.word)
   return KDF9.us
   is (the_deck.quantum * KDF9.us(byte_count));

   -- This is the time the MT deck is busy traversing the interblock gap and the data block.
   function MT_IO_time (the_deck  : MT.deck;
                        Q_operand : in KDF9.Q_register)
   return KDF9.us
   is (KDF9.us(the_deck.inter_block_gap) + 8*KDF9.us(Q_operand.M-Q_operand.I+1) * the_deck.quantum);

   overriding
   function is_open (the_deck : MT.deck)
   return Boolean
   is (the_deck.tape.is_open);

   overriding
   function usage (the_deck : MT.deck)
   return KDF9.word
   is (the_deck.bytes_moved);

   overriding
   procedure close (the_deck : in out MT.deck) is
   begin
      the_deck.tape.close;
   end close;

   procedure update_statistics (the_deck    : in out MT.deck;
                                tape_crossed,
                                bytes_moved : in length_in_frames) is
      real_time : KDF9.us;
   begin
      the_deck.bytes_moved := the_deck.bytes_moved + KDF9.word(bytes_moved);
      real_time := tape_traversal_time(the_deck, KDF9.word(tape_crossed))
                 + data_transfer_time (the_deck, KDF9.word(bytes_moved));
      the_deck.elapsed_time := the_deck.elapsed_time + real_time;
      add_in_the_IO_CPU_time(the_deck, KDF9.word(bytes_moved));
      correct_transfer_time(the_deck, real_time);
   end update_statistics;

   type movement is (forwards, backwards);

   procedure note_tape_position (the_deck    : in out MT.deck;
                                 direction   : in MT.movement;
                                 tape_crossed,
                                 bytes_moved : in length_in_frames) is
   begin
      if direction = forwards then
         the_deck.unwound_frames := the_deck.unwound_frames
                                  + MT.length_in_frames(tape_crossed + bytes_moved);
      elsif MT.length_in_frames(tape_crossed + bytes_moved) > the_deck.unwound_frames then
         the_deck.unwound_frames := 0;
      else
         the_deck.unwound_frames := the_deck.unwound_frames
                                  - MT.length_in_frames(tape_crossed + bytes_moved);
      end if;
   end note_tape_position;

   -- KDF9 MT operations.

   -- Skip back over erased tape, leaving the_slice containing the next preceding data.
   -- Postcondition: the_deck.is_at_BTW or else the_slice.kind not in tape_gap_kind
   procedure skip_back_over_erasure (the_deck  : in out MT.deck;
                                     the_slice : in out MT.slice;
                                     crossed   : in out length_in_frames) is
   begin
      if the_deck.is_at_BTW then
         return; -- We are as far back as we can go;
      end if;
      if the_slice.kind in data_kind then
         return;  -- We have already found the preceding data block.
      end if;
      loop
         read_prev_slice(the_deck.tape, the_slice);
      exit when the_deck.is_at_BTW or else the_slice.kind not in tape_gap_kind;
         crossed := crossed + the_slice.size;
      end loop;
   end skip_back_over_erasure;

   -- Skip forward over erased tape, leaving the_slice containing the next following data.
   -- Postcondition: the_deck.is_at_EOD or else the_slice.kind not in tape_gap_kind
   procedure skip_forward_over_erasure (the_deck  : in out MT.deck;
                                        the_slice : in out MT.slice;
                                        crossed   : in out length_in_frames;
                                        caller    : in String := "") is
   begin
      if the_slice.kind in data_slice then
         return;
      end if;
      loop
         read_next_slice(the_deck.tape, the_slice);
      exit when the_deck.is_at_EOD or else the_slice.kind not in MT.tape_gap_kind;
         crossed := crossed + the_slice.size;
      end loop;
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
         raise end_of_tape with "in skip_forward_over_erasure for " & caller;
   end skip_forward_over_erasure;

   -- Deal with blocks of invalid sizes.
   -- 1081 buffers always write and read a whole number of words;
   --    see Manual §22.1.5, p184, ¶2; and Appendix 7 ¶3, p318.
   -- §3.4.7 of the EGDON 3 manual says that the 7-track tape buffer, due to a hardware
   --    limitation, rejects blocks (other than tape marks) of less than 6 characters.
   procedure handle_any_abnormality (the_deck : in out MT.deck;
                                     the_size : in length_in_frames) is
   begin
      case the_deck.kind is
         when MT_kind =>
            the_deck.is_abnormal := the_deck.is_abnormal or (the_size mod 8 /= 0);
         when ST_kind =>
            the_deck.is_abnormal := the_deck.is_abnormal or (the_size < 6);
         when others  =>
            raise emulation_failure
               with "handle_any_abnormality with a deck kind given as "
                  & the_deck.kind'Image
                  & " by "
                  & the_deck.device_name
                  & " with block size"
                  & the_size'Image;
      end case;
   end handle_any_abnormality;

   procedure read_block (the_deck  : in out MT.deck;
                         the_data  : out MT.block_storage;
                         the_size  : out length_in_frames;
                         direction : in movement := forwards) is

      left,
      right      : length_in_frames := 1;
      block_size,
      crossed    : length_in_frames := 0;
      is_last,
      is_flagged : Boolean := False;
      the_slice  : MT.slice := a_NULL_slice;
   begin
      the_deck.is_LBM_flagged := False;

      skip_forward_over_erasure(the_deck, the_slice, crossed, caller => "read_block");

      -- Ensure that we are not beyond the end of valid data.
      if the_deck.is_at_EOD then
         trap_failing_IO_operation(
                                   the_deck,
                                   "there is no data past slice" & the_deck.tape.position'Image
                                  );
      end if;

      if the_slice.kind in MT.tape_mark_kind then
         -- Deal with a tape mark block; according to the Maual, Appendix 7, §2, p.317,
         --    it reads as a single character with value #17.
         block_size := 8;
         the_data(1)    := KDF9_char_sets.TP_CN(KDF9_char_sets.Tape_Mark);
         the_data(2..8) := (others => KDF9_char_sets.TP_CN(KDF9_char_sets.Blank_Space));
         the_deck.is_LBM_flagged := True;
      else
         -- We have a bona fide data block.
         the_size := 0;
         -- Accumulate a series of slicefuls.
         loop
            right := left + the_slice.size - 1;
            the_data(left .. right) := the_slice.data(1..the_slice.size);
            block_size := block_size + the_slice.size;
            left := left + the_slice.size;
            is_flagged := is_flagged or the_slice.is_LBM_flagged;
            is_last  := the_slice.is_last;
         exit when is_last or block_size = max_block_size;
            read_next_slice(the_deck.tape, the_slice);
         end loop;
         the_deck.is_LBM_flagged := is_flagged;
      end if;
      the_size := block_size;

      note_tape_position(the_deck, direction,
                        crossed + the_deck.inter_block_gap, bytes_moved => the_size);
      update_statistics(the_deck,
                        crossed + the_deck.inter_block_gap, bytes_moved => the_size);

     if not is_last and block_size = max_block_size then
         raise emulation_failure with block_size'Image & " > max_block_size in MT read_block";
      end if;
      handle_any_abnormality(the_deck, block_size);
   end read_block;

   procedure increment (word_address : in out KDF9.address;
                        symbol_nr    : in out KDF9_char_sets.symbol_index) is
   begin
      if symbol_nr < 7 then
         symbol_nr := symbol_nr + 1;
      else
         symbol_nr := 0;
         word_address := word_address + 1;
      end if;
   end increment;

   tape_mark_data_word : constant KDF9.word := 8#17_00_00_00_00_00_00_00#;

   procedure read (the_deck       : in out MT.deck;
                   Q_operand      : in KDF9.Q_register;
                   to_terminator  : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      the_data : MT.block_storage;
      s        : KDF9_char_sets.symbol_index;
      w        : KDF9.address;
      stored   : KDF9.word := 0;
      the_size : length_in_frames;
   begin
      validate_device(the_deck, Q_operand);
      check_addresses_and_lockouts(start_address, end_address);

      read_block(the_deck, the_data, the_size);

      if the_size mod 8 /= 0 and the_deck.kind = MT_kind then
         -- Disregard an incomplete final word; see Manual, §22.1.5, p184, ¶2.
         the_deck.is_abnormal := True;
         the_size := the_size - the_size mod 8;
      end if;

      -- Store the relevant words.
      w := start_address;
      s := 0;
      for i in 1 .. the_size loop
         if s = 0 then
            store_word(0, w);
         end if;
         store_symbol(CN_TR(the_data(i)), w, s);
         stored := stored + 1;
      exit when (w = end_address) and (s = 7);
      exit when to_terminator and CN_TR(the_data(i)) = the_deck.terminator;
         increment(w, s);
      end loop;
      if to_terminator then
         correct_transfer_time(the_deck, stored);
      end if;
   exception
      when end_of_tape =>
         deal_with_trying_to_pass_PET(the_deck, "reading");
   end read;

   procedure find_start_of_earlier_block (the_deck : in out MT.deck;
                                          crossed  : in out length_in_frames) is
      the_slice  : MT.slice := a_NULL_slice;
      block_size : length_in_frames;
   begin
      if the_deck.is_at_BTW then
         return; -- We have already gone as far back as possible.
      end if;

      -- Skip back over any erasures or tape marks.
      skip_back_over_erasure(the_deck, the_slice, crossed);
      crossed := crossed + the_deck.inter_block_gap;

      if the_deck.is_at_BTW and the_slice.kind in tape_gap_kind then
         the_deck.is_abnormal := True;
         -- This cannot happen if the tape has (at least) a label.
         raise emulation_failure with "no earlier block, at BTW on " & the_deck.device_name;
      end if;

      if not the_slice.is_last then
         raise emulation_failure
            with "find_start_of_earlier_block at slice "
               & the_deck.tape.position'Image
               & " of "
               & the_deck.device_name
               & " failed to locate the last slice of a block";
      end if;

      -- We have reached the last slice of the block.
      if the_deck.kind = ST_kind and the_slice.kind in tape_mark_kind then
         block_size := 1;
      else
         block_size := the_slice.size;
         -- Jump backwards over data slices until we reach the first of the block.
         while not the_slice.is_first and then the_deck.tape.position > 0 loop
            read_prev_slice(the_deck.tape, the_slice);
            block_size := block_size + the_slice.size;
         end loop;
         handle_any_abnormality(the_deck, block_size);
      end if;

      crossed := crossed + block_size;
   end find_start_of_earlier_block;

   procedure decrement (word_address : in out KDF9.address;
                        symbol_nr    : in out KDF9_char_sets.symbol_index) is
   begin
      if symbol_nr > 0 then
         symbol_nr := symbol_nr - 1;
      else
         symbol_nr := 7;
         word_address := word_address + 1;
      end if;
   end decrement;

   procedure read_backwards (the_deck       : in out MT.deck;
                             Q_operand      : in KDF9.Q_register;
                             to_terminator  : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      terminator    : constant KDF9_char_sets.symbol := the_deck.terminator;
      the_data  : MT.block_storage;
      s         : KDF9_char_sets.symbol_index;
      w         : KDF9.address;
      the_first,
      the_last  : length_in_frames;
      crossed   : length_in_frames := 0 with Warnings => Off;  -- Because its value is never used.
   begin
      validate_device(the_deck, Q_operand);
      check_addresses_and_lockouts(start_address, end_address);

      -- Locate the start of the previous block.
      find_start_of_earlier_block(the_deck, crossed);

      -- Read it normally, i.e. forwards.
      read_block(the_deck, the_data, the_last, backwards);

      -- And retrace our steps, to position the tape as if the block had been read backwards.
      find_start_of_earlier_block(the_deck, crossed);

      -- Disregard an incomplete first word; see Manual §22.1.5, p184, ¶2; and Appendix 7 ¶3, p318.
      if the_last mod 8 = 0 or the_deck.kind = ST_kind then
         the_first := the_data'First;
      elsif the_last = 1 and the_deck.kind = ST_kind then
         -- See Manual, Appendix 7 ¶2, p317.
         store_word(tape_mark_data_word, start_address);
      else
         the_deck.is_abnormal := True;
         the_first := the_data'First + the_last mod 8;
      end if;

      -- Store the relevant words.
      w := start_address;
      s := 7;
      for i in reverse the_first .. the_last loop
         if s = 7 then
            store_word(0, w);
         end if;
         store_symbol(CN_TR(the_data(i)), w, s);
      exit when to_terminator and CN_TR(the_data(i)) = terminator;
         decrement(w, s);
      end loop;
   end read_backwards;

   -- MFRQq
   overriding
   procedure PIA (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      start_data_transfer(the_deck, Q_operand, set_offline, time, input_operation);
      read(the_deck, Q_operand, to_terminator => False);
      lock_out_relative_addresses(Q_operand);
   end PIA;

   -- MFREQq
   overriding
   procedure PIB (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      start_data_transfer(the_deck, Q_operand, set_offline, time, input_operation);
      read(the_deck, Q_operand, to_terminator => True);
      lock_out_relative_addresses(Q_operand);
   end PIB;

   -- as PIA
   overriding
   procedure PIC (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIA(Q_operand, set_offline);
   end PIC;

   -- as PIB
   overriding
   procedure PID (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIB(Q_operand, set_offline);
   end PID;

   -- MBRQq
   overriding
   procedure PIE (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      if the_deck.is_at_BTW then
         trap_illegal_instruction("MBRQq at BTW on " & the_deck.device_name);
      end if;
      start_data_transfer(the_deck, Q_operand, set_offline, time, input_operation);
      read_backwards(the_deck, Q_operand, to_terminator => False);
      if the_deck.kind = ST_kind then
         the_deck.is_LBM_flagged := False;
      end if;
      lock_out_relative_addresses(Q_operand);
   end PIE;

   -- MBREQq
   overriding
   procedure PIF (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      if the_deck.is_at_BTW then
         trap_illegal_instruction("MBREQq at BTW on " & the_deck.device_name);
      end if;
      start_data_transfer(the_deck, Q_operand, set_offline, time, input_operation);
      read_backwards(the_deck, Q_operand, to_terminator => True);
      if the_deck.kind = ST_kind then
         the_deck.is_LBM_flagged := False;
      end if;
      lock_out_relative_addresses(Q_operand);
   end PIF;

   -- as PIE
   overriding
   procedure PIG (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIE(Q_operand, set_offline);
   end PIG;

   -- as PIF
   overriding
   procedure PIH (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      the_deck.PIF(Q_operand, set_offline);
   end PIH;

   procedure find_start_of_later_block (the_deck : in out MT.deck;
                                        crossed  : in out length_in_frames) is
      the_slice  : MT.slice := a_NULL_slice;
      block_size : length_in_frames := 0;
   begin
      -- Skip over any erasures or tape marks.
      skip_forward_over_erasure(the_deck, the_slice, crossed, caller => "find_start_of_later_block");
      crossed := crossed + the_deck.inter_block_gap;

      if not the_slice.is_first then
         raise emulation_failure
            with "find_start_of_later_block at slice"
               & the_deck.tape.position'Image
               & " of "
               & the_deck.device_name
               & " failed to locate the first slice of a block";
      end if;

      -- We have reached the first slice of the block.
      if the_deck.kind = ST_kind and the_slice.kind in tape_mark_kind then
         block_size := 1;
      else
         block_size := the_slice.size;
         -- Ignore data slices until we get to the last slice of the block.
         while not the_slice.is_last loop
            read_next_slice(the_deck.tape, the_slice);
            block_size := block_size + the_slice.size;
         end loop;
         handle_any_abnormality(the_deck, block_size);
      end if;

      the_deck.is_LBM_flagged := the_slice.is_LBM_flagged;
      crossed := crossed + block_size;
   exception
      when end_of_tape =>
         the_deck.is_abnormal := True;
         raise end_of_tape with "find_start_of_later_block";
   end find_start_of_later_block;

   procedure skip_forwards (the_deck       : in out MT.deck;
                            blocks_skipped : in KDF9.word) is
      crossed : length_in_frames := 0;
   begin
      for i in 1 .. blocks_skipped loop
         find_start_of_later_block(the_deck, crossed);
      -- MFSKQq stops at an LBM-flagged block, or on count expiry.
      -- Unlike MBSKQq it does record having seen an LBM-flagged block during the skipping.
      -- See the Manual, §22.1.3, p.183, ¶1 and §22.1.9, p.188, ¶-2.
      exit when the_deck.is_LBM_flagged;
      end loop;
      note_tape_position(the_deck, forwards, crossed, bytes_moved => 0);
      update_statistics(the_deck, crossed, bytes_moved => 0);
   end skip_forwards;

   -- MFSKQq
   overriding
   procedure PMA (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_data_transfer(the_deck, Q_operand, set_offline, 19);
      if Q_operand.M = 0 then
         skip_forwards(the_deck, 32768);  -- See Manual §22.1.9, p188, ¶1.
      else
         require_positive_count(Q_operand.M);
         skip_forwards(the_deck, KDF9.word(Q_operand.M));
      end if;
   end PMA;

   -- MBTQq
   overriding
   procedure PMB (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_deck, Q_operand);
      validate_parity(the_deck);
      deal_with_a_busy_device(the_deck, 14, set_offline);
      the_T_bit_is_set := the_deck.is_at_BTW and the_deck.holds_data;
      take_note_of_test(the_deck.device_name, Q_operand, the_T_bit_is_set);
   end PMB;

   -- MLBQq
   overriding
   procedure PMC (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_deck, Q_operand);
      validate_parity(the_deck);
      deal_with_a_busy_device(the_deck, 14, set_offline);
      the_T_bit_is_set := the_deck.is_LBM_flagged;
      the_deck.is_LBM_flagged := False;
      take_note_of_test(the_deck.device_name, Q_operand, the_T_bit_is_set);
   end PMC;

   procedure skip_backwards (the_deck       : in out MT.deck;
                             blocks_skipped : in KDF9.word) is
      crossed : length_in_frames := 0;
   begin
      for i in 1 .. blocks_skipped loop
      exit when the_deck.is_at_BTW;  -- I.e., the tape is fully rewound.
         find_start_of_earlier_block(the_deck, crossed);
      -- MBSKQq does not stop at an LBM-flagged block, only at BTW or count expiry.
      -- It ignores LBM flags encountered during the skipping.
      -- See the Manual, §22.1.3, p.183, ¶1 and §22.1.9, p.188, ¶-2.
      end loop;
      note_tape_position(the_deck, backwards, crossed, bytes_moved => 0);
      update_statistics(the_deck, crossed, bytes_moved => 0);
   end skip_backwards;

   -- MRWDQq
   overriding
   procedure PMD (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      byte_count,
      tape_length : length_in_frames := 0;
      the_slice   : MT.slice;
   begin  -- PMD
      the_deck.is_abnormal := False;  -- See Manual §22.1.9, p.189, ¶-2.
      start_data_transfer(the_deck, Q_operand, set_offline, 19);
      -- No motion takes place if the tape is at BTW; see Manual §22.1.9, p.190, ¶1.
      if the_deck.tape.position > 0 then
         -- Make sure we dont try to read past the end of data.
         -- Spool back to the BTW, accumulating distances.
         while the_deck.tape.position > 0 loop
            read_prev_slice(the_deck.tape, the_slice);
            case the_slice.kind is
               when data_slice =>
                  byte_count := byte_count + the_slice.size;
                  if the_slice.is_first then
                     tape_length := tape_length + the_deck.inter_block_gap;
                  end if;
               when GAP_slice
                  | WIPE_slice =>
                  tape_length := tape_length + the_slice.size;
               when others =>
                  null;
            end case;
         end loop;
      else
         -- No motion takes place; see Manual §22.1.9, p.190, ¶1.
         null;
      end if;

      update_statistics(the_deck, tape_length + byte_count, bytes_moved => 0);

      reset(the_deck);
   end PMD;

   -- MBSKQq
   overriding
   procedure PME (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if the_deck.is_at_BTW then
         trap_illegal_instruction("MBSKQq at BTW on " & the_deck.device_name);
      end if;
      start_data_transfer(the_deck, Q_operand, set_offline, 19);
      if Q_operand.M = 0 then
         skip_backwards(the_deck, 32768);  -- See Manual §22.1.9, p188, ¶1.
      else
         require_positive_count(Q_operand.M);
         skip_backwards(the_deck, KDF9.word(Q_operand.M));
      end if;
   end PME;

   -- METQq
   overriding
   procedure PMF (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_deck, Q_operand);
      validate_parity(the_deck);
      deal_with_a_busy_device(the_deck, 13, set_offline);
      the_T_bit_is_set := the_deck.is_at_ETW;
      take_note_of_test(the_deck.device_name, Q_operand, the_T_bit_is_set);
   end PMF;

   -- PMKQq, forward skip, even parity, for character data with "group mark" (8#77#)
   overriding
   procedure PMK (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if the_deck.kind = MT_kind then
         trap_illegal_instruction("PMKQq on 1081 deck " & the_deck.device_name);
      else
         the_deck.PMA(Q_operand, set_offline);
      end if;
   end PMK;

   -- PMLQq, backward skip, even parity, for character data with "group mark" (8#77#)
   overriding
   procedure PML (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if the_deck.kind = MT_kind then
         trap_illegal_instruction("PMLQq on 1081 deck " & the_deck.device_name);
      else
         the_deck.PMB(Q_operand, set_offline);
      end if;
   end PML;

   procedure put_data_slice (the_deck   : in out MT.deck;
                             data       : in MT.data_storage;
                             size       : in length_in_frames;
                             is_first,
                             is_last,
                             is_flagged : in Boolean) is
      the_slice : MT.slice;
   begin
      the_slice := (
                    data_slice,
                    is_LBM_flagged => is_flagged,
                    is_first => put_data_slice.is_first,
                    is_last  => put_data_slice.is_last,
                    size     => put_data_slice.size,
                    data     => erased_gap_data
                   );
      the_slice.data(1 .. put_data_slice.size) := put_data_slice.data;
      write_slice(the_deck.tape, the_slice);
   exception
      when end_of_tape =>
         deal_with_trying_to_pass_PET(the_deck, "write " & the_deck.device_name);
   end put_data_slice;

   procedure write_block (the_deck       : in out MT.deck;
                          the_data       : in MT.data_storage;
                          is_LBM_flagged : in Boolean) is
      remnant  : length_in_frames := the_data'Length;
      from     : length_in_frames;
      the_size : length_in_frames;
   begin
      if not the_deck.tape.has_a_WP_ring then
         trap_operator_error(the_deck.device_name, "does not have a Write Permit Ring");
      end if;

      deal_with_trying_to_pass_PET(the_deck, "write");

      the_deck.is_LBM_flagged := False;

      -- Write the first (and possibly final) slice of the block.
      the_size := (if remnant > slice_size_limit then slice_size_limit else remnant);
      remnant := remnant - the_size;
      from := the_data'First;
      put_data_slice (
                      the_deck,
                      the_data(from .. the_size),
                      the_size,
                      is_first   => True,
                      is_last    => remnant = 0,
                      is_flagged => write_block.is_LBM_flagged
                     );

      -- Write any full slices, the last of which may be final.
      while remnant >= slice_size_limit loop
         deal_with_trying_to_pass_PET(the_deck, "write");
         remnant := remnant - slice_size_limit;
         from := from + slice_size_limit;
         put_data_slice (
                         the_deck,
                         the_data(from .. from+slice_size_limit-1),
                         slice_size_limit,
                         is_first   => False,
                         is_last    => remnant = 0,
                         is_flagged => write_block.is_LBM_flagged
                        );
      end loop;

      -- Write the residue as a final slice of the block.
      if remnant > 0 then
         put_data_slice (
                         the_deck,
                         the_data(from+slice_size_limit .. the_data'Last),
                         remnant,
                         is_first   => False,
                         is_last    => True,
                         is_flagged => write_block.is_LBM_flagged
                        );
      end if;

      note_tape_position(the_deck, forwards,
                        the_deck.inter_block_gap, bytes_moved => the_data'Length);
      update_statistics(the_deck,
                        the_deck.inter_block_gap, bytes_moved => the_data'Length);

   exception
      when end_of_tape =>
         deal_with_trying_to_pass_PET(the_deck, "write " & the_deck.device_name);
   end write_block;

   procedure write (the_deck       : in out MT.deck;
                    Q_operand      : in KDF9.Q_register;
                    is_LBM_flagged : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
   begin
      validate_device(the_deck, Q_operand);
      check_addresses_and_lockouts(start_address, end_address);
      declare
         next_byte : length_in_frames := 1;
         the_data  : MT.data_storage(1 .. length_in_frames(end_address-start_address+1)*8);
      begin
      word_loop:
         for w in start_address .. end_address loop
            for c in KDF9_char_sets.symbol_index'Range loop
               the_data(next_byte) := TP_CN(fetch_symbol(w, c));
               next_byte := next_byte + 1;
            end loop;
         end loop word_loop;
         write_block(the_deck, the_data, is_LBM_flagged);
      end;
   end write;

   procedure write_to_terminator (the_deck       : in out MT.deck;
                                  Q_operand      : in KDF9.Q_register;
                                  is_LBM_flagged : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
   begin
      validate_device(the_deck, Q_operand);
      check_addresses_and_lockouts(start_address, end_address);
      declare
         next_byte : length_in_frames := 1;
         the_data  : MT.data_storage(1 .. length_in_frames(end_address-start_address+1)*8);
         symbol    : KDF9_char_sets.symbol;
      begin
      word_loop:
         for w in start_address .. end_address loop
            for c in KDF9_char_sets.symbol_index'Range loop
               symbol := fetch_symbol(w, c);
               the_data(next_byte) := TP_CN(symbol);
               next_byte := next_byte + 1;
         exit word_loop when symbol = the_deck.terminator;
            end loop;
         end loop word_loop;
         if the_deck.kind = MT_kind then
            -- Pad out the last word to a full 8 symbols; 7-track decks do not do this.
            while next_byte mod 8 /= 1 loop
               the_data(next_byte) := TP_CN(0);
               next_byte := next_byte + 1;
            end loop;
         end if;
         write_block(the_deck, the_data(1 .. next_byte-1), is_LBM_flagged);
         correct_transfer_time(the_deck, KDF9.word(next_byte-1));
      end;
   end write_to_terminator;

   -- MWQq
   overriding
   procedure POA (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      start_data_transfer(the_deck, Q_operand, set_offline, time, output_operation);
      write(the_deck, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POA;

   -- MWEQq
   overriding
   procedure POB (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      start_data_transfer(the_deck, Q_operand, set_offline, time, output_operation);
      write_to_terminator(the_deck, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POB;

   procedure put_ST_tapemark_slice (the_deck    : in out MT.deck;
                                    Q_operand   : in KDF9.Q_register;
                                    set_offline : in Boolean;
                                    the_slice   : in MT.slice) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, (Q_operand.C, 0, 0));
   begin
      start_data_transfer(the_deck, Q_operand, set_offline, time, output_operation);
      write_slice(the_deck.tape, the_slice);
   exception
      when end_of_tape =>
         deal_with_trying_to_pass_PET(the_deck, "write " & the_deck.device_name);
   end put_ST_tapemark_slice;

   -- MLWQq
   overriding
   procedure POC (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      if the_deck.kind = MT_kind then
         start_data_transfer(the_deck, Q_operand, set_offline, time, output_operation);
         write(the_deck, Q_operand, is_LBM_flagged => True);
         lock_out_relative_addresses(Q_operand);
      else
         put_ST_tapemark_slice(the_deck, Q_operand, set_offline, odd_parity_tape_mark);
      end if;
   end POC;

   -- MLWEQq
   overriding
   procedure POD (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us :=  22 + MT_IO_time(the_deck, Q_operand);
   begin
      if the_deck.kind = MT_kind then
         start_data_transfer(the_deck, Q_operand, set_offline, time, output_operation);
         write_to_terminator(the_deck, Q_operand, is_LBM_flagged => True);
         lock_out_relative_addresses(Q_operand);
      else
         put_ST_tapemark_slice(the_deck, Q_operand, set_offline, even_parity_tape_mark);
      end if;
   end POD;

   procedure erase_tape_gap (the_deck   : in out MT.deck;
                             the_length : in KDF9.Q_part; -- the_length is a number of words.
                             gap_kind   : in tape_gap_kind) is
      crossing  : constant length_in_frames := length_in_frames(the_length) * 8;
      the_slice : MT.slice := (if gap_kind = GAP_slice then a_GAP_slice else a_WIPE_slice);
      remnant   : length_in_frames := crossing;
      old_slice : MT.slice;
      the_size  : length_in_frames;
   begin
      loop
         deal_with_trying_to_pass_PET(the_deck, "erase");
         the_size := length_in_frames'Min(remnant, slice_size_limit);
         remnant  := remnant - the_size;

         the_slice.size := the_size;

         if gap_kind = GAP_slice  and then
               not the_deck.is_at_EOD then
            -- Safety rules apply to erasing gaps; see the Manual, Appendix 6.8, p.314.
            read_next_slice(the_deck.tape, old_slice);
            if old_slice.kind /= WIPE_slice then
               trap_failing_IO_operation(
                                         the_deck,
                                         "a GAP of length"
                                       & the_length'Image
                                       & " words would overwrite data at slice"
                                       & the_deck.tape.position'Image
                                        );
            end if;
            -- Restore the writing position.
            read_prev_slice(the_deck.tape, old_slice);
         end if;

         write_slice(the_deck.tape, the_slice);
      exit when remnant = 0;
      end loop;

      the_deck.is_LBM_flagged := False;
      note_tape_position(the_deck, forwards, crossing, bytes_moved => 0);
      update_statistics(the_deck, crossing, bytes_moved => 0);
   exception
      when end_of_tape =>
         deal_with_trying_to_pass_PET(the_deck, "WIPE/GAP " & the_deck.device_name);
   end erase_tape_gap;

   -- MGAPQq
   overriding
   procedure POE (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us := 19+IO_elapsed_time(the_deck, KDF9.word(Q_operand.M));
   begin
      if not the_deck.tape.has_a_WP_ring then
         trap_operator_error(the_deck.device_name, "does not have a Write Permit Ring");
      end if;
      require_positive_count(Q_operand.M);
      start_data_transfer(the_deck, Q_operand, set_offline, time);
      erase_tape_gap(the_deck, Q_operand.M, gap_kind => GAP_slice);
   end POE;

   -- MWIPEQq
   overriding
   procedure POF (the_deck    : in out MT.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      time : constant KDF9.us := 19+IO_elapsed_time(the_deck, KDF9.word(Q_operand.M));
   begin
      if not the_deck.tape.has_a_WP_ring then
         trap_operator_error(the_deck.device_name, "does not have a Write Permit Ring");
      end if;
      require_positive_count(Q_operand.M);
      start_data_transfer(the_deck, Q_operand, set_offline, time);
      erase_tape_gap(the_deck, Q_operand.M, gap_kind => WIPE_slice);
   end POF;

   overriding
   procedure Finalize (the_deck : in out MT.deck) is
      the_deck_was_used : constant Boolean := the_deck.bytes_moved /= 0 or not the_deck.is_at_BTW;
      buffer            : constant String  := oct_of(KDF9.Q_part(the_deck.number), 2);
   begin
      if the_deck.is_open then
         if (the_final_state_is_wanted and the_log_is_wanted) and then
               the_deck_was_used                                  then
            log_line(
                     the_deck.device_name
                   & " on buffer #"
                   & buffer
                   & " transferred"
                   & the_deck.bytes_moved'Image
                   & " characters"
                   & (
                      if    the_deck.is_at_PET then ", and is now at PET."
                      elsif the_deck.is_at_ETW then ", and is now at ETW."
                      else                          "."
                     )
                   );
         end if;
         close(the_deck.tape);
      end if;
   exception
      when error : others =>
         raise emulation_failure
            with "Finalizing MT buffer #" & buffer & "; " & Ada.Exceptions.Exception_Message(error);
   end Finalize;

   MT_quantum : constant := 1E6 / 40E3;  -- for 40_000 characters per second.
   ST_quantum : constant := 1E6 / 15E3;  -- for 15_000 characters per second.

   type MT_access is access MT.deck;
   MT_deck         : array (IOC.unit_number range 0..8) of MT_access with Warnings => Off;

   MT_units : IOC.unit_number := 0;
   ST_units : IOC.unit_number := 0;

   procedure enable_MT_deck (b : in KDF9.buffer_number) is
   begin
      if MT_units+ST_units > MT_deck'Last then
         trap_operator_error("MT:", "too many tape decks specified");
      end if;
      MT_deck(MT_units) := new deck (number  => b,
                                     kind    => MT_kind,
                                     unit    => MT_units,
                                     quantum => MT_quantum);
      MT_units := MT_units + 1;
   end enable_MT_deck;

   procedure enable_ST_deck (b : in KDF9.buffer_number) is
   begin
      if ST_units >= 2 then
         trap_operator_error("MT:", "more than 2 ST decks specified");
      end if;
      if MT_units+ST_units > MT_deck'Last then
         trap_operator_error("MT:", "too many tape decks specified");
      end if;
      MT_deck(MT_units) := new deck (number  => b,
                                     kind    => ST_kind,
                                     unit    => ST_units,
                                     quantum => ST_quantum);
      ST_units := ST_units + 1;
      MT_units := MT_units + 1;
   end enable_ST_deck;

   procedure find_tape (the_label  : in  MT.data_storage;
                        its_number : out KDF9.buffer_number;
                        its_serial : out KDF9.word;
                        requestor  : in  String) is

      function as_word (the_serial : MT.data_storage)
      return KDF9.word is
         word : KDF9.word := 0;
      begin
         for b in the_serial'Range loop
            word := (word * 2**6) or KDF9.word(CN_TR(the_serial(b)));
         end loop;
         return word;
      end as_word;

      the_block : MT.data_storage(1 .. max_block_size);
      the_size  : length_in_frames;

   begin -- find_tape
      if the_label'Length < 1 then
         raise emulation_failure with "find_tape was given a null label by " & requestor;
      end if;
      for t in KDF9.buffer_number loop
         if buffer(t) /= null                      and then
               buffer(t).kind in MT_kind | ST_kind and then
                   is_unallocated(buffer(t))           then
            declare
               the_deck : MT.deck renames MT.deck(buffer(t).all);
            begin
               if the_deck.holds_data  and then
                     the_deck.is_at_BTW    then
                  -- Read the label.
                  -- After reading the label the tape must be set back to BTW,
                  -- as is required to emulate Director; see the Manual, §22.1, Ex. 1.
                  read_block(the_deck, the_block, the_size);
                  reset(the_deck);
                  if the_size >= 8+the_label'Length                and then
                        the_block(9 .. 8+the_label'Length) = the_label then
                     its_number := t;
                     its_serial := as_word(the_block(1 .. 8));
                     return;
                  end if;
               end if;
            end;
         end if;
      end loop;
      trap_operator_error("the MT labelled '" & String(the_label) & "'",  "has not been mounted");
   end find_tape;


end IOC.fast.MT;
