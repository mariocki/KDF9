-- Emulation of a paper tape reader buffer.
--
-- This file is part of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
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

with KDF9_char_sets;
with KDF9.TOD_clock;
with KDF9.TSD.timing;

use  KDF9_char_sets;
use  KDF9.TOD_clock;

package body IOC.slow.shift.TR is

   use KDF9_char_sets;

   overriding
   procedure Initialize (the_TR : in out TR.device) is
   begin
      -- Open the associated file.
      open(IOC.device(the_TR), read_mode);
   end Initialize;

   --
   -- See Manual, §17.4 for paper tape 8-bit frame format.
   --

   function has_even_parity (octet : KDF9.syllable)
   return Boolean is
      frame  : KDF9.syllable := octet;
      parity : KDF9.syllable := 0;
   begin
      while frame /= 0 loop
         parity := parity xor (frame and 1);
         frame  := frame / 2;
      end loop;
      return parity = 0;
   end has_even_parity;

   function symbol_from (octet : KDF9.syllable)
   return KDF9_char_sets.symbol
   is (KDF9_char_sets.symbol((octet and 2#01_100_000#)/2 or (octet and 2#00_001_111#)));

   DEL_frame : constant := 8#377#;
   NUL_frame : constant := 8#000#;

   -- Read 8-bit paper tape frames, compress to 6-bit byteacters, and pack into words.
   procedure read_KDF9_tape_code (the_TR        : in out TR.device;
                                  Q_operand     : in KDF9.Q_register;
                                  reading_to_EM,
                                  loading_code  : in Boolean := False) is
      c      : KDF9_char_sets.symbol_index := 0;
      w      : KDF9.Q_part := Q_operand.I;
      size   : KDF9.word := 0;
      octet  : KDF9.syllable;
      symbol : KDF9_char_sets.symbol;
      byte   : Character;
   begin
      check_addresses_and_lockouts(Q_operand.I, Q_operand.M);
   word_loop:
      loop
         loop
            get_byte_from_stream(byte, the_TR);
            octet := KDF9.syllable(Character'Pos(byte));
         exit when octet not in NUL_frame | DEL_frame;
         end loop;
         if has_even_parity(octet) then
            symbol := symbol_from(octet);
         else
            trap_invalid_paper_tape("probably not in KDF9 code (parity error detected)");
         end if;
         store_symbol(symbol, w, c);
         size := size + 1;
         c := c + 1;
         if c = 0 then
      exit word_loop when reading_to_EM and symbol = KDF9_char_sets.End_Message;
            w := w + 1;
      exit word_loop when w > Q_operand.M;
         end if;
         if reading_to_EM and then symbol = KDF9_char_sets.End_Message then
            for d in c .. 7 loop
               store_symbol(KDF9_char_sets.Blank_Space, w, d);
            end loop;
      exit word_loop;
         end if;
      exit word_loop when w > Q_operand.M;
      end loop word_loop;
      if not loading_code then
         do_input_housekeeping(the_TR, read_in => size, stored => size);
      end if;
   exception
      when end_of_stream =>
         if size = 0 then
            trap_invalid_paper_tape("there was no data on the tape");
         end if;
         if not loading_code then
            do_input_housekeeping(the_TR, read_in => size, stored => size);
         end if;
         the_TR.is_abnormal := True;
         if not reading_to_EM and c /= 0 then
            trap_invalid_paper_tape("the last word on the tape was incomplete");
         end if;
   end read_KDF9_tape_code;

   -- PRQq
   overriding
   procedure PIA (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TR, Q_operand, set_offline, input_operation);
      if the_TR.is_transcribing then
         read(the_TR, Q_operand);
      else
         read_KDF9_tape_code(the_TR, Q_operand);
      end if;
      lock_out_relative_addresses(Q_operand);
   end PIA;

   -- PREQq
   overriding
   procedure PIB (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TR, Q_operand, set_offline, input_operation);
      if the_TR.is_transcribing then
         read_to_EM(the_TR, Q_operand);
      else
         read_KDF9_tape_code(the_TR, Q_operand, reading_to_EM => True);
      end if;
      lock_out_relative_addresses(Q_operand);
   end PIB;

   -- PRCQq
   overriding
   procedure PIC (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TR, Q_operand, set_offline, input_operation);
      words_read(the_TR, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end PIC;

   -- PRCEQq
   overriding
   procedure PID (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TR, Q_operand, set_offline, input_operation);
      words_read_to_EM(the_TR, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end PID;

   -- as PIA
   overriding
   procedure PIE (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIA(the_TR, Q_operand, set_offline);
   end PIE;

   -- as PIB
   overriding
   procedure PIF (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIB(the_TR, Q_operand, set_offline);
   end PIF;

   -- as PIC
   overriding
   procedure PIG (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIC(the_TR, Q_operand, set_offline);
   end PIG;

   -- as PID
   overriding
   procedure PIH (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PID(the_TR, Q_operand, set_offline);
   end PIH;

   -- the_T_bit_is_set := (the reader is set to 8-track mode);
   --    it is always in 8-track mode, as 5-track input is not supported by ee9.
   overriding
   procedure PMB (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_TR);
      validate_parity(the_TR);
      deal_with_a_busy_device(the_TR, 13, set_offline);
      the_T_bit_is_set := True;
      take_note_of_test(the_TR.device_name, Q_operand, the_T_bit_is_set);
   end PMB;

   overriding
   procedure Finalize (the_TR : in out TR.device) is
   begin
      close(
            the_TR,
            "read",
            the_TR.byte_count,
            "character" & plurality(the_TR.byte_count)
          & " in "
          & (if the_TR.is_transcribing then "Latin-1" else "KDF9")
          & " code"
           );
   end Finalize;

   type TR_access is access TR.device;

   TR0  : TR_access with Warnings => Off;
   TR1  : TR_access with Warnings => Off;

   unit : IOC.unit_number := 0;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      case unit is
         when 0 =>
            TR0 := new TR.device (number => b, unit => 0);
            TR0_number := b;
         when 1 =>
            TR1 := new TR.device (number => b, unit => 1);
            TR1_number := b;
         when others =>
            trap_operator_error("more than two TR units have been configured");
      end case;
      unit := unit + 1;
   end enable;

   -- Set the character code to be used by the designated TR.
   procedure set_unit_code (unit : in Natural; is_transcribing : in Boolean) is
   begin
      if unit = 0 then
         TR0.is_transcribing := set_unit_code.is_transcribing;
      elsif TR1 /= null then
         TR1.is_transcribing := set_unit_code.is_transcribing;
      end if;
   end set_unit_code;

   --
   -- Support for loading programs and for bootstrapping the KDF9.
   --

   procedure reattach (unit : in Natural; next_file_name : in String) is
      the_reader  : constant TR_access := (if unit = 0 then TR0 else TR1);
   begin
      reattach(the_reader.all, next_file_name);
      if the_reader.is_open then
         the_reader.current_case := KDF9_char_sets.Case_Normal;
      else
         trap_operator_error("«" & next_file_name & "» cannot be found");
      end if;
   end reattach;

   procedure reset_loader_usage (unit : in Natural) is
      the_reader  : constant TR_access := (if unit = 0 then TR0 else TR1);
   begin
      correct_transfer_time(the_reader.all, KDF9.us(0));
      the_reader.byte_count := 0;
      unlock_absolute_addresses((0, 0, 32767));
   end reset_loader_usage;

   -- This emulates the Director's program load from a designated  paper tape reader.
   -- Once the loading is done, the tape reader is reattached to TR0.
   procedure load_a_program (program_file_name : in String) is

      -- This is the call sign for a program on Disc or Drum.
      CN_LS_D_LS : constant KDF9.word := (KDF9.word(Case_Normal)  * 2**18)
                                      or (KDF9.word(Line_Shift)   * 2**12)
                                      or (KDF9.word(Upper_Case_D) * 2** 6)
                                      or (KDF9.word(Line_Shift)   * 2** 0);

      -- This is the call sign for a program on Magnetic Tape.
      CN_LS_M_LS : constant KDF9.word := (KDF9.word(Case_Normal)  * 2**18)
                                      or (KDF9.word(Line_Shift)   * 2**12)
                                      or (KDF9.word(Upper_Case_M) * 2** 6)
                                      or (KDF9.word(Line_Shift)   * 2** 0);

      -- This is the call sign for a program on Paper Tape.
      CN_LS_P_LS : constant KDF9.word := (KDF9.word(Case_Normal)  * 2**18)
                                      or (KDF9.word(Line_Shift)   * 2**12)
                                      or (KDF9.word(Upper_Case_P) * 2** 6)
                                      or (KDF9.word(Line_Shift)   * 2** 0);

      threshold  : constant KDF9.word := 32767 * 2**24;
      substitute : constant KDF9.word := 32736 * 2**24;
      get_a_word : constant KDF9.Q_register := (TR0.number, 0, 0);

      descriptor : KDF9.Q_register := (TR0.number, 1, 7);
      word_count : Positive := 2;

   begin -- load_a_program

      loading_was_successful := False;

      -- Access the program file as TR0.
      reattach(0, program_file_name);

      --
      -- For the structure of a compiled program, see Manual §26.3.
      --

      -- Get the first word of the file into E0: it may start an A block or a B block.
      read_KDF9_tape_code(TR0.all, get_a_word, loading_code => True);

      -- Check for an A block.  If one is found, check its validity, but otherwise ignore it.
      if fetch_halfword(0, 0)/2**24 in CN_LS_D_LS | CN_LS_M_LS | CN_LS_P_LS then
         -- We have an A block.
         -- The next word completes the program name used by Director.  Ignore it.
         read_KDF9_tape_code(TR0.all, get_a_word, loading_code => True);

      block_loop:
         -- An A block is at most 8 words long but can end sooner with a word containing EM.
         loop
            word_count := word_count + 1;
         exit block_loop when word_count > 8;
            read_KDF9_tape_code(TR0.all, get_a_word, loading_code => True, reading_to_EM => True);
            for c in KDF9_char_sets.symbol_index loop
         exit block_loop when fetch_symbol(0, c) = End_Message;
            end loop;
         end loop block_loop;

         if word_count > 8 then
            -- The file is not a valid program tape.
            trap_invalid_paper_tape("excessively long A block");
         end if;

         -- Read the first word of the following B block.
         read_KDF9_tape_code(TR0.all, (TR0.number, 0, 0), loading_code => True);
      end if;

      -- Check for an unconditional jump at the start of the B block.
      if (fetch_word(0)/ 2**32 and 2#1111_0000_1111_0000#) /= 2#1000_0000_1011_0000# then
         -- The file is not a valid program tape.
         trap_invalid_paper_tape("no jump was found in E0U: " & oct_of(fetch_word(0)));
      else
         -- Preserve the initial jump in case of corruption by a buggy program.
         save_the_initial_jump;
      end if;

      -- At this point, E0 contains the first word of the B block, so get the rest of it in E1-E7.
      read_KDF9_tape_code(TR0.all, descriptor, loading_code => True);

      descriptor := as_Q(fetch_word(descriptor.M));
      -- Read the non-final C blocks; the validity of the designated descriptors cannot be assumed.
      while descriptor.C /= 0 loop
         validate_address_range(descriptor.I, descriptor.M);
         read_KDF9_tape_code(TR0.all, descriptor, loading_code => True);
         descriptor := as_Q(fetch_word(descriptor.M));
      end loop;

      -- Read the final C block.
      validate_address_range(descriptor.I, descriptor.M);
      read_KDF9_tape_code(TR0.all, descriptor, loading_code => True);

      -- Set up the rest of the stored image.

      -- Set the (virtual) date in E7.
      store_word(todays_date_28n_years_ago, 7);

      -- Ensure valid parameters in E1 (some binaries may have invalid entries).
      if fetch_halfword(1, 0) > threshold or else fetch_halfword(1, 0) = 0 then
         store_halfword(substitute, 1, 0);
      end if;
      if fetch_halfword(1, 1) > threshold or else fetch_halfword(1, 1) = 0 then
         store_halfword(substitute, 1, 1);
      end if;

      -- Do not set the time if we are computing a signature, so as to get a repeatable hash.
      if not the_signature_is_enabled then
         KDF9.TSD.timing.set_the_time_of_loading(the_time_of_day);
      end if;

      loading_was_successful := True;

      -- Clear up the I/O system.
      reattach(0, "TR0");
      clear_IOC_FIFO;
      reset_loader_usage(0);
   end load_a_program;

   -- This emulates action of a Director call program, including:
   --    1. Moving the JP0 order from E0U to E2U.
   --    2. Inserting the interrupt handling code into E0 and E1, and
   --    3. setting NIA to (4, 0) instead of (0, 0).
   procedure load_a_bare_Director (program_file_name : in String) is

   begin -- load_a_bare_Director
      load_a_program(program_file_name);
      store_halfword(fetch_halfword(0, index => 0), 2, index => 0);
      store_word(8#3620716437675016#, 0); -- #171 #016 #164 #177 #172 #016: Q0; SHL+63; =+Q0;
      store_word(8#6114000037240052#, 1); -- #304 #300 #000 #175 #100 #052: SETB140000;; =K1; ERASE
      set_NIA_to((4, 0));
   end load_a_bare_Director;

   -- TR0 is the hardware bootstrap device for reading initial orders.
   procedure boot_the_KDF9 (program_file_name : in String) is
      boot_descriptor : constant KDF9.Q_register := (C => TR0.number, I => 0, M => 8);
   begin
      loading_was_successful := False;
      reattach(0, program_file_name);

      -- The bootstrap is 9 words of instruction code, which reads in the rest of its file.
      -- The validity of the bootstrap descriptor is hardware defined.
      read_KDF9_tape_code(TR0.all, boot_descriptor, loading_code => True);

      -- Reset the I/O system for execution of the Director.
      clear_IOC_FIFO;
      reset_loader_usage(0);
      loading_was_successful := True;
   end boot_the_KDF9;

end IOC.slow.shift.TR;
