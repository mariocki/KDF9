-- Emulation of magnetic tape decks and buffers.
--
-- This file is part of ee9 (8.1a), the GNU Ada emulator of the English Electric KDF9.
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

private with Ada.Direct_IO;
--
private with IOC_tape_data;

use  KDF9_char_sets;

package IOC.fast.tape is

   -- Both the EE 1081 (MT) and the Ampex TM-4 7-track (ST) decks are emulated.
   --
   -- EE 1081, 16-track tape deck.
      -- The physical characteristics of the deck are taken from the Manual, §22.1.2, i.e.:
      -- 0.3 inch interblock gap, 400 ch/inch density, 100 inch/s tape speed,
      --    full-reel rewind time ~3 minutes.
   --
   -- Ampex TM-4, 7-track IBM-compatible, tape deck.
      -- The physical characteristics of the deck are taken from the Ampex document:
      --    TECHNICAL MANUAL FOR SDSTM-4 TAPE TRANSPORT of 1963/2/15.
      -- Where alternative characteristics are described, this code uses those considered to be of
      --    greatest data interchange compatibility, as this is how the deck was used on KDF9, i.e.:
      -- 0.3 inch interblock gap, 200 ch/inch density, 75 inch/s tape speed,
      --    full-reel rewind time ~3 minutes.
   --

   type deck is abstract new fast.device with private;

   -- MRFQq
   overriding
   procedure PIA (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MFREQq
   overriding
   procedure PIB (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PIA
   overriding
   procedure PIC (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PID
   overriding
   procedure PID (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MBRQq
   overriding
   procedure PIE (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MBREQq
   overriding
   procedure PIF (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PIE
   overriding
   procedure PIG (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PIF
   overriding
   procedure PIH (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MFSKQq, for odd parity on 7-track deck
   overriding
   procedure PMA (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MBTQq
   overriding
   procedure PMB (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MLBQq
   overriding
   procedure PMC (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MRWDQq
   overriding
   procedure PMD (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MBSKQqMFSKQq, for odd parity on 7-track deck
   overriding
   procedure PME (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- METQq
   overriding
   procedure PMF (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMKQq, forward skip, even parity, for 7-track deck only
   overriding
   procedure PMK (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMLQq, backward skip, even parity, for 7-track deck only
   overriding
   procedure PML (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MWQq
   overriding
   procedure POA (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MWEQq
   overriding
   procedure POB (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MLWQq
   overriding
   procedure POC (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MLWEQq
   overriding
   procedure POD (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MGAPQq
   overriding
   procedure POE (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MWIPEQq
   overriding
   procedure POF (the_deck    : in out tape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable_MT_deck (b : in KDF9.buffer_number);

   procedure enable_ST_deck (b : in KDF9.buffer_number);

private

   use IOC_tape_data;

   -- slice_size_limit must be set so that the slice size field fits into 1 byte,
   --    thus avoiding endian-ness and portability issues.
   pragma Compile_Time_Error (slice_size_limit > 255, "IOC_tape_data.slice_size_limit > 255");

   -- I think that both types of tape for the KDF9 had a maximum reel length of 2400 feet.
   -- I assume that the recording density of the 7-track deck was no greater than that of the 1081.

   max_bits_per_inch      : constant := 400;
   max_reel_length        : constant := 12 * 2400;
   type length_in_frames is range 0 .. max_reel_length * max_bits_per_inch;
   type data_storage     is array (tape.length_in_frames range <>) of Character;

   -- Attempts to write a block of more than max_block_size/8 words will be rejected.
   -- The largest recommended size, as stated in the Manual, §22.1.3, is 3000 words.
   -- The present value cannot logically be exceeded, and so allows all possible usages.

   max_block_size         : constant := 32768 * 8;
   subtype block_range   is tape.length_in_frames range 0 .. max_block_size;
   subtype block_storage is data_storage (tape.block_range range 1 .. max_block_size);

   -- A data block consists of one or more slices:
   --
   -- 1. a block of data length <= slice_size_limit has 1 slice, with (is_last and is_first) = True;
   --
   -- 2. a longer block has 1 or more prior slices, which all have data length = slice_size_limit,
   --       all of them having is_last = False, and the first of them having is_first = True;
   --    and 1 final slice of data length <= slice_size_limit, with is_last = True.
   --
   -- The total data length of all the slices in a block is <= max_block_size.
   --
   -- GAP and WIPE slices represent erased lengths of tape.
   -- They are implemented, in effect, as data slices with non-significant data.
   --
   -- Parity mark slices represent tape marks on IBM-compatible Ampex TM4 decks.
   -- See Manual, Appendix 7, p.317.

   type basis_kind is (data_slice,
                       GAP_slice,
                       NULL_slice,
                       WIPE_slice,
                       even_parity_mark,
                       odd_parity_mark);

   -- These representations make for easy inspection of a MT file (e.g. using the UNIX od command).
   for basis_kind use (data_slice       => Character'Pos('D'),
                       GAP_slice        => Character'Pos('G'),
                       NULL_slice       => Character'Pos('N'),
                       WIPE_slice       => Character'Pos('W'),
                       even_parity_mark => Character'Pos('e'),
                       odd_parity_mark  => Character'Pos('o'));

   subtype gap_kind is tape.basis_kind
      with Static_Predicate => gap_kind in GAP_slice | WIPE_slice;

   subtype tape_mark_kind is tape.basis_kind
      with Static_Predicate => tape_mark_kind in odd_parity_mark | even_parity_mark;

   subtype data_kind is tape.basis_kind
      with Static_Predicate => data_kind = data_slice;

   subtype slice_range   is tape.block_range range 0 .. IOC_tape_data.slice_size_limit;
   subtype slice_storage is data_storage (1 .. slice_range'Last);

   tape_mark_data  : constant tape.slice_storage := (1 => tape_mark_sign, others => block_padding);
   erased_gap_data : constant tape.slice_storage := (others => block_padding);

   type slice is
      record
         kind              : tape.basis_kind;
         is_first, is_last : Boolean;
         is_LBM_flagged    : Boolean;
         size              : tape.slice_range;
         data              : tape.slice_storage; -- Only data(1 .. size) are valid.
      end record
   with Size => 8 * MT_record_length;

   -- These two representation specifications put the kind and is_* fields at convenient positions
   --    for easy inspection in a legible print of a MT file (e.g. using the UNIX od command).

   -- The first byte contains the initial letter of the slice type (see basis_kind).

   -- The second byte takes the following octal/ASCII values for non-tape mark slices:
   --    000 = NUL  => no flags
   --    001 = SOH  => first slice of block
   --    010 = BEL  => last slice of block
   --    011 = HT   => only slice of block (first and last)
   --    100 = @    => LBM flag
   --    101 = A    => first slice of block with LBM flag
   --    110 = H    => last slice of block with LBM flag
   --    111 = I    => only slice of block with LBM flag

   for slice use
      record
         kind           at 0 range  0..7;
         is_first       at 1 range  0..2;
         is_last        at 1 range  3..5;
         is_LBM_flagged at 1 range  6..7;
         size           at 2 range  0..7;
         data           at 3 range  0..8*slice_size_limit - 1;
      end record;

   even_parity_tape_mark : constant tape.slice := (even_parity_mark,
                                                   is_first       => True,
                                                   is_last        => True,
                                                   is_LBM_flagged => True,
                                                   size           => 1,
                                                   data           => tape_mark_data);

   odd_parity_tape_mark  : constant tape.slice := (odd_parity_mark,
                                                   is_first       => True,
                                                   is_last        => True,
                                                   is_LBM_flagged => True,
                                                   size           => 1,
                                                   data           => tape_mark_data);

   a_NULL_slice          : constant tape.slice := (NULL_slice,
                                                   is_first       => False,
                                                   is_last        => False,
                                                   is_LBM_flagged => False,
                                                   size           => 0,
                                                   data           => erased_gap_data);

   a_WIPE_slice          : constant tape.slice := (WIPE_slice,
                                                   is_first       => True,
                                                   is_last        => True,
                                                   is_LBM_flagged => False,
                                                   size           => 0,
                                                   data           => erased_gap_data);

   a_GAP_slice           : constant tape.slice := (GAP_slice,
                                                   is_first       => True,
                                                   is_last        => True,
                                                   is_LBM_flagged => False,
                                                   size           => 0,
                                                   data           => erased_gap_data);

   package MT_slice_IO is new Ada.Direct_IO(tape.slice);
   use MT_slice_IO;

   type file is tagged limited
      record
         has_a_WP_ring   : Boolean := True;
         last_data_index : MT_slice_IO.Count := 0;
         position        : MT_slice_IO.Count := 0;
         reel            : MT_slice_IO.File_Type;
      end record;

   -- The complete deck type with its primitive operations.

   type deck is new fast.device with
      record
         -- unwound_frames tallies the amount of tape wound from its spool to the takeup spool;
         --    i.e. how much has to be wound back before being able to unload the tape.
         unwound_frames    : tape.length_in_frames := 0;
         bytes_moved       : KDF9.word := 0;
         is_LBM_flagged    : Boolean   := False;
         terminator        : KDF9_char_sets.symbol;
         recording_density : tape.length_in_frames;
         max_reel_length   : tape.length_in_frames;
         inter_block_gap   : tape.length_in_frames;
         tape_capacity     : tape.length_in_frames;
         PET_position      : tape.length_in_frames;
         ETW_position      : tape.length_in_frames;
         model             : IOC.device_kind;
         tape_file         : tape.file;
      end record;

   overriding
   procedure Finalize (the_deck : in out tape.deck);

   overriding
   function quantum (the_deck : tape.deck)
   return KDF9.us
   is (tape.deck'Class(the_deck).quantum);
   pragma Warnings(Off, quantum);

   overriding
   function kind (the_deck : tape.deck)
   return IOC.device_kind
   is (tape.deck'Class(the_deck).kind);
   pragma Warnings(Off, kind);

   procedure handle_any_abnormality (the_deck : in out tape.deck;
                                     the_size : in length_in_frames)
   is null;

   overriding
   function IO_elapsed_time_total (the_deck : tape.deck)
   return KDF9.us
   is (tape.deck'Class(the_deck).elapsed_time);
   pragma Warnings(Off, IO_elapsed_time_total);

   procedure open (the_deck : in out tape.deck;
                   the_mode : in POSIX.access_mode)
   is null;

   overriding
   function is_open (the_deck : tape.deck)
   return Boolean;

   overriding
   function usage (the_deck : tape.deck)
   return KDF9.word;

   overriding
   procedure close (the_deck : in out tape.deck);

   overriding
   procedure flush(the_deck : in out tape.deck) is null;

   type MT_deck is new tape.deck with null record;

   overriding
   procedure Initialize (the_deck : in out MT_deck);

   overriding
   function kind (the_deck : MT_deck)
   return IOC.device_kind
   is (MT_kind);

   overriding
   function quantum (the_deck : MT_deck)
   return KDF9.us
   is (1E6 / 40E3);  -- ch/s

   overriding
   procedure handle_any_abnormality (the_deck : in out MT_deck;
                                     the_size : in length_in_frames);

   type ST_deck is new tape.deck with null record;

   overriding
   procedure Initialize (the_deck : in out ST_deck);

   overriding
   function kind (the_deck : ST_deck)
   return IOC.device_kind
   is (ST_kind);

   overriding
   function quantum (the_deck : ST_deck)
   return KDF9.us
   is (1E6 / 16E3);  -- ch/s

   overriding
   procedure handle_any_abnormality (the_deck : in out ST_deck;
                                     the_size : in length_in_frames);


   procedure find_tape (the_label  : in  tape.data_storage;
                        its_number : out KDF9.buffer_number;
                        its_serial : out KDF9.word);

end IOC.fast.tape;
