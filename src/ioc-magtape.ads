-- ioc-magtape.ads
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

private with Ada.Direct_IO;
--
private with KDF9.store;

package IOC.magtape is

   pragma Unsuppress(All_Checks);

   type deck is new IOC.device with private;

   -- MRFQq
   overriding
   procedure PIA (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MFREQq
   overriding
   procedure PIB (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PIA
   overriding
   procedure PIC (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PID
   overriding
   procedure PID (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MBRQq
   overriding
   procedure PIE (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MBREQq
   overriding
   procedure PIF (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PIE
   overriding
   procedure PIG (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- as PIF
   overriding
   procedure PIH (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MFSKQq
   overriding
   procedure PMA (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MBTQq
   overriding
   procedure PMB (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MLBQq
   overriding
   procedure PMC (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MRWDQq
   overriding
   procedure PMD (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MBSKQq
   overriding
   procedure PME (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- METQq
   overriding
   procedure PMF (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- MWQq
   overriding
   procedure POA (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MWEQq
   overriding
   procedure POB (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MLWQq
   overriding
   procedure POC (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MLWEQq
   overriding
   procedure POD (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MGAPQq
   overriding
   procedure POE (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   -- MWIPEQq
   overriding
   procedure POF (the_deck    : in out magtape.deck;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- The following support the emulation of OUTs 4 and 10.
   type short_label   is new String(1 .. 8);
   type long_label    is new String(1 .. 16);

   procedure find_tape_labelled (the_label  : in magtape.short_label;
                                 its_number : out KDF9.buffer_number;
                                 its_serial : out KDF9.word);

   procedure find_tape_labelled (the_label  : in magtape.long_label;
                                 its_number : out KDF9.buffer_number;
                                 its_serial : out KDF9.word);

private

   use KDF9.store; pragma Warnings(Off, KDF9.store);

   -- For now, a data block or erased gap is confined to a single brick.
   -- The next version will allow for as many bricks as necessary.

   type MT_brick_kind is (MT_mark_brick, MT_erased_brick, MT_data_brick);

   type MT_mark_kind is (even_parity_tape_mark, odd_parity_tape_mark);

   max_brick_size : constant := 512 * 8;

   subtype brick_size_range is Natural range 0 .. max_brick_size;

   type brick (kind : MT_brick_kind    := MT_data_brick;
               size : brick_size_range := 0) is
      record
         case kind is
            when MT_mark_brick =>
               tape_mark : MT_mark_kind;
            when MT_erased_brick =>
               erased_length : KDF9.word;
            when MT_data_brick =>
               is_LBM_flagged : Boolean;
               data : String(1 .. size);
         end case;
      end record;

   IBM_even_mark : constant brick
                 := (size => 0, kind => MT_mark_brick, tape_mark => even_parity_tape_mark);
   IBM_odd_mark  : constant brick
                 := (size => 0, kind => MT_mark_brick, tape_mark => odd_parity_tape_mark);

   package MT_brick_IO is new Ada.Direct_IO(IOC.magtape.brick);

   type file is tagged limited
      record
         reel : MT_brick_IO.File_Type;
      end record;

   function is_open (tape : in magtape.file)
   return Boolean;

   procedure open_RW (tape : in out magtape.file;
                      name : in String);

   procedure open_RO (tape : in out magtape.file;
                      name : in String);

   procedure close (tape : in out magtape.file);

   procedure write (tape  : in magtape.file;
                    index : in KDF9.word;
                    stuff : in IOC.magtape.brick);

   procedure read (tape  : in magtape.file;
                   index : in KDF9.word;
                   stuff : out IOC.magtape.brick);

   end_of_tape : exception;

   type deck is new IOC.device with
      record
         tape : magtape.file;
         has_a_WP_ring  : Boolean := True;
         is_LBM_flagged : Boolean := False;
         bytes_moved,
         gaps_crossed,
         brick_number,
         erased_length : KDF9.word := 0;
         elapsed_time_total : KDF9.microseconds := 0;
      end record;

   overriding
   procedure Initialize (the_deck : in out magtape.deck);

   overriding
   procedure Finalize (the_deck : in out magtape.deck);

   procedure open (the_deck : in out magtape.deck;
                   the_mode : in POSIX.access_mode)
   is null;

   procedure open (the_deck : in out magtape.deck);

   overriding
   function is_open (the_deck : magtape.deck)
   return Boolean;

   overriding
   function usage (the_deck : magtape.deck)
   return KDF9.word;

   overriding
   procedure close (the_deck : in out magtape.deck);

   overriding
   function IO_elapsed_time_total (the_deck : magtape.deck)
   return KDF9.microseconds;

   -- Is the tape at the Beginning Of Tape window?
   not overriding
   function is_at_BOT (the_deck : magtape.deck)
   return Boolean;

   -- Is the tape at the End of Tape Warning?
   not overriding
   function is_at_ETW (the_deck : magtape.deck)
   return Boolean;

   -- Does the last block read have a Last Block Marker?
   not overriding
   function is_at_LBM (the_deck : magtape.deck)
   return Boolean;

   overriding
   procedure flush(the_deck : in out magtape.deck) is null;

   procedure read_block (the_deck      : in out magtape.deck;
                         Q_operand     : in KDF9.Q_register;
                         reading_to_EM : in Boolean := False);

   procedure read_block_backwards  (the_deck      : in out magtape.deck;
                                    Q_operand     : in KDF9.Q_register;
                                    reading_to_EM : in Boolean := False);

   procedure write  (the_deck       : in out magtape.deck;
                     Q_operand      : in KDF9.Q_register;
                     is_LBM_flagged : in Boolean := False);

   procedure write_to_EM (the_deck       : in out magtape.deck;
                          Q_operand      : in KDF9.Q_register;
                          is_LBM_flagged : in Boolean := False);

   procedure skip_forwards (the_deck     : in out magtape.deck;
                            gaps_crossed : in KDF9.word);

   procedure skip_backwards (the_deck     : in out magtape.deck;
                             gaps_crossed : in KDF9.word);

   type tape_gap_kind is (MGAP_gap, MWIPE_gap);

   procedure erase_tape_gap (the_deck   : in out magtape.deck;
                             the_length : in KDF9.word;
                             gap_kind   : in tape_gap_kind := MGAP_gap);

end IOC.magtape;
