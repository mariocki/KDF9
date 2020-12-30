-- ioc-fast-dr.adb
--
-- Emulation of a drum store.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020, W. Findlay; all rights reserved.
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

with HCI;

use  HCI;

package body IOC.fast.DR is

   use KDF9_char_sets;

   -- The host_IO stream for DR0 is used only to open and close the file.
   -- I/O operations for the DR0 file are done with unmediated POSIX system calls.

   function file_offset (locus : DR.drum_range)
   return POSIX.file_position
   is (bytes_per_sector * POSIX.file_position(locus));

   procedure get (the_DR : in out DR.device; s : out sector; the_index : in KDF9.word) is
      byte_address : constant POSIX.file_position := file_offset(the_index);
   begin
      if seek(fd_of(the_DR.stream), file_offset(the_index)) /= byte_address then
         raise emulation_failure
            with "POSIX seek failure in DR.get at position " & the_index'Image;
      end if;
      if read(fd_of(the_DR.stream), s, bytes_per_sector) /= bytes_per_sector then
         raise emulation_failure
            with "POSIX read failure in DR.get at position " & the_index'Image;
      end if;
   end get;

   procedure put (the_DR : in out DR.device; s : in sector; the_index : in KDF9.word) is
      byte_address : constant POSIX.file_position := file_offset(the_index);
   begin
      if seek(fd_of(the_DR.stream), file_offset(the_index)) /= byte_address then
         raise emulation_failure
            with "POSIX seek failure in DR.put";
      end if;
      if write(fd_of(the_DR.stream), s, bytes_per_sector) /= bytes_per_sector then
         raise emulation_failure
            with "POSIX write failure in DR.put";
      end if;
   end put;

   overriding
   procedure Initialize (the_DR : in out DR.device) is
   begin
      open(IOC.device(the_DR), rd_wr_mode);
   exception
      when others =>
         trap_operator_error(the_DR.device_name & " cannot be opened for reading and writing");
   end Initialize;

   procedure keep_house (the_DR        : in out DR.device;
                         transfer_size : in KDF9.word;
                         busy_time     : out KDF9.us;
                         from_core     : Boolean := True) is
      full_sectors : constant KDF9.word := transfer_size / bytes_per_sector;
      residue      : constant KDF9.word := transfer_size mod bytes_per_sector;
   begin
      the_DR.latency_count := the_DR.latency_count + 1;
      the_DR.word_count := the_DR.word_count + transfer_size / 8;
      if from_core then
         add_in_the_IO_CPU_time(the_DR, bytes_moved => transfer_size);
      end if;
      busy_time := gap_time
                 + KDF9.us(residue * us_per_char)
                 + KDF9.us(full_sectors) * sector_time;
   end keep_house;

   procedure update_statistics (the_DR       : in out DR.device;
                                latency_time : in KDF9.us) is
   begin
      the_DR.latency_time := the_DR.latency_time + latency_time;
   end update_statistics;

   function angular_position (sector_number : sector_range)
   return KDF9.us
   is (KDF9.us(sector_number mod sectors_per_track * sector_time));

   prev_track  : track_range := 0;
   switch_time : KDF9.us := 0;

   function latent_time (index : drum_range)
   return KDF9.us is
      sector_number : constant sector_range := sector_range(index mod sectors_per_track);
      new_angle     : constant KDF9.us      := angular_position(sector_number);
      old_angle     : constant KDF9.us      := the_clock_time mod track_time;
      this_track    : constant track_range  := index / sectors_per_track;
      offset        : KDF9.us
                    := (if   new_angle > old_angle
                        then new_angle - old_angle
                        else new_angle + track_time - old_angle);
   begin
      if this_track / 2 /= prev_track / 2 then
         offset := offset + switch_delay;
         switch_time := switch_time + switch_delay;
      end if;
      prev_track := this_track;
      -- Hypothesis: we can pick up the next complete sector without a full latency delay,
      --    if we are no closer to it than the start of its preceding inter-block gap.
      return (if offset > gap_time then offset else track_time - offset);
   end latent_time;

   function validated_drum_address (C_operand : KDF9.Q_part)
   return KDF9.word
      with Inline => False;

   function validated_drum_address (C_operand : KDF9.Q_part)
   return KDF9.word is
   begin
      -- We assume that the least significant 4 bits of the C operand are the buffer number,
      --    that the next 2 bits specify one of 4 drums that can be fitted to the buffer,
      --       and that the rest of the C operand is the sector number.
      -- See the Drum Director KKT02E003UPU listing, routine P143 at label 1 et seq.
      -- Returning C_operand / 16 treats the drums as consecutive sectors.
      if C_operand / 16 / 4 >= sectors_per_drum then
         trap_invalid_operand("DR sector number too big" & C_operand'Image);
      end if;
      return KDF9.word(C_operand / 16);
   end validated_drum_address;

   procedure validate (sector_index : in KDF9.word) is
   begin
      if sector_index > sectors_per_system then
         trap_invalid_operand("this transfer would exceed the capacity of DR0" & sector_index'Image);
      end if;
   end validate;

   procedure increment (word_address : in out KDF9.address;
                        symbol_nr    : in out KDF9_char_sets.symbol_index)
      with Inline => True;

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

   procedure read_drum (the_DR    : in out DR.device;
                        Q_operand : in KDF9.Q_register) is
      end_address  : constant KDF9.address := Q_operand.M;
      next_address : KDF9.address := Q_operand.I;
      the_sector   : DR.sector := (others => Character'Val(0));
      symbol_nr : KDF9_char_sets.symbol_index;
      size      : KDF9.word := 0;
      the_index : KDF9.word;
      latency,
      busy_time : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(Q_operand.C);
      latency := latent_time(the_index);
      get(the_DR, the_sector, the_index);
   sector_loop:
      loop
         symbol_nr := 0;
      byte_loop:
         for i in 1 .. bytes_per_sector loop
            if symbol_nr = 0 then
               store_word(0, next_address);
            end if;
            store_symbol(CN_TR(the_sector(i)), next_address, symbol_nr);
            size := size + 1;
         exit byte_loop when next_address = end_address and symbol_nr = 7;
            increment(next_address, symbol_nr);
         end loop byte_loop;
      exit sector_loop when next_address = end_address and symbol_nr = 7;
         if the_index = DR.drum_range'Last then
            trap_invalid_operand("this transfer would exceed the capacity of DR0");
         else
            the_index := the_index + 1;
         end if;
         validate(the_index);
         get(the_DR, the_sector, the_index);
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time);
      lock_out_relative_addresses(Q_operand);
      update_statistics(the_DR, latency);
   end read_drum;

   overriding
   procedure PIA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      validate_parity(the_DR);
      read_drum(the_DR, Q_operand);
   end PIA;

   procedure read_drum_to_EM (the_DR    : in out DR.device;
                              Q_operand : in KDF9.Q_register) is
      end_address  : constant KDF9.address := Q_operand.M;
      next_address : KDF9.address := Q_operand.I;
      the_sector   : DR.sector := (others => Character'Val(0));
      at_EM     : Boolean := False;
      symbol_nr : KDF9_char_sets.symbol_index;
      size      : KDF9.word := 0;
      the_index : KDF9.word;
      latency,
      busy_time : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(Q_operand.C);
      latency := latent_time(the_index);
      get(the_DR, the_sector, the_index);
   sector_loop:
      loop
         symbol_nr := 0;
      byte_loop:
         for i in 1 .. bytes_per_sector loop
            if symbol_nr = 0 then
               store_word(0, next_address);
            end if;
            store_symbol(CN_TR(the_sector(i)), next_address, symbol_nr);
            size := size + 1;
         at_EM := the_sector(i) = KDF9_char_sets.E_M;
         exit byte_loop when at_EM or else (next_address = end_address and symbol_nr = 7);
            increment(next_address, symbol_nr);
         end loop byte_loop;
         exit sector_loop when at_EM or else (next_address = end_address and symbol_nr = 7);
         if the_index = DR.drum_range'Last then
            trap_invalid_operand("this transfer would exceed the capacity of DR0");
         else
            the_index := the_index + 1;
         end if;
         validate(the_index);
         get(the_DR, the_sector, the_index);
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time);
      lock_out_relative_addresses(Q_operand);
      update_statistics(the_DR, latency);
   end read_drum_to_EM;

   overriding
   procedure PIB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
       pragma Unreferenced(set_offline);
  begin
      validate_transfer(the_DR, Q_operand);
      validate_parity(the_DR);
      read_drum_to_EM(the_DR, Q_operand);
   end PIB;

   overriding
   procedure PIC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIA(the_DR, Q_operand, set_offline);
   end PIC;

   overriding
   procedure PID (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIB(the_DR, Q_operand, set_offline);
   end PID;

   overriding
   procedure PIE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIA(the_DR, Q_operand, set_offline);
   end PIE;

   overriding
   procedure PIF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIB(the_DR, Q_operand, set_offline);
   end PIF;

   overriding
   procedure PIG (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIA(the_DR, Q_operand, set_offline);
   end PIG;

   overriding
   procedure PIH (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIB(the_DR, Q_operand, set_offline);
   end PIH;

   procedure write_drum (the_DR    : in out DR.device;
                         Q_operand : in KDF9.Q_register) is
      end_address  : constant KDF9.address := Q_operand.M;
      next_address : KDF9.address := Q_operand.I;
      the_sector   : DR.sector := (others => Character'Val(0));
      symbol_nr : KDF9_char_sets.symbol_index;
      size      : KDF9.word := 0;
      the_index : KDF9.word;
      latency,
      busy_time : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(Q_operand.C);
      latency := latent_time(the_index);
   sector_loop:
      loop
         symbol_nr := 0;
      byte_loop:
         for i in 1 .. bytes_per_sector loop
            the_sector(i) := TP_CN(fetch_symbol(next_address, symbol_nr));
            size := size + 1;
         exit byte_loop when next_address = end_address and symbol_nr = 7;
            increment(next_address, symbol_nr);
         end loop byte_loop;
         validate(the_index);
         put(the_DR, the_sector, the_index);
      exit sector_loop when next_address = end_address and symbol_nr = 7;
         if the_index = DR.drum_range'Last then
            trap_invalid_operand("this transfer would exceed the capacity of DR0");
         else
            the_index := the_index + 1;
         end if;
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time);
      lock_out_relative_addresses(Q_operand);
      update_statistics(the_DR, latency);
   end write_drum;

   overriding
   procedure POA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      validate_parity(the_DR);
      write_drum(the_DR, Q_operand);
   end POA;

   procedure write_drum_to_EM (the_DR    : in out DR.device;
                               Q_operand : in KDF9.Q_register) is
      end_address  : constant KDF9.address := Q_operand.M;
      next_address : KDF9.address := Q_operand.I;
      the_sector   : DR.sector := (others => Character'Val(0));
      at_EM     : Boolean := False;
      size      : KDF9.word := 0;
      symbol_nr : KDF9_char_sets.symbol_index;
      the_index : KDF9.word;
      latency,
      busy_time : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(Q_operand.C);
      latency := latent_time(the_index);
   sector_loop:
      loop
         symbol_nr := 0;
      byte_loop:
         for i in 1 .. bytes_per_sector loop
            the_sector(i) := TP_CN(fetch_symbol(next_address, symbol_nr));
            size := size + 1;
         at_EM := the_sector(i) = KDF9_char_sets.E_M;
         exit byte_loop when at_EM or else (next_address = end_address and symbol_nr = 7);
            increment(next_address, symbol_nr);
         end loop byte_loop;
         validate(the_index);
         put(the_DR, the_sector, the_index);
         exit sector_loop when at_EM or else (next_address = end_address and symbol_nr = 7);
         if the_index = DR.drum_range'Last then
            trap_invalid_operand("this transfer would exceed the capacity of DR0");
         else
            the_index := the_index + 1;
         end if;
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time);
      lock_out_relative_addresses(Q_operand);
      update_statistics(the_DR, latency);
   end write_drum_to_EM;

   overriding
   procedure POB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      validate_parity(the_DR);
      write_drum_to_EM(the_DR, Q_operand);
   end POB;

   overriding
   procedure POC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POA(the_DR, Q_operand, Set_offline);
   end POC;

   overriding
   procedure POD (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POB(the_DR, Q_operand, Set_offline);
   end POD;

   procedure write_zeroes (the_DR      : in out DR.device;
                           Q_operand   : in KDF9.Q_register;
                           set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      the_sector    : constant DR.sector := (others => Character'Val(0));
      size      : KDF9.word := 0;
      the_index : KDF9.word;
      latency,
      busy_time : KDF9.us;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      the_index := validated_drum_address(Q_operand.C);
      latency := latent_time(the_index);
      for s in 1 .. (KDF9.word(end_address) - KDF9.word(start_address) + 128) / 128 loop
         validate(the_index);
         put(the_DR, the_sector, the_index);
         size := size + bytes_per_sector;
         if the_index = DR.drum_range'Last then
            trap_invalid_operand("this transfer would exceed the capacity of DR0");
         else
            the_index := the_index + 1;
         end if;
      end loop;
      keep_house(the_DR, size, busy_time, from_core => False);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time);
      lock_out_relative_addresses(Q_operand);
      update_statistics(the_DR, latency);
   end write_zeroes;

   overriding
   procedure POE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_transfer(the_DR, Q_operand);
      write_zeroes(the_DR, Q_operand, set_offline);
   end POE;

   overriding
   procedure POF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POE(the_DR, Q_operand, Set_offline);
   end POF;

   overriding
   procedure Finalize (the_DR : in out DR.device) is
      transfer_time : constant KDF9.us := KDF9.us(the_DR.word_count * 8 * us_per_char);
   begin
      if the_DR.is_open then

         if (the_final_state_is_wanted and the_log_is_wanted)    and then
               (the_DR.word_count /= 0 or the_DR.latency_count /= 0) then
            log_line
                (
                 the_DR.device_name
               & " on buffer #"
               & oct_of(KDF9.Q_part(the_DR.number), 2)
               & " spent:"
                );
            log_line
                (
                 "    "
               & just_right(KDF9.us'Image(transfer_time / 1_000), 6)
               & " ms in"
               & the_DR.latency_count'Image
               & " data transfer" & plurality(the_DR.latency_count)
               & " totalling"
               & KDF9.word'Image(the_DR.word_count)
               & " word" & plurality(the_DR.word_count)
               & ","
                );
            log_line
                (
                 "    "
               & just_right(KDF9.us'Image(switch_time / 1_000), 6)
               & " ms in"
               & KDF9.us'Image(switch_time / switch_delay)
               &  plurality(KDF9.word(switch_time / switch_delay), " band switch", " band switches")
               & ", and"
                );
            log_line
                (
                 "    "
               & just_right(KDF9.us'Image(the_DR.latency_time / 1_000), 6)
               & " ms in"
               & the_DR.latency_count'Image
               & plurality(the_DR.latency_count, " rotational latency.", " rotational latencies.")
                );
         end if;

         IOC.device(the_DR).Finalize;
         close(the_DR.stream);
      end if;
   end Finalize;

   DR_quantum : constant := us_per_char;

   type DR_access is access DR.device;

   DR0 : DR_access with Warnings => Off;

   already_enabled : Boolean := False;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      if already_enabled then trap_operator_error("more than 1 DR unit specified"); end if;
      DR0 := new DR.device (number  => b,
                            kind    => DR_kind,
                            unit    => 0,
                            quantum => DR_quantum);
      already_enabled := True;
   end enable;

end IOC.fast.DR;
