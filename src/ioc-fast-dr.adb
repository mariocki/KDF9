-- Emulation of a drum store.
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

package body IOC.fast.DR is

   use KDF9_char_sets;

   function as_DR_command (Q_operand : KDF9.Q_register; for_OUT : Boolean := False)
   return String is
      C, sector, track, drive :KDF9.Q_part;
   begin
      if for_OUT then
         -- There is no buffer number.
         C := Q_operand.C;
      else
         -- Get rid of the buffer number.
         C := Q_operand.C / 16;
      end if;
      drive := C mod drums_per_system;
      C := C  /  drums_per_system;
      sector := C mod sectors_per_track;
      track  := C mod sectors_per_drum / sectors_per_track;
      return "D"
           & dec_of(drive)
           & "T"
           & dec_of(track)
           & "S"
           & dec_of(sector);
   end as_DR_command;

   -- The host_IO stream for DR0 is used only to open and close the file.
   -- I/O operations for the DR0 file are done with unmediated POSIX system calls.

   function file_offset (sector_number : DR.drum_index)
   return POSIX.file_position
   is (bytes_per_sector * POSIX.file_position(sector_number));

   procedure get (the_DR : in out DR.device; s : out sector; the_index : in KDF9.word) is
      result : POSIX.file_position with Unreferenced;
   begin
      result := seek(fd_of(the_DR.stream), file_offset(the_index));
      result := POSIX.file_position(read(fd_of(the_DR.stream), s, bytes_per_sector));
   end get;

   procedure put (the_DR : in out DR.device; s : in sector; the_index : in KDF9.word) is
      result : POSIX.file_position with Unreferenced;
   begin
      result := seek(fd_of(the_DR.stream), file_offset(the_index));
      result := POSIX.file_position(write(fd_of(the_DR.stream), s, bytes_per_sector));
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
      full_sectors  : constant KDF9.word := transfer_size / bytes_per_sector;
      residue       : constant KDF9.word := transfer_size mod bytes_per_sector;
      total_sectors : constant KDF9.word := (if residue /= 0 then 1 else 0) + full_sectors;
      gapping_time  : constant KDF9.us   := short_gap_time * KDF9.us(total_sectors-1)
                                          + long_gap_time  * KDF9.us(total_sectors/sectors_per_track);
   begin
      the_DR.latency_count := the_DR.latency_count + 1;
      the_DR.word_count := the_DR.word_count + transfer_size / 8;
      if from_core then
         add_in_the_IO_CPU_time(the_DR, bytes_moved => transfer_size);
      end if;
      busy_time :=  gapping_time + KDF9.us(transfer_size) * the_DR.quantum;
   end keep_house;

   procedure update_statistics (the_DR       : in out DR.device;
                                latency_time : in KDF9.us) is
   begin
      the_DR.latency_time := the_DR.latency_time + latency_time;
   end update_statistics;

   function angular_position (sector_number : sector_range)
   return KDF9.us
   is (KDF9.us(sector_number mod sectors_per_track * sector_time));

   function latent_time (index : drum_index)
   return KDF9.us is
      sector_number : constant sector_range := sector_range(index mod sectors_per_track);
      new_angle     : constant KDF9.us      := angular_position(sector_number);
      old_angle     : constant KDF9.us      := the_clock_time mod track_time;
      gap_time      : constant KDF9.us
                    := (if old_angle > critical_time then short_gap_time else long_gap_time);
      offset        : constant KDF9.us
                    := (
                        if   new_angle > old_angle
                        then new_angle - old_angle
                        else new_angle + track_time - old_angle
                       );
   begin
      -- Hypothesis: we can pick up the next complete sector without a full latency delay,
      --    if we are no closer to it than the start of its preceding inter-block gap.
      return (if offset > gap_time then offset else track_time - offset);
   end latent_time;

   procedure validate_the_sector_number (the_DR : in out DR.device; sector_number : in KDF9.word) is
   begin
      if sector_number > sectors_per_system then
         trap_failing_IO_operation(the_DR, "sector number too big =" & sector_number'Image);
      end if;
   end validate_the_sector_number;

   function validated_drum_address (the_DR : in out DR.device; C_operand : KDF9.Q_part)
   return KDF9.word is
      result : constant KDF9.word := KDF9.word(C_operand / 16);
   begin
      -- We assume that the least significant 4 bits of the C operand are the buffer number,
      --    that the next 2 bits specify one of 4 drums that can be fitted to the buffer,
      --       and that the rest of the C operand is the sector number.
      -- See the Drum Director KKT02E003UPU listing, routine P143 at label 1 et seq.
      -- Returning C_operand / 16 treats the drums as consecutive sectors.
      validate_the_sector_number(the_DR, result);
      return result;
   end validated_drum_address;

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

   a_zero_sector   : constant DR.sector := (others => SP);

   procedure read_drum (the_DR    : in out DR.device;
                        Q_operand : in KDF9.Q_register) is
      end_address  : constant KDF9.address := Q_operand.M;
      next_address : KDF9.address := Q_operand.I;
      the_sector   : DR.sector := a_zero_sector;
      symbol_nr    : KDF9_char_sets.symbol_index;
      size         : KDF9.word := 0;
      the_index    : KDF9.word;
      latency,
      busy_time    : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(the_DR, Q_operand.C);
      latency := latent_time(the_index);
      get(the_DR, the_sector, the_index);
   sector_loop:
      loop
         if the_index > DR.drum_index'Last then
            trap_failing_IO_operation(the_DR, "reading more would exceed the storage available");
         end if;
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
         validate_the_sector_number(the_DR, the_index);
         get(the_DR, the_sector, the_index);
         the_index := the_index + 1;
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time, input_operation);
      update_statistics(the_DR, latency);
      lock_out_relative_addresses(Q_operand);
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
      the_sector   : DR.sector := a_zero_sector;
      at_EM        : Boolean := False;
      symbol_nr    : KDF9_char_sets.symbol_index;
      size         : KDF9.word := 0;
      the_index    : KDF9.word;
      latency,
      busy_time    : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(the_DR, Q_operand.C);
      latency := latent_time(the_index);
      get(the_DR, the_sector, the_index);
   sector_loop:
      loop
         if the_index > DR.drum_index'Last then
            trap_failing_IO_operation(the_DR, "reading more would exceed the storage available");
         end if;
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
         validate_the_sector_number(the_DR, the_index);
         get(the_DR, the_sector, the_index);
         the_index := the_index + 1;
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time, input_operation);
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
      the_sector   : DR.sector := a_zero_sector;
      symbol_nr    : KDF9_char_sets.symbol_index;
      size         : KDF9.word := 0;
      the_index    : KDF9.word;
      latency,
      busy_time    : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(the_DR, Q_operand.C);
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
         validate_the_sector_number(the_DR, the_index);
         put(the_DR, the_sector, the_index);
      exit sector_loop when next_address = end_address and symbol_nr = 7;
         if the_index = DR.drum_index'Last then
            trap_failing_IO_operation(the_DR, "writing more would exceed the storage available");
         else
            the_index := the_index + 1;
         end if;
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time, output_operation);
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
      the_sector   : DR.sector := a_zero_sector;
      at_EM        : Boolean := False;
      size         : KDF9.word := 0;
      symbol_nr    : KDF9_char_sets.symbol_index;
      the_index    : KDF9.word;
      latency,
      busy_time    : KDF9.us;
   begin
      check_addresses_and_lockouts(next_address, end_address);
      the_index := validated_drum_address(the_DR, Q_operand.C);
      latency := latent_time(the_index);
   sector_loop:
      loop
         if the_index > DR.drum_index'Last then
            trap_failing_IO_operation(the_DR, "writing more would exceed the storage available");
         end if;
         symbol_nr := 0;
      byte_loop:
         for i in 1 .. bytes_per_sector loop
            the_sector(i) := TP_CN(fetch_symbol(next_address, symbol_nr));
            size := size + 1;
         at_EM := the_sector(i) = KDF9_char_sets.E_M;
         exit byte_loop when at_EM or else (next_address = end_address and symbol_nr = 7);
            increment(next_address, symbol_nr);
         end loop byte_loop;
         validate_the_sector_number(the_DR, the_index);
         put(the_DR, the_sector, the_index);
         exit sector_loop when at_EM or else (next_address = end_address and symbol_nr = 7);
         the_index := the_index + 1;
      end loop sector_loop;
      keep_house(the_DR, size, busy_time);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time, output_operation);
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
      POA(the_DR, Q_operand, set_offline);
   end POC;

   overriding
   procedure POD (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POB(the_DR, Q_operand, set_offline);
   end POD;

   procedure write_zeroes (the_DR      : in out DR.device;
                           Q_operand   : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size      : KDF9.word := 0;
      the_index : KDF9.word;
      latency,
      busy_time : KDF9.us;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      the_index := validated_drum_address(the_DR, Q_operand.C);
      latency := latent_time(the_index);
      for s in 1 .. (KDF9.word(end_address) - KDF9.word(start_address) + 128) / 128 loop
         if the_index > DR.drum_index'Last then
            trap_failing_IO_operation(the_DR, "writing more would exceed the storage available");
         end if;
         validate_the_sector_number(the_DR, the_index);
         put(the_DR, a_zero_sector, the_index);
         size := size + bytes_per_sector;
         the_index := the_index + 1;
      end loop;
      keep_house(the_DR, size, busy_time, from_core => False);
      start_data_transfer(the_DR, Q_operand, False, latency + busy_time, output_operation);
      lock_out_relative_addresses(Q_operand);
      update_statistics(the_DR, latency);
   end write_zeroes;

   overriding
   procedure POE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_DR, Q_operand);
      write_zeroes(the_DR, Q_operand);
   end POE;

   overriding
   procedure POF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POE(the_DR, Q_operand, set_offline);
   end POF;

   overriding
   procedure Finalize (the_DR : in out DR.device) is
      transfer_time : constant KDF9.us := KDF9.us(the_DR.word_count) * 8 * the_DR.quantum;
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
               & ", and"
                );
            log_line
                (
                 "    "
               & just_right(KDF9.us'Image(the_DR.latency_time / 1_000), 6)
               & " ms in"
               & the_DR.latency_count'Image
               & " rotational latenc" & plurality(the_DR.latency_count, "y.", "ies.")
                );
         end if;

         close(IOC.device(the_DR));
      end if;
   end Finalize;

   type DR_access is access DR.device;

   DR0 : DR_access with Warnings => Off;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      if DR0_is_enabled then
         trap_operator_error("more than one DR control unit has been configured");
      end if;
      DR0 := new DR.device (number => b, unit => 0);
      DR0_is_enabled := True;
      DR0_number := b;
   end enable;

   procedure replace_on_buffer (b : in KDF9.buffer_number) is
   begin
      if DR0_is_enabled and then
            b = DR0.number  then
         return;
      end if;
      buffer(b) := null;
      enable(b);
   end replace_on_buffer;

   procedure remove_from_buffer (b : in KDF9.buffer_number) is
   begin
      if DR0_is_enabled and DR0_number = b then
         buffer(b) := null;
         DR0_is_enabled := False;
      end if;
   end remove_from_buffer;

end IOC.fast.DR;
