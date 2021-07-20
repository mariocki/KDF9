-- Emulation of a fixed disc drive.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

package body IOC.fast.FD is

   use KDF9_char_sets;

   function as_FD_command (Q_operand : KDF9.Q_register; for_seek, for_FH : Boolean := False)
   return String is
      parameter : constant KDF9.Q_part := Q_operand.C / 16;
      cylinder  : constant KDF9.Q_part := parameter mod seek_areas_per_platter;
      residue   : constant KDF9.Q_part := parameter  /  seek_areas_per_platter;
      platter   : constant KDF9.Q_part := residue mod main_discs_per_drive;
      drive     : constant KDF9.Q_part := residue  /  main_discs_per_drive;
   begin
      -- The disc geometry and I/O command bits are as defined in the FD package.
      if for_seek then
         return "D" & dec_of(drive)
              & "P" & dec_of(if for_FH then KDF9.Q_part'(the_fixed_head_platter) else platter)
              & "C" & dec_of(cylinder);
      else -- for data transfer, parameter is sector #, with maximum 96 sectors per track.
         return " S" & (if parameter < 10 then "0" else "") & dec_of(parameter);
      end if;
   end as_FD_command;

   overriding
   procedure Initialize (the_FD : in out FD.device) is
   begin
      open(the_FD, rd_wr_mode);
   exception
      when others =>
         trap_operator_error(the_FD.device_name & " cannot be opened for reading and writing");
   end Initialize;

   -- Hypothesis:
   -- Where a specification of the Fixed Disc subsystem cannot be inferred from extant software,
   -- such as the Eldon 2 Director, or the EE KDF9 Programming Manual,
   -- then it is reasonable to extrapolate from the Data Products Corporation documents:
   --    "SPECIFICATION FOR MODEL 5022 DISCfILE STORAGE SYSTEM", of December 1964, and
   --    "GENERAL INFORMATION MANUAL dp/f-5022 DISCfILE STORAGE SYSTEM", of March 1965,
   -- which describe an (at least) very similar model.
   -- These document are referred to herein as "GIM".
   -- Confirmation of much of this material has been gained from the ICT document:
   --    "Data Disc Store 1956", of September 1964",
   -- which describes the same device, offered as a fixed disc drive for the 1900 Series.
   -- All three depict the drive as having a different division of tracks into sectors.

   -- Hypothesis:
   -- The locus is set by a PMA or PMD operation; the sector number is updated by a transfer.

   -- Hypothesis:
   -- A PMA or PMD command sets the sector number to 0.

   -- Hypothesis:
   -- If a transfer would increase the sector number past 95, the end-of-area flag is set,
   --    and the sector number is set to 95.

   function locus_from (Q_operand : KDF9.Q_register)
   return FD.locus is
      parameter : constant KDF9.Q_part := Q_operand.C / 16; -- remove the buffer number
      seek_area : constant KDF9.Q_part := parameter mod seek_areas_per_platter;
      platter   : constant KDF9.Q_part
                := parameter / seek_areas_per_platter mod platters_per_drive;
      drive     : constant KDF9.Q_part
                := parameter / seek_areas_per_platter / platters_per_drive;
   begin
      -- Hypothesis:
      -- Seeking to a new locus zeroizes the sector number and clears the end-of-area flag.
      return (
              drive_number      => drive,
              platter_number    => platter,
              seek_area_number  => seek_area,
              sector_number     => 0,
              has_fixed_heads   => False,
              is_at_end_of_area => False
             );
   end locus_from;

   function sector_span (Q_operand : KDF9.Q_register)
   return KDF9.Q_part
   is ((Q_operand.M - Q_operand.I + words_per_sector - 1) / words_per_sector);

   procedure advance_the_sector_number (the_FD : in out FD.device) is
   begin
      if the_FD.locus.sector_number = FD.sector_range'Last then
         the_FD.locus.is_at_end_of_area := True;
      else
         the_FD.locus.is_at_end_of_area := False;
         the_FD.locus.sector_number := the_FD.locus.sector_number + 1;
      end if;
   end advance_the_sector_number;

   procedure set_the_new_sector_number (the_FD    : in out FD.device;
                                    Q_operand : in KDF9.Q_register) is
      the_sector_number : constant KDF9.Q_part := (Q_operand.C / 16) and 8#177#;
   begin
      if the_sector_number in FD.sector_range then
         the_FD.locus.is_at_end_of_area := False;
         the_FD.locus.sector_number := the_sector_number;
      else
         the_FD.locus.is_at_end_of_area := True;
         the_FD.locus.sector_number := FD.sector_range'Last;
      end if;
   end set_the_new_sector_number;

   sectors_per_platter : constant := sectors_per_seek_area * seek_areas_per_platter;
   sectors_per_drive   : constant := platters_per_drive  * sectors_per_platter;

   function file_offset (locus : FD.locus)
   return POSIX.file_position
   is (
       bytes_per_sector *
                        ( POSIX.file_position(locus.drive_number)     * sectors_per_drive
                        + POSIX.file_position(locus.platter_number)   * sectors_per_platter
                        + POSIX.file_position(locus.seek_area_number) * sectors_per_seek_area
                        + POSIX.file_position(locus.sector_number)    * 1
                        )
      );

   function disc_busy_time (first : FD.sector_range; size : KDF9.word)
   return KDF9.us is

      function ceiling (first : FD.sector_range; size : KDF9.word)
      return FD.sector_range is
         length : constant KDF9.Q_part := sector_span((0, 1, KDF9.Q_part(size)));
      begin
         return FD.sector_range'Min(first + length - 1, FD.sector_range'Last);
      end ceiling;

      function time_for (bytes : KDF9.word; in_outer_zone : Boolean)
      return KDF9.word
      is (1E6 * bytes / (if in_outer_zone then outer_rate else inner_rate));

      boundary   : constant := sectors_per_seek_area / 3 * 2;
      last       : constant FD.sector_range := ceiling(first, size);
      bytes_left : KDF9.word := size * 8;
      total_time : KDF9.word := 0;

   begin
      for s in first .. last loop
      exit when bytes_left < bytes_per_sector;
         total_time := total_time + time_for(bytes_per_sector, in_outer_zone => s < boundary);
         bytes_left := bytes_left - bytes_per_sector;
      end loop;
      if bytes_left /= 0 then
         total_time := total_time + time_for(bytes_left, in_outer_zone => last < boundary);
      end if;
      return KDF9.us(total_time);
   end disc_busy_time;

   -- The rotational position of the disc is measured in term of the time,
   --    in microseconds, taken to get to that position from sector 0.

   rotation_time : constant := 60E3;  -- 1000 RPM => 60 ms = 60_000 KDF9.us
   track_size    : constant := 16;    -- There are only 8 sectors per track in the inner zone.
   sector_time   : constant := rotation_time / track_size;

   function angular_position (sector_number : FD.sector_range)
   return KDF9.us
   is (-- This is <= rotation_time.
       if sector_number < sectors_in_outer_zone then
          KDF9.us(sector_number * sector_time)
       else
          -- Inner zone sectors are twice as long as in the outer zone.
          -- Assumes (without evidence) that the inner zone is offset 1/2 sector from the outer.
          KDF9.us((sector_number - sectors_in_outer_zone) * sector_time * 2 - sector_time/2)
      );

   function latent_time (the_FD : FD.device; latency_start_time : in KDF9.us)
   return KDF9.us is
      new_angle  : constant KDF9.us := angular_position(the_FD.locus.sector_number);
      old_angle  : constant KDF9.us := latency_start_time mod rotation_time;
      offset     : constant KDF9.us
                 := (if   new_angle > old_angle
                     then new_angle - old_angle
                     else new_angle + rotation_time - old_angle);
   begin
      -- According to GIM, the minimum latency is one sector;
      --    if less we have to go all the way around.
      return (if offset > sector_time then offset else rotation_time - offset );
   end latent_time;

   -- These times come from the Manual, §6.1, and from GIM.
   checking_time  : constant :=   47E3;
   min_seek_time  : constant :=  109E3;
   max_seek_time  : constant :=  321E3;
   per_track_time : constant KDF9.us
                  := KDF9.us(max_seek_time - min_seek_time) / seek_areas_per_platter;

   -- A seek distance of 1 takes the minimum seek time: zero_seek_time + per_track_time.
   zero_seek_time : constant KDF9.us := min_seek_time - per_track_time;

   subtype seek_time_range is KDF9.us range 0 .. max_seek_time + checking_time;

   function arm_seek_time (the_FD : FD.device)
   return seek_time_range is
      next   : FD.locus renames the_FD.target;
      drive  : FD.drive_range renames next.drive_number;
      here   : constant FD.seek_area_range := the_FD.comb(drive, next.platter_number);
      there  : constant FD.seek_area_range := next.seek_area_number;
      span   : constant FD.seek_area_range := (if here > there then here - there else there - here);
      cost   : constant KDF9.us := KDF9.us(span) * per_track_time;
   begin
      if cost > 0 and next.platter_number /= the_fixed_head_platter then
         -- Hypothesis:
         return KDF9.us'Min(zero_seek_time + cost, max_seek_time) + checking_time;
      else
         -- Hypothesis:
         -- A seek to the present position takes no time.
         return 0;
      end if;
   end arm_seek_time;

   -- These times come from GIM.
   -- A switch to a different platter takes 26 ms on average, as specified in GIM.
   -- This time is needed to power-down then power-up the arm actuators for the R/W heads.
   drive_switch_time : constant :=  5_000;
   arm_switch_time   : constant := 26_000;

   function platter_switch_time (the_FD : FD.device)
   return KDF9.us is
      the_drive_time : KDF9.us;
   begin
      if the_FD.target.drive_number /= the_FD.locus.drive_number then
         the_drive_time := drive_switch_time;
      else
         the_drive_time := 0;
      end if;
      -- Hypothesis:
      -- Operating successively on the current platter, or working on the fixed-head platter,
      --    incurs no arm switch time.
      if the_FD.target.platter_number = the_FD.locus.platter_number or else
            the_FD.target.platter_number = the_fixed_head_platter      then
          return the_drive_time;
       else
          return the_drive_time + arm_switch_time;
      end if;
   end platter_switch_time;

   procedure set_seek_target (the_FD       : in out FD.device;
                              Q_operand    : in KDF9.Q_register;
                              it_will_seek : out Boolean) is
      here : FD.locus renames the_FD.locus;
      next : FD.locus renames the_FD.target;
   begin
      it_will_seek := False;
      next := locus_from(Q_operand);
      next.has_fixed_heads := False;
      if next.platter_number /= here.platter_number then
         the_FD.switch_count := the_FD.switch_count +1;
      end if;
      if the_FD.comb(next.drive_number, here.platter_number) /= next.seek_area_number then
         the_FD.seek_count := the_FD.seek_count + 1;
         it_will_seek := True;
      end if;
    end set_seek_target;

   procedure seek_to_the_target_area (the_FD      : in out FD.device;
                                      seek_time,
                                      switch_time : out seek_time_range) is
      here : FD.locus renames the_FD.locus;
      next : FD.locus renames the_FD.target;
   begin
      seek_time := arm_seek_time(the_FD);
      switch_time := platter_switch_time(the_FD);
      here := the_FD.target;
      the_FD.comb(here.drive_number, here.platter_number) := next.seek_area_number;
    end seek_to_the_target_area;

   subtype sector_image is String(1 .. bytes_per_sector);

   empty_sector : constant sector_image := (others => SP);
   this_sector  : sector_image;

   procedure get_next_sector (the_FD : in out FD.device) is
      result : POSIX.file_position with Unreferenced;
   begin
      result := seek(fd_of(the_FD.stream), file_offset(the_FD.locus));
      result := POSIX.file_position(read(fd_of(the_FD.stream), this_sector, bytes_per_sector));
      the_FD.sector_count := the_FD.sector_count + 1;
      advance_the_sector_number(the_FD);
   end get_next_sector;

   procedure keep_house (the_FD        : in out FD.device;
                         transfer_size : in KDF9.word;
                         busy_time     : out KDF9.us) is
   begin
      the_FD.latency_count := the_FD.latency_count + 1;
      the_FD.word_count := the_FD.word_count + transfer_size;
      add_in_the_IO_CPU_time(the_FD, bytes_moved => transfer_size*8);
      busy_time := disc_busy_time(the_FD.locus.sector_number, transfer_size);
   end keep_house;

   procedure update_statistics (the_FD        : in out FD.device;
                                switch_time,
                                seek_time,
                                latency_time,
                                data_time     : in KDF9.us := 0) is
   begin
      the_FD.switch_time := the_FD.switch_time + switch_time;
      the_FD.seek_time := the_FD.seek_time + seek_time;
      the_FD.latency_time := the_FD.latency_time + latency_time;
      the_FD.data_time := the_FD.data_time + data_time;
      the_FD.elapsed_time := the_FD.elapsed_time
                                 + switch_time
                                 + seek_time
                                 + latency_time
                                 + data_time;
    end update_statistics;

   procedure read (the_FD        : in out FD.device;
                   start_address,
                   end_address   : in  KDF9.address;
                   busy_time     : out KDF9.us;
                   reading_to_EM : in  Boolean := False) is
      size : KDF9.word := 0;
      next : Natural := 0;
      char : Character;
   begin
      check_addresses_and_lockouts(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         if size mod bytes_per_sector = 0 then
            this_sector := empty_sector;
            get_next_sector(the_FD);
         exit word_loop when the_FD.locus.is_at_end_of_area;
            next := 0;
         end if;
         for c in KDF9_char_sets.symbol_index'Range loop
            next := next + 1;
            char := this_sector(next);
            store_symbol(CN_TR(char), w, c);
            size := size + 1;
         exit word_loop when reading_to_EM and char = E_M;
         end loop;
      end loop word_loop;
      keep_house(the_FD, (size+7)/8, busy_time);
   end read;

   procedure PI_all (the_FD         : in out FD.device;
                     Q_operand      : in KDF9.Q_register;
                     transfer_to_EM : in Boolean := False) is
      the_present_time   : constant KDF9.us := the_clock_time;
      latency_start_time : KDF9.us;
      seek_duration,
      switch_duration,
      latency_duration,
      data_duration,
      total_duration     : KDF9.us;
   begin
      validate_device(the_FD);
      validate_parity(the_FD);
      seek_to_the_target_area(the_FD, seek_duration, switch_duration);
      set_the_new_sector_number(the_FD, Q_operand);
      latency_start_time := the_present_time + seek_duration + switch_duration;
      latency_duration := latent_time(the_FD, latency_start_time);

      if the_FD.locus.is_at_end_of_area then
         -- Cannot transfer past the last sector in a seek area.
         trap_failing_IO_operation(the_FD, "attempt to read FD at the end of a seek area");
      end if;

      -- Read from the newly established position.
      read(
           the_FD,
           Q_operand.I, Q_operand.M,
           data_duration,
           reading_to_EM => transfer_to_EM
          );

      total_duration := seek_duration + switch_duration + latency_duration + data_duration;
      start_data_transfer(the_FD, Q_operand, False, total_duration, input_operation);
      lock_out_relative_addresses(Q_operand);
      update_statistics(
                        the_FD,
                        switch_time  => switch_duration,
                        seek_time    => seek_duration,
                        latency_time => latency_duration,
                        data_time    => data_duration
                       );
   end PI_all;

   overriding
   procedure PIA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      PI_all(the_FD, Q_operand);
   end PIA;

   overriding
   procedure PIB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      PI_all(the_FD, Q_operand, transfer_to_EM => True);
   end PIB;

   -- Set up FD parameters for a transfer in the fixed-head platter.
   procedure prepare_fixed_head_transfer (the_FD      : in out FD.device;
                                          Q_operand   : in KDF9.Q_register) is
   begin
      the_FD.target := locus_from(Q_operand);
      the_FD.target.has_fixed_heads := True;
      the_FD.target.platter_number := 16;
      the_FD.switch_time := platter_switch_time(the_FD);
      the_FD.locus := the_FD.target;
      the_FD.switch_count := the_FD.switch_count + (if the_FD.switch_time = 0 then 0 else 1);
   end prepare_fixed_head_transfer;

   overriding
   procedure PIC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      prepare_fixed_head_transfer(the_FD, Q_operand);
      PI_all(the_FD, Q_operand);
   end PIC;

   overriding
   procedure PID (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      prepare_fixed_head_transfer(the_FD, Q_operand);
      PI_all(the_FD, Q_operand, transfer_to_EM => True);
   end PID;

   -- Find the number of the next (first) sector to rotate into an accessible position.
   function next_sector_number (the_FD : FD.device)
   return FD.sector_range is
      now        : constant KDF9.us := the_clock_time;
      best_time  : KDF9.us          := KDF9.us'Last;
      the_sector : FD.sector_range  := FD.sector_range'Last;
   begin
      for s in FD.sector_range loop
         if latent_time(the_FD, now) in sector_time+1 .. 2*sector_time then
            if latent_time(the_FD, now) <= best_time then
               the_sector := s;
               best_time := latent_time(the_FD, now);
            end if;
         end if;
      end loop;
      return the_sector;
   end next_sector_number;

   -- Modify the given command to apply to the next sector to come under the heads.
   function for_next_sector (the_FD : FD.device; Q : KDF9.Q_register)
   return KDF9.Q_register
   is (
       C => (Q.C and 8#174000#) or (Q.C and 8#17#) or (next_sector_number(the_FD)*16),
       I => Q.I,
       M => Q.M
      );

   overriding
   procedure PIE (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      PI_all(the_FD, for_next_sector(the_FD, Q_operand));
   end PIE;

   overriding
   procedure PIF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      PI_all(the_FD, for_next_sector(the_FD, Q_operand), transfer_to_EM => True);
   end PIF;

   overriding
   procedure PIG (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      prepare_fixed_head_transfer(the_FD, for_next_sector(the_FD, Q_operand));
      PI_all(the_FD, for_next_sector(the_FD, Q_operand));
   end PIG;

   overriding
   procedure PIH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      prepare_fixed_head_transfer(the_FD, for_next_sector(the_FD, Q_operand));
      PI_all(the_FD, for_next_sector(the_FD, Q_operand), transfer_to_EM => True);
   end PIH;

   -- Set up, but do not yet effect, a seek to the locus specified by the Q_operand.
   -- This follows advice from David Holdsworth that seeks were not effected
   --    until a data transfer operation was obeyed.
   -- PMA does not lockout for a busy device.  This may not be authentic.
   overriding
   procedure PMA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      a_seek_is_needed : Boolean := False;
   begin
      validate_device(the_FD);
      validate_parity(the_FD);
      set_seek_target(the_FD, Q_operand, a_seek_is_needed);
      deal_with_a_busy_device(the_FD, 19, set_offline);
      take_note_of_test(the_FD.device_name, Q_operand, a_seek_is_needed);
   end PMA;

   overriding
   procedure PMB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_FD);
      validate_parity(the_FD);
      null;
   end PMB;

   overriding
   procedure PMC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_FD);
      validate_parity(the_FD);
      null;
   end PMC;

   overriding
   procedure PMD (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      -- clear out seek area # and platter #, leaving buffer # and drive #
      platter_0        : constant KDF9.Q_part := Q_operand.C and 16#C00F#;
      control_word     : KDF9.Q_register;
      a_seek_is_needed : Boolean;
      seek_duration,
      switch_duration  : KDF9.us;
   begin
      validate_device(the_FD);
      -- Hypothesis: drive reset clears the parity flag.
      the_FD.is_abnormal := False;
      -- In effect, do 16 PMA operations, but treat them as a single operation.
      for p in KDF9.Q_part range 0..15 loop -- p is platter #
         control_word := (platter_0 + p*16#400#, Q_operand.I, Q_operand.M);
         set_seek_target(the_FD, control_word, a_seek_is_needed);
         seek_to_the_target_area(the_FD, seek_duration, switch_duration);
         update_statistics(
                           the_FD,
                           switch_time  => switch_duration,
                           seek_time    => seek_duration
                          );
      end loop;
      deal_with_a_busy_device(the_FD, 16, set_offline);
   end PMD;

   overriding
   procedure PMF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_FD);
      validate_parity(the_FD);
      deal_with_a_busy_device(the_FD, 13, set_offline);
      the_T_bit_is_set := the_FD.locus.is_at_end_of_area;
      take_note_of_test(the_FD.device_name, Q_operand, the_T_bit_is_set);
   end PMF;

   procedure put_next_sector (the_FD : in out FD.device) is
      result : POSIX.file_position with Unreferenced;
   begin
      result := seek(fd_of(the_FD.stream), file_offset(the_FD.locus));
      result := POSIX.file_position(write(fd_of(the_FD.stream), this_sector, bytes_per_sector));
      the_FD.sector_count := the_FD.sector_count + 1;
      advance_the_sector_number(the_FD);
      this_sector := empty_sector;
   end put_next_sector;

   procedure write (the_FD       : in out FD.device;
                   start_address,
                   end_address   : in KDF9.address;
                   busy_time     : out KDF9.us;
                   writing_to_EM : in  Boolean := False) is
      size   : KDF9.word := 0;
      next   : Natural := 0;
      symbol : KDF9_char_sets.symbol;
      char   : Character;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      this_sector := empty_sector;
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9_char_sets.symbol_index'Range loop
            symbol := fetch_symbol(w, c);
            char := TP_CN(symbol);
            next := next + 1;
            this_sector(next) := char;
            size := size + 1;
            exit when writing_to_EM and char = E_M;
         end loop;
         if writing_to_EM and char = E_M then
            put_next_sector(the_FD);
            next := 0;
            exit word_loop;
         elsif size mod bytes_per_sector = 0 then
            put_next_sector(the_FD);
            next := 0;
            exit word_loop when the_FD.locus.is_at_end_of_area;
         end if;
      end loop word_loop;
      if next > 0 then
         -- Write out any untransferred residue of less than a full sector.
         put_next_sector(the_FD);
      end if;
      keep_house(the_FD, (size+7)/8, busy_time);
   end write;

   procedure PO_all (the_FD         : in out FD.device;
                     Q_operand      : in KDF9.Q_register;
                     transfer_to_EM : in Boolean := False) is
      the_present_time   : constant KDF9.us := the_clock_time;
      seek_duration,
      switch_duration,
      latency_duration,
      data_duration,
      total_duration     : KDF9.us;
      latency_start_time : KDF9.us;
   begin
      validate_device(the_FD);
      validate_parity(the_FD);
      seek_to_the_target_area(the_FD, seek_duration, switch_duration);
      set_the_new_sector_number(the_FD, Q_operand);
      latency_start_time := the_present_time + seek_duration + switch_duration;
      latency_duration := latent_time(the_FD, latency_start_time);

      if the_FD.locus.is_at_end_of_area then
         -- Cannot transfer past the last sector in a seek area.
         trap_failing_IO_operation(the_FD, "attempt to write FD at the end of a seek area");
      end if;

      -- Write to the newly established position.
      write(
            the_FD,
            Q_operand.I, Q_operand.M,
            data_duration,
            writing_to_EM => transfer_to_EM
           );

      total_duration := seek_duration + switch_duration + latency_duration + data_duration;
      start_data_transfer(the_FD, Q_operand, False, total_duration, output_operation);
      lock_out_relative_addresses(Q_operand);
      update_statistics(
                        the_FD,
                        seek_time    => seek_duration,
                        switch_time  => switch_duration,
                        latency_time => latency_duration,
                        data_time    => data_duration
                       );
   end PO_all;

   overriding
   procedure POA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      PO_all(the_FD, Q_operand);
   end POA;

   overriding
   procedure POB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      PO_all(the_FD, Q_operand, transfer_to_EM => True);
   end POB;

   overriding
   procedure POC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      prepare_fixed_head_transfer(the_FD, Q_operand);
      PO_all(the_FD, Q_operand);
   end POC;

   overriding
   procedure POD (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      prepare_fixed_head_transfer(the_FD, Q_operand);
      PO_all(the_FD, Q_operand, transfer_to_EM => True);
   end POD;

   overriding
   procedure POE (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POC(the_FD, Q_operand, set_offline);
   end POE;

   overriding
   procedure POF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POA(the_FD, Q_operand, set_offline);
   end POF;

   overriding
   procedure POG (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      PO_all(the_FD, for_next_sector(the_FD, Q_operand));
   end POG;

   overriding
   procedure POH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      PO_all(the_FD, for_next_sector(the_FD, Q_operand), transfer_to_EM => True);
   end POH;

   overriding
   procedure POK (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      prepare_fixed_head_transfer(the_FD, for_next_sector(the_FD, Q_operand));
      PO_all(the_FD, for_next_sector(the_FD, Q_operand), transfer_to_EM => True);
   end POK;

   overriding
   procedure POL (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, Q_operand);
      prepare_fixed_head_transfer(the_FD, for_next_sector(the_FD, Q_operand));
      PO_all(the_FD, for_next_sector(the_FD, Q_operand));
   end POL;

   overriding
   procedure Finalize (the_FD : in out FD.device) is
      buffer : constant String := oct_of(KDF9.Q_part(the_FD.number), 2);
   begin
      if the_FD.is_open then
         if (the_final_state_is_wanted and the_log_is_wanted)    and then
               (the_FD.word_count /= 0 or the_FD.latency_count /= 0  or
                the_FD.seek_count /= 0 or the_FD.switch_time   /= 0) then
            log_line
                   (
                    the_FD.device_name
                  & " on buffer #"
                  & buffer
                  & " spent:"
                   );
            log_line
                   (
                    "    "
                  & just_right(KDF9.us'Image(the_FD.data_time / 1_000), 6)
                  & " ms in"
                  & the_FD.latency_count'Image
                  & " data transfer" & plurality(the_FD.latency_count)
                  & " totalling"
                  & KDF9.word'Image(the_FD.word_count)
                  & " word" & plurality(the_FD.word_count)
                  & ","
                   );
            log_line
                   (
                    "    "
                  & just_right(KDF9.us'Image(the_FD.switch_time / 1_000), 6)
                  & " ms in"
                  & the_FD.switch_count'Image
                  & " platter switch" & plurality(the_FD.switch_count, "", "es")
                  & ","
                   );
            log_line
                   (
                    "    "
                  & just_right(KDF9.us'Image(the_FD.latency_time / 1_000), 6)
                  & " ms in"
                  & the_FD.latency_count'Image
                  & " rotational latenc" & plurality(the_FD.latency_count, "y", "ies")
                  & ", and"
                   );
            log_line
                   (
                    "    "
                  & just_right(KDF9.us'Image(the_FD.seek_time / 1_000), 6)
                  & " ms in"
                  & the_FD.seek_count'Image
                  & " seek" & plurality(the_FD.seek_count) & "."
                   );
         end if;
         IOC.device(the_FD).Finalize;
         close(the_FD);
      end if;
   end Finalize;

   type FD_access is access FD.device;

   FD0 : FD_access with Warnings => Off;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      if FD0_is_enabled then
         trap_operator_error("more than one FD control unit has been configured");
      end if;
      FD0 := new FD.device (number => b, unit => 0);
      FD0_is_enabled := True;
      FD0_number := b;
   end enable;

   procedure replace_on_buffer (b : in KDF9.buffer_number) is
   begin
      if FD0 /= null    and then
            b = FD0.number  then
         return;
      end if;
      buffer(b) := null;
      FD0 := null;
      FD0_number := 0;
      FD0_is_enabled := False;
      enable(b);
   end replace_on_buffer;

   procedure remove_from_buffer (b : in KDF9.buffer_number) is
   begin
      if FD0 /= null    and then
            b = FD0.number  then
         buffer(b) := null;
         FD0 := null;
         FD0_number := 0;
         FD0_is_enabled := False;
      end if;
   end remove_from_buffer;

end IOC.fast.FD;
