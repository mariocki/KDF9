-- ioc-fd.adb
--
-- Emulation of a fixed disc drive buffer.
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

with IOC; pragma Elaborate_All(IOC);
with exceptions;
with formatting;
with KDF9.store;
with POSIX;
with settings;

use  exceptions;
use  formatting;
use  KDF9.store;
use  POSIX;
use  settings;

package body IOC.FD is

   pragma Unsuppress(All_Checks);

   -- Hypothesis:
   -- Where a specification of the Fixed Disc subsystem cannot be inferred from extant
   -- software, such as the Eldon 2 Director, or the EE KDF9 Programming Manual,
   -- then it is reasonable to extrapolate from the document:
   --    "GENERAL INFORMATION MANUAL dp/f-5022 DISCfILE (sic) STORAGE SYSTEM",
   -- by Data Products Corporation, dated March 1965; which describes a similar model.
   -- This document is referred to here as "GIM".
   -- Confirmation of much of this material has been gained from the ICT document:
   --    "Data Disc Store 1956:, dated September 1964"
   -- which describes the same device offered as the first disc drive for the 1900 Series.
   -- All three depict the drive as having a different division of tracks into sectors.

   overriding
   procedure Initialize (the_FD : in out FD.device) is
   begin
      open(the_FD, rd_wr_mode, attaching => False);
   end Initialize;

   overriding
   procedure Finalize (the_FD : in out FD.device) is
   begin
      if the_FD.is_open then
         if the_final_state_is_wanted and
               (the_FD.usage      /= 0 or the_FD.latency_count /= 0 or
                the_FD.seek_count /= 0 or the_FD.switch_time   /= 0) then
            output_line
                   (the_FD.device_name
                  & " on buffer #"
                  & oct_of(KDF9.Q_part(the_FD.number), 2)
                  & " spent:"
                   );
            output_line
                   ("   "
                  & KDF9.microseconds'Image(the_FD.switch_time / 1_000)
                  & " ms in drive-switching,"
                   );
            output_line
                   ("   "
                  & KDF9.microseconds'Image(the_FD.seek_time / 1_000)
                  & " ms in"
                  & KDF9.word'Image(the_FD.seek_count)
                  & " seek(s),"
                   );
            output_line
                   ("   "
                  & KDF9.microseconds'Image(the_FD.latency_time / 1_000)
                  & " ms in"
                  & KDF9.word'Image(the_FD.latency_count)
                  & " rotational latencies, and"
                   );
            output_line
                   ("   "
                  & KDF9.microseconds'Image(the_FD.data_time / 1_000)
                  & " ms in transferring"
                  & KDF9.word'Image(the_FD.usage)
                  & " character(s)."
                   );
         end if;
         IOC.device(the_FD).Finalize;
         close(the_FD);
      end if;
   exception
      when others =>
         output_line("Finalize error for buffer #" & oct_of(KDF9.Q_part(the_FD.number)));
         raise;
   end Finalize;

   overriding
   function usage (the_FD : FD.device)
   return KDF9.word is
   begin
      return the_FD.byte_count;
   end usage;

   overriding
   function IO_elapsed_time_total (the_FD : FD.device)
   return KDF9.microseconds is
   begin
      return the_FD.elapsed_time_total;
   end IO_elapsed_time_total;

   -- For brevity:
   subtype us is KDF9.microseconds;

   -- Hypothesis:
   -- The locus is set by a seek operation; the sector_number is updated by a transfer;
   --     but the arm motion does not take place until the read/write operation,
   --        according to David Holdsworth.
   -- Hypothesis:
   -- A seek command sets sector_number to 0.
   -- Hypothesis:
   -- A data transfer command updates sector_number to the next sequential sector number.
   -- Hypothesis:
   -- If a transfer would increase sector_number past 95, the end-of-area flag is set,
   --     and sector_number is set to 95.

   function sector_span (Q_operand : KDF9.Q_register)
   return KDF9.Q_part is
   begin
      return (Q_operand.M - Q_operand.I + sector_size - 1) / sector_size;
   end sector_span;

   function updated_locus (the_FD    : FD.device;
                           Q_operand : KDF9.Q_register)
   return FD_layout.locus is
      the_new_locus : FD_layout.locus := the_FD.locus;
   begin
      if the_FD.locus.sector_number + sector_span(Q_operand) > FD_layout.sector_number'Last then
         the_new_locus.sector_number := FD_layout.sector_number'Last;
         the_new_locus.is_at_end_of_area := 1;
      else
         the_new_locus.sector_number := the_FD.locus.sector_number + sector_span(Q_operand);
         the_new_locus.is_at_end_of_area := 0;
      end if;
      return the_new_locus;
   end updated_locus;

   procedure advance_the_sector_number (the_FD   : in out FD.device) is
   begin
      if the_FD.locus.sector_number = FD_layout.sector_number'Last then
         the_FD.locus.is_at_end_of_area := 1;
      else
         the_FD.locus.is_at_end_of_area := 0;
         the_FD.locus.sector_number := the_FD.locus.sector_number + 1;
      end if;
   end advance_the_sector_number;

   procedure set_the_sector_number (the_FD    : in out FD.device;
                                    Q_operand : in KDF9.Q_register) is
      the_sector_number : constant KDF9.Q_part := Q_operand.C / 16; -- remove the buffer number
   begin
      if the_sector_number >= FD_layout.sector_number'Last then
         the_FD.locus.is_at_end_of_area := 1;
         the_FD.locus.sector_number := FD_layout.sector_number'Last;
      else
         the_FD.locus.is_at_end_of_area := 0;
         the_FD.locus.sector_number := the_sector_number;
      end if;
   end set_the_sector_number;

   sectors_per_platter : constant := sectors_per_seek_area * seek_areas_per_platter;
   sectors_per_drive   : constant := platters_per_drive  * sectors_per_platter;

   function file_offset (locus : FD_layout.locus)
   return POSIX.file_position is
   begin
      return (bytes_per_sector *
               ( POSIX.file_position(locus.drive_number)     * sectors_per_drive
               + POSIX.file_position(locus.platter_number)   * sectors_per_platter
               + POSIX.file_position(locus.seek_area_number) * sectors_per_seek_area
               + POSIX.file_position(locus.sector_number)    * 1
               )
             );
   end file_offset;

   function transfer_time (first : FD_layout.sector_number; size : KDF9.word)
   return us is

      function ceiling (point : KDF9.Q_part; size : KDF9.word)
      return FD_layout.sector_number is
         length : constant KDF9.Q_part := sector_span((0, 1, KDF9.Q_part(size)));
      begin
         return FD_layout.sector_number'Min(point + length - 1, FD_layout.sector_number'Last);
      end ceiling;

      -- These rates come from the Manual, §6.1.
      outer_rate : constant := 84_800;          -- bytes per second in the outer zone
      inner_rate : constant := outer_rate / 2;  -- bytes per second in the outer zone
      boundary   : constant := sectors_per_seek_area / 3 * 2;
      last       : constant FD_layout.sector_number := ceiling(first, size);

      function time_for (bytes : KDF9.word; in_outer_zone : Boolean)
      return KDF9.word is
      begin
         if in_outer_zone then
            return 1E6 * bytes / outer_rate;
         else
            return 1E6 * bytes / inner_rate;
         end if;
      end time_for;

      bytes_left : KDF9.word := size;
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
      return us(total_time);
   end transfer_time;

   -- The rotational position of the disc is measured in term of the time,
   --    in microseconds, taken to get to that position from sector 0.

   rotation_time : constant := 60E3;  -- 1000 RPM => 60 ms = 60_000 us
   track_size    : constant := 16;    -- There are only 8 sectors per track in the inner zone.
   sector_time   : constant := rotation_time / track_size;

   function angular_position (sector_number : FD_layout.sector_number)
   return us is
   begin
      if sector_number >= track_size * 4 then
         -- We are in the inner zone, with half as many sectors.
         return us((sector_number * 2) mod track_size * sector_time);
      else
         -- We are in the outer zone.
         return us((sector_number * 1) mod track_size * sector_time);
      end if;
   end angular_position;

   function latent_time (sector_number : FD_layout.sector_number)
   return us is
      new_angle  : constant us := angular_position(sector_number);
      old_angle  : constant us := the_clock_time mod rotation_time;
   begin
      -- According to GIM, the minimum latency_time is one sector time.
      return us'Max((new_angle - old_angle) mod rotation_time, sector_time);
   end latent_time;

   -- These times come from the Manual, §6.1.
   checking_time  : constant :=   39E3;
   min_seek_time  : constant :=  156E3 - checking_time;
   max_seek_time  : constant :=  368E3 - checking_time;
   per_track_time : constant us := us(max_seek_time - min_seek_time) / seek_areas_per_platter;

   function arm_seek_time (the_FD : FD.device)
   return us is
      stroke : us := 0;
   begin
      if the_FD.target.platter_number /= the_FD.locus.platter_number then
         declare
            this : constant FD_layout.seek_area_number := the_FD.comb(the_FD.target.platter_number);
            next : constant FD_layout.seek_area_number := the_FD.target.seek_area_number;
         begin
            if this >= next then
               stroke := us(this - next);
            else
               stroke := us(next - this);
            end if;
         end;
      end if;
      if stroke > 0 then
         -- Hypothesis:
         -- A seek distance of 1 position takes the minimum seek time.
         return us'Min(min_seek_time + stroke*per_track_time - per_track_time, max_seek_time)
              + checking_time;
      else
         -- Hypothesis:
         -- A seek to the present position takes no time.
         return 0;
      end if;
   end arm_seek_time;

   function platter_switch_time (the_FD : FD.device)
   return us is
   begin
      if the_FD.target.platter_number = the_FD.locus.platter_number then
         -- Hypothesis:
         -- Opersting on the current platter incurs no switch time.
         return 0;
      else
         -- Hypothesis:
         -- A switch to a different platter_number takes 26 ms on average, as specified in GIM.
         return 26_000;
      end if;
   end platter_switch_time;

   procedure update_statistics (the_FD        : in out FD.device;
                                switch_time,
                                seek_time,
                                latency_time,
                                data_time     : in us := 0) is
      real_time : us;
   begin
      the_FD.switch_time := the_FD.switch_time + switch_time;
      the_FD.seek_time := the_FD.seek_time + seek_time;
      the_FD.latency_time := the_FD.latency_time + latency_time;
      the_FD.data_time := the_FD.data_time + data_time;
      real_time := seek_time + latency_time + data_time;
      the_FD.elapsed_time_total := the_FD.elapsed_time_total + real_time;
      add_in_the_IO_CPU_time(data_time);
   end update_statistics;

   procedure select_new_seek_area (the_FD       : in out FD.device;
                                   switch_time,
                                   seek_time    : out us) is
   begin
      switch_time := platter_switch_time(the_FD);
      seek_time := arm_seek_time(the_FD);
      the_FD.locus := the_FD.target;
      if the_FD.comb(the_FD.locus.platter_number) /= the_FD.locus.seek_area_number then
         the_FD.comb(the_FD.locus.platter_number) := the_FD.locus.seek_area_number;
         the_FD.seek_count := the_FD.seek_count + 1;
      end if;
    end select_new_seek_area;

   subtype sector_image is String(1 .. bytes_per_sector);

   empty_sector : constant sector_image := (others => ' ');
   this_sector  : sector_image;

   procedure get_next_sector (the_FD : in out FD.device) is
      byte_address : constant POSIX.file_position := file_offset(the_FD.locus);
      byte_count   : Integer;
   begin
      if seek(fd_of(the_FD.stream), byte_address) /= byte_address then
         raise emulation_failure with "seek failure in get_next_sector";
      end if;
      byte_count := read(fd_of(the_FD.stream), this_sector, bytes_per_sector);
      if byte_count /= bytes_per_sector and then  -- A short transfer ..
            byte_count /= 0                 then  -- ... is OK at EOF with a 0 count.
         raise emulation_failure with "read failure in get_next_sector";
      end if;
      the_FD.sector_count := the_FD.sector_count + 1;
      advance_the_sector_number(the_FD);
   end get_next_sector;

   procedure keep_house (the_FD        : in out FD.device;
                         transfer_size : in KDF9.word;
                         the_data_time : out us) is
   begin
      the_FD.latency_count := the_FD.latency_count + 1;
      the_FD.byte_count := the_FD.byte_count + transfer_size;
      add_in_the_IO_CPU_time(the_FD, transfer_size);
      the_data_time := transfer_time(the_FD.locus.sector_number, transfer_size);
   end keep_house;

   procedure read (the_FD        : in out FD.device;
                   start_address,
                   end_address   : in  KDF9.address;
                   the_data_time : out us;
                   reading_to_EM : in  Boolean := False) is
      size : KDF9.word := 0;
      next : Natural := 0;
      char : Character;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         if size mod bytes_per_sector = 0 then
            if the_FD.locus.is_at_end_of_area = 1 then
               -- Cannot transfer past the last sector in a seek area.
               raise emulation_failure with "attempt to write past the end of a seek area";
            end if;
            this_sector := empty_sector;
            get_next_sector(the_FD);
            next := 0;
         end if;
         for c in KDF9.symbol_number'Range loop
            next := next + 1;
            char := this_sector(next);
            store_symbol(CN_TR(char), w, c);
            size := size + 1;
         exit word_loop when reading_to_EM and char = E_M;
         end loop;
      end loop word_loop;
      keep_house(the_FD, size, the_data_time);
   end read;


   procedure PIABCD (the_FD         : in out FD.device;
                     Q_operand      : in KDF9.Q_register;
                     set_offline    : in Boolean;
                     transfer_to_EM,
                     on_fixed_heads : in Boolean := False) is
      switch_time, seek_time, latency_time, data_time : us;
   begin
      validate_device(the_FD, Q_operand);
      validate_parity(the_FD);
      -- Action the previously set-up seek (see PMA).
      if on_fixed_heads then
         switch_time := 0;
         seek_time := 0;
      else
         select_new_seek_area(the_FD, switch_time, seek_time);
      end if;
      -- What about the drive number ??
      set_the_sector_number(the_FD, Q_operand);
      latency_time := latent_time(the_FD.locus.sector_number);
      -- Read from the newly established position.
      read(
           the_FD,
           Q_operand.I, Q_operand.M,
           data_time,
           reading_to_EM => transfer_to_EM
          );
      start_timed_transfer(
                           the_FD,
                           Q_operand,
                           set_offline,
                           switch_time + seek_time + latency_time + data_time
                          );
      the_FD.locus := updated_locus(the_FD, Q_operand);
      set_lockouts(Q_operand);
      update_statistics(
                        the_FD,
                        switch_time  => PIABCD.switch_time,
                        seek_time    => PIABCD.seek_time,
                        latency_time => PIABCD.latency_time,
                        data_time    => PIABCD.data_time
                       );
   end PIABCD;

   overriding
   procedure PIA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIABCD(the_FD, Q_operand, set_offline);
   end PIA;

   overriding
   procedure PIB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIABCD(the_FD, Q_operand, set_offline, transfer_to_EM => True);
   end PIB;

   overriding
   procedure PIC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIABCD(the_FD, Q_operand, set_offline, on_fixed_heads => True);
   end PIC;

   overriding
   procedure PID (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIABCD(the_FD, Q_operand, set_offline, transfer_to_EM => True, on_fixed_heads => True);
   end PID;

   overriding
   procedure PIE (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end PIE;

   overriding
   procedure PIF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end PIF;

   overriding
   procedure PIG (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end PIG;

   overriding
   procedure PIH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end PIH;

   -- Set up, but do not effect, a seek to the locus specified by the Q_operand.
   -- This follows advice from David Holdsworth that seeks were not effected
   --    until a data transfer operation was obeyed.
   overriding
   procedure PMA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_FD, Q_operand);
      validate_parity(the_FD);
      the_FD.target := locus_from(Q_operand);
      start_timed_transfer(
                           the_FD,
                           Q_operand,
                           set_offline,
                           busy_time => 0,
                           is_DMAing => False
                          );
   end PMA;

   overriding
   procedure PMB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_FD, canonical(Q_operand));
      validate_parity(the_FD);
      null;
   end PMB;

   overriding
   procedure PMC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_FD, canonical(Q_operand));
      validate_parity(the_FD);
      null;
   end PMC;

   overriding
   procedure PMD (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
      null_locus : constant FD_layout.locus := (others => <>);
   begin
      validate_device(the_FD, canonical(Q_operand));
      validate_parity(the_FD);
      the_FD.comb := (others => 0);
      the_FD.locus := null_locus;
   end PMD;

   overriding
   procedure PMF (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_FD, canonical(Q_operand));
      validate_parity(the_FD);
      the_T_bit := the_FD.locus.is_at_end_of_area;
   end PMF;

   overriding
   procedure PMG (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_FD, canonical(Q_operand));
      validate_parity(the_FD);
      LIV_if_user_mode;
      raise NYI_trap;
   end PMG;

   overriding
   procedure PMH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_FD, canonical(Q_operand));
      validate_parity(the_FD);
      LIV_if_user_mode;
      raise NYI_trap;
   end PMH;

   procedure put_next_sector (the_FD : in out FD.device) is
      byte_address : constant POSIX.file_position := file_offset(the_FD.locus);
   begin
      if seek(fd_of(the_FD.stream), byte_address) /= byte_address then
         raise emulation_failure with "seek failure in put_next_sector";
      end if;
      if write(fd_of(the_FD.stream), this_sector, bytes_per_sector) /= bytes_per_sector then
         raise emulation_failure with "read failure in put_next_sector";
      end if;
      the_FD.sector_count := the_FD.sector_count + 1;
      advance_the_sector_number(the_FD);
      this_sector := empty_sector;
   end put_next_sector;

   procedure write (the_FD       : in out FD.device;
                   start_address,
                   end_address   : in KDF9.address;
                   the_data_time : out us;
                   writing_to_EM : in  Boolean := False) is
      size   : KDF9.word := 0;
      next   : Natural := 0;
      symbol : KDF9.symbol;
      char   : Character;
   begin
      validate_range_access(start_address, end_address);
      if the_FD.locus.is_at_end_of_area = 1 then
         -- Cannot transfer past the last sector in a seek area.
         raise emulation_failure with "attempt to write past the end of a seek area";
      end if;
      this_sector := empty_sector;
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9.symbol_number'Range loop
            symbol := fetch_symbol(w, c);
            char := TP_CN(symbol);
            next := next + 1;
            this_sector(next) := char;
            size := size + 1;
            exit when writing_to_EM and char = E_M;
         end loop;
         if (writing_to_EM and char = E_M) or size mod bytes_per_sector = 0 then
            put_next_sector(the_FD);
            next := 0;
            -- Cannot transfer past the last sector in a seek area.
         exit word_loop when the_FD.locus.is_at_end_of_area = 1 or
                                (writing_to_EM and char = E_M);
         end if;
      end loop word_loop;
      if next > 0 then
         -- Write out any untransferred residue of less than a sector.
         put_next_sector(the_FD);
      end if;
      keep_house(the_FD, size, the_data_time);
   end write;

   procedure POABCD (the_FD           : in out FD.device;
                       Q_operand      : in KDF9.Q_register;
                       set_offline    : in Boolean;
                       transfer_to_EM,
                       on_fixed_heads : in Boolean := False) is
      switch_time, seek_time, latency_time, data_time : us;
   begin
      validate_device(the_FD, Q_operand);
      validate_parity(the_FD);
      -- Action the previously set-up seek (see POA).
      if on_fixed_heads then
         switch_time := 0;
         seek_time := 0;
      else
         select_new_seek_area(the_FD, switch_time, seek_time);
      end if;
      -- What about the drive number ??
      set_the_sector_number(the_FD, Q_operand);
      latency_time := latent_time(the_FD.locus.sector_number);
      -- Write to the newly established position.
      write(
            the_FD,
            Q_operand.I, Q_operand.M,
            data_time,
            writing_to_EM => transfer_to_EM
           );
      start_timed_transfer(
                           the_FD,
                           Q_operand,
                           set_offline,
                           switch_time + seek_time + latency_time + data_time
                          );
      set_lockouts(Q_operand);
      the_FD.locus := updated_locus(the_FD, Q_operand);
      update_statistics(
                        the_FD,
                        switch_time  => POABCD.switch_time,
                        seek_time    => POABCD.seek_time,
                        latency_time => POABCD.latency_time,
                        data_time    => POABCD.data_time
                       );
   end POABCD;

   overriding
   procedure POA (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POABCD(the_FD, Q_operand, set_offline);
   end POA;

   overriding
   procedure POB (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POABCD(the_FD, Q_operand, set_offline, transfer_to_EM => True);
   end POB;

   overriding
   procedure POC (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POABCD(the_FD, Q_operand, set_offline, on_fixed_heads => True);
   end POC;

   overriding
   procedure POD (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POABCD(the_FD, Q_operand, set_offline, transfer_to_EM => True, on_fixed_heads => True);
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
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end POG;

   overriding
   procedure POH (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end POH;

   overriding
   procedure POK (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end POK;

   overriding
   procedure POL (the_FD      : in out FD.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_FD, canonical(Q_operand));
      raise NYI_trap;
   end POL;

   FD_quantum : constant := 1E6 / 85E3;  -- 85_000 characters per second in the outer zone.

   FD0 : aliased FD.device (number => FD0_number, kind => FD_kind, unit => 0,
                            quantum => FD_quantum,
                            is_slow => False);
   pragma Unreferenced(FD0);

end IOC.FD;
