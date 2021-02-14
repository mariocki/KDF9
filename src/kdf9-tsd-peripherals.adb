-- Implement OUTs 5, 6 and 7 of the EE Time Sharing Directors.
--
-- This file is part of ee9 (6.0a), the GNU Ada emulator of the English Electric KDF9.
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

with exceptions;
with formatting;
with HCI;
with IOC;
with IOC.equipment;
with IOC.fast.MT.TSD_OUTs;
with IOC.slow.shift.SI;
with IOC.slow.shift.TR;
with KDF9_char_sets;
with KDF9.store;
with settings;
with state_display;
with tracing;

use  exceptions;
use  formatting;
use  HCI;
use  IOC;
use  IOC.equipment;
use  IOC.fast.MT.TSD_OUTs;
use  IOC.slow.shift.SI;
use  IOC.slow.shift.TR;
use  KDF9_char_sets;
use  KDF9.store;
use  settings;
use  state_display;
use  tracing;

package body KDF9.TSD.peripherals is

   -- is_free_for_explicit_allocation keeps a note of explicitly requested allocations by OUT 5.
   -- Implicit allocations by OUT 8 are done to prevent spurious LIVs on the output devices,
   --    but they must not cause explicit reservations by OUT 5 to fail.

   is_free_for_explicit_allocation : array(KDF9.buffer_number) of Boolean := (others => True);
   is_implicitly_allocated_to_OUT8 : array(KDF9.buffer_number) of Boolean := (others => False);

   procedure free_all_devices is
   begin
      is_free_for_explicit_allocation := (others => True);
      is_implicitly_allocated_to_OUT8 := (others => False);
      for b in KDF9.buffer_number loop
         set_state_of(buffer(b), allocated => False);
      end loop;
      --  Keep FW0 online.
      set_state_of(buffer(0), allocated => True);
   end free_all_devices;

   procedure let_OUT_8_use_the_device_on_buffer (B : in KDF9.buffer_number) is
   begin
      is_implicitly_allocated_to_OUT8(B) := True;
      set_state_of(buffer(B), allocated => True);
   end let_OUT_8_use_the_device_on_buffer;

   -- These are the device-type codes to be given when requesting
   --    the allocation of a peripheral with TSD OUT 5,
   --       according to the Manual and the document:
   --          "Service Routine Library Manual" §22.13, p22-28-0.

   FW_OUT5_code : constant := 0;
   TP_OUT5_code : constant := 1;
   TR_OUT5_code : constant := 2;
   LP_OUT5_code : constant := 3;
   CR_OUT5_code : constant := 4;
   FP_OUT5_code : constant := 5;      -- Ferranti 5-channel Tape punch
   CP_OUT5_code : constant := 7;
   GP_OUT5_code : constant := 8#20#;
   SI_OUT5_code : constant := 8#21#;  -- Standard Interface, "Data Link, N.P.L. Special Buffer"
   FE_OUT5_code : constant := 8#65#;  -- Tape buffer link for PDP-8 on Eldon2, and perhaps COTAN
   UT_OUT5_code : constant := 8#67#;  -- Unlabelled Tape

   procedure select_the_next_device_from_among
      (device_A, device_B : in  KDF9.buffer_number;
       chosen_device      : out KDF9.buffer_number;
       wanted_type        : in String := "") is
   begin
      if device_A /= 0                            and then
            is_free_for_explicit_allocation(device_A) then
         chosen_device := device_A;
      elsif device_B /= 0                         and then
            is_free_for_explicit_allocation(device_B) then
         chosen_device := device_B;
      else
         trap_failing_OUT(5, "there is no available device of type " & wanted_type);
      end if;
   end select_the_next_device_from_among;

   procedure allocate_a_device is
      B : KDF9.buffer_number;
      W : KDF9.word;
   begin
      ensure_that_the_nest_holds_an_operand;
      W := read_top;

      case W is
         -- 8 was added to the code to pre-allocate a device.
         -- I treat pre-allocating and allocating the same way here.
         when FW_OUT5_code
            | FW_OUT5_code+8 =>
            B := 0;  -- Always allowed, no checking performed.
         when TP_OUT5_code
            | TP_OUT5_code+8
            | FP_OUT5_code
            | FP_OUT5_code+8 =>
            select_the_next_device_from_among(TP0_number, TP1_number, B, "TP");
         when TR_OUT5_code
            | TR_OUT5_code+8 =>
            -- TR0 is used for reading the bootstrap/problem program in KDF9 code.
            -- When there is Latin-1 data it therefore needs to go in via TR1.
            -- N.B. the TR devices must appear in this order.
            select_the_next_device_from_among(TR1_number, TR0_number, B, "TR");
            set_case(IOC.slow.shift.TR.device(buffer(B).all));
         when LP_OUT5_code
            | LP_OUT5_code+8 =>
            select_the_next_device_from_among(LP0_number, LP1_number, B, "LP");
         when CR_OUT5_code
            | CR_OUT5_code+8 =>
            select_the_next_device_from_among(CR0_number, CR1_number, B, "CR");
         when CP_OUT5_code
            | CP_OUT5_code+8 =>
            select_the_next_device_from_among(CP0_number, CP1_number, B, "CP");
         when GP_OUT5_code
            | GP_OUT5_code+8 =>
            -- There is only 1 graph plotter.
            select_the_next_device_from_among(GP0_number, GP0_number, B, "GP");
         when SI_OUT5_code =>
            if SI0_is_enabled then
               select_the_next_device_from_among(SI0_number, SI1_number, B, "SI");
            else
               trap_failing_OUT(5, "the SI buffer has not been enabled");
            end if;
         when FE_OUT5_code =>
            trap_unimplemented_feature("PDP-8 Front End Tape buffers");
         when UT_OUT5_code =>
            trap_unimplemented_feature("Unlabelled Tape buffers");
         when others =>
            trap_failing_OUT(5, "unknown device type #" & oct_of(W));
      end case;

      is_free_for_explicit_allocation(B) := False;
      set_state_of(buffer(B), allocated => True);

      pop;
      push(KDF9.word(B));
      the_trace_operand := KDF9.word(B);

      if buffer(B).all in IOC.slow.shift.device'Class and then
            buffer(B).kind /= GP_kind                     then
         log_API_message("OUT 5: requested a device of type #"
                       & oct_of(KDF9.Q_part(W), 2)
                       & " and got "
                       & device_name_of(buffer(B).all)
                       & " on buffer #"
                       & oct_of(B, 2)
                       & ", using "
                       & (
                          if IOC.slow.shift.device(buffer(B).all).uses_Latin_1 then
                             "Latin-1"
                          else
                             "KDF9"
                         )
                       & " code"
                        );
      else
         log_API_message("OUT 5: requested a device of type #"
                       & oct_of(KDF9.Q_part(W), 2)
                       & " and got "
                       & device_name_of(buffer(B).all)
                       & " on buffer #"
                       & oct_of(B, 2)
                        );
      end if;
   end allocate_a_device;

   procedure free_the_device_on_buffer (B : in KDF9.buffer_number; OUT_number : in KDF9.word) is
   begin
      if buffer(B).kind in MT_kind | ST_kind then
         if needs_rewinding(b) then
            PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => (OUT_number = 6));
         end if;
      elsif OUT_number = 7 then
         trap_failing_OUT(7, "device #" & oct_of(B, 2) & ", is not a tape deck");
      end if;
      is_free_for_explicit_allocation(B) := True;
      set_state_of(buffer(B), allocated => is_implicitly_allocated_to_OUT8(B));
      log_API_message("OUT" & OUT_number'Image & ": released " & device_name_of(buffer(B).all));
   end free_the_device_on_buffer;

   procedure deallocate_a_device (OUT_number : in KDF9.word) is
      B : KDF9.Q_part;
   begin
      ensure_that_the_nest_holds_an_operand;
      the_trace_operand := pop;
      if the_trace_operand > 15 then
         notify_state_display_of_final_ICR;
         trap_failing_OUT(OUT_number, "#" & oct_of(the_trace_operand) & " is not a valid buffer number");
      end if;
      B := KDF9.buffer_number(the_trace_operand);
      if is_unallocated(buffer(B)) then
         trap_failing_OUT(OUT_number, "device #" & oct_of(B, 2) & " is not allocated to this program");
      end if;
      free_the_device_on_buffer(B, OUT_number);
   end deallocate_a_device;

   procedure do_OUT_5 is
   begin
      allocate_a_device;
   end do_OUT_5;

   procedure do_OUT_6 is
   begin
      deallocate_a_device(OUT_number => 6);
   end do_OUT_6;

   procedure do_OUT_7 is
   begin
      deallocate_a_device(OUT_number => 7);
   end do_OUT_7;


end KDF9.TSD.peripherals;
