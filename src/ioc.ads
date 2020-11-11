-- ioc.ads
--
-- Emulation of the common functionality of a KDF9 IOC "buffer" (DMA channel),
--    with fail-stop stubs for operations having device-specific behaviour.
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

with Ada.Finalization;
--
with KDF9;
with Latin_1; pragma Unreferenced(Latin_1);
with POSIX;

private with exceptions;
private with IO;
private with tracing;

use  Ada.Finalization;
--
use  KDF9;

package IOC is

   pragma Unsuppress(All_Checks);

   -- N.B. the KDF9 'buffer' is a DMA controller in more modern terminology.

   -- Each KDF9 buffer is externally characterized by:
   --    its (absolute) number,
   --    its (attached-device) kind, and
   --    its unit (the number of that device within its kind).

   -- A device of ND_kind is attached to a buffer with No Device connected.
   -- If commanded, it performs a basic default action,
   --   which is to cause a LIV interrupt in the case of transfers,
   --     but is both benign and appropriate for all other operations.

   type device_kind is
      (CP_kind,  -- Card Punch
       CR_kind,  -- Card Reader
       DR_kind,  -- Drum
       FD_kind,  -- Fixed Disc
       FW_kind,  -- FlexoWriter (monitor typewriter)
       GP_kind,  -- Graph Plotter (Calcomp 120' by 29.5" model)
       LP_kind,  -- Line Printer
       MT_kind,  -- Magnetic Tape
       ST_kind,  -- Seven Track (IBM) magnetic Tape
       TP_kind,  -- Tape Punch
       TR_kind,  -- Tape Reader
       ND_kind   -- No Device
      );

   subtype device_number is KDF9.Q_part range 0 .. 16;

--
   -- This is the root for all I/O device types.
--

   -- The quantum is the time, in µs, taken to transfer a basic datum.
   -- For unit-record devices (CR, CP, LP) this is the card/line, respectively.
   -- For other devices it is the KDF9 character.
   -- A device is slow if it transfers data byte-by-byte; fast devices transfer whole words.

   type device (
                number  : IOC.device_number;
                kind    : IOC.device_kind;
                unit    : KDF9.buffer_number;
                quantum : KDF9.microseconds;
                is_slow : Boolean
               )
   is abstract new Limited_Controlled with private;

   no_fd : constant := -1;

   -- Make the_buffer ready for I/O use by opening its associated file in the_mode;
   --    or, if attaching, by using an already-open stream with an established fd.
   procedure open (the_buffer : in out IOC.device'Class;
                   the_mode   : in POSIX.access_mode;
                   attaching  : in Boolean := False;
                   to_fd      : in Integer := no_fd);

   -- True iff the_buffer has been opened but not yet closed.
   function is_open (the_buffer : IOC.device)
   return Boolean;

   -- A measure of the I/O volume enacted by the_buffer, so far.
   function usage (the_buffer : IOC.device)
   return KDF9.word;

   -- Ensure that all output to the_buffer has been transmitted.
   procedure flush (the_buffer : in out IOC.device);

   -- Make the_buffer unavailable for further I/O use, after flushing if necessary.
   procedure close (the_buffer : in out IOC.device);

   -- A KDF9.logical_device_name is of the form XYu, where XY is a two-letter device-type
   --    code (e.g., "LP" or "CR"); and u is the one- or two-digit logical unit number
   --       of a device within its category.

   function logical_device_name_of (the_buffer : IOC.device)
   return KDF9.logical_device_name;

   function logical_device_name_of (the_number : IOC.device_number)
   return KDF9.logical_device_name;

   -- The elapsed time for the I/O of the given number of atomic_items
   --    which may be, e.g., bytes, or card images, or printer lines.
   function IO_elapsed_time (the_buffer   : IOC.device;
                             atomic_items : KDF9.word)
   return KDF9.microseconds;

   -- The total elapsed time taken, so far, by transfers on the attached device.
   function IO_elapsed_time_total (the_buffer : IOC.device)
   return KDF9.microseconds;


   --
   -- The CLOQq, SLOQq and TLOQq operations do NOT address a buffer,
   --    and so are fully implemented elsewhere.
   --

   --
   -- The INTQq, BUSYQq, PARQq and CTQq operations DO address a buffer,
   --    but do NOT initiate an I/O transfer, and so are common to all devices.
   --

   -- INTQq
   procedure INT (the_buffer  : in IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- BUSYQq
   procedure BUSY (the_buffer  : in IOC.device'Class;
                   Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out KDF9.word);

   -- PARQq
   procedure PAR (the_buffer   : in out IOC.device'Class;
                   Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out KDF9.word);

   -- CTQq
   procedure CTQ (the_buffer  : in out IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- These KDF9 data-transfer operations must be overridden for non-trivial functionality.
   -- Invoking any of them raises a LIV exception. This exactly mirrors the action of the
   --    KDF9 in causing a LIV interrupt when an invalid operation was applied to a device.
   -- A device without some of these operations inherits them from this list and so
   --    implements correctly the original semantics of the KDF9.

   --
   -- The PI* are input operations.
   --

   -- PIAQq
   procedure PIA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PIBQq
   procedure PIB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PICQq
   procedure PIC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PIDQq
   procedure PID (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PIEQq
   procedure PIE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PIFQq
   procedure PIF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PIGQq
   procedure PIG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PIHQq
   procedure PIH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   --
   -- The PM* are device-status operations.
   --

   -- PMAQq
   procedure PMA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMBQq
   procedure PMB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMCQq
   procedure PMC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMDQq
   procedure PMD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMEQq
   procedure PME (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMFQq
   procedure PMF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMGQq
   procedure PMG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMHQq
   procedure PMH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMGQq
   procedure PMK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PMHQq
   procedure PML (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   --
   -- The PO* are output operations.
   --

   -- POAQq
   procedure POA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POBQq
   procedure POB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POCQq
   procedure POC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- PODQq
   procedure POD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POEQq
   procedure POE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POFQq
   procedure POF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POGQq
   procedure POG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POHQq
   procedure POH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POKQq
   procedure POK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- POLQq
   procedure POL (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);


--
   -- The byte_device type is the root type for all byte-mode I/O device types.
--

   type byte_device is abstract new IOC.device with private;

   -- Change the file associated with a device.
   procedure reattach (the_buffer   : in out IOC.byte_device;
                       the_file     : in String);

   -- Allow a file to be appended to the device when EOF is detected.
   -- If the user does not specify such a file, set the device abnormal.
   procedure deal_with_end_of_data (the_buffer : in out IOC.byte_device);


--
   -- The unit_record_device type is the root type for all unit-record I/O device types.
--

   type unit_record_device is abstract new IOC.byte_device with private;


--
   -- The buffer_configuration type enables the dynamic setting-up of an IOC configuration.
--

   type device_class_access  is access all IOC.device'Class;

   type buffer_configuration is array (KDF9.buffer_number) of IOC.device_class_access;

   -- These are the buffer numbers for the devices in this configuration.

   ND0_number : constant := 16;          -- No Device (not in configuration)

   FW0_number : constant := 0;            -- FlexoWriter (monitor typewriter)
   TR0_number : constant := 1;            -- Tape Reader Unit 0
   TR1_number : constant := 2;            -- Tape Reader Unit 1
   TP1_number : constant := 3;            -- Tape Punch Unit 1 (sic)
   GP0_number : constant := 3;            -- Graph Plotter, on the TP1 buffer
   TP0_number : constant := 4;            -- Tape Punch Unit 0 (sic)
   LP0_number : constant := 5;            -- Line Printer
   CR0_number : constant := 6;            -- Card Reader
   MT0_number : constant := 7;            -- Magnetic Tape Unit 0
   CP0_number : constant := 15;           -- Card Punch
   DR0_number : constant := ND0_number;   -- Drum
   ST0_number : constant := ND0_number;   -- Seven track IBM magnetic Tape
   -- FD0_number is defined in KDF9       -- Fixed Disc

   -- These are the I/O devices installed in this configuration.
   -- The configuration is initialised elsewhere to avoid cyclical type dependencies.

   buffer : buffer_configuration;

--
   -- These operations are used by Directors to manage device allocation to problem programs.
--

   procedure set_state_of (the_buffer : in device_class_access;
                           allocated  : in Boolean);

   function is_unallocated (the_buffer : device_class_access)  -- N.B. is_UNallocated.
   return Boolean;

--
   -- These buffer-implementation operations are used outside IOC and apply to all device types.
--

   -- Complete all extant transfers, then Finalize each buffer.
   procedure finalize_all_KDF9_buffers;

   -- Advance the elapsed time to a point after all extant transfer have terminated.
   procedure complete_all_extant_transfers;

   -- Complete any terminated transfer operations and take any needed interrupts.
   procedure act_on_pending_interrupts;

   -- A LOV interupt caused by an attempted store access must arrange
   --    for the interrupted instruction to be resumed.
   -- In boot mode, signal the LOV interrupt to Director.
   -- In other modes, advance the elapsed time to the end-of-transfer time
   --    for the locked-out address, then act on pending interrupts.
   procedure handle_a_main_store_lockout (address : in KDF9.Q_part);

   -- Take note of the start of a transfer.
   procedure start_timed_transfer (the_buffer  : in out IOC.device'Class;
                                   Q_operand   : in KDF9.Q_register;
                                   set_offline : in Boolean;
                                   busy_time   : in KDF9.microseconds;
                                   is_DMAing   : in Boolean := True);

private

   -- The following packages are hereby made available to all children of IOC.
   use exceptions; pragma Warnings(Off, exceptions);
   use IO;         pragma Warnings(Off, IO);
   use tracing;    pragma Warnings(Off, tracing);
   use POSIX;

--
   -- Completing the root for all I/O device types.
--

   type device (
                number  : IOC.device_number;
                kind    : IOC.device_kind;
                unit    : KDF9.buffer_number;
                quantum : KDF9.microseconds;
                is_slow : Boolean
               )
   is abstract new Limited_Controlled with
               record
                  is_abnormal,
                  is_busy,
                  is_DMAing,
                  is_offline,
                  is_allocated,
                  is_for_Director : Boolean := False;
                  initiation_time : KDF9.microseconds := KDF9.microseconds'Last;
                  transfer_time   : KDF9.microseconds := KDF9.microseconds'Last;
                  completion_time : KDF9.microseconds := KDF9.microseconds'Last;
                  control_word    : KDF9.Q_register;
                  decoded_order   : KDF9.decoded_order;
                  device_name     : KDF9.logical_device_name;
                  order_address   : KDF9.code_point;
                  order_count     : KDF9.order_counter;
                  priority_level  : KDF9.priority;
                  stream          : IO.stream;
               end record;

   overriding
   procedure Initialize (the_buffer : in out IOC.device);

   overriding
   procedure Finalize (the_buffer : in out IOC.device);

--
   -- Completing the root for all byte-mode I/O device types.
--

   type byte_device is abstract new IOC.device with
      record
         byte_count : KDF9.word := 0;
      end record;

   procedure close (the_buffer  : in out IOC.byte_device;
                    the_action  : in String;
                    the_amount  : in KDF9.word;
                    the_quantum : in String);

   -- The number of timed transfer units in the designated core-store area.
   -- In the case of unit-record devices, such as card readers and line printers,
   --    this is the number of unit records (cards, or lines, respectively).
   -- In all other cases it is the number of characters in the designated core-store area.
   function atomic_item_count (the_buffer : IOC.byte_device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word;

   -- Check the IO parameters and the buffer state, and handle any old lockout.
   -- Set the new buffer state, and project the next interrupt time.
   procedure initialize_byte_mode_gapping (the_buffer   : in out IOC.byte_device;
                                           Q_operand    : in KDF9.Q_register;
                                           set_offline  : in Boolean);

   -- Check the IO parameters and the buffer state, and handle any old lockout.
   -- Set the new buffer state, and project the next interrupt time.
   procedure initialize_byte_mode_transfer (the_buffer   : in out IOC.byte_device;
                                            Q_operand    : in KDF9.Q_register;
                                            set_offline  : in Boolean);

   -- Read a character from the stream and deal with any input file concatenation.
   procedure get_char_from_stream (char       : out Character;
                                   the_buffer : in out IOC.byte_device;
                                   size       : in out KDF9.word);


--
   -- Completing the root the root type for all unit-record I/O device types.
--

   type unit_record_device is abstract new IOC.byte_device with
      record
         unit_count : KDF9.word := 0;
      end record;

   overriding
   function IO_elapsed_time_total (the_buffer : IOC.unit_record_device)
   return KDF9.microseconds;

   overriding
   function atomic_item_count (the_buffer : IOC.unit_record_device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word;

--
--
   -- Operations, used only within the IOC hierarchy, that apply to all device types.
--
--

   -- Mask off the buffer number in the Q_operand.C; to remove any disc parameter.
   function canonical (Q_operand : KDF9.Q_register)
   return KDF9.Q_register;


   -- Check that the_buffer is online, validly identified by the Q_operand,
   --    and that access to it is permitted by the (perhaps simulated) Director;
   --       LIV if not.
   procedure validate_device (the_buffer : in IOC.device'Class;
                              Q_operand  : in KDF9.Q_register);

   -- Check that the buffer for the_device is unused, then set it to the_device.
   procedure install (the_device : in out IOC.device'Class);

   -- Check that the device and the transfer address bounds are valid;
   --    LIV if not.
   procedure validate_transfer (the_buffer : in IOC.device'Class;
                                Q_operand  : in KDF9.Q_register);

   -- LIV if the_buffer is in the abnormal state.
   procedure validate_parity (the_buffer : in IOC.device'Class);

   -- When the real duration of a variable-length transfer is known,
   --    its completion time can be made accurate by giving its actual_time.
   -- correct_transfer_time must be called before finalize_transfer is called.

   procedure correct_transfer_time (the_buffer  : in out IOC.device'Class;
                                    actual_time : in KDF9.microseconds);

   procedure correct_transfer_time (the_buffer    : in out IOC.device'Class;
                                    actual_length : in KDF9.word);

   -- If the buffer has a terminated transfer, clear its lockouts, reset its state,
   --    update the PHUs, and demand an EDT or PR interrupt as needed.
   procedure finalize_transfer (the_buffer : in out IOC.device'Class;
                                need_EDT,
                                need_PR    : out Boolean);

   -- In boot mode, signal the LOV interrupt to Director.
   -- In other modes, advance the elapsed time to the next-interrupt time,
   --    and suppress the LOV by simulating an earlier end of transfer.
   procedure handle_a_buffer_lockout (the_buffer : in IOC.device'Class);

   -- Account for the CPU (i.e., core store) time taken by the buffer's DMA cycles.
   procedure add_in_the_IO_CPU_time (IO_CPU_time : in KDF9.microseconds);

   procedure add_in_the_IO_CPU_time (the_buffer  : in IOC.device'Class;
                                     bytes_moved : in KDF9.word);

   -- LIV if the repetition count is negative.
   procedure require_nonnegative_count (count : in KDF9.Q_part);

   -- LIV if the repetition count is negative or zero.
   procedure require_positive_count (count : in KDF9.Q_part);

   -- Account for the CPU time taken by the buffer in setting store lockouts.
   procedure add_in_the_IO_lockout_CPU_time (Q_operand : in KDF9.Q_register);

end IOC;
