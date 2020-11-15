-- kdf9-PHU_store.ads
--
-- The K5 operation data formats.
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

package KDF9.PHU_store is

   -- PHU, the Program Hold-Up register is internal to I/O Control.
   -- It has one element for each of the 4 program priority levels, 0..3.
   -- A subset of its content is exposed to Director by means of the K5 order.

   type blockage_kind is (buffer_busy, locked_out);
   for  blockage_kind'Size use 1;

   type group_address is mod 1024;
   for  group_address'Size use 10;

   type PHU_reason (reason : PHU_store.blockage_kind := buffer_busy) is
      record
         case reason is
            when buffer_busy =>
               buffer_nr  : KDF9.buffer_number;
               INTQq_wait : KDF9.one_bit;
            when locked_out =>
               group_nr   : PHU_store.group_address;
         end case;
      end record;

   type PHU_register (is_held_up : Boolean := False) is
      record
         case is_held_up is
            when False =>
               null;
            when True =>
               blockage : PHU_reason;
         end case;
      end record;

   idle_PHU : constant PHU_register := (is_held_up => False);

   PHU : array (KDF9.priority) of PHU_store.PHU_register := (others => idle_PHU);

   type PHU_subset is
      record
         is_held_up : Boolean;
         reason     : PHU_store.blockage_kind;
         parameter  : KDF9.buffer_number;
      end record;
   for PHU_subset'Size use 6;
   for PHU_subset'Bit_Order use Low_Order_First;

   for  PHU_subset use
      record
         is_held_up at 0 range 5 .. 5;
         reason     at 0 range 4 .. 4;
         parameter  at 0 range 0 .. 3;
      end record;

   -- A K5_operand is a KDF9 word, D00-D47, with the content:
   --    PHU_subset(0) in D00 .. D05
   --    PHU_subset(1) in D06 .. D11
   --    PHU_subset(2) in D12 .. D17
   --    PHU_subset(3) in D18 .. D23
   --    zeros         in D24 .. D47

   function K5_operand
   return KDF9.word;

end KDF9.PHU_store;
