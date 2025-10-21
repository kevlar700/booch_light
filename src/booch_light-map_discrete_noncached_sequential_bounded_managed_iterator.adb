--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light
  .Map_Discrete_Noncached_Sequential_Bounded_Managed_Iterator is

   procedure Copy
     (From_The_Map : in     Map;
      To_The_Map   : in out Map)
   is
   begin
      To_The_Map := From_The_Map;
   end Copy;

   procedure Clear (The_Map : in out Map) is
   begin
      for Index in The_Map'Range loop
         The_Map (Index).Is_Bound := False;
      end loop;
   end Clear;

   procedure Bind
     (The_Domain    : in     Domain;
      And_The_Range : in     Ranges;
      In_The_Map    : in out Map;
      Booch_Status  :    out Locus.Bind)
   is
   begin
      if In_The_Map (The_Domain).Is_Bound then
         Alogs.Log
           (Log_ID  => "286768A4AB552851",
            Message => "Multiple_Binding: Bind failed");
         Booch_Status := Multiple_Binding;
         return;
      end if;

      In_The_Map (The_Domain) :=
        Node'
          (Is_Bound  => True,
           The_Range => And_The_Range);

      Booch_Status := OK;
   end Bind;

   procedure Unbind
     (The_Domain   : in     Domain;
      In_The_Map   : in out Map;
      Booch_Status :    out Locus.Unbind)
   is
   begin
      if not In_The_Map (The_Domain).Is_Bound then
         Alogs.Log
           (Log_ID  => "053FB0CC57AEE092",
            Message => "Domain_Is_Not_Bound: Unbind failed");
         Booch_Status := Domain_Is_Not_Bound;
         return;

      end if;

      In_The_Map (The_Domain).Is_Bound := False;
      Booch_Status                     := OK;
   end Unbind;

   function Is_Equal
     (Left  : in Map;
      Right : in Map)
      return Boolean
   is
   begin
      for Index in Left'Range loop
         if Left (Index).Is_Bound /= Right (Index).Is_Bound then
            return False;
         elsif Left (Index).Is_Bound
           and then Left (Index).The_Range /= Right (Index).The_Range
         then
            return False;
         end if;
      end loop;
      return True;
   end Is_Equal;

   function Extent_Of
     (The_Map : in Map)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Index in The_Map'Range loop
         if The_Map (Index).Is_Bound then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Extent_Of;

   function Is_Empty
     (The_Map : in Map)
      return Boolean
   is
   begin
      for Index in The_Map'Range loop
         if The_Map (Index).Is_Bound then
            return False;
         end if;
      end loop;
      return True;
   end Is_Empty;

   function Is_Bound
     (The_Domain : in Domain;
      In_The_Map : in Map)
      return Boolean
   is
   begin
      return In_The_Map (The_Domain).Is_Bound;
   end Is_Bound;

   procedure Range_Of
     (The_Domain   : in     Domain;
      In_The_Map   : in     Map;
      Result       :    out Ranges;
      Booch_Status :    out Locus.Range_Of)
   is
   begin
      if not In_The_Map (The_Domain).Is_Bound then
         Alogs.Log
           (Log_ID  => "94104FFBD9E3A428",
            Message => "Domain_Is_Not_Bound: Range_Of failed");
         Booch_Status := Domain_Is_Not_Bound;
         return;
      end if;

      Result       := In_The_Map (The_Domain).The_Range;
      Booch_Status := OK;

   end Range_Of;

   procedure Iterate (Over_The_Map : in Map) is
      Continue : Boolean;
   begin
      for The_Iterator in Over_The_Map'Range loop
         if Over_The_Map (The_Iterator).Is_Bound then
            Process
              (The_Iterator, Over_The_Map (The_Iterator).The_Range, Continue);
         end if;
      end loop;
   end Iterate;

end Booch_Light.Map_Discrete_Noncached_Sequential_Bounded_Managed_Iterator;

--              Original Booch Components (Ada 83 version)
--  License: MIT
--  Copyright (C) 1987 Grady Booch Copyright (C) 2024 Kevin Chadwick (Light
--  runtime compatibility)
--
--  Permission is hereby granted, free of charge, to any person obtaining
--  a copy of this software and associated documentation files (the
--  “Software”), to deal in the Software without restriction, including
--  without limitation the rights to use, copy, modify, merge, publish,
--  distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to
--  the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
