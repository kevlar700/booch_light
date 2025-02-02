--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light
  .Map_Simple_Cached_Sequential_Bounded_Managed_Iterator is

   procedure Find
     (The_Domain : in     Domain;
      In_The_Map : in     Map;
      The_Bucket :    out Natural)
   is
      Initial_Probe    : constant Natural :=
        Hash_Of (The_Domain) mod In_The_Map.The_Size;
      Temporary_Index  : Positive;
      Temporary_Bucket : Natural;
   begin
      Temporary_Bucket := 0;
      for Index in In_The_Map.The_Items'Range loop
         Temporary_Index :=
           ((Index + Initial_Probe - 2) mod In_The_Map.The_Size) + 1;
         case In_The_Map.The_Items (Temporary_Index).The_State is
            when Empty =>
               if Temporary_Bucket = 0 then
                  Temporary_Bucket := Temporary_Index;
               end if;
               The_Bucket := Temporary_Bucket;
               return;
            when Deleted =>
               if Temporary_Bucket = 0 then
                  Temporary_Bucket := Temporary_Index;
               end if;
            when Bound =>
               if In_The_Map.The_Items (Temporary_Index).The_Domain =
                 The_Domain
               then
                  The_Bucket := Temporary_Index;
                  return;
               end if;
         end case;
      end loop;
      The_Bucket := Temporary_Bucket;
   end Find;

   procedure Copy
     (From_The_Map : in     Map;
      To_The_Map   : in out Map;
      Booch_Status :    out Locus.Copy)
   is
      The_Bucket : Natural;
   begin
      if From_The_Map.The_Count > To_The_Map.The_Size then
         Alterable_Log.Log
           (Log_ID  => "E13065CDB909A83C",
            Message => "Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;
      end if;

      for Index in To_The_Map.The_Items'Range loop
         To_The_Map.The_Items (Index).The_State := Empty;
      end loop;
      To_The_Map.The_Count := 0;
      for Index in From_The_Map.The_Items'Range loop
         if From_The_Map.The_Items (Index).The_State = Bound then
            Find
              (From_The_Map.The_Items (Index).The_Domain, To_The_Map,
               The_Bucket);
            To_The_Map.The_Items (The_Bucket) :=
              From_The_Map.The_Items (Index);
         end if;
      end loop;
      To_The_Map.The_Count            := From_The_Map.The_Count;
      To_The_Map.The_Cache.Is_Current := False;

      Booch_Status := OK;

   end Copy;

   procedure Clear (The_Map : in out Map) is
   begin
      for Index in The_Map.The_Items'Range loop
         The_Map.The_Items (Index).The_State := Empty;
      end loop;
      The_Map.The_Count            := 0;
      The_Map.The_Cache.Is_Current := False;
   end Clear;

   procedure Bind
     (The_Domain    : in     Domain;
      And_The_Range : in     Ranges;
      In_The_Map    : in out Map;
      Booch_Status  :    out Locus.Bind)
   is
      The_Bucket : Natural;
   begin
      Find (The_Domain, In_The_Map, The_Bucket);
      if In_The_Map.The_Items (The_Bucket).The_State = Bound then
         Alterable_Log.Log
           (Log_ID  => "965F816779985A28",
            Message => "Multiple_Binding: Bind failed");
         Booch_Status := Multiple_Binding;

         return;
      end if;

      In_The_Map.The_Items (The_Bucket) :=
        Node'
          (Bound,
           The_Domain,
           And_The_Range);

      In_The_Map.The_Count := In_The_Map.The_Count + 1;

      In_The_Map.The_Cache :=
        Cache'
          (The_Domain => The_Domain,
           The_Range  => And_The_Range,
           Is_Bound   => True,
           Is_Current => True);

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "2EA27411FE76A797",
            Message => "Constraint_Error: Exception_Overflow: Bind failed");
         Booch_Status := Exception_Overflow;
         return;

   end Bind;

   procedure Unbind
     (The_Domain   : in     Domain;
      In_The_Map   : in out Map;
      Booch_Status :    out Locus.Unbind)
   is
      The_Bucket : Natural;
   begin
      Find (The_Domain, In_The_Map, The_Bucket);
      if In_The_Map.The_Items (The_Bucket).The_State = Bound then
         In_The_Map.The_Items (The_Bucket).The_State := Deleted;
         In_The_Map.The_Count := In_The_Map.The_Count - 1;
         In_The_Map.The_Cache.The_Domain             := The_Domain;
         In_The_Map.The_Cache.Is_Bound               := False;
         In_The_Map.The_Cache.Is_Current             := True;
         Booch_Status                                := OK;
         return;
      end if;

      Alterable_Log.Log
        (Log_ID  => "84973A797E99D282",
         Message => "Domain_Is_Not_Bound: Unbind failed");
      Booch_Status := Domain_Is_Not_Bound;
      return;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "D03D3D0BD4D1F85D",
            Message => "Constraint_Error: Domain_Is_Not_Bound: Unbind failed");
         Booch_Status := Domain_Is_Not_Bound;
         return;

   end Unbind;

   function Is_Equal
     (Left  : in Map;
      Right : in Map)
      return Boolean
   is
      Temporary_Index : Natural;
   begin
      if Left.The_Count /= Right.The_Count then
         return False;
      else
         for Index in 1 .. Left.The_Size loop
            if Left.The_Items (Index).The_State = Bound then
               Temporary_Index := 0;
               for Inner_Index in 1 .. Right.The_Size loop
                  if (Right.The_Items (Index).The_State = Bound)
                    and then
                    (Left.The_Items (Index).The_Domain =
                     Right.The_Items (Inner_Index).The_Domain)
                  then
                     Temporary_Index := Inner_Index;
                     exit;
                  end if;
               end loop;
               if Left.The_Items (Index).The_Range /=
                 Right.The_Items (Temporary_Index).The_Range
               then
                  return False;
               end if;
            end if;
         end loop;
         return True;
      end if;
   exception
      when Constraint_Error =>
         return False;
   end Is_Equal;

   function Extent_Of
     (The_Map : in Map)
      return Natural
   is
   begin
      return The_Map.The_Count;
   end Extent_Of;

   function Is_Empty
     (The_Map : in Map)
      return Boolean
   is
   begin
      return (The_Map.The_Count = 0);
   end Is_Empty;

   function Is_Bound
     (The_Domain : in Domain;
      In_The_Map : in Map)
      return Boolean
   is
      The_Bucket : Natural;
   begin
      if In_The_Map.The_Cache.Is_Current
        and then In_The_Map.The_Cache.The_Domain = The_Domain
      then
         return In_The_Map.The_Cache.Is_Bound;
      else
         Find (The_Domain, In_The_Map, The_Bucket);
         return (In_The_Map.The_Items (The_Bucket).The_State = Bound);
      end if;
   exception
      when Constraint_Error =>
         return False;
   end Is_Bound;

   procedure Range_Of
     (The_Domain   : in     Domain;
      In_The_Map   : in     Map;
      Result       :    out Ranges;
      Booch_Status :    out Locus.Range_Of)
   is
      The_Bucket : Natural;
   begin
      if In_The_Map.The_Cache.Is_Current
        and then In_The_Map.The_Cache.The_Domain = The_Domain
      then
         if In_The_Map.The_Cache.Is_Bound then
            Result       := In_The_Map.The_Cache.The_Range;
            Booch_Status := OK;
            return;
         else
            Alterable_Log.Log
              (Log_ID  => "A59D9430219EABDB",
               Message => "Domain_Is_Not_Bound: Range_Of failed");
            Booch_Status := Domain_Is_Not_Bound;
            return;
         end if;
      else
         Find (The_Domain, In_The_Map, The_Bucket);
         if In_The_Map.The_Items (The_Bucket).The_State = Bound then
            Result       := In_The_Map.The_Items (The_Bucket).The_Range;
            Booch_Status := OK;
            return;
         else
            Alterable_Log.Log
              (Log_ID  => "5D121FC7E9D78291",
               Message => "Domain_Is_Not_Bound: Range_Of failed");
            Booch_Status := Domain_Is_Not_Bound;
            return;
         end if;
      end if;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "10D33EDCE7E3BFB3",
            Message =>
              "Constraint_Error: Domain_Is_Not_Bound: Range_Of failed");
         Booch_Status := Domain_Is_Not_Bound;
         return;

   end Range_Of;

   procedure Iterate (Over_The_Map : in Map) is
      Continue : Boolean;
   begin
      for Index in Over_The_Map.The_Items'Range loop
         if Over_The_Map.The_Items (Index).The_State = Bound then
            Process
              (Over_The_Map.The_Items (Index).The_Domain,
               Over_The_Map.The_Items (Index).The_Range, Continue);
            exit when not Continue;
         end if;
      end loop;
   end Iterate;

end Booch_Light.Map_Simple_Cached_Sequential_Bounded_Managed_Iterator;

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
