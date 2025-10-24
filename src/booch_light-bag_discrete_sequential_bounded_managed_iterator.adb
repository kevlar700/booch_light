--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light.Bag_Discrete_Sequential_Bounded_Managed_Iterator is

   procedure Copy
     (From_The_Bag :     Bag;
      To_The_Bag   : in out Bag)
   is
   begin
      To_The_Bag := From_The_Bag;
   end Copy;

   procedure Clear (The_Bag : in out Bag) is
   begin
      The_Bag.The_Items := Items'(others => 0);
   end Clear;

   procedure Add
     (The_Item   :     Item;
      To_The_Bag : in out Bag)
   is
   begin
      To_The_Bag.The_Items (The_Item) := To_The_Bag.The_Items (The_Item) + 1;
   end Add;

   procedure Remove
     (The_Item     :     Item;
      From_The_Bag : in out Bag;
      Booch_Status :    out Locus.Remove)
   is
   begin
      From_The_Bag.The_Items (The_Item) :=
        From_The_Bag.The_Items (The_Item) - 1;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Log
           (Log_ID  => "D9C98776F487FDBB",
            Message => "Item_Is_Not_In_Bag: Cannot remove it");
         Booch_Status := Item_Is_Not_In_Bag;
         return;

   end Remove;

   procedure Union
     (Of_The_Bag  :     Bag;
      And_The_Bag :     Bag;
      To_The_Bag  : in out Bag)
   is
   begin
      for Index in To_The_Bag.The_Items'Range loop
         To_The_Bag.The_Items (Index) :=
           Of_The_Bag.The_Items (Index) + And_The_Bag.The_Items (Index);
      end loop;
   end Union;

   procedure Intersection
     (Of_The_Bag  :     Bag;
      And_The_Bag :     Bag;
      To_The_Bag  : in out Bag)
   is
   begin
      for Index in To_The_Bag.The_Items'Range loop
         if Of_The_Bag.The_Items (Index) < And_The_Bag.The_Items (Index) then
            To_The_Bag.The_Items (Index) := Of_The_Bag.The_Items (Index);
         else
            To_The_Bag.The_Items (Index) := And_The_Bag.The_Items (Index);
         end if;
      end loop;
   end Intersection;

   procedure Difference
     (Of_The_Bag  :     Bag;
      And_The_Bag :     Bag;
      To_The_Bag  : in out Bag)
   is
   begin
      for Index in To_The_Bag.The_Items'Range loop
         if Of_The_Bag.The_Items (Index) > And_The_Bag.The_Items (Index) then
            To_The_Bag.The_Items (Index) :=
              Of_The_Bag.The_Items (Index) - And_The_Bag.The_Items (Index);
         else
            To_The_Bag.The_Items (Index) := 0;
         end if;
      end loop;
   end Difference;

   function Is_Equal
     (Left  : Bag;
      Right : Bag)
      return Boolean
   is
   begin
      return (Left = Right);
   end Is_Equal;

   function Extent_Of
     (The_Bag : Bag)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Index in The_Bag.The_Items'Range loop
         Count := Count + The_Bag.The_Items (Index);
      end loop;
      return Count;
   end Extent_Of;

   function Unique_Extent_Of
     (The_Bag : Bag)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Index in The_Bag.The_Items'Range loop
         if The_Bag.The_Items (Index) > 0 then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Unique_Extent_Of;

   procedure Number_Of
     (The_Item      :     Item;
      In_The_Bag    :     Bag;
      The_Number_Of :    out Positive;
      Booch_Status  :    out Locus.Number_Of)
   is
   begin
      if In_The_Bag.The_Items (The_Item) = 0 then
         Alogs.Log
           (Log_ID  => "05F5ED2BFD976EAB",
            Message => "Item_Is_Not_In_Bag: Number_Of failed");
         Booch_Status  := Item_Is_Not_In_Bag;
         The_Number_Of := Positive'Last;
         return;
      else
         The_Number_Of := In_The_Bag.The_Items (The_Item);
      end if;

      Booch_Status := OK;

   end Number_Of;

   function Is_Empty
     (The_Bag : Bag)
      return Boolean
   is
   begin
      return (The_Bag.The_Items = Items'(others => 0));
   end Is_Empty;

   function Is_A_Member
     (The_Item   : Item;
      Of_The_Bag : Bag)
      return Boolean
   is
   begin
      return (Of_The_Bag.The_Items (The_Item) > 0);
   end Is_A_Member;

   function Is_A_Subset
     (Left  : Bag;
      Right : Bag)
      return Boolean
   is
   begin
      for Index in Left.The_Items'Range loop
         if Left.The_Items (Index) > Right.The_Items (Index) then
            return False;
         end if;
      end loop;
      return True;
   end Is_A_Subset;

   function Is_A_Proper_Subset
     (Left  : Bag;
      Right : Bag)
      return Boolean
   is
      Unique_Left_Count  : Natural := 0;
      Unique_Right_Count : Natural := 0;
      Total_Left_Count   : Natural := 0;
      Total_Right_Count  : Natural := 0;
   begin
      for Index in Left.The_Items'Range loop
         if Left.The_Items (Index) > Right.The_Items (Index) then
            return False;
         end if;
         if Left.The_Items (Index) > 0 then
            Unique_Left_Count := Unique_Left_Count + 1;
            Total_Left_Count  := Total_Left_Count + Left.The_Items (Index);
         end if;
         if Right.The_Items (Index) > 0 then
            Unique_Right_Count := Unique_Right_Count + 1;
            Total_Right_Count  := Total_Right_Count + Right.The_Items (Index);
         end if;
      end loop;
      if Unique_Left_Count < Unique_Right_Count then
         return True;
      elsif Unique_Left_Count > Unique_Right_Count then
         return False;
      else
         return (Total_Left_Count < Total_Right_Count);
      end if;
   end Is_A_Proper_Subset;

   procedure Iterate (Over_The_Bag : Bag) is
      Continue : Boolean := True;
   begin
      for The_Iterator in Over_The_Bag.The_Items'Range loop
         if Over_The_Bag.The_Items (The_Iterator) > 0 then
            Process
              (The_Iterator, Over_The_Bag.The_Items (The_Iterator), Continue);
         end if;
         exit when not Continue;
      end loop;
   end Iterate;

end Booch_Light.Bag_Discrete_Sequential_Bounded_Managed_Iterator;

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
