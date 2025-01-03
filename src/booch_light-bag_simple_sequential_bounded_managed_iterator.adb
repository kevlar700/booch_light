--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Bag_Simple_Sequential_Bounded_Managed_Iterator is

   procedure Copy
     (From_The_Bag : in     Bag;
      To_The_Bag   : in out Bag;
      Booch_Status :    out Locus.Copy)
   is
   begin
      if From_The_Bag.The_Back > To_The_Bag.The_Size then
         Alterable_Log.Log
           (Log_ID  => "B16D9DE9925817D9",
            Message => "Exception_Overflow: Cannot copy between the bags");
         Booch_Status := Exception_Overflow;
         return;

      else
         To_The_Bag.The_Items (1 .. From_The_Bag.The_Back) :=
           From_The_Bag.The_Items (1 .. From_The_Bag.The_Back);
         To_The_Bag.The_Back := From_The_Bag.The_Back;
      end if;

      Booch_Status := OK;

   end Copy;

   procedure Clear (The_Bag : in out Bag) is
   begin
      The_Bag.The_Back := 0;
   end Clear;

   procedure Add
     (The_Item     : in     Item;
      To_The_Bag   : in out Bag;
      Booch_Status :    out Locus.Add)
   is
   begin
      for Index in 1 .. To_The_Bag.The_Back loop
         if The_Item = To_The_Bag.The_Items (Index).The_Item then

            To_The_Bag.The_Items (Index).The_Count :=
              To_The_Bag.The_Items (Index).The_Count + 1;

            Booch_Status := OK;
            return;

         end if;
      end loop;

      To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Item  := The_Item;
      To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Count := 1;
      To_The_Bag.The_Back := To_The_Bag.The_Back + 1;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "78EC63445113DF18",
            Message => "Exception_Overflow: Cannot Add To_The_Bag");
         Booch_Status := Exception_Overflow;
         return;

   end Add;

   procedure Remove
     (The_Item     : in     Item;
      From_The_Bag : in out Bag;
      Booch_Status :    out Locus.Remove)
   is
   begin
      for Index in 1 .. From_The_Bag.The_Back loop
         if The_Item = From_The_Bag.The_Items (Index).The_Item then
            if From_The_Bag.The_Items (Index).The_Count > 1 then

               From_The_Bag.The_Items (Index).The_Count :=
                 From_The_Bag.The_Items (Index).The_Count - 1;

            else
               From_The_Bag.The_Items (Index .. (From_The_Bag.The_Back - 1)) :=
                 From_The_Bag.The_Items ((Index + 1) .. From_The_Bag.The_Back);
               From_The_Bag.The_Back := From_The_Bag.The_Back - 1;
            end if;

            Booch_Status := OK;
            return;

         end if;
      end loop;

      Alterable_Log.Log
        (Log_ID  => "D9C9FF52CF33F56B",
         Message => "Item_Is_Not_In_Bag: Cannot remove it from the bag");
      Booch_Status := Item_Is_Not_In_Bag;
      return;
   end Remove;

   procedure Union
     (Of_The_Bag   : in     Bag;
      And_The_Bag  : in     Bag;
      To_The_Bag   : in out Bag;
      Booch_Status :    out Locus.Union)
   is
      To_Index : Natural;
      To_Back  : Natural;
   begin
      To_The_Bag.The_Items (1 .. Of_The_Bag.The_Back) :=
        Of_The_Bag.The_Items (1 .. Of_The_Bag.The_Back);
      To_The_Bag.The_Back                             := Of_The_Bag.The_Back;
      To_Back                                         := To_The_Bag.The_Back;
      for And_Index in 1 .. And_The_Bag.The_Back loop
         To_Index := To_Back;
         while To_Index > 0 loop
            if To_The_Bag.The_Items (To_Index).The_Item =
              And_The_Bag.The_Items (And_Index).The_Item
            then
               exit;
            else
               To_Index := To_Index - 1;
            end if;
         end loop;
         if To_Index = 0 then
            To_The_Bag.The_Items (To_The_Bag.The_Back + 1) :=
              And_The_Bag.The_Items (And_Index);
            To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
         else
            To_The_Bag.The_Items (To_Index).The_Count :=
              To_The_Bag.The_Items (To_Index).The_Count +
              And_The_Bag.The_Items (And_Index).The_Count;
         end if;
      end loop;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "5848BD73C08A5CEE",
            Message => "Exception_Overflow: Union of bags failed");
         Booch_Status := Exception_Overflow;
         return;

   end Union;

   procedure Intersection
     (Of_The_Bag   : in     Bag;
      And_The_Bag  : in     Bag;
      To_The_Bag   : in out Bag;
      Booch_Status :    out Locus.Intersection)
   is
      And_Index : Natural;
   begin
      To_The_Bag.The_Back := 0;
      for Of_Index in 1 .. Of_The_Bag.The_Back loop
         And_Index := And_The_Bag.The_Back;
         while And_Index > 0 loop
            if Of_The_Bag.The_Items (Of_Index).The_Item =
              And_The_Bag.The_Items (And_Index).The_Item
            then
               if Of_The_Bag.The_Items (Of_Index).The_Count <
                 And_The_Bag.The_Items (And_Index).The_Count
               then
                  To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Item  :=
                    Of_The_Bag.The_Items (Of_Index).The_Item;
                  To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Count :=
                    Of_The_Bag.The_Items (Of_Index).The_Count;
                  To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
               else
                  To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Item  :=
                    Of_The_Bag.The_Items (Of_Index).The_Item;
                  To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Count :=
                    And_The_Bag.The_Items (And_Index).The_Count;
                  To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
               end if;
               exit;
            else
               And_Index := And_Index - 1;
            end if;
         end loop;
      end loop;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "5848BD73C08A5CEE",
            Message => "Exception_Overflow: Intersection of bags failed");
         Booch_Status := Exception_Overflow;
         return;

   end Intersection;

   procedure Difference
     (Of_The_Bag   : in     Bag;
      And_The_Bag  : in     Bag;
      To_The_Bag   : in out Bag;
      Booch_Status :    out Locus.Difference)
   is
      And_Index : Natural;
   begin
      To_The_Bag.The_Back := 0;
      for Of_Index in 1 .. Of_The_Bag.The_Back loop
         And_Index := And_The_Bag.The_Back;
         while And_Index > 0 loop
            if Of_The_Bag.The_Items (Of_Index).The_Item =
              And_The_Bag.The_Items (And_Index).The_Item
            then
               exit;
            else
               And_Index := And_Index - 1;
            end if;
         end loop;
         if And_Index = 0 then
            To_The_Bag.The_Items (To_The_Bag.The_Back + 1) :=
              Of_The_Bag.The_Items (Of_Index);
            To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
         elsif Of_The_Bag.The_Items (Of_Index).The_Count >
           And_The_Bag.The_Items (And_Index).The_Count
         then
            To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Item  :=
              Of_The_Bag.The_Items (Of_Index).The_Item;
            To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Count :=
              Of_The_Bag.The_Items (Of_Index).The_Count -
              And_The_Bag.The_Items (And_Index).The_Count;
            To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
         end if;
      end loop;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "EB5D4DA7AB2EA11E",
            Message => "Exception_Overflow: Difference of bags failed");
         Booch_Status := Exception_Overflow;
         return;

   end Difference;

   function Is_Equal
     (Left  : in Bag;
      Right : in Bag)
      return Boolean
   is
      Right_Index : Natural;
   begin
      if Left.The_Back /= Right.The_Back then
         return False;
      else
         for Left_Index in 1 .. Left.The_Back loop
            Right_Index := Right.The_Back;
            while Right_Index > 0 loop
               if Left.The_Items (Left_Index).The_Item =
                 Right.The_Items (Right_Index).The_Item
               then
                  if Left.The_Items (Left_Index).The_Count /=
                    Right.The_Items (Right_Index).The_Count
                  then
                     return False;
                  else
                     exit;
                  end if;
               else
                  Right_Index := Right_Index - 1;
               end if;
            end loop;
            if Right_Index = 0 then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Extent_Of
     (The_Bag : in Bag)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Index in 1 .. The_Bag.The_Back loop
         Count := Count + The_Bag.The_Items (Index).The_Count;
      end loop;
      return Count;
   end Extent_Of;

   function Unique_Extent_Of
     (The_Bag : in Bag)
      return Natural
   is
   begin
      return The_Bag.The_Back;
   end Unique_Extent_Of;

   procedure Number_Of
     (The_Item      : in     Item;
      In_The_Bag    : in     Bag;
      The_Number_Of :    out Positive;
      Booch_Status  :    out Locus.Number_Of)
   is
   begin
      for Index in 1 .. In_The_Bag.The_Back loop
         if The_Item = In_The_Bag.The_Items (Index).The_Item then
            The_Number_Of := In_The_Bag.The_Items (Index).The_Count;
            Booch_Status  := OK;
         end if;
      end loop;

      Alterable_Log.Log
        (Log_ID  => "9F26B4639B25F58D",
         Message => "Item_Is_Not_In_Bag: Number_Of failed");
      Booch_Status := Item_Is_Not_In_Bag;
      return;

   end Number_Of;

   function Is_Empty
     (The_Bag : in Bag)
      return Boolean
   is
   begin
      return (The_Bag.The_Back = 0);
   end Is_Empty;

   function Is_A_Member
     (The_Item   : in Item;
      Of_The_Bag : in Bag)
      return Boolean
   is
   begin
      for Index in 1 .. Of_The_Bag.The_Back loop
         if Of_The_Bag.The_Items (Index).The_Item = The_Item then
            return True;
         end if;
      end loop;
      return False;
   end Is_A_Member;

   function Is_A_Subset
     (Left  : in Bag;
      Right : in Bag)
      return Boolean
   is
      Right_Index : Natural;
   begin
      for Left_Index in 1 .. Left.The_Back loop
         Right_Index := Right.The_Back;
         while Right_Index > 0 loop
            if Left.The_Items (Left_Index).The_Item =
              Right.The_Items (Right_Index).The_Item
            then
               exit;
            else
               Right_Index := Right_Index - 1;
            end if;
         end loop;
         if Right_Index = 0 then
            return False;
         elsif Left.The_Items (Left_Index).The_Count >
           Right.The_Items (Right_Index).The_Count
         then
            return False;
         end if;
      end loop;
      return True;
   end Is_A_Subset;

   function Is_A_Proper_Subset
     (Left  : in Bag;
      Right : in Bag)
      return Boolean
   is
      Total_Left_Count  : Natural := 0;
      Total_Right_Count : Natural := 0;
      Right_Index       : Natural;
   begin
      for Left_Index in 1 .. Left.The_Back loop
         Right_Index := Right.The_Back;
         while Right_Index > 0 loop
            if Left.The_Items (Left_Index).The_Item =
              Right.The_Items (Right_Index).The_Item
            then
               exit;
            else
               Right_Index := Right_Index - 1;
            end if;
         end loop;
         if Right_Index = 0 then
            return False;
         elsif Left.The_Items (Left_Index).The_Count >
           Right.The_Items (Right_Index).The_Count
         then
            return False;
         end if;
         Total_Left_Count :=
           Total_Left_Count + Left.The_Items (Left_Index).The_Count;
      end loop;
      for Index in 1 .. Right.The_Back loop
         Total_Right_Count :=
           Total_Right_Count + Right.The_Items (Index).The_Count;
      end loop;
      if Left.The_Back < Right.The_Back then
         return True;
      elsif Left.The_Back > Right.The_Back then
         return False;
      else
         return (Total_Left_Count < Total_Right_Count);
      end if;
   end Is_A_Proper_Subset;

   procedure Iterate (Over_The_Bag : in Bag) is
      Continue : Boolean;
   begin
      for The_Iterator in 1 .. Over_The_Bag.The_Back loop
         Process
           (Over_The_Bag.The_Items (The_Iterator).The_Item,
            Over_The_Bag.The_Items (The_Iterator).The_Count, Continue);
         exit when not Continue;
      end loop;
   end Iterate;

end Booch_Light.Bag_Simple_Sequential_Bounded_Managed_Iterator;

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
