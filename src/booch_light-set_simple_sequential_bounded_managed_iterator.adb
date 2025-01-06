--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.
with Booch_Light.Alterable_Log;

package body Booch_Light.Set_Simple_Sequential_Bounded_Managed_Iterator is

   procedure Copy
     (From_The_Set : in     Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Copy)
   is
   begin
      if From_The_Set.The_Back > To_The_Set.The_Size then
         Booch_Status := Exception_Overflow;
         Alterable_Log.Log
           (Log_ID  => "91D739548184A9BE",
            Message => "Exception_Overflow: Copy failed");
         return;
      else
         To_The_Set.The_Items (1 .. From_The_Set.The_Back) :=
           From_The_Set.The_Items (1 .. From_The_Set.The_Back);
         To_The_Set.The_Back := From_The_Set.The_Back;
      end if;

      Booch_Status := OK;

   end Copy;

   procedure Clear (The_Set : in out Set) is
   begin
      The_Set.The_Back := 0;
   end Clear;

   procedure Add
     (The_Item     : in     Item;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Add)
   is
   begin

      for Index in 1 .. To_The_Set.The_Back loop
         if The_Item = To_The_Set.The_Items (Index) then
            Alterable_Log.Log
              (Log_ID  => "8965B5BC68EB0E0E",
               Message =>
                 "Item_Is_In_Set: Adding to bounded sequential set failed");
            Booch_Status := Item_Is_In_Set;
            return;
         end if;
      end loop;

      To_The_Set.The_Items (To_The_Set.The_Back + 1) := The_Item;
      To_The_Set.The_Back := To_The_Set.The_Back + 1;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "9E624910669B84F7",
            Message => "Constraint_Error: Bounded set is likely full");
         Booch_Status := Exception_Overflow;
         return;
   end Add;

   procedure Remove
     (The_Item     : in     Item;
      From_The_Set : in out Set;
      Booch_Status :    out Locus.Remove)
   is
   begin
      for Index in 1 .. From_The_Set.The_Back loop
         if The_Item = From_The_Set.The_Items (Index) then
            From_The_Set.The_Items (Index .. (From_The_Set.The_Back - 1)) :=
              From_The_Set.The_Items ((Index + 1) .. From_The_Set.The_Back);
            From_The_Set.The_Back := From_The_Set.The_Back - 1;
            Booch_Status := OK;
            return;
         end if;
      end loop;

      Alterable_Log.Log
        (Log_ID  => "AE45D7381D8BDD9F",
         Message => "Item_Is_Not_In_Set: Remove failed");
      Booch_Status := Item_Is_Not_In_Set;
   end Remove;

   procedure Union
     (Of_The_Set   : in     Set;
      And_The_Set  : in     Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Union)
   is
      To_Index : Natural;
      To_Back  : Natural;
   begin
      To_The_Set.The_Items (1 .. Of_The_Set.The_Back) :=
        Of_The_Set.The_Items (1 .. Of_The_Set.The_Back);
      To_The_Set.The_Back                             := Of_The_Set.The_Back;
      To_Back                                         := To_The_Set.The_Back;
      for And_Index in 1 .. And_The_Set.The_Back loop
         To_Index := To_Back;
         while To_Index > 0 loop
            if To_The_Set.The_Items (To_Index) =
              And_The_Set.The_Items (And_Index)
            then
               exit;
            else
               To_Index := To_Index - 1;
            end if;
         end loop;
         if To_Index = 0 then
            To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
              And_The_Set.The_Items (And_Index);
            To_The_Set.The_Back := To_The_Set.The_Back + 1;
         end if;
      end loop;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "C3F0C44D3AE8632D",
            Message => "Constraint_Error: Union of Sets failed");
         Booch_Status := Exception_Overflow;
         return;

   end Union;

   procedure Intersection
     (Of_The_Set   : in     Set;
      And_The_Set  : in     Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Intersection)
   is
      And_Index : Natural;
   begin
      To_The_Set.The_Back := 0;
      for Of_Index in 1 .. Of_The_Set.The_Back loop
         And_Index := And_The_Set.The_Back;
         while And_Index > 0 loop
            if Of_The_Set.The_Items (Of_Index) =
              And_The_Set.The_Items (And_Index)
            then
               To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
                 Of_The_Set.The_Items (Of_Index);
               To_The_Set.The_Back := To_The_Set.The_Back + 1;
               exit;
            else
               And_Index := And_Index - 1;
            end if;
         end loop;
      end loop;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "FDABB96AB4964564",
            Message => "Constraint_Error: Intersection of Sets failed");
         Booch_Status := Exception_Overflow;
         return;

   end Intersection;

   procedure Difference
     (Of_The_Set   : in     Set;
      And_The_Set  : in     Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Difference)
   is
      And_Index : Natural;
   begin
      To_The_Set.The_Back := 0;
      for Of_Index in 1 .. Of_The_Set.The_Back loop
         And_Index := And_The_Set.The_Back;
         while And_Index > 0 loop
            if Of_The_Set.The_Items (Of_Index) =
              And_The_Set.The_Items (And_Index)
            then
               exit;
            else
               And_Index := And_Index - 1;
            end if;
         end loop;
         if And_Index = 0 then
            To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
              Of_The_Set.The_Items (Of_Index);
            To_The_Set.The_Back := To_The_Set.The_Back + 1;
         end if;
      end loop;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "EC8038191403699A",
            Message => "Constraint_Error: Difference of Sets failed");
         Booch_Status := Exception_Overflow;
         return;
   end Difference;

   function Is_Equal
     (Left  : in Set;
      Right : in Set)
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
               if Left.The_Items (Left_Index) = Right.The_Items (Right_Index)
               then
                  exit;
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
     (The_Set : in Set)
      return Natural
   is
   begin
      return The_Set.The_Back;
   end Extent_Of;

   function Is_Empty
     (The_Set : in Set)
      return Boolean
   is
   begin
      return (The_Set.The_Back = 0);
   end Is_Empty;

   function Is_A_Member
     (The_Item   : in Item;
      Of_The_Set : in Set)
      return Boolean
   is
   begin
      for Index in 1 .. Of_The_Set.The_Back loop
         if Of_The_Set.The_Items (Index) = The_Item then
            return True;
         end if;
      end loop;
      return False;
   end Is_A_Member;

   function Is_A_Subset
     (Left  : in Set;
      Right : in Set)
      return Boolean
   is
      Right_Index : Natural;
   begin
      for Left_Index in 1 .. Left.The_Back loop
         Right_Index := Right.The_Back;
         while Right_Index > 0 loop
            if Left.The_Items (Left_Index) = Right.The_Items (Right_Index) then
               exit;
            else
               Right_Index := Right_Index - 1;
            end if;
         end loop;
         if Right_Index = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_A_Subset;

   function Is_A_Proper_Subset
     (Left  : in Set;
      Right : in Set)
      return Boolean
   is
      Right_Index : Natural;
   begin
      for Left_Index in 1 .. Left.The_Back loop
         Right_Index := Right.The_Back;
         while Right_Index > 0 loop
            if Left.The_Items (Left_Index) = Right.The_Items (Right_Index) then
               exit;
            else
               Right_Index := Right_Index - 1;
            end if;
         end loop;
         if Right_Index = 0 then
            return False;
         end if;
      end loop;
      return (Left.The_Back < Right.The_Back);
   end Is_A_Proper_Subset;

   procedure Iterate_With_Status
     (Over_The_Set : in     Set;
      Booch_Status :    out Status_Item)
   is
      Continue : Boolean;
   begin
      for The_Iterator in 1 .. Over_The_Set.The_Back loop
         Process
           (The_Item     => Over_The_Set.The_Items (The_Iterator),
            Continue     => Continue,
            Booch_Status => Booch_Status);
         exit when not Continue;
      end loop;
   end Iterate_With_Status;

   procedure Iterate (Over_The_Set : in Set) is
      Continue : Boolean;
   begin
      for The_Iterator in 1 .. Over_The_Set.The_Back loop
         Process
           (The_Item => Over_The_Set.The_Items (The_Iterator),
            Continue => Continue);
         exit when not Continue;
      end loop;
   end Iterate;

end Booch_Light.Set_Simple_Sequential_Bounded_Managed_Iterator;

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
