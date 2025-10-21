--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light.Ring_Sequential_Bounded_Managed_Iterator_Exposed is

   procedure Copy
     (From_The_Ring : in     Ring;
      To_The_Ring   : in out Ring;
      Booch_Status  :    out Locus.Copy)
   is
   begin
      if From_The_Ring.Concealed.The_Back > To_The_Ring.The_Size then
         Alogs.Log
           (Log_ID  => "E0226DEA590B15A0",
            Message => "Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;

      elsif From_The_Ring.Concealed.The_Back = 0 then
         To_The_Ring.Concealed.The_Top  := 0;
         To_The_Ring.Concealed.The_Back := 0;
         To_The_Ring.Concealed.The_Mark := 0;
      else
         To_The_Ring.The_Items (1 .. From_The_Ring.Concealed.The_Back) :=
           From_The_Ring.The_Items (1 .. From_The_Ring.Concealed.The_Back);
         To_The_Ring.Concealed.The_Top := From_The_Ring.Concealed.The_Top;
         To_The_Ring.Concealed.The_Back := From_The_Ring.Concealed.The_Back;
         To_The_Ring.Concealed.The_Mark := From_The_Ring.Concealed.The_Mark;
      end if;

      Booch_Status := OK;
   end Copy;

   procedure Clear (The_Ring : in out Ring) is
   begin
      The_Ring.Concealed.The_Top  := 0;
      The_Ring.Concealed.The_Back := 0;
      The_Ring.Concealed.The_Mark := 0;
   end Clear;

   procedure Insert
     (The_Item     : in     Item;
      In_The_Ring  : in out Ring;
      Booch_Status :    out Locus.Insert)
   is
   begin
      if In_The_Ring.Concealed.The_Back = In_The_Ring.The_Size then
         Alogs.Log
           (Log_ID  => "3A6FE56A5F9E48A8",
            Message => "Exception_Overflow: Insert failed due to full ring");
         Booch_Status := Exception_Overflow;
         return;
      elsif In_The_Ring.Concealed.The_Back = 0 then
         In_The_Ring.Concealed.The_Top  := 1;
         In_The_Ring.Concealed.The_Back := 1;
         In_The_Ring.Concealed.The_Mark := 1;
         In_The_Ring.The_Items (1)      := The_Item;
      else
         In_The_Ring.The_Items
           ((In_The_Ring.Concealed.The_Top + 1) ..
                (In_The_Ring.Concealed.The_Back + 1))          :=
           In_The_Ring.The_Items
             (In_The_Ring.Concealed.The_Top .. In_The_Ring.Concealed.The_Back);
         In_The_Ring.The_Items (In_The_Ring.Concealed.The_Top) := The_Item;
         In_The_Ring.Concealed.The_Back := In_The_Ring.Concealed.The_Back + 1;
         if In_The_Ring.Concealed.The_Mark >= In_The_Ring.Concealed.The_Top
         then
            In_The_Ring.Concealed.The_Mark :=
              In_The_Ring.Concealed.The_Mark + 1;
         end if;
      end if;

      Booch_Status := OK;
   end Insert;

   procedure Pop
     (The_Ring     : in out Ring;
      Booch_Status :    out Locus.Pop)
   is
   begin
      if The_Ring.Concealed.The_Back = 0 then
         Alogs.Log
           (Log_ID  => "C7B73016257FF434",
            Message => "Exception_Underflow: Pop failed");
         Booch_Status := Exception_Underflow;
         return;

      elsif The_Ring.Concealed.The_Back = 1 then
         The_Ring.Concealed.The_Top  := 0;
         The_Ring.Concealed.The_Back := 0;
         The_Ring.Concealed.The_Mark := 0;
      else
         The_Ring.The_Items
           (The_Ring.Concealed.The_Top .. (The_Ring.Concealed.The_Back - 1)) :=
           The_Ring.The_Items
             ((The_Ring.Concealed.The_Top + 1) .. The_Ring.Concealed.The_Back);
         The_Ring.Concealed.The_Back := The_Ring.Concealed.The_Back - 1;
         if The_Ring.Concealed.The_Top > The_Ring.Concealed.The_Back then
            if The_Ring.Concealed.The_Top = The_Ring.Concealed.The_Mark then
               The_Ring.Concealed.The_Mark := 1;
            end if;
            The_Ring.Concealed.The_Top := 1;
         else
            if The_Ring.Concealed.The_Mark > The_Ring.Concealed.The_Top then
               The_Ring.Concealed.The_Mark := The_Ring.Concealed.The_Mark - 1;
            end if;
         end if;
      end if;

      Booch_Status := OK;
   end Pop;

   procedure Rotate
     (The_Ring         : in out Ring;
      In_The_Direction : in     Direction;
      Booch_Status     :    out Locus.Rotate)
   is
   begin
      if The_Ring.Concealed.The_Back = 0 then
         Alogs.Log
           (Log_ID  => "B99958D1C2DE80DD",
            Message => "Rotate_Error: Rotate failed as the ring is empty");
         Booch_Status := Rotate_Error;
         return;

      elsif In_The_Direction = Forward then
         The_Ring.Concealed.The_Top := The_Ring.Concealed.The_Top + 1;
         if The_Ring.Concealed.The_Top > The_Ring.Concealed.The_Back then
            The_Ring.Concealed.The_Top := 1;
         end if;
      else
         The_Ring.Concealed.The_Top := The_Ring.Concealed.The_Top - 1;
         if The_Ring.Concealed.The_Top = 0 then
            The_Ring.Concealed.The_Top := The_Ring.Concealed.The_Back;
         end if;
      end if;

      Booch_Status := OK;
   end Rotate;

   procedure Mark (The_Ring : in out Ring) is
   begin
      The_Ring.Concealed.The_Mark := The_Ring.Concealed.The_Top;
   end Mark;

   procedure Rotate_To_Mark (The_Ring : in out Ring) is
   begin
      The_Ring.Concealed.The_Top := The_Ring.Concealed.The_Mark;
   end Rotate_To_Mark;

   function Is_Equal
     (Left  : in Ring;
      Right : in Ring)
      return Boolean
   is
      Left_Index  : Natural := Left.Concealed.The_Top;
      Right_Index : Natural := Right.Concealed.The_Top;
   begin
      if Left.Concealed.The_Back /= Right.Concealed.The_Back then
         return False;
      elsif Left.The_Items (Left_Index) /= Right.The_Items (Right_Index) then
         return False;
      elsif (Left.Concealed.The_Mark = Left_Index)
        and then (Right.Concealed.The_Mark /= Right_Index)
      then
         return False;
      else
         Left_Index := Left_Index + 1;
         if Left_Index > Left.Concealed.The_Back then
            Left_Index := 1;
         end if;
         Right_Index := Right_Index + 1;
         if Right_Index > Right.Concealed.The_Back then
            Right_Index := 1;
         end if;
         while Left_Index /= Left.Concealed.The_Top loop
            if Left.The_Items (Left_Index) /= Right.The_Items (Right_Index)
            then
               return False;
            elsif (Left.Concealed.The_Mark = Left_Index)
              and then (Right.Concealed.The_Mark /= Right_Index)
            then
               return False;
            else
               Left_Index := Left_Index + 1;
               if Left_Index > Left.Concealed.The_Back then
                  Left_Index := 1;
               end if;
               Right_Index := Right_Index + 1;
               if Right_Index > Right.Concealed.The_Back then
                  Right_Index := 1;
               end if;
            end if;
         end loop;
         return (Right_Index = Right.Concealed.The_Top);
      end if;
   exception
      when Constraint_Error =>
         return (Left.Concealed.The_Top = Right.Concealed.The_Top);
   end Is_Equal;

   function Extent_Of
     (The_Ring : in Ring)
      return Natural
   is
   begin
      return The_Ring.Concealed.The_Back;
   end Extent_Of;

   function Is_Empty
     (The_Ring : in Ring)
      return Boolean
   is
   begin
      return (The_Ring.Concealed.The_Back = 0);
   end Is_Empty;

   procedure Top_Of
     (The_Ring     : in     Ring;
      Result       :    out Item;
      Booch_Status :    out Locus.Top_Of)
   is
   begin
      Result := The_Ring.The_Items (The_Ring.Concealed.The_Top);

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "1CDBE3159F743C04",
            Message => "Constraint_Error: Exception_Underflow: Top_Of failed");
         Booch_Status := Exception_Underflow;
         return;
   end Top_Of;

   function At_Mark
     (The_Ring : in Ring)
      return Boolean
   is
   begin
      return (The_Ring.Concealed.The_Top = The_Ring.Concealed.The_Mark);
   end At_Mark;

   procedure Iterate (Over_The_Ring : in Ring) is
      Continue : Boolean := True;
   begin
      for The_Iterator in
        Over_The_Ring.Concealed.The_Top .. Over_The_Ring.Concealed.The_Back
      loop
         Process (Over_The_Ring.The_Items (The_Iterator), Continue);
         exit when not Continue;
      end loop;
      if Continue then
         for The_Iterator in 1 .. Over_The_Ring.Concealed.The_Top - 1 loop
            Process (Over_The_Ring.The_Items (The_Iterator), Continue);
            exit when not Continue;
         end loop;
      end if;
   end Iterate;

end Booch_Light.Ring_Sequential_Bounded_Managed_Iterator_Exposed;

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
