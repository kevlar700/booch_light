--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light
  .Deque_Priority_Balking_Sequential_Bounded_Managed_Iterator is

   procedure Copy
     (From_The_Deque : in     Deque;
      To_The_Deque   : in out Deque;
      Status_BC      :    out Locus.Copy)
   is
   begin
      if From_The_Deque.The_Back > To_The_Deque.The_Size then
         Status_BC := Exception_Overflow;
         return;
      elsif From_The_Deque.The_Back = 0 then
         To_The_Deque.The_Back := 0;
      else
         To_The_Deque.The_Items (1 .. From_The_Deque.The_Back) :=
           From_The_Deque.The_Items (1 .. From_The_Deque.The_Back);
         To_The_Deque.The_Back := From_The_Deque.The_Back;
      end if;

      Status_BC := OK;
   end Copy;

   procedure Clear (The_Deque : in out Deque) is
   begin
      The_Deque.The_Back := 0;
   end Clear;

   procedure Add
     (The_Item        : in     Item;
      To_The_Deque    : in out Deque;
      At_The_Location : in     Location;
      Status_BC       :    out Locus.Add)
   is
      Index : Natural := 1;
   begin
      if To_The_Deque.The_Back = 0 then
         To_The_Deque.The_Items (To_The_Deque.The_Back + 1) := The_Item;
         To_The_Deque.The_Back := To_The_Deque.The_Back + 1;
      else
         if At_The_Location = Front then
            while (Index <= To_The_Deque.The_Back)
              and then
              (Priority_Of (The_Item) <
               Priority_Of (To_The_Deque.The_Items (Index)))
            loop
               Index := Index + 1;
            end loop;
         else
            while (Index <= To_The_Deque.The_Back)
              and then
              (Priority_Of (The_Item) <=
               Priority_Of (To_The_Deque.The_Items (Index)))
            loop
               Index := Index + 1;
            end loop;
         end if;
         if Index > To_The_Deque.The_Back then
            To_The_Deque.The_Items (To_The_Deque.The_Back + 1) := The_Item;
            To_The_Deque.The_Back := To_The_Deque.The_Back + 1;
         else
            To_The_Deque.The_Items
              ((Index + 1) .. (To_The_Deque.The_Back + 1)) :=
              To_The_Deque.The_Items (Index .. To_The_Deque.The_Back);
            To_The_Deque.The_Items (Index)                 := The_Item;
            To_The_Deque.The_Back := To_The_Deque.The_Back + 1;
         end if;
      end if;

      Status_BC := OK;
   exception
      when Constraint_Error =>
         Status_BC := Exception_Overflow;
         return;
   end Add;

   procedure Pop
     (The_Deque       : in out Deque;
      At_The_Location : in     Location;
      Status_BC       :    out Locus.Pop)
   is
   begin
      if The_Deque.The_Back = 0 then
         Status_BC := Exception_Underflow;
         return;
      elsif The_Deque.The_Back = 1 then
         The_Deque.The_Back := 0;
      elsif At_The_Location = Front then
         The_Deque.The_Items (1 .. (The_Deque.The_Back - 1)) :=
           The_Deque.The_Items (2 .. The_Deque.The_Back);
         The_Deque.The_Back := The_Deque.The_Back - 1;
      else
         The_Deque.The_Back := The_Deque.The_Back - 1;
      end if;

      Status_BC := OK;
   end Pop;

   procedure Remove_Item
     (From_The_Deque  : in out Deque;
      At_The_Position : in     Positive;
      Status_BC       :    out Locus.Remove_Item)
   is
   begin
      if From_The_Deque.The_Back < At_The_Position then
         Status_BC := Position_Error;
         return;
      elsif From_The_Deque.The_Back /= At_The_Position then
         From_The_Deque.The_Items
           (At_The_Position .. (From_The_Deque.The_Back - 1)) :=
           From_The_Deque.The_Items
             ((At_The_Position + 1) .. From_The_Deque.The_Back);
      end if;
      From_The_Deque.The_Back := From_The_Deque.The_Back - 1;

      Status_BC := OK;
   end Remove_Item;

   function Is_Equal
     (Left  : in Deque;
      Right : in Deque)
      return Boolean
   is
   begin
      if Left.The_Back /= Right.The_Back then
         return False;
      else
         for Index in 1 .. Left.The_Back loop
            if Left.The_Items (Index) /= Right.The_Items (Index) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Length_Of
     (The_Deque : in Deque)
      return Natural
   is
   begin
      return The_Deque.The_Back;
   end Length_Of;

   function Is_Empty
     (The_Deque : in Deque)
      return Boolean
   is
   begin
      return (The_Deque.The_Back = 0);
   end Is_Empty;

   procedure Front_Of
     (The_Deque : in     Deque;
      The_Item  :    out Item;
      BC_Status :    out Status_Code)
   is
   begin
      if The_Deque.The_Back = 0 then
         BC_Status := Exception_Underflow;
      else
         The_Item  := The_Deque.The_Items (1);
         BC_Status := OK;
      end if;
   end Front_Of;

   procedure Back_Of
     (The_Deque : in     Deque;
      The_Item  :    out Item;
      BC_Status :    out Status_Code)
   is
   begin
      The_Item  := The_Deque.The_Items (The_Deque.The_Back);
      BC_Status := OK;

   exception
      when Constraint_Error =>
         BC_Status := Exception_Underflow;
   end Back_Of;

   function Position_Of
     (The_Item     : in Item;
      In_The_Deque : in Deque)
      return Natural
   is
   begin
      for Index in 1 .. In_The_Deque.The_Back loop
         if In_The_Deque.The_Items (Index) = The_Item then
            return Index;
         end if;
      end loop;
      return 0;
   end Position_Of;

   procedure Iterate (Over_The_Deque : in Deque) is
      Continue : Boolean;
   begin
      for The_Iterator in 1 .. Over_The_Deque.The_Back loop
         Process (Over_The_Deque.The_Items (The_Iterator), Continue);
         exit when not Continue;
      end loop;
   end Iterate;

end Booch_Light.Deque_Priority_Balking_Sequential_Bounded_Managed_Iterator;

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
