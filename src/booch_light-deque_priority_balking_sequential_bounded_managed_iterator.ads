--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
   type Priority is limited private;
   with function Priority_Of
     (The_Item : in Item)
      return Priority;
   with function "<"
     (Left  : in Priority;
      Right : in Priority)
      return Boolean;
   with function "<="
     (Left  : in Priority;
      Right : in Priority)
      return Boolean;
package Booch_Light.Deque_Priority_Balking_Sequential_Bounded_Managed_Iterator
is

   package Locus is

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Exception_Overflow | OK;

      subtype Add is Status_Code with
          Static_Predicate => Add in Exception_Overflow | OK;

      subtype Pop is Status_Code with
          Static_Predicate => Pop in Exception_Underflow | OK;

      subtype Remove_Item is Status_Code with
          Static_Predicate => Remove_Item in Position_Error | OK;

      subtype Front_Of is Status_Code with
          Static_Predicate => Front_Of in Exception_Underflow | OK;

      subtype Back_Of is Status_Code with
          Static_Predicate => Back_Of in Exception_Underflow | OK;

   end Locus;

   type Deque (The_Size : Positive) is limited private;

   type Location is
     (Front,
      Back);

   procedure Copy
     (From_The_Deque : in     Deque;
      To_The_Deque   : in out Deque;
      Status_BC      :    out Locus.Copy);

   procedure Clear (The_Deque : in out Deque);

   --  XXX determine the condition of To_The_Deque when Status_BC is not OK.
   --  Confirm that it is safe to continue the use of To_The_Deque;
   procedure Add
     (The_Item        : in     Item;
      To_The_Deque    : in out Deque;
      At_The_Location : in     Location;
      Status_BC       :    out Locus.Add);

   procedure Pop
     (The_Deque       : in out Deque;
      At_The_Location : in     Location;
      Status_BC       :    out Locus.Pop);

   procedure Remove_Item
     (From_The_Deque  : in out Deque;
      At_The_Position : in     Positive;
      Status_BC       :    out Locus.Remove_Item);

   function Is_Equal
     (Left  : in Deque;
      Right : in Deque)
      return Boolean;

   function Length_Of
     (The_Deque : in Deque)
      return Natural;

   function Is_Empty
     (The_Deque : in Deque)
      return Boolean;

   procedure Front_Of
     (The_Deque : in     Deque;
      The_Item  :    out Item;
      BC_Status :    out Status_Code);

   procedure Back_Of
     (The_Deque : in     Deque;
      The_Item  :    out Item;
      BC_Status :    out Status_Code);

   function Position_Of
     (The_Item     : in Item;
      In_The_Deque : in Deque)
      return Natural;

   generic
      with procedure Process
        (The_Item : in     Item;
         Continue :    out Boolean);
   procedure Iterate (Over_The_Deque : in Deque);

private
   type Items is array (Positive range <>) of Item;
   type Deque (The_Size : Positive) is record
      The_Back  : Natural := 0;
      The_Items : Items (1 .. The_Size);
   end record;
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
