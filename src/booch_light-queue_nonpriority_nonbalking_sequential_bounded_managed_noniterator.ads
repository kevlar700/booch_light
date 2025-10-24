--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
package Booch_Light
  .Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator
is

   package Locus is

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Exception_Overflow | OK;

      subtype Add is Status_Code with
          Static_Predicate => Add in Exception_Overflow | OK;

      subtype Pop is Status_Code with
          Static_Predicate => Pop in Exception_Underflow | OK;

      subtype Front_Of is Status_Code with
          Static_Predicate => Front_Of in Exception_Underflow | OK;

   end Locus;

   type Queue (The_Size : Positive) is limited private;

   --  From_The_Queue and To_The_Queue may be different capacity arrays but
   --  will have the same accessible content and lengths after this operation.
   procedure Copy
     (From_The_Queue :        Queue;
      To_The_Queue   : in out Queue;
      Booch_Status   :    out Locus.Copy);

   procedure Clear (The_Queue : in out Queue);
   procedure Add
     (The_Item     :        Item;
      To_The_Queue : in out Queue;
      Booch_Status :    out Locus.Add);

   procedure Pop
     (The_Queue    : in out Queue;
      Booch_Status :    out Locus.Pop);

   procedure Front_Of
     (The_Queue    :     Queue;
      The_Front    : out Item;
      Booch_Status : out Locus.Front_Of);

   function Is_Equal
     (Left  : Queue;
      Right : Queue)
      return Boolean;

   function Length_Of
     (The_Queue : Queue)
      return Natural;

   function Is_Empty
     (The_Queue : Queue)
      return Boolean;

private
   type Items is array (Positive range <>) of Item;
   type Queue (The_Size : Positive) is record
      The_Back  : Natural := 0;
      The_Items : Items (1 .. The_Size);
   end record;

end Booch_Light
  .Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator;

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
--  DEALINGS IN THE SOFTWARE. DEALINGS IN THE SOFTWARE.
