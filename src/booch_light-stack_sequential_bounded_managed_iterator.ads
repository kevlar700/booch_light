--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
package Booch_Light.Stack_Sequential_Bounded_Managed_Iterator is

   package Locus is

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Exception_Overflow | OK;

      subtype Push is Status_Code with
          Static_Predicate => Push in Exception_Overflow | OK;

      subtype Pop is Status_Code with
          Static_Predicate => Pop in Exception_Underflow | OK;

      subtype Top_Of is Status_Code with
          Static_Predicate => Top_Of in Exception_Underflow | OK;

   end Locus;

   type Stack (The_Size : Positive) is limited private;

   procedure Copy
     (From_The_Stack :        Stack;
      To_The_Stack   : in out Stack;
      Booch_Status   :    out Locus.Copy);

   procedure Clear (The_Stack : in out Stack);
   procedure Push
     (The_Item     :        Item;
      On_The_Stack : in out Stack;
      Booch_Status :    out Locus.Push);

   procedure Pop
     (The_Stack    : in out Stack;
      Booch_Status :    out Locus.Pop);

   procedure Top_Of
     (The_Stack    :     Stack;
      The_Top      : out Item;
      Booch_Status : out Locus.Top_Of);

   function Is_Equal
     (Left  : Stack;
      Right : Stack)
      return Boolean;

   function Depth_Of
     (The_Stack : Stack)
      return Natural;

   function Is_Empty
     (The_Stack : Stack)
      return Boolean;

   generic
      with procedure Process
        (The_Item :     Item;
         Continue : out Boolean);

   procedure Iterate (Over_The_Stack : Stack);

private
   type Items is array (Positive range <>) of Item;
   type Stack (The_Size : Positive) is record
      The_Top   : Natural := 0;
      The_Items : Items (1 .. The_Size);
   end record;

end Booch_Light.Stack_Sequential_Bounded_Managed_Iterator;

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
