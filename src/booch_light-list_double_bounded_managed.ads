--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
   The_Size : in Positive;
package Booch_Light.List_Double_Bounded_Managed is

   type List is private;

   Null_List : constant List;

   package Locus is

      subtype New_Item is Status_Code with
          Static_Predicate => New_Item in No_Storage_Available | OK;

      subtype Copy is Status_Code with
          Static_Predicate => Copy in New_Item | Exception_Overflow;

      subtype Head_Of is Status_Code with
          Static_Predicate => Head_Of in List_Is_Null | OK;

      subtype Tail_Of is Status_Code with
          Static_Predicate => Tail_Of in List_Is_Null | OK;

      subtype Predecessor_Of is Status_Code with
          Static_Predicate => Predecessor_Of in List_Is_Null | OK;

      subtype Set_Head is Status_Code with
          Static_Predicate => Set_Head in List_Is_Null | OK;

      subtype Construct is Status_Code with
          Static_Predicate =>
           Construct in New_Item | Exception_Overflow | Not_At_Head | OK;

      subtype Swap_Tail is Status_Code with
          Static_Predicate => Swap_Tail in List_Is_Null | Not_At_Head | OK;

   end Locus;

   procedure Copy
     (From_The_List : in     List;
      To_The_List   : in out List;
      Booch_Status  :    out Locus.Copy);

   procedure Clear (The_List : in out List);

   procedure Construct
     (The_Item     : in     Item;
      And_The_List : in out List;
      Booch_Status :    out Locus.Construct);

   procedure Set_Head
     (Of_The_List  : in out List;
      To_The_Item  : in     Item;
      Booch_Status :    out Locus.Set_Head);

   procedure Swap_Tail
     (Of_The_List  : in out List;
      And_The_List : in out List;
      Booch_Status :    out Locus.Swap_Tail);

   procedure Head_Of
     (The_List     : in     List;
      The_Item     :    out Item;
      Booch_Status :    out Locus.Head_Of);

   procedure Tail_Of
     (The_List     : in     List;
      The_Tail     :    out List;
      Booch_Status :    out Locus.Tail_Of);

   procedure Predecessor_Of
     (The_List        : in     List;
      The_Predecessor :    out List;
      Booch_Status    :    out Locus.Predecessor_Of);

   function Is_Equal
     (Left  : in List;
      Right : in List)
      return Boolean;

   function Length_Of
     (The_List : in List)
      return Natural;

   function Is_Null
     (The_List : in List)
      return Boolean;

private
   type List is record
      The_Head : Natural := 0;
   end record;
   Null_List : constant List := List'(The_Head => 0);

end Booch_Light.List_Double_Bounded_Managed;

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
