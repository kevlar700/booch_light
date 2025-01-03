--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Map_Simple_Noncached_Sequential_Bounded_Managed_Iterator;

generic
   type Item is private;
   The_Size : in Positive;
   Maximum_Children : in Positive;

package Booch_Light.Tree_Arbitrary_Single_Bounded_Managed is

   type Tree is private;

   Null_Tree : constant Tree;

   function Hash_Of
     (The_Child : in Positive)
      return Positive;

     -- The enums provided by these packages are centralised in booch_light.ads
     -- and therefore all compatible
   package Map is new Map_Simple_Noncached_Sequential_Bounded_Managed_Iterator
     (Domain  => Positive,
      Ranges  => Positive,
      Hash_Of => Hash_Of);

   package Locus is

      subtype Copy_Child is Status_Code with
          Static_Predicate =>
           Copy_Child in Multiple_Binding | Exception_Overflow | OK;

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Copy_Child | Exception_Overflow | OK;

      subtype Construct is Status_Code with
          Static_Predicate =>
           Construct in
             Map.Locus.Bind | Tree_Is_Not_Null | Child_Error
             | Exception_Overflow | OK;

      subtype Set_Item is Status_Code with
          Static_Predicate => Set_Item in Tree_Is_Null | OK;

      subtype Swap_Child is Status_Code with
          Static_Predicate =>
           Swap_Child in
             Map.Locus.Range_Of | Map.Locus.Unbind | Map.Locus.Bind
             | Tree_Is_Not_Null | Child_Error | Not_At_Root | OK;

      subtype Is_Equal is Status_Code with
          Static_Predicate => Is_Equal in Map.Locus.Range_Of | OK;

      subtype Item_Of is Status_Code with
          Static_Predicate => Item_Of in Tree_Is_Null | OK;

      subtype Number_Of_Children_In is Status_Code with
          Static_Predicate => Number_Of_Children_In in Tree_Is_Null | OK;

      subtype Child_Of is Status_Code with
          Static_Predicate =>
           Child_Of in Map.Locus.Range_Of | Tree_Is_Null | Child_Error | OK;

      subtype Parent_Of is Status_Code with
          Static_Predicate => Parent_Of in Tree_Is_Null | OK;

   end Locus;

   procedure Copy
     (From_The_Tree : in     Tree;
      To_The_Tree   : in out Tree;
      Booch_Status  :    out Locus.Copy);

   procedure Clear (The_Tree : in out Tree);

   procedure Construct
     (The_Item           : in     Item;
      And_The_Tree       : in out Tree;
      Number_Of_Children : in     Natural;
      On_The_Child       : in     Natural;
      Booch_Status       :    out Locus.Construct);

   procedure Set_Item
     (Of_The_Tree  : in out Tree;
      To_The_Item  : in     Item;
      Booch_Status :    out Locus.Set_Item);

   procedure Swap_Child
     (The_Child    : in     Positive;
      Of_The_Tree  : in out Tree;
      And_The_Tree : in out Tree;
      Booch_Status :    out Locus.Swap_Child);

   procedure Is_Equal
     (Left         : in     Tree;
      Right        : in     Tree;
      Result       :    out Boolean;
      Booch_Status :    out Locus.Is_Equal);

   function Is_Null
     (The_Tree : in Tree)
      return Boolean;
   procedure Item_Of
     (The_Tree     : in     Tree;
      Result       :    out Item;
      Booch_Status :    out Locus.Item_Of);

   procedure Number_Of_Children_In
     (The_Tree     : in     Tree;
      Result       :    out Natural;
      Booch_Status :    out Locus.Number_Of_Children_In);

   procedure Child_Of
     (The_Tree     : in     Tree;
      The_Child    : in     Positive;
      Result       :    out Tree;
      Booch_Status :    out Locus.Child_Of);

private
   type Tree is record
      The_Head : Natural := 0;
   end record;
   Null_Tree : constant Tree := Tree'(The_Head => 0);

end Booch_Light.Tree_Arbitrary_Single_Bounded_Managed;

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
