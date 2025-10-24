--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
   The_Size : Positive;

package Booch_Light.Tree_Binary_Double_Bounded_Managed is

   package Locus is

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Exception_Overflow | OK;

      subtype Construct is Status_Code with
          Static_Predicate =>
           Construct in Exception_Overflow | Not_At_Root | OK;

      subtype Set_Item is Status_Code with
          Static_Predicate => Set_Item in Tree_Is_Null | OK;

      subtype Swap_Child is Status_Code with
          Static_Predicate => Swap_Child in Not_At_Root | Tree_Is_Null | OK;

      subtype Item_Of is Status_Code with
          Static_Predicate => Item_Of in Tree_Is_Null | OK;

      subtype Child_Of is Status_Code with
          Static_Predicate => Child_Of in Tree_Is_Null | OK;

      subtype Parent_Of is Status_Code with
          Static_Predicate => Parent_Of in Tree_Is_Null | OK;

   end Locus;

   type Tree is private;

   type Child is
     (Left,
      Right);

   Null_Tree : constant Tree;

   procedure Copy
     (From_The_Tree :        Tree;
      To_The_Tree   : in out Tree;
      Booch_Status  :    out Locus.Copy);

   procedure Clear (The_Tree : in out Tree);

   procedure Construct
     (The_Item     :        Item;
      And_The_Tree : in out Tree;
      On_The_Child :        Child;
      Booch_Status :    out Locus.Construct);

   procedure Set_Item
     (Of_The_Tree  : in out Tree;
      To_The_Item  :        Item;
      Booch_Status :    out Locus.Set_Item);

   procedure Swap_Child
     (The_Child    :        Child;
      Of_The_Tree  : in out Tree;
      And_The_Tree : in out Tree;
      Booch_Status :    out Locus.Swap_Child);

   function Is_Equal
     (Left  : Tree;
      Right : Tree)
      return Boolean;

   function Is_Null
     (The_Tree : Tree)
      return Boolean;

   procedure Item_Of
     (The_Tree     :     Tree;
      Result       : out Item;
      Booch_Status : out Locus.Item_Of);

   procedure Child_Of
     (The_Tree     :     Tree;
      The_Child    :     Child;
      Result       : out Tree;
      Booch_Status : out Locus.Child_Of);

   procedure Parent_Of
     (The_Tree     :     Tree;
      Result       : out Tree;
      Booch_Status : out Locus.Parent_Of);

private

   type Tree is record
      The_Head : Natural := 0;
   end record;

   Null_Tree : constant Tree := Tree'(The_Head => 0);

end Booch_Light.Tree_Binary_Double_Bounded_Managed;

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
