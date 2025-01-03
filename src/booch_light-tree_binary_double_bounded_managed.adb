--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Tree_Binary_Double_Bounded_Managed is

   type Node is record
      Previous      : Tree;
      The_Item      : Item;
      Left_Subtree  : Tree;
      Right_Subtree : Tree;
   end record;

   Heap : array (Positive range 1 .. The_Size) of Node;

   Free_List : Tree;

   procedure Free (The_Tree : in out Tree) is
      Temporary_Node : Tree;
   begin
      if The_Tree.The_Head /= 0 then
         Free (Heap (The_Tree.The_Head).Left_Subtree);
         Free (Heap (The_Tree.The_Head).Right_Subtree);
         Heap (The_Tree.The_Head).Previous      := Null_Tree;
         Heap (The_Tree.The_Head).Left_Subtree  := Free_List;
         Heap (The_Tree.The_Head).Right_Subtree := Null_Tree;
         Free_List                              := The_Tree;
         The_Tree                               := Null_Tree;
      end if;
   end Free;

   function New_Item return Tree is
      Temporary_Node : Tree;
   begin
      if Free_List.The_Head = 0 then
         raise Storage_Error;
      else
         Temporary_Node                              := Free_List;
         Free_List := Heap (Free_List.The_Head).Left_Subtree;
         Heap (Temporary_Node.The_Head).Left_Subtree := Null_Tree;
         return Temporary_Node;
      end if;
   end New_Item;

   procedure Copy
     (From_The_Tree : in     Tree;
      To_The_Tree   : in out Tree;
      Booch_Status  :    out Locus.Copy)
   is
   begin
      Free (To_The_Tree);
      if From_The_Tree = Null_Tree then
         To_The_Tree := Null_Tree;
      else
         To_The_Tree                          := New_Item;
         Heap (To_The_Tree.The_Head).The_Item :=
           Heap (From_The_Tree.The_Head).The_Item;

         Copy
           (From_The_Tree => Heap (From_The_Tree.The_Head).Left_Subtree,
            To_The_Tree   => Heap (To_The_Tree.The_Head).Left_Subtree,
            Booch_Status  => Booch_Status);

         case Booch_Status is
            when Exception_Overflow =>
               Alterable_Log.Log
                 (Log_ID  => "1B340F5ED5302515",
                  Message => "Exception_Overflow: Copy failed");
               return;

            when OK =>
               null;

         end case;

         if Heap (To_The_Tree.The_Head).Left_Subtree /= Null_Tree then
            Heap (Heap (To_The_Tree.The_Head).Left_Subtree.The_Head)
              .Previous :=
              To_The_Tree;
         end if;

         Copy
           (From_The_Tree => Heap (From_The_Tree.The_Head).Right_Subtree,
            To_The_Tree   => Heap (To_The_Tree.The_Head).Right_Subtree,
            Booch_Status  => Booch_Status);

         case Booch_Status is
            when Exception_Overflow =>
               Alterable_Log.Log
                 (Log_ID  => "EB7B7893E0D8CBBB",
                  Message => "Exception_Overflow: Copy failed");
               return;

            when OK =>
               null;

         end case;

         if Heap (To_The_Tree.The_Head).Right_Subtree /= Null_Tree then
            Heap (Heap (To_The_Tree.The_Head).Right_Subtree.The_Head)
              .Previous :=
              To_The_Tree;
         end if;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "24542F432ABFBA23",
            Message => "Storage_Error: Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;
   end Copy;

   procedure Clear (The_Tree : in out Tree) is
   begin
      Free (The_Tree);
   end Clear;

   procedure Construct
     (The_Item     : in     Item;
      And_The_Tree : in out Tree;
      On_The_Child : in     Child;
      Booch_Status :    out Locus.Construct)
   is
      Temporary_Node : Tree;
   begin
      if And_The_Tree = Null_Tree then
         And_The_Tree                          := New_Item;
         Heap (And_The_Tree.The_Head).The_Item := The_Item;
      elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
         Temporary_Node                          := New_Item;
         Heap (Temporary_Node.The_Head).The_Item := The_Item;
         Heap (And_The_Tree.The_Head).Previous   := Temporary_Node;
         if On_The_Child = Left then
            Heap (Temporary_Node.The_Head).Left_Subtree := And_The_Tree;
         else
            Heap (Temporary_Node.The_Head).Right_Subtree := And_The_Tree;
         end if;
         And_The_Tree := Temporary_Node;
      else
         Alterable_Log.Log
           (Log_ID  => "0C45A913C8B3B788",
            Message => "Not_At_Root: Set_Item failed");
         Booch_Status := Not_At_Root;
         return;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "A4EE89DF94BFE0BB",
            Message => "Storage_Error: Exception_Overflow: Set_Item failed");
         Booch_Status := Exception_Overflow;
         return;

   end Construct;

   procedure Set_Item
     (Of_The_Tree  : in out Tree;
      To_The_Item  : in     Item;
      Booch_Status :    out Locus.Set_Item)
   is
   begin
      Heap (Of_The_Tree.The_Head).The_Item := To_The_Item;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "A4EE89DF94BFE0BB",
            Message => "Constraint_Error: Tree_Is_Null: Set_Item failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Set_Item;

   procedure Swap_Child
     (The_Child    : in     Child;
      Of_The_Tree  : in out Tree;
      And_The_Tree : in out Tree;
      Booch_Status :    out Locus.Swap_Child)
   is
      Temporary_Node : Tree;
   begin
      if The_Child = Left then
         if And_The_Tree = Null_Tree then
            if Heap (Of_The_Tree.The_Head).Left_Subtree /= Null_Tree then
               Temporary_Node := Heap (Of_The_Tree.The_Head).Left_Subtree;
               Heap (Temporary_Node.The_Head).Previous  := Null_Tree;
               Heap (Of_The_Tree.The_Head).Left_Subtree := Null_Tree;
               And_The_Tree                             := Temporary_Node;
            end if;
         elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
            if Heap (Of_The_Tree.The_Head).Left_Subtree /= Null_Tree then
               Temporary_Node := Heap (Of_The_Tree.The_Head).Left_Subtree;
               Heap (Temporary_Node.The_Head).Previous  := Null_Tree;
               Heap (Of_The_Tree.The_Head).Left_Subtree := And_The_Tree;
               Heap (And_The_Tree.The_Head).Previous    := Of_The_Tree;
               And_The_Tree                             := Temporary_Node;
            else
               Heap (And_The_Tree.The_Head).Previous    := Of_The_Tree;
               Heap (Of_The_Tree.The_Head).Left_Subtree := And_The_Tree;
               And_The_Tree                             := Null_Tree;
            end if;
         else
            Alterable_Log.Log
              (Log_ID  => "A30177DD3C97B94C",
               Message => "Not_At_Root: Swap_Child failed");
            Booch_Status := Not_At_Root;
            return;
         end if;
      else
         if And_The_Tree = Null_Tree then
            if Heap (Of_The_Tree.The_Head).Right_Subtree /= Null_Tree then
               Temporary_Node := Heap (Of_The_Tree.The_Head).Right_Subtree;
               Heap (Temporary_Node.The_Head).Previous   := Null_Tree;
               Heap (Of_The_Tree.The_Head).Right_Subtree := Null_Tree;
               And_The_Tree                              := Temporary_Node;
            end if;
         elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
            if Heap (Of_The_Tree.The_Head).Right_Subtree /= Null_Tree then
               Temporary_Node := Heap (Of_The_Tree.The_Head).Right_Subtree;
               Heap (Temporary_Node.The_Head).Previous   := Null_Tree;
               Heap (Of_The_Tree.The_Head).Right_Subtree := And_The_Tree;
               Heap (And_The_Tree.The_Head).Previous     := Of_The_Tree;
               And_The_Tree                              := Temporary_Node;
            else
               Heap (And_The_Tree.The_Head).Previous     := Of_The_Tree;
               Heap (Of_The_Tree.The_Head).Right_Subtree := And_The_Tree;
               And_The_Tree                              := Null_Tree;
            end if;
         else
            Alterable_Log.Log
              (Log_ID  => "6F9BC575D65996B9",
               Message => "Not_At_Root: Swap_Child failed");
            Booch_Status := Not_At_Root;
            return;
         end if;
      end if;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "6F9BC575D65996B9",
            Message => "Constraint_Error: Tree_Is_Null: Swap_Child failed");
         Booch_Status := Tree_Is_Null;
         return;
   end Swap_Child;

   function Is_Equal
     (Left  : in Tree;
      Right : in Tree)
      return Boolean
   is
   begin
      if Heap (Left.The_Head).The_Item /= Heap (Right.The_Head).The_Item then
         return False;
      else
         return
           (Is_Equal
              (Heap (Left.The_Head).Left_Subtree,
               Heap (Right.The_Head).Left_Subtree)
            and then Is_Equal
              (Heap (Left.The_Head).Right_Subtree,
               Heap (Right.The_Head).Right_Subtree));
      end if;
   exception
      when Constraint_Error =>
         return (Left = Null_Tree) and then (Right = Null_Tree);
   end Is_Equal;

   function Is_Null
     (The_Tree : in Tree)
      return Boolean
   is
   begin
      return (The_Tree = Null_Tree);
   end Is_Null;

   procedure Item_Of
     (The_Tree     : in     Tree;
      Result       :    out Item;
      Booch_Status :    out Locus.Item_Of)
   is
   begin
      Result       := Heap (The_Tree.The_Head).The_Item;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "BA9F780B9EC3317A",
            Message => "Constraint_Error: Tree_Is_Null: Item_Of failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Item_Of;

   procedure Child_Of
     (The_Tree     : in     Tree;
      The_Child    : in     Child;
      Result       :    out Tree;
      Booch_Status :    out Locus.Child_Of)
   is
   begin
      if The_Child = Left then
         Result := Heap (The_Tree.The_Head).Left_Subtree;
      else
         Result := Heap (The_Tree.The_Head).Right_Subtree;
      end if;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "18DBC643F84EC9E6",
            Message => "Constraint_Error: Tree_Is_Null: Child_Of failed");
         Booch_Status := Tree_Is_Null;
         return;
   end Child_Of;

   procedure Parent_Of
     (The_Tree     : in     Tree;
      Result       :    out Tree;
      Booch_Status :    out Locus.Parent_Of)
   is
   begin
      Result       := Heap (The_Tree.The_Head).Previous;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "BB9AB575F21BFF4D",
            Message => "Constraint_Error: Tree_Is_Null: Parent_Of failed");
         Booch_Status := Tree_Is_Null;
         return;
   end Parent_Of;

begin
   Free_List.The_Head := 1;
   for Index in 1 .. (The_Size - 1) loop
      Heap (Index).Left_Subtree := Tree'(The_Head => (Index + 1));
   end loop;
   Heap (The_Size).Left_Subtree := Null_Tree;

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
