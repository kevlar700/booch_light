--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light.Tree_Arbitrary_Single_Bounded_Managed is

   package Children is new Map_Simple_Noncached_Sequential_Bounded_Managed_Iterator
     (Domain  => Positive,
      Ranges  => Tree,
      Hash_Of => Hash_Of);

   type Node is record
      The_Item     : Item;
      The_Children : Children.Map (Maximum_Children);
      Next         : Tree;
   end record;

   Heap : array (Positive range 1 .. The_Size) of Node;

   Free_List : Tree;

   function Hash_Of
     (The_Child : Positive)
      return Positive
   is
   begin
      return The_Child;
   end Hash_Of;

   procedure Free (The_Tree : in out Tree) is
      procedure Clear_Child
        (The_Domain :     Positive;
         The_Range  :     Tree;
         Continue   : out Boolean)
      is
         --  Avoid the warning as here just to match the Generic interface for
         --  Clear_Children
         pragma Unreferenced (The_Domain);

         Temporary_Node : Tree := The_Range;
      begin
         Free (Temporary_Node);
         Continue := True;
      end Clear_Child;
      procedure Clear_Children is new Children.Iterate (Clear_Child);
   begin
      if The_Tree /= Null_Tree
      then
         Clear_Children (Heap (The_Tree.The_Head).The_Children);
         Children.Clear (Heap (The_Tree.The_Head).The_Children);
         Heap (The_Tree.The_Head).Next := Free_List;
         Free_List                     := The_Tree;
         The_Tree                      := Null_Tree;
      end if;
   end Free;

   function New_Item return Tree is
      Temporary_Node : Tree;
   begin
      if Free_List = Null_Tree
      then
         raise Storage_Error;
      else
         Temporary_Node                      := Free_List;
         Free_List                           := Heap (Free_List.The_Head).Next;
         Heap (Temporary_Node.The_Head).Next := Null_Tree;
         return Temporary_Node;
      end if;
   end New_Item;

   procedure Copy
     (From_The_Tree :        Tree;
      To_The_Tree   : in out Tree;
      Booch_Status  :    out Locus.Copy)
   is
      procedure Copy_Child
        (The_Domain          :     Positive;
         The_Range           :     Tree;
         Continue_Nested     : out Boolean;
         Booch_Status_Nested : out Locus.Copy_Child)
      is
         Temporary_Node : Tree;
         Tmp_Status     : Children.Locus.Bind;
      begin
         Copy
           (The_Range,
            To_The_Tree  => Temporary_Node,
            Booch_Status => Booch_Status);

         case Booch_Status is
            when Multiple_Binding =>
               Alogs.Log
                 (Log_ID  => "C0C8154DB53EF80F",
                  Message => "Multiple_Binding: Copy_Child failed");
               Booch_Status_Nested := Multiple_Binding;
               Continue_Nested     := False;
               return;

            when Exception_Overflow =>
               Alogs.Log
                 (Log_ID  => "9F49BC98A31AF4E9",
                  Message => "Exception_Overflow: Copy_Child failed");
               Booch_Status_Nested := Exception_Overflow;
               Continue_Nested     := False;
               return;

            when OK =>
               null;

         end case;

         Children.Bind
           (The_Domain    => The_Domain,
            And_The_Range => Temporary_Node,
            In_The_Map    => Heap (To_The_Tree.The_Head).The_Children,
            Booch_Status  => Tmp_Status);

         case Tmp_Status is
            when Exception_Overflow =>
               Alogs.Log
                 (Log_ID  => "B098377AEF46F66B",
                  Message => "Exception_Overflow: Copy_Child failed");
               Booch_Status_Nested := Exception_Overflow;
               Continue_Nested     := False;
               return;

            when Multiple_Binding =>
               Alogs.Log
                 (Log_ID  => "386F654B3C939C6E",
                  Message => "Multiple_Binding: Copy_Child failed");
               Booch_Status_Nested := Multiple_Binding;
               Continue_Nested     := False;
               return;

            when OK =>
               null;

         end case;

         Booch_Status_Nested := OK;
         Continue_Nested     := True;
      end Copy_Child;

      procedure Copy_Children is new Children.Iterate_With_Status
        (Process     => Copy_Child,
         Status_Item => Locus.Copy_Child);
   begin
      Free (To_The_Tree);
      if From_The_Tree /= Null_Tree
      then
         To_The_Tree                          := New_Item;
         Heap (To_The_Tree.The_Head).The_Item :=
           Heap (From_The_Tree.The_Head).The_Item;
         declare
            Tmp_Status : Locus.Copy_Child;
         begin
            Copy_Children
              (Over_The_Map => Heap (From_The_Tree.The_Head).The_Children,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Multiple_Binding =>
                  Alogs.Log
                    (Log_ID  => "159E2A3FB7AA1B09",
                     Message =>
                       "Multiple_Binding: Copy (Copy_Children) failed");
                  Booch_Status := Multiple_Binding;
                  return;

               when Exception_Overflow =>
                  Alogs.Log
                    (Log_ID  => "FA9C07F2A6522EA7",
                     Message =>
                       "Exception_Overflow: Copy (Copy_Children) failed");
                  Booch_Status := Exception_Overflow;
                  return;

               when OK =>
                  null;

            end case;
         end;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alogs.Status_Exception
           (Log_ID  => "565AC41CBCCC30B8",
            Message => "Storage_Error: Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;

   end Copy;

   procedure Clear (The_Tree : in out Tree) is
   begin
      Free (The_Tree);
   end Clear;

   procedure Construct
     (The_Item           :        Item;
      And_The_Tree       : in out Tree;
      Number_Of_Children :        Natural;
      On_The_Child       :        Natural;
      Booch_Status       :    out Locus.Construct)
   is
      Temporary_Node : Tree;
      Map_Status     : Children.Locus.Bind;
   begin
      if Number_Of_Children = 0
      then
         if And_The_Tree = Null_Tree
         then
            And_The_Tree                          := New_Item;
            Heap (And_The_Tree.The_Head).The_Item := The_Item;
            Booch_Status                          := OK;
            return;
         else
            Alogs.Log
              (Log_ID  => "5ADEC896EA953100",
               Message => "Tree_Is_Not_Null: Construct failed");
            Booch_Status := Tree_Is_Not_Null;
            return;
         end if;

      elsif On_The_Child > Number_Of_Children
      then
         Alogs.Log
           (Log_ID  => "3BD71D02EF94B5DB",
            Message => "Child_Error: Construct failed");
         Booch_Status := Child_Error;
         return;
      else
         Temporary_Node                          := New_Item;
         Heap (Temporary_Node.The_Head).The_Item := The_Item;
         for Index in 1 .. Number_Of_Children loop
            if Index = On_The_Child
            then
               Children.Bind
                 (The_Domain    => Index,
                  And_The_Range => And_The_Tree,
                  In_The_Map    => Heap (Temporary_Node.The_Head).The_Children,
                  Booch_Status  => Map_Status);
            else
               Children.Bind
                 (The_Domain    => Index,
                  And_The_Range => Null_Tree,
                  In_The_Map    => Heap (Temporary_Node.The_Head).The_Children,
                  Booch_Status  => Map_Status);
            end if;

            case Map_Status is
               when Exception_Overflow =>
                  Alogs.Log
                    (Log_ID  => "15A8162AC90887DA",
                     Message => "Exception_Overflow: Construct failed");
                  Booch_Status := Map_Status;
                  return;

               when Multiple_Binding =>
                  Alogs.Log
                    (Log_ID  => "C42428F2CC404508",
                     Message => "Multiple_Binding: Construct failed");
                  Booch_Status := Map_Status;
                  return;

               when OK =>
                  null;
            end case;

         end loop;
         And_The_Tree := Temporary_Node;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alogs.Status_Exception
           (Log_ID  => "2AC884F5EBCD9830",
            Message => "Storage_Error: Construct failed");
         Booch_Status := Exception_Overflow;
         return;

   end Construct;

   procedure Set_Item
     (Of_The_Tree  : in out Tree;
      To_The_Item  :        Item;
      Booch_Status :    out Locus.Set_Item)
   is
   begin
      Heap (Of_The_Tree.The_Head).The_Item := To_The_Item;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "1DABA23D7E43EC8D",
            Message => "Constraint_Error: Tree_Is_Null: Set_Item failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Set_Item;

   procedure Swap_Child
     (The_Child    :        Positive;
      Of_The_Tree  : in out Tree;
      And_The_Tree : in out Tree;
      Booch_Status :    out Locus.Swap_Child)
   is
      Temporary_Node  : Tree;
      Status_Range_Of : Children.Locus.Range_Of;
      Status_Unbind   : Children.Locus.Unbind;
      Status_Bind     : Children.Locus.Bind;
   begin
      Children.Range_Of
        (The_Domain   => The_Child,
         In_The_Map   => Heap (Of_The_Tree.The_Head).The_Children,
         Result       => Temporary_Node,
         Booch_Status => Status_Range_Of);

      case Status_Range_Of is
         when Domain_Is_Not_Bound =>
            Alogs.Log
              (Log_ID  => "AF79F4BE8CEFD1D8",
               Message =>
                 "Domain_Is_Not_Bound: Child_Error Swap_Child failed");
            Booch_Status := Child_Error;
            return;

         when OK =>
            null;
      end case;

      Children.Unbind
        (The_Domain   => The_Child,
         In_The_Map   => Heap (Of_The_Tree.The_Head).The_Children,
         Booch_Status => Status_Unbind);

      case Status_Unbind is
         when Domain_Is_Not_Bound =>
            Alogs.Log
              (Log_ID  => "3DB08BD04D764F8A",
               Message =>
                 "Domain_Is_Not_Bound: Child_Error: Swap_Child failed");
            Booch_Status := Child_Error;
            return;

         when OK =>
            null;
      end case;

      Children.Bind
        (The_Domain    => The_Child,
         And_The_Range => And_The_Tree,
         In_The_Map    => Heap (Of_The_Tree.The_Head).The_Children,
         Booch_Status  => Status_Bind);

      case Status_Bind is
         when Exception_Overflow =>
            Alogs.Log
              (Log_ID  => "BE41D2170DA6BBF7",
               Message => "Exception_Overflow: Swap_Child failed");
            Booch_Status := Status_Bind;
            return;

         when Multiple_Binding =>
            Alogs.Log
              (Log_ID  => "6FE49FAD32040D71",
               Message => "Multiple_Binding: Swap_Child failed");
            Booch_Status := Status_Bind;
            return;

         when OK =>
            null;
      end case;

      And_The_Tree := Temporary_Node;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "2A963FCB56155963",
            Message => "Constraint_Error: Tree_Is_Null: Swap_Child failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Swap_Child;

   --  Todo: nested and recursive!; consider replacing this pattern throughout.
   procedure Is_Equal
     (Left         :     Tree;
      Right        :     Tree;
      Result       : out Boolean;
      Booch_Status : out Locus.Is_Equal)
   is
      Trees_Are_Equal : Boolean := True;
      procedure Check_Child_Equality
        (The_Domain    :     Positive;
         The_Range     :     Tree;
         Continue      : out Boolean;
         Nested_Status : out Locus.Is_Equal)
      is
         Range_Of_Children : Tree;
         Status_Range_Of   : Children.Locus.Range_Of;
         Equal             : Boolean;
      begin
         Children.Range_Of
           (The_Domain,
            In_The_Map   => Heap (Right.The_Head).The_Children,
            Result       => Range_Of_Children,
            Booch_Status => Status_Range_Of);

         case Status_Range_Of is
            when Domain_Is_Not_Bound =>
               Alogs.Log
                 (Log_ID  => "0481375987C547C9",
                  Message =>
                    "Domain_Is_Not_Bound: Check_Child_Equality failed");
               Nested_Status := Status_Range_Of;
               Continue      := False;
               return;

            when OK =>
               null;
         end case;

         Is_Equal
           (Left         => The_Range,
            Right        => Range_Of_Children,
            Result       => Equal,
            Booch_Status => Nested_Status);

         if not Equal
         then
            Trees_Are_Equal := False;
            Continue        := False;
         else
            Continue := True;
         end if;

      end Check_Child_Equality;

      procedure Check_Equality is new Children.Iterate_With_Status
        (Process     => Check_Child_Equality,
         Status_Item => Locus.Is_Equal);

   begin
      if Heap (Left.The_Head).The_Item /= Heap (Right.The_Head).The_Item
      then
         Result       := False;
         Booch_Status := OK;
         return;
      else
         if Children.Extent_Of (Heap (Left.The_Head).The_Children) /=
           Children.Extent_Of (Heap (Right.The_Head).The_Children)
         then
            Result       := False;
            Booch_Status := OK;
            return;
         else
            Check_Equality
              (Over_The_Map => Heap (Left.The_Head).The_Children,
               Booch_Status => Booch_Status);
            Result := Trees_Are_Equal;
            return;
         end if;
      end if;

   exception
      --  XXX check this makes sense, especially is the log on both being null
      --  a practical inefficiency or something to always be noted as strange
      when Constraint_Error =>
         Result := (Left = Null_Tree) and then (Right = Null_Tree);
         if Result
         then
            Alogs.Status_Exception
              (Log_ID  => "A38D0E21B8FA5DE0",
               Message => "Constraint_Error: Both were null and so Is_Equal");
            Booch_Status := OK;
         else
            Alogs.Status_Exception
              (Log_ID  => "6AD4740FA59B7900",
               Message => "Constraint_Error: Is_Equal failed");
            Booch_Status := Exception_Constraint_Error;
         end if;
         return;

   end Is_Equal;

   function Is_Null
     (The_Tree : Tree)
      return Boolean
   is
   begin
      return (The_Tree = Null_Tree);
   end Is_Null;

   procedure Item_Of
     (The_Tree     :     Tree;
      Result       : out Item;
      Booch_Status : out Locus.Item_Of)
   is
   begin
      Result       := Heap (The_Tree.The_Head).The_Item;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "0FA3C3FB26D6284B",
            Message => "Constraint_Error: Tree_Is_Null: Item_Of failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Item_Of;

   procedure Number_Of_Children_In
     (The_Tree     :     Tree;
      Result       : out Natural;
      Booch_Status : out Locus.Number_Of_Children_In)
   is
   begin
      Result := Children.Extent_Of (Heap (The_Tree.The_Head).The_Children);
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "DD2C9BC408E1CC9D",
            Message =>
              "Constraint_Error: Tree_Is_Null: Number_Of_Children_In failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Number_Of_Children_In;

   procedure Child_Of
     (The_Tree     :     Tree;
      The_Child    :     Positive;
      Result       : out Tree;
      Booch_Status : out Locus.Child_Of)
   is
      Status_Range_Of : Children.Locus.Range_Of;
   begin
      Children.Range_Of
        (The_Domain   => The_Child,
         In_The_Map   => Heap (The_Tree.The_Head).The_Children,
         Result       => Result,
         Booch_Status => Status_Range_Of);

      case Status_Range_Of is
         when Domain_Is_Not_Bound =>
            Booch_Status := Child_Error;
            Alogs.Log
              (Log_ID  => "E4E0BB86B5BEE838",
               Message => "Child_Error: Domain_Is_Not_Bound: Child_Of failed");

         when OK =>
            Booch_Status := OK;
      end case;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "6864A4ECFC5BEF51",
            Message => "Constraint_Error: Tree_Is_Null: Child_Of failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Child_Of;

begin
   Free_List.The_Head := 1;
   for Index in 1 .. (The_Size - 1) loop
      Heap (Index).Next := Tree'(The_Head => (Index + 1));
   end loop;
   Heap (The_Size).Next := Null_Tree;
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
