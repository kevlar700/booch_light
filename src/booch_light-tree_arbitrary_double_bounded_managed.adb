--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Tree_Arbitrary_Double_Bounded_Managed is

   package Children is new Map_Simple_Noncached_Sequential_Bounded_Managed_Iterator
     (Domain  => Positive,
      Ranges  => Tree,
      Hash_Of => Hash_Of);

   type Node is record
      Previous     : Tree;
      The_Item     : Item;
      The_Children : Children.Map (Maximum_Children);
      Next         : Tree;
   end record;

   Heap : array (Positive range 1 .. The_Size) of Node;

   Free_List : Tree;

   function Hash_Of
     (The_Child : in Positive)
      return Positive
   is
   begin
      return The_Child;
   end Hash_Of;

   procedure Free (The_Tree : in out Tree) is
      procedure Clear_Child
        (The_Domain : in     Positive;
         The_Range  : in     Tree;
         Continue   :    out Boolean)
      is
         Temporary_Node : Tree := The_Range;
      begin
         Free (Temporary_Node);
         Continue := True;
      end Clear_Child;
      procedure Clear_Children is new Children.Iterate (Clear_Child);
   begin
      if The_Tree /= Null_Tree then
         Clear_Children (Heap (The_Tree.The_Head).The_Children);
         Heap (The_Tree.The_Head).Previous := Null_Tree;
         Children.Clear (Heap (The_Tree.The_Head).The_Children);
         Heap (The_Tree.The_Head).Next := Free_List;
         Free_List                     := The_Tree;
         The_Tree                      := Null_Tree;
      end if;
   end Free;

   function New_Item return Tree is
      Temporary_Node : Tree;
   begin
      if Free_List = Null_Tree then
         raise Storage_Error;
      else
         Temporary_Node                      := Free_List;
         Free_List                           := Heap (Free_List.The_Head).Next;
         Heap (Temporary_Node.The_Head).Next := Null_Tree;
         return Temporary_Node;
      end if;
   end New_Item;

   procedure Copy
     (From_The_Tree : in     Tree;
      To_The_Tree   : in out Tree;
      Booch_Status  :    out Locus.Copy)
   is
      procedure Copy_Child
        (The_Domain          : in     Positive;
         The_Range           : in     Tree;
         Continue_Nested     :    out Boolean;
         Booch_Status_Nested :    out Locus.Copy_Child)
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
               Alterable_Log.Log
                 (Log_ID  => "82CDADCBF9AE6892",
                  Message => "Multiple_Binding: Copy_Child failed");
               Booch_Status_Nested := Multiple_Binding;
               Continue_Nested     := False;
               return;

            when Exception_Overflow =>
               Alterable_Log.Log
                 (Log_ID  => "873FC2A80AE082AE",
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
               Alterable_Log.Log
                 (Log_ID  => "02446CA72430A41C",
                  Message => "Exception_Overflow: Copy_Child failed");
               Booch_Status_Nested := Exception_Overflow;
               Continue_Nested     := False;
               return;

            when Multiple_Binding =>
               Alterable_Log.Log
                 (Log_ID  => "068E1C8EC676C1C1",
                  Message => "Multiple_Binding: Copy_Child failed");
               Booch_Status_Nested := Multiple_Binding;
               Continue_Nested     := False;
               return;

            when OK =>
               null;

         end case;

         if Temporary_Node /= Null_Tree then
            Heap (Temporary_Node.The_Head).Previous := To_The_Tree;
         end if;
         Booch_Status_Nested := OK;
         Continue_Nested     := True;
      end Copy_Child;

      procedure Copy_Children is new Children.Iterate_With_Status
        (Process     => Copy_Child,
         Status_Item => Locus.Copy_Child);
   begin
      Free (To_The_Tree);
      if From_The_Tree /= Null_Tree then
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
                  Alterable_Log.Log
                    (Log_ID  => "E4F6E037A869EFBD",
                     Message =>
                       "Multiple_Binding: Copy (Copy_Children) failed");
                  Booch_Status := Multiple_Binding;
                  return;

               when Exception_Overflow =>
                  Alterable_Log.Log
                    (Log_ID  => "7A79CDD61650FE41",
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
         Alterable_Log.Status_Exception
           (Log_ID  => "22DAC7C09D68E709",
            Message => "Storage_Error: Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;

   end Copy;

   procedure Clear (The_Tree : in out Tree) is
   begin
      Free (The_Tree);
   end Clear;

   procedure Construct
     (The_Item           : in     Item;
      And_The_Tree       : in out Tree;
      Number_Of_Children : in     Natural;
      On_The_Child       : in     Natural;
      Booch_Status       :    out Locus.Construct)
   is
      Temporary_Node : Tree;
      Map_Status     : Children.Locus.Bind;
   begin
      if Number_Of_Children = 0 then

         if And_The_Tree = Null_Tree then
            And_The_Tree                          := New_Item;
            Heap (And_The_Tree.The_Head).The_Item := The_Item;
            Booch_Status                          := OK;
            return;
         else
            Alterable_Log.Log
              (Log_ID  => "E2C8EEAB3D1A7F6B",
               Message => "Tree_Is_Not_Null: Construct failed");
            Booch_Status := Tree_Is_Not_Null;
            return;
         end if;

      elsif On_The_Child > Number_Of_Children then

         Alterable_Log.Log
           (Log_ID  => "0118EC363D44C2BF",
            Message => "Child_Error: Construct failed");
         Booch_Status := Child_Error;
         return;

      elsif And_The_Tree = Null_Tree then
         And_The_Tree                          := New_Item;
         Heap (And_The_Tree.The_Head).The_Item := The_Item;

         for Index in 1 .. Number_Of_Children loop
            Children.Bind
              (The_Domain    => Index,
               And_The_Range => Null_Tree,
               In_The_Map    => Heap (And_The_Tree.The_Head).The_Children,
               Booch_Status  => Map_Status);

            case Map_Status is
               when Exception_Overflow =>
                  Alterable_Log.Log
                    (Log_ID  => "931D7B90317FFC69",
                     Message => "Exception_Overflow: Construct failed");
                  Booch_Status := Map_Status;
                  return;

               when Multiple_Binding =>
                  Alterable_Log.Log
                    (Log_ID  => "AC98B972BAB7159F",
                     Message => "Multiple_Binding: Construct failed");
                  Booch_Status := Map_Status;
                  return;

               when OK =>
                  null;

            end case;
         end loop;

      elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
         Temporary_Node                          := New_Item;
         Heap (Temporary_Node.The_Head).The_Item := The_Item;
         for Index in 1 .. Number_Of_Children loop
            if Index = On_The_Child then
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
                  Alterable_Log.Log
                    (Log_ID  => "C864192D5D04CF92",
                     Message => "Exception_Overflow: Construct failed");
                  Booch_Status := Map_Status;
                  return;

               when Multiple_Binding =>
                  Alterable_Log.Log
                    (Log_ID  => "90E96CFDC792E3CA",
                     Message => "Multiple_Binding: Construct failed");
                  Booch_Status := Map_Status;
                  return;

               when OK =>
                  null;
            end case;

         end loop;

         Heap (And_The_Tree.The_Head).Previous := Temporary_Node;
         And_The_Tree                          := Temporary_Node;
         Booch_Status                          := OK;

      else
         Alterable_Log.Log
           (Log_ID  => "6AC3802F233BA4A9",
            Message => "Not_At_Root: Construct failed");
         Booch_Status := Not_At_Root;
         return;

      end if;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "268B906611D973F9",
            Message => "Storage_Error: Construct failed");
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

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "5B64B4A81C67FDEB",
            Message => "Constraint_Error: Tree_Is_Null: Set_Item failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Set_Item;

   procedure Swap_Child
     (The_Child    : in     Positive;
      Of_The_Tree  : in out Tree;
      And_The_Tree : in out Tree;
      Booch_Status :    out Locus.Swap_Child)
   is
      Temporary_Node  : Tree;
      Status_Range_Of : Children.Locus.Range_Of;
      Status_Unbind   : Children.Locus.Unbind;
      Status_Bind     : Children.Locus.Bind;
   begin
      if And_The_Tree = Null_Tree then
         Children.Range_Of
           (The_Domain   => The_Child,
            In_The_Map   => Heap (Of_The_Tree.The_Head).The_Children,
            Result       => Temporary_Node,
            Booch_Status => Status_Range_Of);

         case Status_Range_Of is
            when Domain_Is_Not_Bound =>
               Alterable_Log.Log
                 (Log_ID  => "1436DBC9F93B094D",
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
               Alterable_Log.Log
                 (Log_ID  => "FB67CE895E370AE9",
                  Message =>
                    "Domain_Is_Not_Bound: Child_Error: Swap_Child failed");
               Booch_Status := Child_Error;
               return;

            when OK =>
               null;
         end case;

         Children.Bind
           (The_Domain    => The_Child,
            And_The_Range => Null_Tree,
            In_The_Map    => Heap (Of_The_Tree.The_Head).The_Children,
            Booch_Status  => Status_Bind);

         case Status_Bind is
            when Exception_Overflow =>
               Alterable_Log.Log
                 (Log_ID  => "427FA41382A33C21",
                  Message => "Exception_Overflow: Swap_Child failed");
               Booch_Status := Status_Bind;
               return;

            when Multiple_Binding =>
               Alterable_Log.Log
                 (Log_ID  => "74F68C595C46E4C2",
                  Message => "Multiple_Binding: Swap_Child failed");
               Booch_Status := Status_Bind;
               return;

            when OK =>
               null;
         end case;

         if Temporary_Node /= Null_Tree then
            Heap (Temporary_Node.The_Head).Previous := Null_Tree;
         end if;
         And_The_Tree := Temporary_Node;

      elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
         Children.Range_Of
           (The_Domain   => The_Child,
            In_The_Map   => Heap (Of_The_Tree.The_Head).The_Children,
            Result       => Temporary_Node,
            Booch_Status => Status_Range_Of);

         case Status_Range_Of is
            when Domain_Is_Not_Bound =>
               Alterable_Log.Log
                 (Log_ID  => "FE52FECBBD5F53D1",
                  Message =>
                    "Domain_Is_Not_Bound: Child_Error: Swap_Child failed");
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
               Alterable_Log.Log
                 (Log_ID  => "EAD92607B03D9C94",
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
               Alterable_Log.Log
                 (Log_ID  => "427FA41382A33C21",
                  Message => "Exception_Overflow: Swap_Child failed");
               Booch_Status := Status_Bind;
               return;

            when Multiple_Binding =>
               Alterable_Log.Log
                 (Log_ID  => "74F68C595C46E4C2",
                  Message => "Multiple_Binding: Swap_Child failed");
               Booch_Status := Status_Bind;
               return;

            when OK =>
               null;
         end case;

         if Temporary_Node /= Null_Tree then
            Heap (Temporary_Node.The_Head).Previous := Null_Tree;
         end if;
         Heap (And_The_Tree.The_Head).Previous := Of_The_Tree;
         And_The_Tree                          := Temporary_Node;
         Booch_Status                          := OK;
      else
         Alterable_Log.Log
           (Log_ID  => "17408E4B439DACCD",
            Message => "Not_At_Root: Swap_Child failed");
         Booch_Status := Not_At_Root;
         return;
      end if;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "2A963FCB56155963",
            Message => "Constraint_Error: Tree_Is_Null: Swap_Child failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Swap_Child;

   --  Todo: nested and recursive!; consider replacing this pattern throughout.
   procedure Is_Equal
     (Left         : in     Tree;
      Right        : in     Tree;
      Result       :    out Boolean;
      Booch_Status :    out Locus.Is_Equal)
   is
      Trees_Are_Equal : Boolean := True;
      procedure Check_Child_Equality
        (The_Domain    : in     Positive;
         The_Range     : in     Tree;
         Continue      :    out Boolean;
         Nested_Status :    out Locus.Is_Equal)
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
               Alterable_Log.Log
                 (Log_ID  => "107C574B1DBBBDD2",
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

         if not Equal then
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
      if Heap (Left.The_Head).The_Item /= Heap (Right.The_Head).The_Item then
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
         if Result then
            Alterable_Log.Status_Exception
              (Log_ID  => "727DD72982316930",
               Message => "Constraint_Error: Both were null and so Is_Equal");
            Booch_Status := OK;
         else
            Alterable_Log.Status_Exception
              (Log_ID  => "430B9B1A8F2EEC00",
               Message => "Constraint_Error: Is_Equal failed");
            Booch_Status := Exception_Constraint_Error;
         end if;
         return;

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
           (Log_ID  => "F462AD557B775811",
            Message => "Constraint_Error: Tree_Is_Null: Item_Of failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Item_Of;

   procedure Number_Of_Children_In
     (The_Tree     : in     Tree;
      Result       :    out Natural;
      Booch_Status :    out Locus.Number_Of_Children_In)
   is
   begin
      Result := Children.Extent_Of (Heap (The_Tree.The_Head).The_Children);
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "6D4DA4A33DC834C7",
            Message =>
              "Constraint_Error: Tree_Is_Null: Number_Of_Children_In failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Number_Of_Children_In;

   procedure Child_Of
     (The_Tree     : in     Tree;
      The_Child    : in     Positive;
      Result       :    out Tree;
      Booch_Status :    out Locus.Child_Of)
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
            Alterable_Log.Log
              (Log_ID  => "E4E0BB86B5BEE838",
               Message => "Child_Error: Domain_Is_Not_Bound: Child_Of failed");

         when OK =>
            Booch_Status := OK;
      end case;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "6864A4ECFC5BEF51",
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
           (Log_ID  => "47DAA208002A571E",
            Message => "Constraint_Error: Tree_Is_Null: Parent_Of failed");
         Booch_Status := Tree_Is_Null;
         return;

   end Parent_Of;

begin
   Free_List.The_Head := 1;
   for Index in 1 .. (The_Size - 1) loop
      Heap (Index).Next := Tree'(The_Head => (Index + 1));
   end loop;
   Heap (The_Size).Next := Null_Tree;

end Booch_Light.Tree_Arbitrary_Double_Bounded_Managed;

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
