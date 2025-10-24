--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;
with Booch_Light.Storage_Manager_Sequential;

package body Booch_Light.Set_Simple_Sequential_Unbounded_Managed_Iterator is

   type Node is record
      The_Item : Item;
      Next     : Set;
   end record;

   procedure Free (The_Node : in out Node) is
   begin
      null;
   end Free;

   procedure Set_Next
     (The_Node : in out Node;
      To_Next  :        Set)
   is
   begin
      The_Node.Next := To_Next;
   end Set_Next;

   function Next_Of
     (The_Node : Node)
      return Set
   is
   begin
      return The_Node.Next;
   end Next_Of;

   package Node_Manager is new Booch_Light.Storage_Manager_Sequential
     (Item        => Node,
      Pointer     => Set,
      Free        => Free,
      Set_Pointer => Set_Next,
      Pointer_Of  => Next_Of);

   procedure Copy
     (From_The_Set :        Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Copy)
   is
      From_Index : Set := From_The_Set;
      To_Index   : Set;
   begin
      Node_Manager.Free (To_The_Set);
      if From_The_Set /= null
      then
         To_The_Set          := Node_Manager.New_Item;
         To_The_Set.The_Item := From_Index.The_Item;
         To_Index            := To_The_Set;
         From_Index          := From_Index.Next;
         while From_Index /= null loop
            To_Index.Next     := Node_Manager.New_Item;
            To_Index          := To_Index.Next;
            To_Index.The_Item := From_Index.The_Item;
            From_Index        := From_Index.Next;
         end loop;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alogs.Status_Exception
           (Log_ID  => "0DF12A5FDE5C2E42",
            Message => "Storage_Error: Copy failed");
         return;
   end Copy;

   procedure Clear (The_Set : in out Set) is
   begin
      Node_Manager.Free (The_Set);
   end Clear;

   procedure Add
     (The_Item     :        Item;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Add)
   is
      Temporary_Node : Set;
      Index          : Set := To_The_Set;
   begin
      while Index /= null loop
         if Index.The_Item = The_Item
         then
            Booch_Status := Item_Is_In_Set;
            return;
         else
            Index := Index.Next;
         end if;
      end loop;
      Temporary_Node          := Node_Manager.New_Item;
      Temporary_Node.The_Item := The_Item;
      Temporary_Node.Next     := To_The_Set;
      To_The_Set              := Temporary_Node;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alogs.Status_Exception
           (Log_ID  => "41C0B28479E3827A",
            Message => "Storage_Error: Unbounded set is likely full");
         Booch_Status := Exception_Overflow;
         return;

   end Add;

   procedure Remove
     (The_Item     :        Item;
      From_The_Set : in out Set;
      Booch_Status :    out Locus.Remove)
   is
      Previous : Set;
      Index    : Set := From_The_Set;
   begin
      while Index /= null loop
         if Index.The_Item = The_Item
         then
            if Previous = null
            then
               From_The_Set := From_The_Set.Next;
            else
               Previous.Next := Index.Next;
            end if;
            Index.Next := null;
            Node_Manager.Free (Index);
            Booch_Status := OK;
            return;
         else
            Previous := Index;
            Index    := Index.Next;
         end if;
      end loop;

      Alogs.Log
        (Log_ID  => "0F05EAAEC2B42437",
         Message => "Item_Is_Not_In_Set: Remove failed");
      Booch_Status := Item_Is_Not_In_Set;
   end Remove;

   procedure Union
     (Of_The_Set   :        Set;
      And_The_Set  :        Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Union)
   is
      From_Index     : Set := Of_The_Set;
      To_Index       : Set;
      To_Top         : Set;
      Temporary_Node : Set;
   begin
      Node_Manager.Free (To_The_Set);
      while From_Index /= null loop
         Temporary_Node          := Node_Manager.New_Item;
         Temporary_Node.The_Item := From_Index.The_Item;
         Temporary_Node.Next     := To_The_Set;
         To_The_Set              := Temporary_Node;
         From_Index              := From_Index.Next;
      end loop;
      From_Index := And_The_Set;
      To_Top     := To_The_Set;
      while From_Index /= null loop
         To_Index := To_Top;
         while To_Index /= null loop
            if From_Index.The_Item = To_Index.The_Item
            then
               exit;
            else
               To_Index := To_Index.Next;
            end if;
         end loop;
         if To_Index = null
         then
            Temporary_Node          := Node_Manager.New_Item;
            Temporary_Node.The_Item := From_Index.The_Item;
            Temporary_Node.Next     := To_The_Set;
            To_The_Set              := Temporary_Node;
         end if;
         From_Index := From_Index.Next;
      end loop;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alogs.Status_Exception
           (Log_ID  => "BE910133B1CF3D35",
            Message => "Storage_Error: Union of Sets failed");
         return;

   end Union;

   procedure Intersection
     (Of_The_Set   :        Set;
      And_The_Set  :        Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Intersection)
   is
      Of_Index       : Set := Of_The_Set;
      And_Index      : Set;
      Temporary_Node : Set;
   begin
      Node_Manager.Free (To_The_Set);
      while Of_Index /= null loop
         And_Index := And_The_Set;
         while And_Index /= null loop
            if Of_Index.The_Item = And_Index.The_Item
            then
               Temporary_Node          := Node_Manager.New_Item;
               Temporary_Node.The_Item := Of_Index.The_Item;
               Temporary_Node.Next     := To_The_Set;
               To_The_Set              := Temporary_Node;
               exit;
            else
               And_Index := And_Index.Next;
            end if;
         end loop;
         Of_Index := Of_Index.Next;
      end loop;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alogs.Status_Exception
           (Log_ID  => "E263024C578E73CE",
            Message => "Storage_Error: Intersection of Sets failed");
         return;

   end Intersection;

   procedure Difference
     (Of_The_Set   :        Set;
      And_The_Set  :        Set;
      To_The_Set   : in out Set;
      Booch_Status :    out Locus.Difference)
   is
      Of_Index       : Set := Of_The_Set;
      And_Index      : Set;
      Temporary_Node : Set;
   begin
      Node_Manager.Free (To_The_Set);
      while Of_Index /= null loop
         And_Index := And_The_Set;
         while And_Index /= null loop
            if Of_Index.The_Item = And_Index.The_Item
            then
               exit;
            else
               And_Index := And_Index.Next;
            end if;
         end loop;
         if And_Index = null
         then
            Temporary_Node          := Node_Manager.New_Item;
            Temporary_Node.The_Item := Of_Index.The_Item;
            Temporary_Node.Next     := To_The_Set;
            To_The_Set              := Temporary_Node;
         end if;
         Of_Index := Of_Index.Next;
      end loop;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alogs.Status_Exception
           (Log_ID  => "AAF51CFA75F4F059",
            Message => "Storage_Error: Difference of Sets failed");
         return;

   end Difference;

   function Is_Equal
     (Left  : Set;
      Right : Set)
      return Boolean
   is
      Left_Count  : Natural := 0;
      Right_Count : Natural := 0;
      Left_Index  : Set     := Left;
      Right_Index : Set;
   begin
      while Left_Index /= null loop
         Right_Index := Right;
         while Right_Index /= null loop
            if Left_Index.The_Item = Right_Index.The_Item
            then
               exit;
            else
               Right_Index := Right_Index.Next;
            end if;
         end loop;
         if Right_Index = null
         then
            return False;
         else
            Left_Count := Left_Count + 1;
            Left_Index := Left_Index.Next;
         end if;
      end loop;
      Right_Index := Right;
      while Right_Index /= null loop
         Right_Count := Right_Count + 1;
         Right_Index := Right_Index.Next;
      end loop;
      return (Left_Count = Right_Count);
   end Is_Equal;

   function Extent_Of
     (The_Set : Set)
      return Natural
   is
      Count : Natural := 0;
      Index : Set     := The_Set;
   begin
      while Index /= null loop
         Count := Count + 1;
         Index := Index.Next;
      end loop;
      return Count;
   end Extent_Of;

   function Is_Empty
     (The_Set : Set)
      return Boolean
   is
   begin
      return (The_Set = null);
   end Is_Empty;

   function Is_A_Member
     (The_Item   : Item;
      Of_The_Set : Set)
      return Boolean
   is
      Index : Set := Of_The_Set;
   begin
      while Index /= null loop
         if The_Item = Index.The_Item
         then
            return True;
         end if;
         Index := Index.Next;
      end loop;
      return False;
   end Is_A_Member;

   function Is_A_Subset
     (Left  : Set;
      Right : Set)
      return Boolean
   is
      Left_Index  : Set := Left;
      Right_Index : Set;
   begin
      while Left_Index /= null loop
         Right_Index := Right;
         while Right_Index /= null loop
            if Left_Index.The_Item = Right_Index.The_Item
            then
               exit;
            else
               Right_Index := Right_Index.Next;
            end if;
         end loop;
         if Right_Index = null
         then
            return False;
         else
            Left_Index := Left_Index.Next;
         end if;
      end loop;
      return True;
   end Is_A_Subset;

   function Is_A_Proper_Subset
     (Left  : Set;
      Right : Set)
      return Boolean
   is
      Left_Count  : Natural := 0;
      Right_Count : Natural := 0;
      Left_Index  : Set     := Left;
      Right_Index : Set;
   begin
      while Left_Index /= null loop
         Right_Index := Right;
         while Right_Index /= null loop
            if Left_Index.The_Item = Right_Index.The_Item
            then
               exit;
            else
               Right_Index := Right_Index.Next;
            end if;
         end loop;
         if Right_Index = null
         then
            return False;
         else
            Left_Count := Left_Count + 1;
            Left_Index := Left_Index.Next;
         end if;
      end loop;
      Right_Index := Right;
      while Right_Index /= null loop
         Right_Count := Right_Count + 1;
         Right_Index := Right_Index.Next;
      end loop;
      return (Left_Count < Right_Count);
   end Is_A_Proper_Subset;

   procedure Iterate (Over_The_Set : Set) is
      The_Iterator : Set := Over_The_Set;
      Continue     : Boolean;
   begin
      while The_Iterator /= null loop
         Process (The_Iterator.The_Item, Continue);
         exit when not Continue;
         The_Iterator := The_Iterator.Next;
      end loop;
   end Iterate;

end Booch_Light.Set_Simple_Sequential_Unbounded_Managed_Iterator;

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
