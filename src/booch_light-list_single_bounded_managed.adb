--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.List_Single_Bounded_Managed is

   type Node is record
      The_Item : Item;
      Next     : List;
   end record;

   Heap : array (Positive range 1 .. The_Size) of Node;

   Free_List : List;

   procedure Free (The_List : in out List) is
      Temporary_Node : List;
   begin
      while The_List /= Null_List loop
         Temporary_Node                      := The_List;
         The_List                            := Heap (The_List.The_Head).Next;
         Heap (Temporary_Node.The_Head).Next := Free_List;
         Free_List                           := Temporary_Node;
      end loop;
   end Free;

   procedure New_Item
     (The_Item     : out List;
      Booch_Status : out Locus.New_Item)
   is
      Temporary_Node : List;
   begin
      if Free_List = Null_List then
         Booch_Status := No_Storage_Available;
         Alterable_Log.Log
           (Log_ID  => "95FAF582E22FF7C2",
            Message => "No_Storage_Available: New_Item failed");
         return;
      else
         Temporary_Node                      := Free_List;
         Free_List                           := Heap (Free_List.The_Head).Next;
         Heap (Temporary_Node.The_Head).Next := Null_List;
         The_Item                            := Temporary_Node;
      end if;

      Booch_Status := OK;

   end New_Item;

   procedure Copy
     (From_The_List : in     List;
      To_The_List   : in out List;
      Booch_Status  :    out Locus.Copy)
   is
      From_Index      : List := From_The_List;
      To_Index        : List;
      New_List        : List;
      New_Item_Status : Locus.New_Item;
   begin
      Free (To_The_List);
      if From_The_List /= Null_List then
         New_Item
           (The_Item     => New_List,
            Booch_Status => New_Item_Status);

         case New_Item_Status is
            when No_Storage_Available =>
               Booch_Status := New_Item_Status;
               Alterable_Log.Log
                 (Log_ID  => "7CBFE8DF93C34F17",
                  Message => "No_Storage_Available: Copy failed");
               return;

            when OK =>
               null;
         end case;

         To_The_List                          := New_List;
         Heap (To_The_List.The_Head).The_Item :=
           Heap (From_Index.The_Head).The_Item;
         To_Index                             := To_The_List;
         From_Index := Heap (From_Index.The_Head).Next;
         while From_Index /= Null_List loop
            New_Item
              (The_Item     => New_List,
               Booch_Status => New_Item_Status);

            case New_Item_Status is
               when No_Storage_Available =>
                  Booch_Status := New_Item_Status;
                  Alterable_Log.Log
                    (Log_ID  => "9D6AF7BE2839F959",
                     Message => "No_Storage_Available: Copy failed");
                  return;

               when OK =>
                  null;
            end case;

            Heap (To_Index.The_Head).Next     := New_List;
            To_Index                          := Heap (To_Index.The_Head).Next;
            Heap (To_Index.The_Head).The_Item :=
              Heap (From_Index.The_Head).The_Item;
            From_Index := Heap (From_Index.The_Head).Next;
         end loop;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alterable_Log.Log
           (Log_ID  => "C206C4FC169A7F33",
            Message => "Exception_Overflow: Copy failed");
         return;
   end Copy;

   procedure Clear (The_List : in out List) is
   begin
      Free (The_List);
   end Clear;

   procedure Construct
     (The_Item     : in     Item;
      And_The_List : in out List;
      Booch_Status :    out Locus.Construct)
   is
      Temporary_Node  : List;
      New_Item_Status : Locus.New_Item;
   begin

      New_Item
        (The_Item     => Temporary_Node,
         Booch_Status => New_Item_Status);

      case New_Item_Status is
         when No_Storage_Available =>
            Booch_Status := New_Item_Status;
            Alterable_Log.Log
              (Log_ID  => "F0D233B082F2F188",
               Message => "No_Storage_Available: Construct failed");
            return;

         when OK =>
            null;
      end case;

      Heap (Temporary_Node.The_Head).The_Item := The_Item;
      Heap (Temporary_Node.The_Head).Next     := And_The_List;
      And_The_List                            := Temporary_Node;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alterable_Log.Log
           (Log_ID  => "B56965EBEE4FA56F",
            Message => "Exception_Overflow: Construct failed");
         return;
   end Construct;

   procedure Set_Head
     (Of_The_List  : in out List;
      To_The_Item  : in     Item;
      Booch_Status :    out Locus.Set_Head)
   is
   begin
      Heap (Of_The_List.The_Head).The_Item := To_The_Item;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := List_Is_Null;
         Alterable_Log.Log
           (Log_ID  => "7442F77D8E77B8F7",
            Message => "List_Is_Null: Set_Head failed");
         return;
   end Set_Head;

   procedure Swap_Tail
     (Of_The_List  : in out List;
      And_The_List : in out List;
      Booch_Status :    out Locus.Swap_Tail)
   is
      Temporary_Node : List;
   begin

      Temporary_Node                   := Heap (Of_The_List.The_Head).Next;
      Heap (Of_The_List.The_Head).Next := And_The_List;
      And_The_List                     := Temporary_Node;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := List_Is_Null;
         Alterable_Log.Log
           (Log_ID  => "9713C8E3C100008E",
            Message => "List_Is_Null: Swap_Tail failed");
         return;

   end Swap_Tail;

   function Is_Equal
     (Left  : in List;
      Right : in List)
      return Boolean
   is
      Left_Index  : List := Left;
      Right_Index : List := Right;
   begin

      while Left_Index /= Null_List loop
         if Heap (Left_Index.The_Head).The_Item /=
           Heap (Right_Index.The_Head).The_Item
         then
            return False;
         end if;
         Left_Index  := Heap (Left_Index.The_Head).Next;
         Right_Index := Heap (Right_Index.The_Head).Next;
      end loop;

      return (Right_Index = Null_List);

   exception
      when Constraint_Error =>
         return False;
   end Is_Equal;

   function Length_Of
     (The_List : in List)
      return Natural
   is
      Count : Natural := 0;
      Index : List    := The_List;
   begin
      while Index /= Null_List loop
         Count := Count + 1;
         Index := Heap (Index.The_Head).Next;
      end loop;
      return Count;
   end Length_Of;

   function Is_Null
     (The_List : in List)
      return Boolean
   is
   begin
      return (The_List = Null_List);
   end Is_Null;

   procedure Head_Of
     (The_List     : in     List;
      The_Item     :    out Item;
      Booch_Status :    out Locus.Head_Of)
   is
   begin
      The_Item     := Heap (The_List.The_Head).The_Item;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := List_Is_Null;
         Booch_Status := List_Is_Null;
         Alterable_Log.Log
           (Log_ID  => "5DB77EC0850A1464",
            Message => "Head_Of: Head_Of failed");
         return;

   end Head_Of;

   procedure Tail_Of
     (The_List     : in     List;
      The_Tail     :    out List;
      Booch_Status :    out Locus.Tail_Of)
   is
   begin
      The_Tail     := Heap (The_List.The_Head).Next;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := List_Is_Null;
         Alterable_Log.Log
           (Log_ID  => "3BE6BB963081D031",
            Message => "List_Is_Null: Tail_Of failed");
         return;

   end Tail_Of;

begin
   Free_List.The_Head := 1;
   for Index in 1 .. (The_Size - 1) loop
      Heap (Index).Next := List'(The_Head => (Index + 1));
   end loop;
   Heap (The_Size).Next := Null_List;

end Booch_Light.List_Single_Bounded_Managed;

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
