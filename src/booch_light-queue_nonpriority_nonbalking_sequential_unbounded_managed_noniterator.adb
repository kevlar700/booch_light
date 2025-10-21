--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Storage_Manager_Sequential;
with Booch_Light.Alogs;

package body Booch_Light
  .Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator is

   type Node is record
      The_Item : Item;
      Next     : Structure;
   end record;

   procedure Free (The_Node : in out Node) is
   begin
      null;
   end Free;

   procedure Set_Next
     (The_Node : in out Node;
      To_Next  : in     Structure)
   is
   begin
      The_Node.Next := To_Next;
   end Set_Next;

   function Next_Of
     (The_Node : in Node)
      return Structure
   is
   begin
      return The_Node.Next;
   end Next_Of;

   package Node_Manager is new Storage_Manager_Sequential
     (Item        => Node,
      Pointer     => Structure,
      Free        => Free,
      Set_Pointer => Set_Next,
      Pointer_Of  => Next_Of);

   procedure Copy
     (From_The_Queue : in     Queue;
      To_The_Queue   : in out Queue;
      Booch_Status   :    out Locus.Copy)
   is
      From_Index : Structure := From_The_Queue.The_Front;
      To_Index   : Structure;
   begin
      Node_Manager.Free (To_The_Queue.The_Front);
      To_The_Queue.The_Back := null;
      if From_The_Queue.The_Front /= null then
         To_The_Queue.The_Front          := Node_Manager.New_Item;
         To_The_Queue.The_Back           := To_The_Queue.The_Front;
         To_The_Queue.The_Front.The_Item := From_Index.The_Item;
         To_Index                        := To_The_Queue.The_Front;
         From_Index                      := From_Index.Next;
         while From_Index /= null loop
            To_Index.Next          := Node_Manager.New_Item;
            To_Index.Next.The_Item := From_Index.The_Item;
            To_Index               := To_Index.Next;
            From_Index             := From_Index.Next;
            To_The_Queue.The_Back  := To_Index;
         end loop;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alogs.Log
           (Log_ID  => "9DA6C3C44B6543BB",
            Message => "Exception_Overflow: Copy failed");
         return;

   end Copy;

   procedure Clear (The_Queue : in out Queue) is
   begin
      Node_Manager.Free (The_Queue.The_Front);
      The_Queue.The_Back := null;
   end Clear;

   procedure Add
     (The_Item     : in     Item;
      To_The_Queue : in out Queue;
      Booch_Status :    out Locus.Add)
   is
   begin
      if To_The_Queue.The_Front = null then
         To_The_Queue.The_Front          := Node_Manager.New_Item;
         To_The_Queue.The_Front.The_Item := The_Item;
         To_The_Queue.The_Back           := To_The_Queue.The_Front;
      else
         To_The_Queue.The_Back.Next          := Node_Manager.New_Item;
         To_The_Queue.The_Back.Next.The_Item := The_Item;
         To_The_Queue.The_Back               := To_The_Queue.The_Back.Next;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Booch_Status := Exception_Overflow;
         Alogs.Log
           (Log_ID  => "0E33AC90999650F5",
            Message => "Exception_Overflow: Add failed");

   end Add;

   procedure Pop
     (The_Queue    : in out Queue;
      Booch_Status :    out Locus.Pop)
   is
      Temporary_Node : Structure;
   begin
      Temporary_Node      := The_Queue.The_Front;
      The_Queue.The_Front := The_Queue.The_Front.Next;
      Temporary_Node.Next := null;
      Node_Manager.Free (Temporary_Node);
      if The_Queue.The_Front = null then
         The_Queue.The_Back := null;
      end if;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := Exception_Underflow;
         Alogs.Log
           (Log_ID  => "15612B9B74348C2D",
            Message => "Exception_Underflow: Pop failed");
         return;

   end Pop;

   function Is_Equal
     (Left  : in Queue;
      Right : in Queue)
      return Boolean
   is
      Left_Index  : Structure := Left.The_Front;
      Right_Index : Structure := Right.The_Front;
   begin
      while Left_Index /= null loop
         if Left_Index.The_Item /= Right_Index.The_Item then
            return False;
         else
            Left_Index  := Left_Index.Next;
            Right_Index := Right_Index.Next;
         end if;
      end loop;
      return (Right_Index = null);
   exception
      when Constraint_Error =>
         Alogs.Log
           (Log_ID  => "A2B9AD5091648740",
            Message => "Exception_Overflow: Add failed");
         return False;
   end Is_Equal;

   function Length_Of
     (The_Queue : in Queue)
      return Natural
   is
      Count : Natural   := 0;
      Index : Structure := The_Queue.The_Front;
   begin
      while Index /= null loop
         Count := Count + 1;
         Index := Index.Next;
      end loop;
      return Count;
   end Length_Of;

   function Is_Empty
     (The_Queue : in Queue)
      return Boolean
   is
   begin
      return (The_Queue.The_Front = null);
   end Is_Empty;

   procedure Front_Of
     (The_Queue    : in     Queue;
      Result       :    out Item;
      Booch_Status :    out Locus.Front_Of)
   is
   begin
      Result       := The_Queue.The_Front.The_Item;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := Exception_Underflow;
         Alogs.Log
           (Log_ID  => "DF394D35304D4052",
            Message => "Exception_Underflow: Front_Of failed");
         return;

   end Front_Of;

end Booch_Light
  .Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator;

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
