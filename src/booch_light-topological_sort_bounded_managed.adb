--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Set_Simple_Sequential_Bounded_Managed_Iterator;
with
  Booch_Light
    .Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator;
with Booch_Light.Alogs;

package body Booch_Light.Topological_Sort_Bounded_Managed is

   package Node_Set is new Booch_Light
     .Set_Simple_Sequential_Bounded_Managed_Iterator
     (Item => Positive);

   package Node_Queue is new Booch_Light
     .Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator
     (Positive);

   procedure Sort
     (The_Graph    : in     Graph;
      Booch_Status :    out Locus.Sort)
   is
      Total_Vertices : constant Natural := Number_Of_Vertices_In (The_Graph);

      type Node is record
         The_Vertex           : Vertex;
         Number_Of_References : Natural := 0;
         The_Successors       : Node_Set.Set (Total_Vertices);
      end record;

      Node_Table          : array (1 .. Total_Vertices) of Node;
      Last_Node           : Natural := 0;
      Current_Node        : Positive;
      Successor_Node      : Positive;
      Process_Queue       : Node_Queue.Queue (Total_Vertices);
      The_Graph_Iterator  : Iterator;
      The_Vertex_Iterator : Iterator;
      Continue            : Boolean;

      procedure Add
        (The_Vertex : in     Vertex;
         The_Node   :    out Positive)
      is
      begin
         for Index in 1 .. Last_Node loop
            if The_Vertex = Node_Table (Index).The_Vertex then
               The_Node := Index;
               return;
            end if;
         end loop;
         Last_Node                         := Last_Node + 1;
         Node_Table (Last_Node).The_Vertex := The_Vertex;
         The_Node                          := Last_Node;
      end Add;

      procedure Process_Successor
        (The_Node            : in     Positive;
         Continue_Nested     :    out Boolean;
         Booch_Status_Nested :    out Locus.Process_Successor)
      is
         Add_Status : Node_Queue.Locus.Add;
      begin
         Node_Table (The_Node).Number_Of_References :=
           Node_Table (The_Node).Number_Of_References - 1;
         if Node_Table (The_Node).Number_Of_References = 0 then
            Node_Queue.Add
              (The_Node,
               To_The_Queue => Process_Queue,
               Booch_Status => Add_Status);
            case Add_Status is
               when Exception_Overflow =>
                  Alogs.Log
                    (Log_ID  => "BC34E6DA54D6A14C",
                     Message =>
                       "Overflow when adding a node to the Process Queue");

                  Booch_Status_Nested := Add_Status;
                  Continue_Nested     := False;
                  return;

               when OK =>
                  null;
            end case;
         end if;

         Booch_Status_Nested := OK;
         Continue_Nested     := True;
      end Process_Successor;

      procedure Traverse_Successors is new Node_Set.Iterate_With_Status
        (Process     => Process_Successor,
         Status_Item => Locus.Process_Successor);

   begin
      Initialize_Vertices
        (The_Graph_Iterator,
         With_The_Graph => The_Graph);
      while not Is_Done (The_Graph_Iterator) loop
         Add (Value_Of (The_Graph_Iterator), Current_Node);
         Initialize_Arcs
           (The_Vertex_Iterator,
            With_The_Vertex => Value_Of (The_Graph_Iterator));
         while not Is_Done (The_Vertex_Iterator) loop
            Add (Value_Of (The_Vertex_Iterator), Successor_Node);
            if not ((Current_Node = Successor_Node) and then Ignore_Self_Loops)
            then
               Node_Table (Successor_Node).Number_Of_References :=
                 Node_Table (Successor_Node).Number_Of_References + 1;

               declare
                  Add_Status : Node_Set.Locus.Add;
               begin
                  Node_Set.Add
                    (The_Item     => Successor_Node,
                     To_The_Set   => Node_Table (Current_Node).The_Successors,
                     Booch_Status => Add_Status);

                  case Add_Status is
                     when Item_Is_In_Set | Exception_Overflow =>
                        Alogs.Log
                          (Log_ID  => "920143CF25D33375",
                           Message =>
                             "Error whilst adding a Successsor node to the " &
                             "Set");

                        Booch_Status := Add_Status;
                        Continue     := False;
                        return;

                     when OK =>
                        null;
                  end case;
               end;

            end if;
            Get_Next (The_Vertex_Iterator);
         end loop;
         Get_Next (The_Graph_Iterator);
      end loop;
      for Index in Node_Table'Range loop
         if Node_Table (Index).Number_Of_References = 0 then
            declare
               Add_Status : Node_Queue.Locus.Add;
            begin

               Node_Queue.Add
                 (The_Item     => Index,
                  To_The_Queue => Process_Queue,
                  Booch_Status => Add_Status);

               case Add_Status is
                  when Exception_Overflow =>
                     Alogs.Log
                       (Log_ID  => "57A823692FAFC559",
                        Message =>
                          "Overflow when adding an Index to the " &
                          "Process_Queue");

                     Booch_Status := Add_Status;
                     Continue     := False;
                     return;

                  when OK =>
                     null;
               end case;

            end;
         end if;
      end loop;

      declare
         The_Front    : Positive;
         Queue_Status : Node_Queue.Locus.Front_Of;
         Set_Status   : Locus.Process_Successor;
      begin
         while not Node_Queue.Is_Empty (Process_Queue) loop
            Node_Queue.Front_Of
              (The_Queue    => Process_Queue,
               The_Front    => The_Front,
               Booch_Status => Queue_Status);

            case Queue_Status is
               when Exception_Underflow =>
                  Alogs.Log
                    (Log_ID  => "5E6FF87D141BFD68",
                     Message =>
                       "Exception_Underflow during Topological sorting");
                  Booch_Status := Queue_Status;
                  Continue     := False;
                  return;

               when OK =>
                  null;
            end case;

            Process_Acyclic (Node_Table (The_Front).The_Vertex, Continue);
            exit when not Continue;

            Node_Queue.Front_Of
              (The_Queue    => Process_Queue,
               The_Front    => The_Front,
               Booch_Status => Queue_Status);

            case Queue_Status is
               when Exception_Underflow =>
                  Alogs.Log
                    (Log_ID  => "316B0B5B1B98470D",
                     Message =>
                       "Exception_Underflow during Topological sorting");
                  Booch_Status := Queue_Status;
                  Continue     := False;
                  return;

               when OK =>
                  null;
            end case;

            Traverse_Successors
              (Over_The_Set => Node_Table (The_Front).The_Successors,
               Booch_Status => Set_Status);

            case Set_Status is
               when Exception_Overflow =>
                  Alogs.Log
                    (Log_ID  => "0D9E83379205E791",
                     Message =>
                       "Exception_Overflow whilst traversing The_Successors " &
                       "in the set");
                  Booch_Status := Set_Status;
                  Continue     := False;
                  return;

               when OK =>
                  null;
            end case;

            Node_Queue.Pop
              (The_Queue    => Process_Queue,
               Booch_Status => Queue_Status);

            case Queue_Status is
               when Exception_Underflow =>
                  Alogs.Log
                    (Log_ID  => "6322AFB958E49D7B",
                     Message =>
                       "Exception_Underflow whilst popping the Node_Queue");
                  Booch_Status := Queue_Status;
                  Continue     := False;
                  return;

               when OK =>
                  null;
            end case;

         end loop;
      end;

      if not Continue then
         Node_Queue.Clear (Process_Queue);
         Continue := True;
      end if;
      for Index in Node_Table'Range loop
         if Node_Table (Index).Number_Of_References /= 0 then
            if Continue and then Process_Cycles then
               Process_Cyclic (Node_Table (Index).The_Vertex, Continue);
            end if;
            Node_Set.Clear (Node_Table (Index).The_Successors);
         end if;
      end loop;
   end Sort;

end Booch_Light.Topological_Sort_Bounded_Managed;

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
--  DEALINGS IN THE SOFTWARE. DEALINGS IN THE SOFTWARE.
