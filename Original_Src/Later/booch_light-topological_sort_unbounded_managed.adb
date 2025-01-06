--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Set_Simple_Sequential_Unbounded_Managed_Iterator;
with
  Booch_Light
    .Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator;

package body Booch_Light.Topological_Sort_Unbounded_Managed is

   package Node_Set is new Set_Simple_Sequential_Unbounded_Managed_Iterator
     (Positive);

   package Node_Queue is new Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator
     (Positive);

   procedure Sort (The_Graph : in Graph) is

      type Node is record
         The_Vertex           : Vertex;
         Number_Of_References : Natural := 0;
         The_Successors       : Node_Set.Set;
      end record;

      Node_Table : array (1 .. Number_Of_Vertices_In (The_Graph)) of Node;
      Last_Node           : Natural := 0;
      Current_Node        : Positive;
      Successor_Node      : Positive;
      Process_Queue       : Node_Queue.Queue;
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
        (The_Node : in     Positive;
         Continue :    out Boolean)
      is
      begin
         Node_Table (The_Node).Number_Of_References :=
           Node_Table (The_Node).Number_Of_References - 1;
         if Node_Table (The_Node).Number_Of_References = 0 then
            Node_Queue.Add
              (The_Node,
               To_The_Queue => Process_Queue);
         end if;
         Continue := True;
      end Process_Successor;

      procedure Traverse_Successors is new Node_Set.Iterate
        (Process_Successor);

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
               Node_Set.Add
                 (Successor_Node,
                  To_The_Set => Node_Table (Current_Node).The_Successors);
            end if;
            Get_Next (The_Vertex_Iterator);
         end loop;
         Get_Next (The_Graph_Iterator);
      end loop;
      for Index in Node_Table'Range loop
         if Node_Table (Index).Number_Of_References = 0 then
            Node_Queue.Add
              (Index,
               To_The_Queue => Process_Queue);
         end if;
      end loop;
      while not Node_Queue.Is_Empty (Process_Queue) loop
         Process_Acyclic
           (Node_Table (Node_Queue.Front_Of (Process_Queue)).The_Vertex,
            Continue);
         exit when not Continue;
         Traverse_Successors
           (Node_Table (Node_Queue.Front_Of (Process_Queue)).The_Successors);
         Node_Queue.Pop (Process_Queue);
      end loop;
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

end Booch_Light.Topological_Sort_Unbounded_Managed;

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
