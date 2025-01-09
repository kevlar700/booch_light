--                       Original Booch Components
--
--                            (Ada 83 version)
--                               
--                     Copyright (C) 2000 Grady Booch
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but 
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU General Public License.  
--
-- The book _SOFTWARE_COMPONENTS_WITH_Ada__Structures,_Tools,_and_Subsystems_
-- ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage 
-- of this software.
--
-- The Ada 83 version of the components is the exact version described in
-- the book mentioned above.  The Ada 95 elaboration version differs only in
-- that each component unit is a child of a root package named "Booch" and
-- each package declaration includes the most appropriate elaboration control
-- pragma.  In addition, the Ada 95 child iteration version eliminates the
-- distinct iterator and noniterator forms for components that had them and
-- instead makes the associated "Iterate" procedures available as children of
-- those components.  More enhanced versions may be produced in the future.
--
-- The Original Booch Components are actively maintained and enhanced
-- by Vincent Marciante and Samuel T. Harris and may be found at the 
-- AdaPower web site (http://www.adapower.com) provided by David Botton.

with Set_Simple_Sequential_Unbounded_Managed_Iterator,
     Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator;
package body Topological_Sort_Unbounded_Managed is

    package Node_Set is
       new Set_Simple_Sequential_Unbounded_Managed_Iterator (Positive);

    package Node_Queue is
       new Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator
              (Positive);

    procedure Sort (The_Graph : in Graph) is

        type Node is
            record
                The_Vertex : Vertex;
                Number_Of_References : Natural := 0;
                The_Successors : Node_Set.Set;
            end record;

        Node_Table : array (1 .. Number_Of_Vertices_In (The_Graph)) of Node;
        Last_Node : Natural := 0;
        Current_Node : Positive;
        Successor_Node : Positive;
        Process_Queue : Node_Queue.Queue;
        The_Graph_Iterator : Iterator;
        The_Vertex_Iterator : Iterator;
        Continue : Boolean;

        procedure Add (The_Vertex : in Vertex; The_Node : out Positive) is
        begin
            for Index in 1 .. Last_Node loop
                if The_Vertex = Node_Table (Index).The_Vertex then
                    The_Node := Index;
                    return;
                end if;
            end loop;
            Last_Node := Last_Node + 1;
            Node_Table (Last_Node).The_Vertex := The_Vertex;
            The_Node := Last_Node;
        end Add;

        procedure Process_Successor
                     (The_Node : in Positive; Continue : out Boolean) is
        begin
            Node_Table (The_Node).Number_Of_References :=
               Node_Table (The_Node).Number_Of_References - 1;
            if Node_Table (The_Node).Number_Of_References = 0 then
                Node_Queue.Add (The_Node, To_The_Queue => Process_Queue);
            end if;
            Continue := True;
        end Process_Successor;

        procedure Traverse_Successors is
           new Node_Set.Iterate (Process_Successor);

    begin
        Initialize_Vertices (The_Graph_Iterator, With_The_Graph => The_Graph);
        while not Is_Done (The_Graph_Iterator) loop
            Add (Value_Of (The_Graph_Iterator), Current_Node);
            Initialize_Arcs (The_Vertex_Iterator,
                             With_The_Vertex => Value_Of (The_Graph_Iterator));
            while not Is_Done (The_Vertex_Iterator) loop
                Add (Value_Of (The_Vertex_Iterator), Successor_Node);
                if not ((Current_Node = Successor_Node) and then
                        Ignore_Self_Loops) then
                    Node_Table (Successor_Node).Number_Of_References :=
                       Node_Table (Successor_Node).Number_Of_References + 1;
                    Node_Set.Add (Successor_Node,
                                  To_The_Set => Node_Table (Current_Node).
                                                   The_Successors);
                end if;
                Get_Next (The_Vertex_Iterator);
            end loop;
            Get_Next (The_Graph_Iterator);
        end loop;
        for Index in Node_Table'Range loop
            if Node_Table (Index).Number_Of_References = 0 then
                Node_Queue.Add (Index, To_The_Queue => Process_Queue);
            end if;
        end loop;
        while not Node_Queue.Is_Empty (Process_Queue) loop
            Process_Acyclic
               (Node_Table (Node_Queue.Front_Of (Process_Queue)).The_Vertex,
                Continue);
            exit when not Continue;
            Traverse_Successors
               (Node_Table (Node_Queue.Front_Of (Process_Queue)).
                The_Successors);
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

end Topological_Sort_Unbounded_Managed;
