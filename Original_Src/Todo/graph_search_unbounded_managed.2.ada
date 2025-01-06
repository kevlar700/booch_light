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

with Set_Simple_Sequential_Unbounded_Managed_Noniterator,
     Stack_Sequential_Unbounded_Managed_Noniterator,
     Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator;
package body Graph_Search_Unbounded_Managed is

    package Vertex_Set is
       new Set_Simple_Sequential_Unbounded_Managed_Noniterator (Item => Vertex);

    package Vertex_Stack is
       new Stack_Sequential_Unbounded_Managed_Noniterator (Item => Vertex);

    package Vertex_Queue is
       new Queue_Nonpriority_Nonbalking_Sequential_Unbounded_Managed_Noniterator
              (Item => Vertex);

    procedure Traverse_Depth_First (From_The_Vertex : in Vertex) is
        Vertices_Visited : Vertex_Set.Set;
        Vertices_Ready : Vertex_Stack.Stack;
        Temporary_Vertex : Vertex;
        The_Iterator : Iterator;
        Continue : Boolean;
    begin
        Vertex_Set.Add (From_The_Vertex, To_The_Set => Vertices_Visited);
        Vertex_Stack.Push (From_The_Vertex, On_The_Stack => Vertices_Ready);
        while not Vertex_Stack.Is_Empty (Vertices_Ready) loop
            Temporary_Vertex := Vertex_Stack.Top_Of (Vertices_Ready);
            Vertex_Stack.Pop (Vertices_Ready);
            Process (Temporary_Vertex, Continue);
            exit when not Continue;
            Initialize (The_Iterator, With_The_Vertex => Temporary_Vertex);
            while not Is_Done (The_Iterator) loop
                if not Vertex_Set.Is_A_Member
                          (Value_Of (The_Iterator), Vertices_Visited) then
                    Vertex_Set.Add (Value_Of (The_Iterator),
                                    To_The_Set => Vertices_Visited);
                    Vertex_Stack.Push (Value_Of (The_Iterator),
                                       On_The_Stack => Vertices_Ready);
                end if;
                Get_Next (The_Iterator);
            end loop;
        end loop;
    end Traverse_Depth_First;

    procedure Traverse_Breadth_First (From_The_Vertex : in Vertex) is
        Vertices_Visited : Vertex_Set.Set;
        Vertices_Ready : Vertex_Queue.Queue;
        Temporary_Vertex : Vertex;
        The_Iterator : Iterator;
        Continue : Boolean;
    begin
        Vertex_Set.Add (From_The_Vertex, To_The_Set => Vertices_Visited);
        Vertex_Queue.Add (From_The_Vertex, To_The_Queue => Vertices_Ready);
        while not Vertex_Queue.Is_Empty (Vertices_Ready) loop
            Temporary_Vertex := Vertex_Queue.Front_Of (Vertices_Ready);
            Vertex_Queue.Pop (Vertices_Ready);
            Process (Temporary_Vertex, Continue);
            exit when not Continue;
            Initialize (The_Iterator, With_The_Vertex => Temporary_Vertex);
            while not Is_Done (The_Iterator) loop
                if not Vertex_Set.Is_A_Member
                          (Value_Of (The_Iterator), Vertices_Visited) then
                    Vertex_Set.Add (Value_Of (The_Iterator),
                                    To_The_Set => Vertices_Visited);
                    Vertex_Queue.Add (Value_Of (The_Iterator),
                                      To_The_Queue => Vertices_Ready);
                end if;
                Get_Next (The_Iterator);
            end loop;
        end loop;
    end Traverse_Breadth_First;

end Graph_Search_Unbounded_Managed;
