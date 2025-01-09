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

with Map_Simple_Noncached_Sequential_Unbounded_Unmanaged_Iterator;
package body Graph_Directed_Unbounded_Unmanaged is

    type Vertex_Node is
        record
            The_Item : Item;
            The_Arcs : Arc_Set.Set;
            Reference_Count : Natural := 0;
        end record;

    type Arc_Node is
        record
            The_Attribute : Attribute;
            The_Source : Vertex;
            The_Destination : Vertex;
        end record;

    function Hash_Of (The_Vertex : in Vertex) return Positive is
    begin
        return 1;
    end Hash_Of;

    package Vertex_Map is
       new Map_Simple_Noncached_Sequential_Unbounded_Unmanaged_Iterator
              (Domain => Vertex,
               Ranges => Vertex,
               Number_Of_Buckets => 1,
               Hash_Of => Hash_Of);

    procedure Copy (From_The_Graph : in Graph; To_The_Graph : in out Graph) is
        Vertices_Visited : Vertex_Map.Map;
        procedure Visit (The_Vertex : in Vertex; Continue : out Boolean) is
            Temporary_Vertex : Vertex;
            procedure Duplicate (The_Arc : in Arc; Continue : out Boolean) is
                Temporary_Arc : Arc;
            begin
                Temporary_Arc := new Arc_Node;
                Temporary_Arc.The_Attribute := The_Arc.The_Attribute;
                Temporary_Arc.The_Source := Temporary_Vertex;
                Visit (The_Arc.The_Destination, Continue);
                Temporary_Arc.The_Destination :=
                   Vertex_Map.Range_Of
                      (The_Arc.The_Destination, Vertices_Visited);
                Arc_Set.Add (Temporary_Arc, Temporary_Vertex.The_Arcs);
                Arc_Set.Add (Temporary_Arc, To_The_Graph.The_Arcs);
            end Duplicate;
            procedure Process is new Arc_Set.Iterate (Duplicate);
        begin
            if not Vertex_Map.Is_Bound (The_Vertex,
                                        In_The_Map => Vertices_Visited) then
                Temporary_Vertex := new Vertex_Node;
                Temporary_Vertex.The_Item := The_Vertex.The_Item;
                Temporary_Vertex.Reference_Count := The_Vertex.Reference_Count;
                Vertex_Map.Bind (The_Vertex, Temporary_Vertex,
                                 In_The_Map => Vertices_Visited);
                Vertex_Set.Add (Temporary_Vertex, To_The_Graph.The_Vertices);
                Process (The_Vertex.The_Arcs);
            end if;
            Continue := True;
        end Visit;
        procedure Traverse is new Vertex_Set.Iterate (Visit);
    begin
        Vertex_Set.Clear (To_The_Graph.The_Vertices);
        Arc_Set.Clear (To_The_Graph.The_Arcs);
        Traverse (From_The_Graph.The_Vertices);
    exception
        when Vertex_Set.Overflow | Arc_Set.Overflow |
             Vertex_Map.Overflow | Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Graph : in out Graph) is
    begin
        Vertex_Set.Clear (The_Graph.The_Vertices);
        Arc_Set.Clear (The_Graph.The_Arcs);
    end Clear;

    procedure Add (The_Vertex : in out Vertex;
                   With_The_Item : in Item;
                   To_The_Graph : in out Graph) is
    begin
        The_Vertex := new Vertex_Node;
        The_Vertex.The_Item := With_The_Item;
        Vertex_Set.Add (The_Vertex, To_The_Graph.The_Vertices);
    exception
        when Vertex_Set.Overflow | Storage_Error =>
            raise Overflow;
    end Add;

    procedure Remove (The_Vertex : in out Vertex;
                      From_The_Graph : in out Graph) is
        procedure Remove_Arc (The_Arc : in Arc; Continue : out Boolean) is
        begin
            The_Arc.The_Destination.Reference_Count :=
               The_Arc.The_Destination.Reference_Count - 1;
            Arc_Set.Remove (The_Arc, From_The_Graph.The_Arcs);
            Continue := True;
        end Remove_Arc;
        procedure Traverse is new Arc_Set.Iterate (Remove_Arc);
    begin
        if The_Vertex.Reference_Count /= 0 then
            raise Vertex_Has_References;
        elsif not Vertex_Set.Is_A_Member (The_Vertex,
                                          From_The_Graph.The_Vertices) then
            raise Vertex_Is_Not_In_Graph;
        else
            Traverse (The_Vertex.The_Arcs);
            Arc_Set.Clear (The_Vertex.The_Arcs);
            Vertex_Set.Remove (The_Vertex, From_The_Graph.The_Vertices);
            The_Vertex := Null_Vertex;
        end if;
    exception
        when Constraint_Error =>
            raise Vertex_Is_Null;
    end Remove;

    procedure Set_Item (Of_The_Vertex : in out Vertex; To_The_Item : in Item) is
    begin
        Of_The_Vertex.The_Item := To_The_Item;
    exception
        when Constraint_Error =>
            raise Vertex_Is_Null;
    end Set_Item;

    procedure Create (The_Arc : in out Arc;
                      With_The_Attribute : in Attribute;
                      From_The_Vertex : in out Vertex;
                      To_The_Vertex : in Vertex;
                      In_The_Graph : in out Graph) is
    begin
        if (From_The_Vertex = Null_Vertex) or else
           (To_The_Vertex = Null_Vertex) then
            raise Vertex_Is_Null;
        elsif not (Vertex_Set.Is_A_Member
                      (From_The_Vertex, In_The_Graph.The_Vertices) and then
                   Vertex_Set.Is_A_Member (To_The_Vertex,
                                           In_The_Graph.The_Vertices)) then
            raise Vertex_Is_Not_In_Graph;
        else
            The_Arc := new Arc_Node'(The_Attribute => With_The_Attribute,
                                     The_Source => From_The_Vertex,  
                                     The_Destination => To_The_Vertex);
            Arc_Set.Add (The_Arc, In_The_Graph.The_Arcs);
            Arc_Set.Add (The_Arc, From_The_Vertex.The_Arcs);
            if From_The_Vertex /= To_The_Vertex then
                To_The_Vertex.Reference_Count :=
                   To_The_Vertex.Reference_Count + 1;
            end if;
        end if;
    exception
        when Arc_Set.Overflow | Storage_Error =>
            raise Overflow;
    end Create;

    procedure Destroy (The_Arc : in out Arc; In_The_Graph : in out Graph) is
    begin
        if The_Arc = Null_Arc then
            raise Arc_Is_Null;
        elsif not Arc_Set.Is_A_Member (The_Arc, In_The_Graph.The_Arcs) then
            raise Arc_Is_Not_In_Graph;
        else
            Arc_Set.Remove (The_Arc, The_Arc.The_Source.The_Arcs);
            if The_Arc.The_Source /= The_Arc.The_Destination then
                The_Arc.The_Destination.Reference_Count :=
                   The_Arc.The_Destination.Reference_Count - 1;
            end if;
            Arc_Set.Remove (The_Arc, In_The_Graph.The_Arcs);
            The_Arc := Null_Arc;
        end if;
    end Destroy;

    procedure Set_Attribute (Of_The_Arc : in out Arc;
                             To_The_Attribute : in Attribute) is
    begin
        Of_The_Arc.The_Attribute := To_The_Attribute;
    exception
        when Constraint_Error =>
            raise Arc_Is_Null;
    end Set_Attribute;

    function Is_Empty (The_Graph : in Graph) return Boolean is
    begin
        return Vertex_Set.Is_Empty (The_Graph.The_Vertices);
    end Is_Empty;

    function Is_Null (The_Vertex : in Vertex) return Boolean is
    begin
        return (The_Vertex = Null_Vertex);
    end Is_Null;

    function Is_Null (The_Arc : in Arc) return Boolean is
    begin
        return (The_Arc = Null_Arc);
    end Is_Null;

    function Number_Of_Vertices_In (The_Graph : in Graph) return Natural is
    begin
        return Vertex_Set.Extent_Of (The_Graph.The_Vertices);
    end Number_Of_Vertices_In;

    function Number_Of_Arcs_In (The_Graph : in Graph) return Natural is
    begin
        return Arc_Set.Extent_Of (The_Graph.The_Arcs);
    end Number_Of_Arcs_In;

    function Number_Of_Arcs_From (The_Vertex : in Vertex) return Natural is
    begin
        return Arc_Set.Extent_Of (The_Vertex.The_Arcs);
    exception
        when Constraint_Error =>
            raise Vertex_Is_Null;
    end Number_Of_Arcs_From;

    function Item_Of (The_Vertex : in Vertex) return Item is
    begin
        return The_Vertex.The_Item;
    exception
        when Constraint_Error =>
            raise Vertex_Is_Null;
    end Item_Of;

    function Attribute_Of (The_Arc : in Arc) return Attribute is
    begin
        return The_Arc.The_Attribute;
    exception
        when Constraint_Error =>
            raise Arc_Is_Null;
    end Attribute_Of;

    function Source_Of (The_Arc : in Arc) return Vertex is
    begin
        return The_Arc.The_Source;
    exception
        when Constraint_Error =>
            raise Arc_Is_Null;
    end Source_Of;

    function Destination_Of (The_Arc : in Arc) return Vertex is
    begin
        return The_Arc.The_Destination;
    exception
        when Constraint_Error =>
            raise Arc_Is_Null;
    end Destination_Of;

    function Is_A_Member (The_Vertex : in Vertex; Of_The_Graph : in Graph)
                         return Boolean is
    begin
        return Vertex_Set.Is_A_Member (The_Vertex, Of_The_Graph.The_Vertices);
    end Is_A_Member;

    function Is_A_Member
                (The_Arc : in Arc; Of_The_Graph : in Graph) return Boolean is
    begin
        return Arc_Set.Is_A_Member (The_Arc, Of_The_Graph.The_Arcs);
    end Is_A_Member;

    procedure Iterate_Vertices (Over_The_Graph : in Graph) is
        procedure Traverse is new Vertex_Set.Iterate (Process);
    begin
        Traverse (Over_The_Graph.The_Vertices);
    end Iterate_Vertices;

    procedure Iterate_Arcs (Over_The_Graph : in Graph) is
        procedure Traverse is new Arc_Set.Iterate (Process);
    begin
        Traverse (Over_The_Graph.The_Arcs);
    end Iterate_Arcs;

    procedure Reiterate (Over_The_Vertex : in Vertex) is
        procedure Traverse is new Arc_Set.Iterate (Process);
    begin
        Traverse (Over_The_Vertex.The_Arcs);
    exception
        when Constraint_Error =>
            raise Vertex_Is_Null;
    end Reiterate;

end Graph_Directed_Unbounded_Unmanaged;
