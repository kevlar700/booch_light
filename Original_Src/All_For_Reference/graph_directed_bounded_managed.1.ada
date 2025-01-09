--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

with Set_Simple_Sequential_Bounded_Managed_Iterator;
generic
    type Item      is private;
    type Attribute is private;
    Number_Of_Vertices : in Positive;
    Number_Of_Arcs     : in Positive;
package Graph_Directed_Bounded_Managed is

    type Graph (Total_Vertices : Positive; 
                Total_Arcs     : Positive) is limited private;
    type Vertex is private;
    type Arc    is private;

    Null_Vertex : constant Vertex;
    Null_Arc    : constant Arc;

    procedure Copy          (From_The_Graph     : in     Graph; 
                             To_The_Graph       : in out Graph);
    procedure Clear         (The_Graph          : in out Graph);
    procedure Add           (The_Vertex         : in out Vertex;
                             With_The_Item      : in     Item;
                             To_The_Graph       : in out Graph);
    procedure Remove        (The_Vertex         : in out Vertex;
                             From_The_Graph     : in out Graph);
    procedure Set_Item      (Of_The_Vertex      : in out Vertex; 
                             To_The_Item        : in     Item);
    procedure Create        (The_Arc            : in out Arc;
                             With_The_Attribute : in     Attribute;
                             From_The_Vertex    : in out Vertex;
                             To_The_Vertex      : in     Vertex;
                             In_The_Graph       : in out Graph);
    procedure Destroy       (The_Arc            : in out Arc; 
                             In_The_Graph       : in out Graph);
    procedure Set_Attribute (Of_The_Arc         : in out Arc;
                             To_The_Attribute   : in     Attribute);

    function Is_Empty              (The_Graph    : in Graph)  return Boolean;
    function Is_Null               (The_Vertex   : in Vertex) return Boolean;
    function Is_Null               (The_Arc      : in Arc)    return Boolean;
    function Number_Of_Vertices_In (The_Graph    : in Graph)  return Natural;
    function Number_Of_Arcs_In     (The_Graph    : in Graph)  return Natural;
    function Number_Of_Arcs_From   (The_Vertex   : in Vertex) return Natural;
    function Item_Of               (The_Vertex   : in Vertex) return Item;
    function Attribute_Of          (The_Arc      : in Arc)    return Attribute;
    function Source_Of             (The_Arc      : in Arc)    return Vertex;
    function Destination_Of        (The_Arc      : in Arc)    return Vertex;
    function Is_A_Member           (The_Vertex   : in Vertex; 
                                    Of_The_Graph : in Graph)  return Boolean;
    function Is_A_Member           (The_Arc      : in Arc; 
                                    Of_The_Graph : in Graph)  return Boolean;

    generic
        with procedure Process (The_Vertex : in  Vertex; 
                                Continue   : out Boolean);
    procedure Iterate_Vertices (Over_The_Graph : in Graph);

    generic
        with procedure Process (The_Arc  : in  Arc; 
                                Continue : out Boolean);
    procedure Iterate_Arcs (Over_The_Graph : in Graph);

    generic
        with procedure Process (The_Arc  : in  Arc; 
                                Continue : out Boolean);
    procedure Reiterate (Over_The_Vertex : in Vertex);

    Overflow               : exception;
    Vertex_Is_Null         : exception;
    Vertex_Is_Not_In_Graph : exception;
    Vertex_Has_References  : exception;  
    Arc_Is_Null            : exception;
    Arc_Is_Not_In_Graph    : exception;

private
    subtype Vertex_Index is Natural range 0 .. Number_Of_Vertices;
    type Vertex is
        record
            The_Head : Vertex_Index := 0;
        end record;
    package Vertex_Set is
      new Set_Simple_Sequential_Bounded_Managed_Iterator (Item => Vertex);
    subtype Arc_Index is Natural range 0 .. Number_Of_Arcs;
    type Arc is
        record
            The_Head : Arc_Index := 0;
        end record;
    package Arc_Set is
      new Set_Simple_Sequential_Bounded_Managed_Iterator (Item => Arc);
    type Graph (Total_Vertices : Positive; 
                Total_Arcs     : Positive) is
        record
            The_Vertices : Vertex_Set.Set (Total_Vertices);
            The_Arcs     : Arc_Set.Set (Total_Arcs);
        end record;
    Null_Vertex : constant Vertex := Vertex'(The_Head => 0);
    Null_Arc    : constant Arc    := Arc'   (The_Head => 0);
end Graph_Directed_Bounded_Managed;

--              Original Booch Components (Ada 83 version)
--                               
-- Copyright (C) 2000 Grady Booch
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
