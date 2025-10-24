--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Set_Simple_Sequential_Bounded_Managed_Iterator;
generic
   type Item is private;
   type Attribute is private;
   Number_Of_Vertices : Positive;
   Number_Of_Arcs : Positive;
package Booch_Light.Graph_Directed_Bounded_Managed is

   package Locus is

      subtype New_Item is Status_Code with
          Static_Predicate => New_Item in No_Storage_Available | OK;

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Visit;

      subtype Visit is Status_Code with
          Static_Predicate =>
           Visit in Duplicate | Exception_Overflow | Multiple_Binding | OK;

      subtype Duplicate is Status_Code with
          Static_Predicate =>
           Duplicate in
             Domain_Is_Not_Bound | Item_Is_In_Set | Exception_Overflow
             | Exception_Storage_Error | OK;

      subtype Add is Status_Code with
          Static_Predicate =>
           Add in New_Item | Exception_Storage_Error | Exception_Overflow | OK;

      subtype Remove is Status_Code with
          Static_Predicate =>
           Remove in
             Item_Is_Not_In_Set | Vertex_Has_References
             | Vertex_Is_Not_In_Graph | Vertex_Is_Null | OK;

      subtype Set_Item is Status_Code with
          Static_Predicate => Set_Item in Vertex_Is_Null | OK;

      subtype Create is Status_Code with
          Static_Predicate =>
           Create in
             Vertex_Is_Null | Vertex_Is_Not_In_Graph | Item_Is_In_Set
             | Exception_Overflow | Exception_Storage_Error | OK;

      subtype Destroy is Status_Code with
          Static_Predicate =>
           Destroy in
             Arc_Is_Null | Vertex_Is_Not_In_Graph | Exception_Storage_Error
             | OK;

      subtype Set_Attribute is Status_Code with
          Static_Predicate => Set_Attribute in Arc_Is_Null | OK;

      subtype Number_Of_Arcs_From is Status_Code with
          Static_Predicate => Number_Of_Arcs_From in Vertex_Is_Null | OK;

      subtype Item_Of is Status_Code with
          Static_Predicate => Item_Of in Vertex_Is_Null | OK;

      subtype Attribute_Of is Status_Code with
          Static_Predicate => Attribute_Of in Arc_Is_Null | OK;

      subtype Source_Of is Status_Code with
          Static_Predicate => Source_Of in Arc_Is_Null | OK;

      subtype Destination_Of is Status_Code with
          Static_Predicate => Destination_Of in Arc_Is_Null | OK;

      subtype Reiterate is Status_Code with
          Static_Predicate => Reiterate in Vertex_Is_Null | OK;

      subtype Always_OK is Status_Code with
          Static_Predicate => Always_OK in OK;

   end Locus;

   type Graph (Total_Vertices : Positive; Total_Arcs : Positive) is
     limited private;
   type Vertex is private;
   type Arc is private;

   Null_Vertex : constant Vertex;
   Null_Arc    : constant Arc;

   --  TODO: Replace as recursion is not permitted in this repo
   --  procedure Copy
   --    (From_The_Graph :        Graph;
   --     To_The_Graph   : in out Graph;
   --     Booch_Status   :    out Locus.Copy);

   procedure Clear (The_Graph : in out Graph);

   procedure Add
     (The_Vertex    : in out Vertex;
      With_The_Item :        Item;
      To_The_Graph  : in out Graph;
      Booch_Status  :    out Locus.Add);

   procedure Remove
     (The_Vertex     : in out Vertex;
      From_The_Graph : in out Graph;
      Booch_Status   :    out Locus.Remove);

   procedure Set_Item
     (Of_The_Vertex : in out Vertex;
      To_The_Item   :        Item;
      Booch_Status  :    out Locus.Set_Item);

   procedure Create
     (The_Arc            : in out Arc;
      With_The_Attribute :        Attribute;
      From_The_Vertex    : in out Vertex;
      To_The_Vertex      :        Vertex;
      In_The_Graph       : in out Graph;
      Booch_Status       :    out Locus.Create);

   procedure Destroy
     (The_Arc      : in out Arc;
      In_The_Graph : in out Graph;
      Booch_Status :    out Locus.Destroy);

   procedure Set_Attribute
     (Of_The_Arc       : in out Arc;
      To_The_Attribute :        Attribute;
      Booch_Status     :    out Locus.Set_Attribute);

   function Is_Empty
     (The_Graph : Graph)
      return Boolean;

   function Is_Null
     (The_Vertex : Vertex)
      return Boolean;

   function Is_Null
     (The_Arc : Arc)
      return Boolean;

   function Number_Of_Vertices_In
     (The_Graph : Graph)
      return Natural;

   function Number_Of_Arcs_In
     (The_Graph : Graph)
      return Natural;

   procedure Number_Of_Arcs_From
     (The_Vertex    :     Vertex;
      The_Arc_Count : out Natural;
      Booch_Status  : out Locus.Number_Of_Arcs_From);

   procedure Item_Of
     (The_Vertex   :     Vertex;
      The_Item     : out Item;
      Booch_Status : out Locus.Item_Of);

   procedure Attribute_Of
     (The_Arc       :     Arc;
      The_Attribute : out Attribute;
      Booch_Status  : out Locus.Attribute_Of);

   procedure Source_Of
     (The_Arc      :     Arc;
      The_Source   : out Vertex;
      Booch_Status : out Locus.Source_Of);

   procedure Destination_Of
     (The_Arc         :     Arc;
      The_Destination : out Vertex;
      Booch_Status    : out Locus.Destination_Of);

   function Is_A_Member
     (The_Vertex   : Vertex;
      Of_The_Graph : Graph)
      return Boolean;

   function Is_A_Member
     (The_Arc      : Arc;
      Of_The_Graph : Graph)
      return Boolean;

   generic
      with procedure Process
        (The_Vertex :     Vertex;
         Continue   : out Boolean);
   procedure Iterate_Vertices (Over_The_Graph : Graph);

   generic
      with procedure Process
        (The_Arc  :     Arc;
         Continue : out Boolean);
   procedure Iterate_Arcs (Over_The_Graph : Graph);

   generic
      with procedure Process
        (The_Arc  :     Arc;
         Continue : out Boolean);
   procedure Reiterate
     (Over_The_Vertex :     Vertex;
      Booch_Status    : out Locus.Reiterate);

private
   subtype Vertex_Index is Natural range 0 .. Number_Of_Vertices;

   type Vertex is record
      The_Head : Vertex_Index := 0;
   end record;

   package Vertex_Set is new Set_Simple_Sequential_Bounded_Managed_Iterator
     (Item => Vertex);

   subtype Arc_Index is Natural range 0 .. Number_Of_Arcs;

   type Arc is record
      The_Head : Arc_Index := 0;
   end record;

   package Arc_Set is new Set_Simple_Sequential_Bounded_Managed_Iterator
     (Item => Arc);

   type Graph (Total_Vertices : Positive; Total_Arcs : Positive) is record
      The_Vertices : Vertex_Set.Set (Total_Vertices);
      The_Arcs     : Arc_Set.Set (Total_Arcs);
   end record;

   Null_Vertex : constant Vertex := Vertex'(The_Head => 0);
   Null_Arc    : constant Arc    := Arc'(The_Head => 0);

end Booch_Light.Graph_Directed_Bounded_Managed;

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
