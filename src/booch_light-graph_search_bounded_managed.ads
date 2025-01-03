--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Graph is limited private;
   type Vertex is private;
   type Iterator is limited private;
   with procedure Initialize
     (The_Iterator    : in out Iterator;
      With_The_Vertex : in     Vertex);
   with procedure Get_Next (The_Iterator : in out Iterator);
   with function Number_Of_Vertices_In
     (The_Graph : in Graph)
      return Natural;
   with function Value_Of
     (The_Iterator : in Iterator)
      return Vertex;
   with function Is_Done
     (The_Iterator : in Iterator)
      return Boolean;
package Booch_Light.Graph_Search_Bounded_Managed is

   package Locus is

      subtype Traverse_Depth_First is Status_Code with
          Static_Predicate =>
           Traverse_Depth_First in
             Exception_Overflow | Exception_Underflow | Item_Is_In_Set
             | Exception_Storage_Error | OK;

      subtype Traverse_Breadth_First is Status_Code with
          Static_Predicate =>
           Traverse_Breadth_First in
             Item_Is_In_Set | Exception_Overflow | Exception_Storage_Error
             | Exception_Underflow | OK;

   end Locus;

   generic
      with procedure Process
        (The_Vertex : in     Vertex;
         Continue   :    out Boolean);
   procedure Traverse_Depth_First
     (From_The_Vertex : in     Vertex;
      In_The_Graph    : in     Graph;
      Booch_Status    :    out Locus.Traverse_Depth_First);

   generic
      with procedure Process
        (The_Vertex : in     Vertex;
         Continue   :    out Boolean);
   procedure Traverse_Breadth_First
     (From_The_Vertex : in     Vertex;
      In_The_Graph    : in     Graph;
      Booch_Status    :    out Locus.Traverse_Breadth_First);

end Booch_Light.Graph_Search_Bounded_Managed;

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
