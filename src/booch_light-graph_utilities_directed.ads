--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Vertex is private;
   type Iterator is limited private;

   with procedure Initialize
     (The_Iterator    : in out Iterator;
      With_The_Vertex : in     Vertex);

   with procedure Get_Next (The_Iterator : in out Iterator);

   with function Number_Of_Arcs_From
     (The_Vertex : in Vertex)
      return Natural;

   with function Value_Of
     (The_Iterator : in Iterator)
      return Vertex;

   with function Is_Done
     (The_Iterator : in Iterator)
      return Boolean;

package Booch_Light.Graph_Utilities_Directed is

   function Is_Terminal
     (The_Vertex : in Vertex)
      return Boolean;

   function Has_Self_Loop
     (The_Vertex : in Vertex)
      return Boolean;

end Booch_Light.Graph_Utilities_Directed;

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