--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Tree is private;
   type Child is (<>);
   Left_Child : in Child;
   Right_Child : in Child;

   with function Is_Null
     (The_Tree : in Tree)
      return Boolean;

   with function Child_Of
     (The_Tree  : in Tree;
      The_Child : in Child)
      return Tree;

package Booch_Light.Tree_Search_Binary is

   generic
      with procedure Process
        (The_Tree : in     Tree;
         Continue :    out Boolean);
   procedure Traverse_Pre_Order (The_Tree : in Tree);

   generic
      with procedure Process
        (The_Tree : in     Tree;
         Continue :    out Boolean);
   procedure Traverse_In_Order (The_Tree : in Tree);

   generic
      with procedure Process
        (The_Tree : in     Tree;
         Continue :    out Boolean);
   procedure Traverse_Post_Order (The_Tree : in Tree);

end Booch_Light.Tree_Search_Binary;

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
