--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package Booch_Light is

   type Status_Code is
     (OK,
      Arc_Is_Not_In_Graph,
      Arc_Is_Null,
      Child_Error,
      Domain_Is_Not_Bound,
      File_Is_Empty,
      Illegal_Pattern,
      Item_Is_In_Set,
      Item_Is_Not_In_Bag,
      Item_Is_Not_In_Set,
      Item_Not_Found,
      Lexical_Error,
      List_Is_Null,
      Multiple_Binding,
      No_Storage_Available,
      Not_At_Head,
      Not_At_Root,
      Pattern_Not_Found,
      Position_Error,
      Rotate_Error,
      Tree_Is_Null,
      Tree_Is_Not_Null,
      Tree_Is_Root,
      Vertex_Has_References,
      Vertex_Is_Not_In_Graph,
      Vertex_Is_Null,
      Exception_Overflow,
      Exception_Underflow,
      Exception_Constraint_Error,
      Exception_Storage_Error);

end Booch_Light;

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
