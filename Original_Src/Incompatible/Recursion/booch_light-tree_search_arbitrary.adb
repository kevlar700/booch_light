--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Tree_Search_Arbitrary is

   procedure Traverse_Pre_Order (The_Tree : Tree) is
      Continue : Boolean;
   begin
      if not Is_Null (The_Tree)
      then
         Process (The_Tree, Continue);
         if not Continue
         then
            return;
         end if;
         for Index in 1 .. Number_Of_Children_In (The_Tree) loop
            Traverse_Pre_Order (Child_Of
                 (The_Tree,
                  The_Child => Index));
         end loop;
      end if;
   end Traverse_Pre_Order;

   procedure Traverse_Post_Order (The_Tree : Tree) is
      Continue : Boolean;
   begin
      if not Is_Null (The_Tree)
      then
         for Index in 1 .. Number_Of_Children_In (The_Tree) loop
            Traverse_Post_Order (Child_Of
                 (The_Tree,
                  The_Child => Index));
         end loop;
         Process (The_Tree, Continue);
         if not Continue
         then
            return;
         end if;
      end if;
   end Traverse_Post_Order;

end Booch_Light.Tree_Search_Arbitrary;

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
