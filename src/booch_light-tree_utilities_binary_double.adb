--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Tree_Utilities_Binary_Double is

   function Is_Root
     (The_Tree : in Tree)
      return Boolean
   is
   begin
      return Is_Null (Parent_Of (The_Tree));
   end Is_Root;

   function Is_Leaf
     (The_Tree : in Tree)
      return Boolean
   is
   begin
      return
        (Is_Null (Child_Of (The_Tree, Left_Child))
         and then Is_Null (Child_Of (The_Tree, Right_Child)));
   end Is_Leaf;

   function Root_Of
     (The_Tree : in Tree)
      return Tree
   is
      Result : Tree := The_Tree;
   begin
      while not Is_Root (Result) loop
         Result := Parent_Of (Result);
      end loop;
      return Result;
   end Root_Of;

   procedure Child_Name_Of
     (The_Tree     : in     Tree;
      Result       :    out Child;
      Booch_Status :    out Locus.Child_Name_Of)
   is
   begin
      if Is_Root (The_Tree) then
         Booch_Status := Tree_Is_Root;
         Alterable_Log.Log
           (Log_ID  => "7ABF61A6F35189FF",
            Message => "Tree_Is_Root: Child_Name_Of failed");
         Result := Child'Last;
         return;

      elsif Child_Of (Parent_Of (The_Tree), Left_Child) = The_Tree then
         Result := Left_Child;
      else
         Result := Right_Child;
      end if;

      Booch_Status := OK;
   end Child_Name_Of;

   procedure Left_Sibling_Of
     (The_Tree     : in     Tree;
      Result       :    out Tree;
      Booch_Status :    out Locus.Left_Sibling_Of)
   is
      Child_Name_Of_Result : Child;
      Tmp_Status           : Locus.Child_Name_Of;
   begin
      Child_Name_Of
        (The_Tree     => The_Tree,
         Result       => Child_Name_Of_Result,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Tree_Is_Root =>
            Booch_Status := Tmp_Status;
            Alterable_Log.Log
              (Log_ID  => "2FD6E47CC43854C0",
               Message => "Tree_Is_Root: Left_Sibling_Of failed");
            return;

         when OK =>
            null;
      end case;

      if Child_Name_Of_Result = Left_Child then
         Result       := Null_Tree;
         Booch_Status := OK;
         return;
      end if;

      Result       := Child_Of (Parent_Of (The_Tree), Left_Child);
      Booch_Status := OK;

   end Left_Sibling_Of;

   procedure Right_Sibling_Of
     (The_Tree     : in     Tree;
      Result       :    out Tree;
      Booch_Status :    out Locus.Right_Sibling_Of)
   is
      Child_Name_Of_Result : Child;
      Tmp_Status           : Locus.Child_Name_Of;
   begin
      Child_Name_Of
        (The_Tree     => The_Tree,
         Result       => Child_Name_Of_Result,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Tree_Is_Root =>
            Booch_Status := Tmp_Status;
            Alterable_Log.Log
              (Log_ID  => "0C915D75FAEB664F",
               Message => "Tree_Is_Root: Right_Sibling_Of failed");
            return;

         when OK =>
            null;
      end case;

      if Child_Name_Of_Result = Right_Child then
         Result       := Null_Tree;
         Booch_Status := OK;
         return;
      end if;

      Result       := Child_Of (Parent_Of (The_Tree), Right_Child);
      Booch_Status := OK;

   end Right_Sibling_Of;

end Booch_Light.Tree_Utilities_Binary_Double;

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
