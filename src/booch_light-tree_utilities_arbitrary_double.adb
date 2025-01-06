--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Tree_Utilities_Arbitrary_Double is

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
      for Index in 1 .. Number_Of_Children_In (The_Tree) loop
         if not Is_Null (Child_Of
                (The_Tree,
                 The_Child => Index)) then
            return False;
         end if;
      end loop;
      return True;
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
      Result       :    out Positive;
      Booch_Status :    out Locus.Child_Name_Of)
   is
   begin
      if Is_Root (The_Tree) then
         Booch_Status := Tree_Is_Root;
         Alterable_Log.Log
           (Log_ID  => "20D5A00297F03441",
            Message => "Tree_Is_Root: Child_Name_Of failed");
         Result := Positive'Invalid_Value;
         return;
      else
         for Index in 1 .. Number_Of_Children_In (Parent_Of (The_Tree)) loop
            if Child_Of
                (Parent_Of (The_Tree),
                 The_Child => Index) = The_Tree then
               Booch_Status := OK;
               Result       := Index;
               return;
            end if;
         end loop;
      end if;
   end Child_Name_Of;

   procedure Number_Of_Siblings_Of
     (The_Tree     : in     Tree;
      Result       :    out Natural;
      Booch_Status :    out Locus.Number_Of_Siblings_Of)
   is
   begin
      if Is_Root (The_Tree) then
         Booch_Status := Tree_Is_Root;
         Alterable_Log.Log
           (Log_ID  => "99F89694B6FF0A72",
            Message => "Tree_Is_Root: Number_Of_Siblings_Of failed");
         Result := Natural'Invalid_Value;
         return;
      end if;

      Result       := (Number_Of_Children_In (Parent_Of (The_Tree)) - 1);
      Booch_Status := OK;

   end Number_Of_Siblings_Of;

   procedure Left_Sibling_Of
     (The_Tree     : in     Tree;
      Result       :    out Tree;
      Booch_Status :    out Locus.Left_Sibling_Of)
   is
      Child_Name_Of_Result : Positive;
   begin
      Child_Name_Of (The_Tree, Child_Name_Of_Result, Booch_Status);
      case Booch_Status is
         when Tree_Is_Root =>
            Alterable_Log.Log
              (Log_ID  => "0E04612F6E18474A",
               Message => "Tree_Is_Root: Left_Sibling_Of failed");
            return;

         when OK =>
            null;

      end case;

      if Child_Name_Of_Result = 1 then
         Result := Null_Tree;
         return;
      else

         Child_Name_Of (The_Tree, Child_Name_Of_Result, Booch_Status);
         case Booch_Status is
            when Tree_Is_Root =>
               Alterable_Log.Log
                 (Log_ID  => "E5A5651E53B6FCF4",
                  Message => "Tree_Is_Root: Left_Sibling_Of failed");
               return;

            when OK =>
               null;

         end case;

         Result := Child_Of
             (Parent_Of (The_Tree),
              The_Child => (Child_Name_Of_Result - 1));
         return;
      end if;

   end Left_Sibling_Of;

   procedure Right_Sibling_Of
     (The_Tree     : in     Tree;
      Result       :    out Tree;
      Booch_Status :    out Locus.Right_Sibling_Of)
   is
      Child_Name_Of_Result : Positive;
   begin
      Child_Name_Of (The_Tree, Child_Name_Of_Result, Booch_Status);
      case Booch_Status is
         when Tree_Is_Root =>
            Alterable_Log.Log
              (Log_ID  => "4AE68586D9511BDD",
               Message => "Tree_Is_Root: Right_Sibling_Of failed");
            return;

         when OK =>
            null;

      end case;

      if Child_Name_Of_Result = Number_Of_Children_In (Parent_Of (The_Tree))
      then
         Result := Null_Tree;
         return;
      else
         Child_Name_Of (The_Tree, Child_Name_Of_Result, Booch_Status);
         case Booch_Status is
            when Tree_Is_Root =>
               Alterable_Log.Log
                 (Log_ID  => "7B3565AE9E202755",
                  Message => "Tree_Is_Root: Right_Sibling_Of failed");
               return;

            when OK =>
               null;

         end case;

         Result := Child_Of
             (Parent_Of (The_Tree),
              The_Child => (Child_Name_Of_Result + 1));
         return;

      end if;
   end Right_Sibling_Of;

   procedure Leftmost_Sibling_Of
     (The_Tree     : in     Tree;
      Result       :    out Tree;
      Booch_Status :    out Locus.Leftmost_Sibling_Of)
   is
      Child_Name_Of_Result : Positive;
   begin
      Child_Name_Of (The_Tree, Child_Name_Of_Result, Booch_Status);
      case Booch_Status is
         when Tree_Is_Root =>
            Alterable_Log.Log
              (Log_ID  => "D736B59058BA7777",
               Message => "Tree_Is_Root: Leftmost_Sibling_Of failed");
            return;

         when OK =>
            null;

      end case;

      if Child_Name_Of_Result = 1 then
         Result := Null_Tree;
         return;
      else
         Result := (Child_Of
              (Parent_Of (The_Tree),
               The_Child => 1));
      end if;
   end Leftmost_Sibling_Of;

   procedure Rightmost_Sibling_Of
     (The_Tree     : in     Tree;
      Result       :    out Tree;
      Booch_Status :    out Locus.Rightmost_Sibling_Of)
   is
      Child_Name_Of_Result : Positive;
   begin
      Child_Name_Of (The_Tree, Child_Name_Of_Result, Booch_Status);
      case Booch_Status is
         when Tree_Is_Root =>
            Alterable_Log.Log
              (Log_ID  => "80E15AF8BF8F4E02",
               Message => "Tree_Is_Root: Rightmost_Sibling_Of failed");
            return;

         when OK =>
            null;

      end case;

      if Child_Name_Of_Result = 1 then
         Result := Null_Tree;
         return;
      else
         Result := (Child_Of
              (Parent_Of (The_Tree),
               The_Child => 1));
      end if;

      if Child_Name_Of_Result = Number_Of_Children_In (Parent_Of (The_Tree))
      then
         Result := Null_Tree;
      else
         Result := (Child_Of
              (Parent_Of (The_Tree),
               The_Child => (Number_Of_Children_In (Parent_Of (The_Tree)))));
      end if;
   end Rightmost_Sibling_Of;

end Booch_Light.Tree_Utilities_Arbitrary_Double;

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
