--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is limited private;
   type List is private;

   with function "="
     (Left  : in Item;
      Right : in Item)
      return Boolean;

   with function Is_Null
     (The_List : in List)
      return Boolean;

   with function Head_Of
     (The_List : in List)
      return Item;

   with function Tail_Of
     (The_List : in List)
      return List;

package Booch_Light.List_Search is

   package Locus is

      subtype Position_Of is Status_Code with
          Static_Predicate => Position_Of in Item_Not_Found | OK;

      subtype Location_Of is Status_Code with
          Static_Predicate => Location_Of in Position_Error | OK;

      subtype Location_Of_Item is Status_Code with
          Static_Predicate => Location_Of_Item in Item_Not_Found | OK;

   end Locus;

   procedure Position_Of
     (The_Item     : in     Item;
      In_The_List  : in     List;
      Position     :    out Positive;
      Booch_Status :    out Locus.Position_Of);

   procedure Location_Of
     (The_Position : in     Positive;
      In_The_List  : in     List;
      Location     :    out List;
      Booch_Status :    out Locus.Location_Of);

   procedure Location_Of
     (The_Item     : in     Item;
      In_The_List  : in     List;
      Location     :    out List;
      Booch_Status :    out Locus.Location_Of_Item);

end Booch_Light.List_Search;

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
