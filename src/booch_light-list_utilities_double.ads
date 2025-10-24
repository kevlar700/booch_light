--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
   type List is private;

   with procedure Clear (The_List : in out List);

   with procedure Construct
     (The_Item     :     Item;
      And_The_List : in out List);

   with procedure Swap_Tail
     (Of_The_List  : in out List;
      And_The_List : in out List);

   with function Is_Null
     (The_List : List)
      return Boolean;

   with function Tail_Of
     (The_List : List)
      return List;

   with function Predecessor_Of
     (The_List : List)
      return List;

package Booch_Light.List_Utilities_Double is

   package Locus is

      subtype Location_Of is Status_Code with
          Static_Predicate => Location_Of in Position_Error | OK;

      subtype Split is Status_Code with
          Static_Predicate => Split in Position_Error | OK;

      subtype Insert is Status_Code with
          Static_Predicate => Insert in Position_Error | OK;

      subtype Remove_Item is Status_Code with
          Static_Predicate => Remove_Item in Position_Error | OK;

   end Locus;

   type Items is array (Positive range <>) of Item;

   procedure Construct
     (The_Items    :     Items;
      And_The_List : in out List);

   procedure Construct
     (The_List     : in out List;
      And_The_List : in out List);

   procedure Split
     (The_List        : in out List;
      At_The_Position :     Positive;
      Into_The_List   : in out List;
      Booch_Status    :    out Locus.Split);

   procedure Insert
     (The_Item           :     Item;
      In_The_List        : in out List;
      After_The_Position :     Positive;
      Booch_Status       :    out Locus.Insert);

   procedure Insert
     (The_List           : in out List;
      In_The_List        : in out List;
      After_The_Position :     Positive;
      Booch_Status       :    out Locus.Insert);

   procedure Insert
     (The_Item       :     Item;
      After_The_List : in out List);

   procedure Insert
     (The_List       :     List;
      After_The_List : in out List);

   procedure Remove_Item
     (In_The_List     : in out List;
      At_The_Position :     Positive;
      Booch_Status    :    out Locus.Remove_Item);

   function Foot_Of
     (The_List : List)
      return List;

   function Is_Head
     (The_List : List)
      return Boolean;

   function Head_Of
     (The_List : List)
      return List;

end Booch_Light.List_Utilities_Double;

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
