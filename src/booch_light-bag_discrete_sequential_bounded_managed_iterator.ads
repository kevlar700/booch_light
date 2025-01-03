--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is (<>);
package Booch_Light.Bag_Discrete_Sequential_Bounded_Managed_Iterator is

   package Locus is

      subtype Remove is Status_Code with
          Static_Predicate => Remove in Item_Is_Not_In_Bag | OK;

      subtype Number_Of is Status_Code with
          Static_Predicate => Number_Of in Item_Is_Not_In_Bag | OK;

   end Locus;

   type Bag is limited private;

   procedure Copy
     (From_The_Bag : in     Bag;
      To_The_Bag   : in out Bag);

   procedure Clear (The_Bag : in out Bag);

   procedure Add
     (The_Item   : in     Item;
      To_The_Bag : in out Bag);

   procedure Remove
     (The_Item     : in     Item;
      From_The_Bag : in out Bag;
      Booch_Status :    out Locus.Remove);

   procedure Union
     (Of_The_Bag  : in     Bag;
      And_The_Bag : in     Bag;
      To_The_Bag  : in out Bag);

   procedure Intersection
     (Of_The_Bag  : in     Bag;
      And_The_Bag : in     Bag;
      To_The_Bag  : in out Bag);

   procedure Difference
     (Of_The_Bag  : in     Bag;
      And_The_Bag : in     Bag;
      To_The_Bag  : in out Bag);

   function Is_Equal
     (Left  : in Bag;
      Right : in Bag)
      return Boolean;

   function Extent_Of
     (The_Bag : in Bag)
      return Natural;

   function Unique_Extent_Of
     (The_Bag : in Bag)
      return Natural;

   procedure Number_Of
     (The_Item      : in     Item;
      In_The_Bag    : in     Bag;
      The_Number_Of :    out Positive;
      Booch_Status  :    out Locus.Number_Of);

   function Is_Empty
     (The_Bag : in Bag)
      return Boolean;

   function Is_A_Member
     (The_Item   : in Item;
      Of_The_Bag : in Bag)
      return Boolean;

   function Is_A_Subset
     (Left  : in Bag;
      Right : in Bag)
      return Boolean;

   function Is_A_Proper_Subset
     (Left  : in Bag;
      Right : in Bag)
      return Boolean;

   generic
      with procedure Process
        (The_Item  : in     Item;
         The_Count : in     Positive;
         Continue  :    out Boolean);
   procedure Iterate (Over_The_Bag : in Bag);

private
   type Items is array (Item) of Natural;
   type Bag is record
      The_Items : Items := Items'(others => 0);
   end record;
end Booch_Light.Bag_Discrete_Sequential_Bounded_Managed_Iterator;

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
