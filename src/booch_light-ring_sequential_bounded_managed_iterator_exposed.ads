--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
package Booch_Light.Ring_Sequential_Bounded_Managed_Iterator_Exposed is

   package Locus is

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Exception_Overflow | OK;

      subtype Pop is Status_Code with
          Static_Predicate => Pop in Exception_Underflow | OK;

      subtype Rotate is Status_Code with
          Static_Predicate => Rotate in Rotate_Error | OK;

      subtype Insert is Status_Code with
          Static_Predicate => Insert in Exception_Overflow | OK;

      subtype Top_Of is Status_Code with
          Static_Predicate => Top_Of in Exception_Underflow | OK;

   end Locus;

   type Locations is private;
   type Items is array (Positive range <>) of Item;
   type Ring (The_Size : Positive) is record
      Concealed : Locations;
      The_Items : Items (1 .. The_Size);
   end record;

   type Direction is
     (Forward,
      Backward);

   procedure Copy
     (From_The_Ring : in     Ring;
      To_The_Ring   : in out Ring;
      Booch_Status  :    out Locus.Copy);

   procedure Clear (The_Ring : in out Ring);

   procedure Insert
     (The_Item     : in     Item;
      In_The_Ring  : in out Ring;
      Booch_Status :    out Locus.Insert);

   procedure Pop
     (The_Ring     : in out Ring;
      Booch_Status :    out Locus.Pop);

   procedure Rotate
     (The_Ring         : in out Ring;
      In_The_Direction : in     Direction;
      Booch_Status     :    out Locus.Rotate);

   procedure Mark (The_Ring : in out Ring);
   procedure Rotate_To_Mark (The_Ring : in out Ring);

   function Is_Equal
     (Left  : in Ring;
      Right : in Ring)
      return Boolean;

   function Extent_Of
     (The_Ring : in Ring)
      return Natural;

   function Is_Empty
     (The_Ring : in Ring)
      return Boolean;

   procedure Top_Of
     (The_Ring     : in     Ring;
      Result       :    out Item;
      Booch_Status :    out Locus.Top_Of);

   function At_Mark
     (The_Ring : in Ring)
      return Boolean;

   generic
      with procedure Process
        (The_Item : in     Item;
         Continue :    out Boolean);
   procedure Iterate (Over_The_Ring : in Ring);

private
   type Locations is record
      The_Top  : Natural := 0;
      The_Back : Natural := 0;
      The_Mark : Natural := 0;
   end record;

end Booch_Light.Ring_Sequential_Bounded_Managed_Iterator_Exposed;

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
