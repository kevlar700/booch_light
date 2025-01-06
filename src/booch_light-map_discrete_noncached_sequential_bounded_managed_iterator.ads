--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Domain is (<>);
   type Ranges is private;

package Booch_Light.Map_Discrete_Noncached_Sequential_Bounded_Managed_Iterator
is
   package Locus is

      subtype Bind is Status_Code with
          Static_Predicate => Bind in Multiple_Binding | OK;

      subtype Unbind is Status_Code with
          Static_Predicate => Unbind in Domain_Is_Not_Bound | OK;

      subtype Range_Of is Status_Code with
          Static_Predicate => Range_Of in Domain_Is_Not_Bound | OK;

   end Locus;

   type Map is limited private;

   procedure Copy
     (From_The_Map : in     Map;
      To_The_Map   : in out Map);

   procedure Clear (The_Map : in out Map);

   procedure Bind
     (The_Domain    : in     Domain;
      And_The_Range : in     Ranges;
      In_The_Map    : in out Map;
      Booch_Status  :    out Locus.Bind);

   procedure Unbind
     (The_Domain   : in     Domain;
      In_The_Map   : in out Map;
      Booch_Status :    out Locus.Unbind);

   function Is_Equal
     (Left  : in Map;
      Right : in Map)
      return Boolean;

   function Extent_Of
     (The_Map : in Map)
      return Natural;

   function Is_Empty
     (The_Map : in Map)
      return Boolean;

   function Is_Bound
     (The_Domain : in Domain;
      In_The_Map : in Map)
      return Boolean;

   procedure Range_Of
     (The_Domain   : in     Domain;
      In_The_Map   : in     Map;
      Result       :    out Ranges;
      Booch_Status :    out Locus.Range_Of);

   generic
      with procedure Process
        (The_Domain : in     Domain;
         The_Range  : in     Ranges;
         Continue   :    out Boolean);
   procedure Iterate (Over_The_Map : in Map);

private

   type Node is record
      Is_Bound  : Boolean := False;
      The_Range : Ranges;
   end record;

   type Map is array (Domain) of Node;

end Booch_Light.Map_Discrete_Noncached_Sequential_Bounded_Managed_Iterator;

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
