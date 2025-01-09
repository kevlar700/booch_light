--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

with Semaphore;
generic
    type Item is private;
package Bag_Simple_Concurrent_Unbounded_Unmanaged_Noniterator is

    type Bag is limited private;

    procedure Copy         (From_The_Bag : in     Bag; 
                            To_The_Bag   : in out Bag);
    procedure Clear        (The_Bag      : in out Bag);
    procedure Add          (The_Item     : in     Item; 
                            To_The_Bag   : in out Bag);
    procedure Remove       (The_Item     : in     Item; 
                            From_The_Bag : in out Bag);
    procedure Union        (Of_The_Bag   : in     Bag;
                            And_The_Bag  : in     Bag;
                            To_The_Bag   : in out Bag);
    procedure Intersection (Of_The_Bag   : in     Bag;
                            And_The_Bag  : in     Bag;
                            To_The_Bag   : in out Bag);
    procedure Difference   (Of_The_Bag   : in     Bag;
                            And_The_Bag  : in     Bag;
                            To_The_Bag   : in out Bag);

    function Is_Equal           (Left       : in Bag; 
                                 Right      : in Bag) return Boolean;
    function Extent_Of          (The_Bag    : in Bag) return Natural;
    function Unique_Extent_Of   (The_Bag    : in Bag) return Natural;
    function Number_Of          (The_Item   : in Item; 
                                 In_The_Bag : in Bag) return Positive;
    function Is_Empty           (The_Bag    : in Bag) return Boolean;
    function Is_A_Member        (The_Item   : in Item; 
                                 Of_The_Bag : in Bag) return Boolean;
    function Is_A_Subset        (Left       : in Bag; 
                                 Right      : in Bag) return Boolean;
    function Is_A_Proper_Subset (Left       : in Bag; 
                                 Right      : in Bag) return Boolean;

    Overflow           : exception;
    Item_Is_Not_In_Bag : exception;

private
    type Node;
    type Structure is access Node;
    type Bag is
        record
            Guard     : Semaphore.Kind;
            The_Items : Structure;
        end record;
end Bag_Simple_Concurrent_Unbounded_Unmanaged_Noniterator;

--              Original Booch Components (Ada 83 version)
--                               
-- Copyright (C) 2000 Grady Booch
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but 
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU General Public License.  
--
-- The book _SOFTWARE_COMPONENTS_WITH_Ada__Structures,_Tools,_and_Subsystems_
-- ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage 
-- of this software.
--
-- The Ada 83 version of the components is the exact version described in
-- the book mentioned above.  The Ada 95 elaboration version differs only in
-- that each component unit is a child of a root package named "Booch" and
-- each package declaration includes the most appropriate elaboration control
-- pragma.  In addition, the Ada 95 child iteration version eliminates the
-- distinct iterator and noniterator forms for components that had them and
-- instead makes the associated "Iterate" procedures available as children of
-- those components.  More enhanced versions may be produced in the future.
--
-- The Original Booch Components are actively maintained and enhanced
-- by Vincent Marciante and Samuel T. Harris and may be found at the 
-- AdaPower web site (http://www.adapower.com) provided by David Botton.
