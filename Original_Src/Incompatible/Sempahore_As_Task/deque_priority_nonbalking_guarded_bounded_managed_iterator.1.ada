--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

with Semaphore;
generic
    type Item is private;
    type Priority is limited private;
    with function Priority_Of (The_Item : in Item)     return Priority;
    with function "<"         (Left     : in Priority; 
                               Right    : in Priority) return Boolean;
    with function "<="        (Left     : in Priority; 
                               Right    : in Priority) return Boolean;
package Deque_Priority_Nonbalking_Guarded_Bounded_Managed_Iterator is

    type Deque (The_Size : Positive) is limited private;

    type Location is (Front, Back);

    procedure Seize   (The_Deque       : in     Deque);
    procedure Release (The_Deque       : in     Deque);
    procedure Copy    (From_The_Deque  : in     Deque; 
                       To_The_Deque    : in out Deque);
    procedure Clear   (The_Deque       : in out Deque);
    procedure Add     (The_Item        : in     Item;
                       To_The_Deque    : in out Deque;
                       At_The_Location : in     Location);
    procedure Pop     (The_Deque       : in out Deque; 
                       At_The_Location : in     Location);

    function Is_Equal  (Left      : in Deque; 
                        Right     : in Deque) return Boolean;
    function Length_Of (The_Deque : in Deque) return Natural;
    function Is_Empty  (The_Deque : in Deque) return Boolean;
    function Front_Of  (The_Deque : in Deque) return Item;
    function Back_Of   (The_Deque : in Deque) return Item;

    generic
        with procedure Process (The_Item : in  Item; 
                                Continue : out Boolean);
    procedure Iterate (Over_The_Deque : in Deque);

    Overflow  : exception;
    Underflow : exception;

private
    type Items is array (Positive range <>) of Item;
    type Deque (The_Size : Positive) is
        record
            Guard     : Semaphore.Kind;
            The_Back  : Natural := 0;
            The_Items : Items (1 .. The_Size);
        end record;
end Deque_Priority_Nonbalking_Guarded_Bounded_Managed_Iterator;

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
