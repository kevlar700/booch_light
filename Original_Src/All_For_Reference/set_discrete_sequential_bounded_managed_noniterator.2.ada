--                       Original Booch Components
--
--                            (Ada 83 version)
--                               
--                     Copyright (C) 2000 Grady Booch
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

package body Set_Discrete_Sequential_Bounded_Managed_Noniterator is

    procedure Copy (From_The_Set : in Set; To_The_Set : in out Set) is
    begin
        To_The_Set := From_The_Set;
    end Copy;

    procedure Clear (The_Set : in out Set) is
    begin
        The_Set.The_Items := Items'(others => False);
    end Clear;

    procedure Add (The_Item : in Item; To_The_Set : in out Set) is
    begin
        if To_The_Set.The_Items (The_Item) then
            raise Item_Is_In_Set;
        else
            To_The_Set.The_Items (The_Item) := True;
        end if;
    end Add;

    procedure Remove (The_Item : in Item; From_The_Set : in out Set) is
    begin
        if not From_The_Set.The_Items (The_Item) then
            raise Item_Is_Not_In_Set;
        else
            From_The_Set.The_Items (The_Item) := False;
        end if;
    end Remove;

    procedure Union (Of_The_Set : in Set;
                     And_The_Set : in Set;
                     To_The_Set : in out Set) is
    begin
        To_The_Set.The_Items := Of_The_Set.The_Items or And_The_Set.The_Items;
    end Union;

    procedure Intersection (Of_The_Set : in Set;
                            And_The_Set : in Set;
                            To_The_Set : in out Set) is
    begin
        To_The_Set.The_Items := Of_The_Set.The_Items and And_The_Set.The_Items;
    end Intersection;

    procedure Difference (Of_The_Set : in Set;
                          And_The_Set : in Set;
                          To_The_Set : in out Set) is
    begin
        To_The_Set.The_Items := Of_The_Set.The_Items and
                                   (not And_The_Set.The_Items);
    end Difference;

    function Is_Equal (Left : in Set; Right : in Set) return Boolean is
    begin
        return (Left = Right);
    end Is_Equal;

    function Extent_Of (The_Set : in Set) return Natural is
        Count : Natural := 0;
    begin
        for Index in The_Set.The_Items'Range loop
            if The_Set.The_Items (Index) then
                Count := Count + 1;
            end if;
        end loop;
        return Count;
    end Extent_Of;

    function Is_Empty (The_Set : in Set) return Boolean is
    begin
        return (The_Set.The_Items = Items'(others => False));
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Set : in Set) return Boolean is
    begin
        return Of_The_Set.The_Items (The_Item);
    end Is_A_Member;

    function Is_A_Subset (Left : in Set; Right : in Set) return Boolean is
        Intersection_Set : Set :=
           Set'(The_Items => Left.The_Items and Right.The_Items);
    begin
        return (Left = Intersection_Set);
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Set; Right : in Set) return Boolean is
        Intersection_Set : Set :=
           Set'(The_Items => Left.The_Items and Right.The_Items);
    begin
        return ((Intersection_Set = Left) and then (Intersection_Set /= Right));
    end Is_A_Proper_Subset;

end Set_Discrete_Sequential_Bounded_Managed_Noniterator;
