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

package body Set_Discrete_Concurrent_Bounded_Managed_Iterator is

    procedure Seize (The_Set : in Set) is
    begin
        Semaphore.Seize (The_Set.Guard);
    end Seize;

    procedure Release (The_Set : in Set) is
    begin
        Semaphore.Release (The_Set.Guard);
    end Release;

    procedure Copy (From_The_Set : in Set; To_The_Set : in out Set) is
    begin
        Seize (From_The_Set);
        Seize (To_The_Set);
        To_The_Set.The_Items := From_The_Set.The_Items;
        Release (From_The_Set);
        Release (To_The_Set);
    end Copy;

    procedure Clear (The_Set : in out Set) is
    begin
        Seize (The_Set);
        The_Set.The_Items := Items'(others => False);
        Release (The_Set);
    end Clear;

    procedure Add (The_Item : in Item; To_The_Set : in out Set) is
    begin
        Seize (To_The_Set);
        if To_The_Set.The_Items (The_Item) then
            Release (To_The_Set);
            raise Item_Is_In_Set;
        else
            To_The_Set.The_Items (The_Item) := True;
            Release (To_The_Set);
        end if;
    end Add;

    procedure Remove (The_Item : in Item; From_The_Set : in out Set) is
    begin
        Seize (From_The_Set);
        if not From_The_Set.The_Items (The_Item) then
            Release (From_The_Set);
            raise Item_Is_Not_In_Set;
        else
            From_The_Set.The_Items (The_Item) := False;
            Release (From_The_Set);
        end if;
    end Remove;

    procedure Union (Of_The_Set : in Set;
                     And_The_Set : in Set;
                     To_The_Set : in out Set) is
    begin
        Seize (Of_The_Set);
        Seize (And_The_Set);
        Seize (To_The_Set);
        To_The_Set.The_Items := Of_The_Set.The_Items or And_The_Set.The_Items;
        Release (Of_The_Set);
        Release (And_The_Set);
        Release (To_The_Set);
    end Union;

    procedure Intersection (Of_The_Set : in Set;
                            And_The_Set : in Set;
                            To_The_Set : in out Set) is
    begin
        Seize (Of_The_Set);
        Seize (And_The_Set);
        Seize (To_The_Set);
        To_The_Set.The_Items := Of_The_Set.The_Items and And_The_Set.The_Items;
        Release (Of_The_Set);
        Release (And_The_Set);
        Release (To_The_Set);
    end Intersection;

    procedure Difference (Of_The_Set : in Set;
                          And_The_Set : in Set;
                          To_The_Set : in out Set) is
    begin
        Seize (Of_The_Set);
        Seize (And_The_Set);
        Seize (To_The_Set);
        To_The_Set.The_Items := Of_The_Set.The_Items and
                                   (not And_The_Set.The_Items);
        Release (Of_The_Set);
        Release (And_The_Set);
        Release (To_The_Set);
    end Difference;

    function Is_Equal (Left : in Set; Right : in Set) return Boolean is
        Result : Boolean;
    begin
        Seize (Left);
        Seize (Right);
        Result := Left.The_Items = Right.The_Items;
        Release (Left);
        Release (Right);
        return Result;
    end Is_Equal;

    function Extent_Of (The_Set : in Set) return Natural is
        Count : Natural := 0;
    begin
        Seize (The_Set);
        for Index in The_Set.The_Items'Range loop
            if The_Set.The_Items (Index) then
                Count := Count + 1;
            end if;
        end loop;
        Release (The_Set);
        return Count;
    end Extent_Of;

    function Is_Empty (The_Set : in Set) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Set);
        Result := The_Set.The_Items = Items'(others => False);
        Release (The_Set);
        return Result;
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Set : in Set) return Boolean is
        Result : Boolean;
    begin
        Seize (Of_The_Set);
        Result := Of_The_Set.The_Items (The_Item);
        Release (Of_The_Set);
        return Result;
    end Is_A_Member;

    function Is_A_Subset (Left : in Set; Right : in Set) return Boolean is
        Intersection_Set : Items;
        Result : Boolean;
    begin
        Seize (Left);
        Seize (Right);
        Intersection_Set := Left.The_Items and Right.The_Items;
        Result := Left.The_Items = Intersection_Set;
        Release (Left);
        Release (Right);
        return Result;
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Set; Right : in Set) return Boolean is
        Intersection_Set : Items;
        Result : Boolean;
    begin
        Seize (Left);
        Seize (Right);
        Intersection_Set := Left.The_Items and Right.The_Items;
        Result := (Intersection_Set = Left.The_Items) and then
                     (Intersection_Set /= Right.The_Items);
        Release (Left);
        Release (Right);
        return Result;
    end Is_A_Proper_Subset;

    procedure Iterate (Over_The_Set : in Set) is
        Continue : Boolean := True;
    begin
        Seize (Over_The_Set);
        for The_Iterator in Over_The_Set.The_Items'Range loop
            if Over_The_Set.The_Items (The_Iterator) then
                Process (The_Iterator, Continue);
            end if;
            exit when not Continue;
        end loop;
        Release (Over_The_Set);
    end Iterate;

end Set_Discrete_Concurrent_Bounded_Managed_Iterator;
