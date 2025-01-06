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

package body Bag_Discrete_Multiple_Bounded_Managed_Iterator is

    procedure Seize_For_Reading (The_Bag : in Bag) is
    begin
        Monitor.Start_Reading (The_Bag.Guard);
    end Seize_For_Reading;

    procedure Release_For_Reading (The_Bag : in Bag) is
    begin
        Monitor.Stop_Reading (The_Bag.Guard);
    end Release_For_Reading;

    procedure Seize_For_Writing (The_Bag : in Bag) is
    begin
        Monitor.Start_Writing (The_Bag.Guard);
    end Seize_For_Writing;

    procedure Release_For_Writing (The_Bag : in Bag) is
    begin
        Monitor.Stop_Writing (The_Bag.Guard);
    end Release_For_Writing;

    procedure Copy (From_The_Bag : in Bag; To_The_Bag : in out Bag) is
    begin
        Seize_For_Reading (From_The_Bag);
        Seize_For_Writing (To_The_Bag);
        To_The_Bag.The_Items := From_The_Bag.The_Items;
        Release_For_Reading (From_The_Bag);
        Release_For_Writing (To_The_Bag);
    end Copy;

    procedure Clear (The_Bag : in out Bag) is
    begin
        Seize_For_Writing (The_Bag);
        The_Bag.The_Items := Items'(others => 0);
        Release_For_Writing (The_Bag);
    end Clear;

    procedure Add (The_Item : in Item; To_The_Bag : in out Bag) is
    begin
        Seize_For_Writing (To_The_Bag);
        To_The_Bag.The_Items (The_Item) := To_The_Bag.The_Items (The_Item) + 1;
        Release_For_Writing (To_The_Bag);
    end Add;

    procedure Remove (The_Item : in Item; From_The_Bag : in out Bag) is
    begin
        Seize_For_Writing (From_The_Bag);
        From_The_Bag.The_Items (The_Item) :=
           From_The_Bag.The_Items (The_Item) - 1;
        Release_For_Writing (From_The_Bag);
    exception
        when Constraint_Error =>
            Release_For_Writing (From_The_Bag);
            raise Item_Is_Not_In_Bag;
    end Remove;

    procedure Union (Of_The_Bag : in Bag;
                     And_The_Bag : in Bag;
                     To_The_Bag : in out Bag) is
    begin
        Seize_For_Reading (Of_The_Bag);
        Seize_For_Reading (And_The_Bag);
        Seize_For_Writing (To_The_Bag);
        for Index in To_The_Bag.The_Items'Range loop
            To_The_Bag.The_Items (Index) :=
               Of_The_Bag.The_Items (Index) + And_The_Bag.The_Items (Index);
        end loop;
        Release_For_Reading (Of_The_Bag);
        Release_For_Reading (And_The_Bag);
        Release_For_Writing (To_The_Bag);
    end Union;

    procedure Intersection (Of_The_Bag : in Bag;
                            And_The_Bag : in Bag;
                            To_The_Bag : in out Bag) is
    begin
        Seize_For_Reading (Of_The_Bag);
        Seize_For_Reading (And_The_Bag);
        Seize_For_Writing (To_The_Bag);
        for Index in To_The_Bag.The_Items'Range loop
            if Of_The_Bag.The_Items (Index) < And_The_Bag.The_Items (Index) then
                To_The_Bag.The_Items (Index) := Of_The_Bag.The_Items (Index);
            else
                To_The_Bag.The_Items (Index) := And_The_Bag.The_Items (Index);
            end if;
        end loop;
        Release_For_Reading (Of_The_Bag);
        Release_For_Reading (And_The_Bag);
        Release_For_Writing (To_The_Bag);
    end Intersection;

    procedure Difference (Of_The_Bag : in Bag;
                          And_The_Bag : in Bag;
                          To_The_Bag : in out Bag) is
    begin
        Seize_For_Reading (Of_The_Bag);
        Seize_For_Reading (And_The_Bag);
        Seize_For_Writing (To_The_Bag);
        for Index in To_The_Bag.The_Items'Range loop
            if Of_The_Bag.The_Items (Index) > And_The_Bag.The_Items (Index) then
                To_The_Bag.The_Items (Index) :=
                   Of_The_Bag.The_Items (Index) - And_The_Bag.The_Items (Index);
            else
                To_The_Bag.The_Items (Index) := 0;
            end if;
        end loop;
        Release_For_Reading (Of_The_Bag);
        Release_For_Reading (And_The_Bag);
        Release_For_Writing (To_The_Bag);
    end Difference;

    function Is_Equal (Left : in Bag; Right : in Bag) return Boolean is
        Result : Boolean;
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        Result := Left.The_Items = Right.The_Items;
        Release_For_Reading (Left);
        Release_For_Reading (Right);
        return Result;
    end Is_Equal;

    function Extent_Of (The_Bag : in Bag) return Natural is
        Count : Natural := 0;
    begin
        Seize_For_Reading (The_Bag);
        for Index in The_Bag.The_Items'Range loop
            Count := Count + The_Bag.The_Items (Index);
        end loop;
        Release_For_Reading (The_Bag);
        return Count;
    end Extent_Of;

    function Unique_Extent_Of (The_Bag : in Bag) return Natural is
        Count : Natural := 0;
    begin
        Seize_For_Reading (The_Bag);
        for Index in The_Bag.The_Items'Range loop
            if The_Bag.The_Items (Index) > 0 then
                Count := Count + 1;
            end if;
        end loop;
        Release_For_Reading (The_Bag);
        return Count;
    end Unique_Extent_Of;

    function Number_Of
                (The_Item : in Item; In_The_Bag : in Bag) return Positive is
        Result : Positive;
    begin
        Seize_For_Reading (In_The_Bag);
        if In_The_Bag.The_Items (The_Item) = 0 then
            Release_For_Reading (In_The_Bag);
            raise Item_Is_Not_In_Bag;
        else
            Result := In_The_Bag.The_Items (The_Item);
            Release_For_Reading (In_The_Bag);
            return Result;
        end if;
    end Number_Of;

    function Is_Empty (The_Bag : in Bag) return Boolean is
        Result : Boolean;
    begin
        Seize_For_Reading (The_Bag);
        Result := The_Bag.The_Items = Items'(others => 0);
        Release_For_Reading (The_Bag);
        return Result;
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Bag : in Bag) return Boolean is
        Result : Boolean;
    begin
        Seize_For_Reading (Of_The_Bag);
        Result := Of_The_Bag.The_Items (The_Item) > 0;
        Release_For_Reading (Of_The_Bag);
        return Result;
    end Is_A_Member;

    function Is_A_Subset (Left : in Bag; Right : in Bag) return Boolean is
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        for Index in Left.The_Items'Range loop
            if Left.The_Items (Index) > Right.The_Items (Index) then
                Release_For_Reading (Left);
                Release_For_Reading (Right);
                return False;
            end if;
        end loop;
        Release_For_Reading (Left);
        Release_For_Reading (Right);
        return True;
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Bag; Right : in Bag) return Boolean is
        Unique_Left_Count : Natural := 0;
        Unique_Right_Count : Natural := 0;
        Total_Left_Count : Natural := 0;
        Total_Right_Count : Natural := 0;
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        for Index in Left.The_Items'Range loop
            if Left.The_Items (Index) > Right.The_Items (Index) then
                Release_For_Reading (Left);
                Release_For_Reading (Right);
                return False;
            end if;
            if Left.The_Items (Index) > 0 then
                Unique_Left_Count := Unique_Left_Count + 1;
                Total_Left_Count := Total_Left_Count + Left.The_Items (Index);
            end if;
            if Right.The_Items (Index) > 0 then
                Unique_Right_Count := Unique_Right_Count + 1;
                Total_Right_Count := Total_Right_Count +
                                        Right.The_Items (Index);
            end if;
        end loop;
        if Unique_Left_Count < Unique_Right_Count then
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return True;
        elsif Unique_Left_Count > Unique_Right_Count then
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return False;
        else
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return (Total_Left_Count < Total_Right_Count);
        end if;
    end Is_A_Proper_Subset;

    procedure Iterate (Over_The_Bag : in Bag) is
        Continue : Boolean := True;
    begin
        Seize_For_Reading (Over_The_Bag);
        for The_Iterator in Over_The_Bag.The_Items'Range loop
            if Over_The_Bag.The_Items (The_Iterator) > 0 then
                Process (The_Iterator,
                         Over_The_Bag.The_Items (The_Iterator), Continue);
            end if;
            exit when not Continue;
        end loop;
        Release_For_Reading (Over_The_Bag);
    end Iterate;

end Bag_Discrete_Multiple_Bounded_Managed_Iterator;
