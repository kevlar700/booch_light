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

package body Bag_Simple_Sequential_Bounded_Managed_Iterator is

    procedure Copy (From_The_Bag : in Bag; To_The_Bag : in out Bag) is
    begin
        if From_The_Bag.The_Back > To_The_Bag.The_Size then
            raise Overflow;
        else
            To_The_Bag.The_Items (1 .. From_The_Bag.The_Back) :=
               From_The_Bag.The_Items (1 .. From_The_Bag.The_Back);
            To_The_Bag.The_Back := From_The_Bag.The_Back;
        end if;
    end Copy;

    procedure Clear (The_Bag : in out Bag) is
    begin
        The_Bag.The_Back := 0;
    end Clear;

    procedure Add (The_Item : in Item; To_The_Bag : in out Bag) is
    begin
        for Index in 1 .. To_The_Bag.The_Back loop
            if The_Item = To_The_Bag.The_Items (Index).The_Item then
                To_The_Bag.The_Items (Index).The_Count :=
                   To_The_Bag.The_Items (Index).The_Count + 1;
                return;
            end if;
        end loop;
        To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Item := The_Item;
        To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Count := 1;
        To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Add;

    procedure Remove (The_Item : in Item; From_The_Bag : in out Bag) is
    begin
        for Index in 1 .. From_The_Bag.The_Back loop
            if The_Item = From_The_Bag.The_Items (Index).The_Item then
                if From_The_Bag.The_Items (Index).The_Count > 1 then
                    From_The_Bag.The_Items (Index).The_Count :=
                       From_The_Bag.The_Items (Index).The_Count - 1;
                else
                    From_The_Bag.The_Items (Index ..
                                               (From_The_Bag.The_Back - 1)) :=
                       From_The_Bag.The_Items ((Index + 1) ..
                                                  From_The_Bag.The_Back);
                    From_The_Bag.The_Back := From_The_Bag.The_Back - 1;
                end if;
                return;
            end if;
        end loop;
        raise Item_Is_Not_In_Bag;
    end Remove;

    procedure Union (Of_The_Bag : in Bag;
                     And_The_Bag : in Bag;
                     To_The_Bag : in out Bag) is
        To_Index : Natural;
        To_Back : Natural;
    begin
        To_The_Bag.The_Items (1 .. Of_The_Bag.The_Back) :=
           Of_The_Bag.The_Items (1 .. Of_The_Bag.The_Back);
        To_The_Bag.The_Back := Of_The_Bag.The_Back;
        To_Back := To_The_Bag.The_Back;
        for And_Index in 1 .. And_The_Bag.The_Back loop
            To_Index := To_Back;
            while To_Index > 0 loop
                if To_The_Bag.The_Items (To_Index).The_Item =
                   And_The_Bag.The_Items (And_Index).The_Item then
                    exit;
                else
                    To_Index := To_Index - 1;
                end if;
            end loop;
            if To_Index = 0 then
                To_The_Bag.The_Items (To_The_Bag.The_Back + 1) :=
                   And_The_Bag.The_Items (And_Index);
                To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
            else
                To_The_Bag.The_Items (To_Index).The_Count :=
                   To_The_Bag.The_Items (To_Index).The_Count +
                      And_The_Bag.The_Items (And_Index).The_Count;
            end if;
        end loop;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Union;

    procedure Intersection (Of_The_Bag : in Bag;
                            And_The_Bag : in Bag;
                            To_The_Bag : in out Bag) is
        And_Index : Natural;
    begin
        To_The_Bag.The_Back := 0;
        for Of_Index in 1 .. Of_The_Bag.The_Back loop
            And_Index := And_The_Bag.The_Back;
            while And_Index > 0 loop
                if Of_The_Bag.The_Items (Of_Index).The_Item =
                   And_The_Bag.The_Items (And_Index).The_Item then
                    if Of_The_Bag.The_Items (Of_Index).The_Count <
                       And_The_Bag.The_Items (And_Index).The_Count then
                        To_The_Bag.The_Items (To_The_Bag.The_Back + 1).
                           The_Item := Of_The_Bag.The_Items (Of_Index).The_Item;
                        To_The_Bag.The_Items (To_The_Bag.The_Back + 1).
                           The_Count :=
                           Of_The_Bag.The_Items (Of_Index).The_Count;
                        To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
                    else
                        To_The_Bag.The_Items (To_The_Bag.The_Back + 1).
                           The_Item := Of_The_Bag.The_Items (Of_Index).The_Item;
                        To_The_Bag.The_Items (To_The_Bag.The_Back + 1).
                           The_Count :=
                           And_The_Bag.The_Items (And_Index).The_Count;
                        To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
                    end if;
                    exit;
                else
                    And_Index := And_Index - 1;
                end if;
            end loop;
        end loop;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Intersection;

    procedure Difference (Of_The_Bag : in Bag;
                          And_The_Bag : in Bag;
                          To_The_Bag : in out Bag) is
        And_Index : Natural;
    begin
        To_The_Bag.The_Back := 0;
        for Of_Index in 1 .. Of_The_Bag.The_Back loop
            And_Index := And_The_Bag.The_Back;
            while And_Index > 0 loop
                if Of_The_Bag.The_Items (Of_Index).The_Item =
                   And_The_Bag.The_Items (And_Index).The_Item then
                    exit;
                else
                    And_Index := And_Index - 1;
                end if;
            end loop;
            if And_Index = 0 then
                To_The_Bag.The_Items (To_The_Bag.The_Back + 1) :=
                   Of_The_Bag.The_Items (Of_Index);
                To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
            elsif Of_The_Bag.The_Items (Of_Index).The_Count >
                  And_The_Bag.The_Items (And_Index).The_Count then
                To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Item :=
                   Of_The_Bag.The_Items (Of_Index).The_Item;
                To_The_Bag.The_Items (To_The_Bag.The_Back + 1).The_Count :=
                   Of_The_Bag.The_Items (Of_Index).The_Count -
                      And_The_Bag.The_Items (And_Index).The_Count;
                To_The_Bag.The_Back := To_The_Bag.The_Back + 1;
            end if;
        end loop;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Difference;

    function Is_Equal (Left : in Bag; Right : in Bag) return Boolean is
        Right_Index : Natural;
    begin
        if Left.The_Back /= Right.The_Back then
            return False;
        else
            for Left_Index in 1 .. Left.The_Back loop
                Right_Index := Right.The_Back;
                while Right_Index > 0 loop
                    if Left.The_Items (Left_Index).The_Item =
                       Right.The_Items (Right_Index).The_Item then
                        if Left.The_Items (Left_Index).The_Count /=
                           Right.The_Items (Right_Index).The_Count then
                            return False;
                        else
                            exit;
                        end if;
                    else
                        Right_Index := Right_Index - 1;
                    end if;
                end loop;
                if Right_Index = 0 then
                    return False;
                end if;
            end loop;
            return True;
        end if;
    end Is_Equal;

    function Extent_Of (The_Bag : in Bag) return Natural is
        Count : Natural := 0;
    begin
        for Index in 1 .. The_Bag.The_Back loop
            Count := Count + The_Bag.The_Items (Index).The_Count;
        end loop;
        return Count;
    end Extent_Of;

    function Unique_Extent_Of (The_Bag : in Bag) return Natural is
    begin
        return The_Bag.The_Back;
    end Unique_Extent_Of;

    function Number_Of
                (The_Item : in Item; In_The_Bag : in Bag) return Positive is
    begin
        for Index in 1 .. In_The_Bag.The_Back loop
            if The_Item = In_The_Bag.The_Items (Index).The_Item then
                return In_The_Bag.The_Items (Index).The_Count;
            end if;
        end loop;
        raise Item_Is_Not_In_Bag;
    end Number_Of;

    function Is_Empty (The_Bag : in Bag) return Boolean is
    begin
        return (The_Bag.The_Back = 0);
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Bag : in Bag) return Boolean is
    begin
        for Index in 1 .. Of_The_Bag.The_Back loop
            if Of_The_Bag.The_Items (Index).The_Item = The_Item then
                return True;
            end if;
        end loop;
        return False;
    end Is_A_Member;

    function Is_A_Subset (Left : in Bag; Right : in Bag) return Boolean is
        Right_Index : Natural;
    begin
        for Left_Index in 1 .. Left.The_Back loop
            Right_Index := Right.The_Back;
            while Right_Index > 0 loop
                if Left.The_Items (Left_Index).The_Item =
                   Right.The_Items (Right_Index).The_Item then
                    exit;
                else
                    Right_Index := Right_Index - 1;
                end if;
            end loop;
            if Right_Index = 0 then
                return False;
            elsif Left.The_Items (Left_Index).The_Count >
                  Right.The_Items (Right_Index).The_Count then
                return False;
            end if;
        end loop;
        return True;
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Bag; Right : in Bag) return Boolean is
        Total_Left_Count : Natural := 0;
        Total_Right_Count : Natural := 0;
        Right_Index : Natural;
    begin
        for Left_Index in 1 .. Left.The_Back loop
            Right_Index := Right.The_Back;
            while Right_Index > 0 loop
                if Left.The_Items (Left_Index).The_Item =
                   Right.The_Items (Right_Index).The_Item then
                    exit;
                else
                    Right_Index := Right_Index - 1;
                end if;
            end loop;
            if Right_Index = 0 then
                return False;
            elsif Left.The_Items (Left_Index).The_Count >
                  Right.The_Items (Right_Index).The_Count then
                return False;
            end if;
            Total_Left_Count := Total_Left_Count +
                                   Left.The_Items (Left_Index).The_Count;
        end loop;
        for Index in 1 .. Right.The_Back loop
            Total_Right_Count := Total_Right_Count +
                                    Right.The_Items (Index).The_Count;
        end loop;
        if Left.The_Back < Right.The_Back then
            return True;
        elsif Left.The_Back > Right.The_Back then
            return False;
        else
            return (Total_Left_Count < Total_Right_Count);
        end if;
    end Is_A_Proper_Subset;

    procedure Iterate (Over_The_Bag : in Bag) is
        Continue : Boolean;
    begin
        for The_Iterator in 1 .. Over_The_Bag.The_Back loop
            Process (Over_The_Bag.The_Items (The_Iterator).The_Item,
                     Over_The_Bag.The_Items (The_Iterator).The_Count, Continue);
            exit when not Continue;
        end loop;
    end Iterate;

end Bag_Simple_Sequential_Bounded_Managed_Iterator;
