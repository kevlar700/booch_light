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

package body Set_Simple_Sequential_Bounded_Managed_Iterator is

    procedure Copy (From_The_Set : in Set; To_The_Set : in out Set) is
    begin
        if From_The_Set.The_Back > To_The_Set.The_Size then
            raise Overflow;
        else
            To_The_Set.The_Items (1 .. From_The_Set.The_Back) :=
               From_The_Set.The_Items (1 .. From_The_Set.The_Back);
            To_The_Set.The_Back := From_The_Set.The_Back;
        end if;
    end Copy;

    procedure Clear (The_Set : in out Set) is
    begin
        The_Set.The_Back := 0;
    end Clear;

    procedure Add (The_Item : in Item; To_The_Set : in out Set) is
    begin
        for Index in 1 .. To_The_Set.The_Back loop
            if The_Item = To_The_Set.The_Items (Index) then
                raise Item_Is_In_Set;
            end if;
        end loop;
        To_The_Set.The_Items (To_The_Set.The_Back + 1) := The_Item;
        To_The_Set.The_Back := To_The_Set.The_Back + 1;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Add;

    procedure Remove (The_Item : in Item; From_The_Set : in out Set) is
    begin
        for Index in 1 .. From_The_Set.The_Back loop
            if The_Item = From_The_Set.The_Items (Index) then
                From_The_Set.The_Items (Index .. (From_The_Set.The_Back - 1)) :=
                   From_The_Set.The_Items ((Index + 1) ..
                                              From_The_Set.The_Back);
                From_The_Set.The_Back := From_The_Set.The_Back - 1;
                return;
            end if;
        end loop;
        raise Item_Is_Not_In_Set;
    end Remove;

    procedure Union (Of_The_Set : in Set;
                     And_The_Set : in Set;
                     To_The_Set : in out Set) is
        To_Index : Natural;
        To_Back : Natural;
    begin
        To_The_Set.The_Items (1 .. Of_The_Set.The_Back) :=
           Of_The_Set.The_Items (1 .. Of_The_Set.The_Back);
        To_The_Set.The_Back := Of_The_Set.The_Back;
        To_Back := To_The_Set.The_Back;
        for And_Index in 1 .. And_The_Set.The_Back loop
            To_Index := To_Back;
            while To_Index > 0 loop
                if To_The_Set.The_Items (To_Index) =
                   And_The_Set.The_Items (And_Index) then
                    exit;
                else
                    To_Index := To_Index - 1;
                end if;
            end loop;
            if To_Index = 0 then
                To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
                   And_The_Set.The_Items (And_Index);
                To_The_Set.The_Back := To_The_Set.The_Back + 1;
            end if;
        end loop;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Union;

    procedure Intersection (Of_The_Set : in Set;
                            And_The_Set : in Set;
                            To_The_Set : in out Set) is
        And_Index : Natural;
    begin
        To_The_Set.The_Back := 0;
        for Of_Index in 1 .. Of_The_Set.The_Back loop
            And_Index := And_The_Set.The_Back;
            while And_Index > 0 loop
                if Of_The_Set.The_Items (Of_Index) =
                   And_The_Set.The_Items (And_Index) then
                    To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
                       Of_The_Set.The_Items (Of_Index);
                    To_The_Set.The_Back := To_The_Set.The_Back + 1;
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

    procedure Difference (Of_The_Set : in Set;
                          And_The_Set : in Set;
                          To_The_Set : in out Set) is
        And_Index : Natural;
    begin
        To_The_Set.The_Back := 0;
        for Of_Index in 1 .. Of_The_Set.The_Back loop
            And_Index := And_The_Set.The_Back;
            while And_Index > 0 loop
                if Of_The_Set.The_Items (Of_Index) =
                   And_The_Set.The_Items (And_Index) then
                    exit;
                else
                    And_Index := And_Index - 1;
                end if;
            end loop;
            if And_Index = 0 then
                To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
                   Of_The_Set.The_Items (Of_Index);
                To_The_Set.The_Back := To_The_Set.The_Back + 1;
            end if;
        end loop;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Difference;

    function Is_Equal (Left : in Set; Right : in Set) return Boolean is
        Right_Index : Natural;
    begin
        if Left.The_Back /= Right.The_Back then
            return False;
        else
            for Left_Index in 1 .. Left.The_Back loop
                Right_Index := Right.The_Back;
                while Right_Index > 0 loop
                    if Left.The_Items (Left_Index) =
                       Right.The_Items (Right_Index) then
                        exit;
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

    function Extent_Of (The_Set : in Set) return Natural is
    begin
        return The_Set.The_Back;
    end Extent_Of;

    function Is_Empty (The_Set : in Set) return Boolean is
    begin
        return (The_Set.The_Back = 0);
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Set : in Set) return Boolean is
    begin
        for Index in 1 .. Of_The_Set.The_Back loop
            if Of_The_Set.The_Items (Index) = The_Item then
                return True;
            end if;
        end loop;
        return False;
    end Is_A_Member;

    function Is_A_Subset (Left : in Set; Right : in Set) return Boolean is
        Right_Index : Natural;
    begin
        for Left_Index in 1 .. Left.The_Back loop
            Right_Index := Right.The_Back;
            while Right_Index > 0 loop
                if Left.The_Items (Left_Index) =
                   Right.The_Items (Right_Index) then
                    exit;
                else
                    Right_Index := Right_Index - 1;
                end if;
            end loop;
            if Right_Index = 0 then
                return False;
            end if;
        end loop;
        return True;
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Set; Right : in Set) return Boolean is
        Right_Index : Natural;
    begin
        for Left_Index in 1 .. Left.The_Back loop
            Right_Index := Right.The_Back;
            while Right_Index > 0 loop
                if Left.The_Items (Left_Index) =
                   Right.The_Items (Right_Index) then
                    exit;
                else
                    Right_Index := Right_Index - 1;
                end if;
            end loop;
            if Right_Index = 0 then
                return False;
            end if;
        end loop;
        return (Left.The_Back < Right.The_Back);
    end Is_A_Proper_Subset;

    procedure Iterate (Over_The_Set : in Set) is
        Continue : Boolean;
    begin
        for The_Iterator in 1 .. Over_The_Set.The_Back loop
            Process (Over_The_Set.The_Items (The_Iterator), Continue);
            exit when not Continue;
        end loop;
    end Iterate;

end Set_Simple_Sequential_Bounded_Managed_Iterator;
