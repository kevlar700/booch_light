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

package body Bag_Simple_Sequential_Unbounded_Unmanaged_Noniterator is

    type Node is
        record
            The_Item : Item;
            The_Count : Positive;
            Next : Bag;
        end record;

    procedure Copy (From_The_Bag : in Bag; To_The_Bag : in out Bag) is
        From_Index : Bag := From_The_Bag;
        To_Index : Bag;
    begin
        if From_The_Bag = null then
            To_The_Bag := null;
        else
            To_The_Bag := new Node'(The_Item => From_Index.The_Item,
                                    The_Count => From_Index.The_Count,
                                    Next => null);
            To_Index := To_The_Bag;
            From_Index := From_Index.Next;
            while From_Index /= null loop
                To_Index.Next := new Node'(The_Item => From_Index.The_Item,
                                           The_Count => From_Index.The_Count,
                                           Next => null);
                To_Index := To_Index.Next;
                From_Index := From_Index.Next;
            end loop;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Bag : in out Bag) is
    begin
        The_Bag := null;
    end Clear;

    procedure Add (The_Item : in Item; To_The_Bag : in out Bag) is
        Index : Bag := To_The_Bag;
    begin
        while Index /= null loop
            if Index.The_Item = The_Item then
                Index.The_Count := Index.The_Count + 1;
                return;
            else
                Index := Index.Next;
            end if;
        end loop;
        To_The_Bag := new Node'(The_Item => The_Item,
                                The_Count => 1,
                                Next => To_The_Bag);
    exception
        when Storage_Error =>
            raise Overflow;
    end Add;

    procedure Remove (The_Item : in Item; From_The_Bag : in out Bag) is
        Previous : Bag;
        Index : Bag := From_The_Bag;
    begin
        while Index /= null loop
            if Index.The_Item = The_Item then
                if Index.The_Count > 1 then
                    Index.The_Count := Index.The_Count - 1;
                elsif Previous = null then
                    From_The_Bag := From_The_Bag.Next;
                else
                    Previous.Next := Index.Next;
                end if;
                return;
            else
                Previous := Index;
                Index := Index.Next;
            end if;
        end loop;
        raise Item_Is_Not_In_Bag;
    end Remove;

    procedure Union (Of_The_Bag : in Bag;
                     And_The_Bag : in Bag;
                     To_The_Bag : in out Bag) is
        From_Index : Bag := Of_The_Bag;
        To_Index : Bag;
        To_Top : Bag;
    begin
        To_The_Bag := null;
        while From_Index /= null loop
            To_The_Bag := new Node'(The_Item => From_Index.The_Item,
                                    The_Count => From_Index.The_Count,
                                    Next => To_The_Bag);
            From_Index := From_Index.Next;
        end loop;
        From_Index := And_The_Bag;
        To_Top := To_The_Bag;
        while From_Index /= null loop
            To_Index := To_Top;
            while To_Index /= null loop
                if From_Index.The_Item = To_Index.The_Item then
                    exit;
                else
                    To_Index := To_Index.Next;
                end if;
            end loop;
            if To_Index = null then
                To_The_Bag := new Node'(The_Item => From_Index.The_Item,
                                        The_Count => From_Index.The_Count,
                                        Next => To_The_Bag);
            else
                To_Index.The_Count := To_Index.The_Count + From_Index.The_Count;
            end if;
            From_Index := From_Index.Next;
        end loop;
    exception
        when Storage_Error =>
            raise Overflow;
    end Union;

    procedure Intersection (Of_The_Bag : in Bag;
                            And_The_Bag : in Bag;
                            To_The_Bag : in out Bag) is
        Of_Index : Bag := Of_The_Bag;
        And_Index : Bag;
    begin
        To_The_Bag := null;
        while Of_Index /= null loop
            And_Index := And_The_Bag;
            while And_Index /= null loop
                if Of_Index.The_Item = And_Index.The_Item then
                    if Of_Index.The_Count < And_Index.The_Count then
                        To_The_Bag := new Node'(The_Item => Of_Index.The_Item,
                                                The_Count => Of_Index.The_Count,
                                                Next => To_The_Bag);
                    else
                        To_The_Bag := new Node'
                                             (The_Item => And_Index.The_Item,
                                              The_Count => And_Index.The_Count,
                                              Next => To_The_Bag);
                    end if;
                    exit;
                else
                    And_Index := And_Index.Next;
                end if;
            end loop;
            Of_Index := Of_Index.Next;
        end loop;
    exception
        when Storage_Error =>
            raise Overflow;
    end Intersection;

    procedure Difference (Of_The_Bag : in Bag;
                          And_The_Bag : in Bag;
                          To_The_Bag : in out Bag) is
        Of_Index : Bag := Of_The_Bag;
        And_Index : Bag;
    begin
        To_The_Bag := null;
        while Of_Index /= null loop
            And_Index := And_The_Bag;
            while And_Index /= null loop
                if Of_Index.The_Item = And_Index.The_Item then
                    exit;
                else
                    And_Index := And_Index.Next;
                end if;
            end loop;
            if And_Index = null then
                To_The_Bag := new Node'(The_Item => Of_Index.The_Item,
                                        The_Count => Of_Index.The_Count,
                                        Next => To_The_Bag);
            elsif Of_Index.The_Count > And_Index.The_Count then
                To_The_Bag := new Node'(The_Item => Of_Index.The_Item,
                                        The_Count => Of_Index.The_Count -
                                                        And_Index.The_Count,
                                        Next => To_The_Bag);
            end if;
            Of_Index := Of_Index.Next;
        end loop;
    exception
        when Storage_Error =>
            raise Overflow;
    end Difference;

    function Is_Equal (Left : in Bag; Right : in Bag) return Boolean is
        Left_Count : Natural := 0;
        Right_Count : Natural := 0;
        Left_Index : Bag := Left;
        Right_Index : Bag;
    begin
        while Left_Index /= null loop
            Right_Index := Right;
            while Right_Index /= null loop
                if Left_Index.The_Item = Right_Index.The_Item then
                    exit;
                else
                    Right_Index := Right_Index.Next;
                end if;
            end loop;
            if Right_Index = null then
                return False;
            elsif Left_Index.The_Count /= Right_Index.The_Count then
                return False;
            else
                Left_Count := Left_Count + 1;
                Left_Index := Left_Index.Next;
            end if;
        end loop;
        Right_Index := Right;
        while Right_Index /= null loop
            Right_Count := Right_Count + 1;
            Right_Index := Right_Index.Next;
        end loop;
        return (Left_Count = Right_Count);
    end Is_Equal;

    function Extent_Of (The_Bag : in Bag) return Natural is
        Count : Natural := 0;
        Index : Bag := The_Bag;
    begin
        while Index /= null loop
            Count := Count + Index.The_Count;
            Index := Index.Next;
        end loop;
        return Count;
    end Extent_Of;

    function Unique_Extent_Of (The_Bag : in Bag) return Natural is
        Count : Natural := 0;
        Index : Bag := The_Bag;
    begin
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        return Count;
    end Unique_Extent_Of;

    function Number_Of
                (The_Item : in Item; In_The_Bag : in Bag) return Positive is
        Index : Bag := In_The_Bag;
    begin
        while Index /= null loop
            if The_Item = Index.The_Item then
                return Index.The_Count;
            else
                Index := Index.Next;
            end if;
        end loop;
        raise Item_Is_Not_In_Bag;
    end Number_Of;

    function Is_Empty (The_Bag : in Bag) return Boolean is
    begin
        return (The_Bag = null);
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Bag : in Bag) return Boolean is
        Index : Bag := Of_The_Bag;
    begin
        while Index /= null loop
            if The_Item = Index.The_Item then
                return True;
            end if;
            Index := Index.Next;
        end loop;
        return False;
    end Is_A_Member;

    function Is_A_Subset (Left : in Bag; Right : in Bag) return Boolean is
        Left_Index : Bag := Left;
        Right_Index : Bag;
    begin
        while Left_Index /= null loop
            Right_Index := Right;
            while Right_Index /= null loop
                if Left_Index.The_Item = Right_Index.The_Item then
                    exit;
                else
                    Right_Index := Right_Index.Next;
                end if;
            end loop;
            if Right_Index = null then
                return False;
            elsif Left_Index.The_Count > Right_Index.The_Count then
                return False;
            else
                Left_Index := Left_Index.Next;
            end if;
        end loop;
        return True;
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Bag; Right : in Bag) return Boolean is
        Unique_Left_Count : Natural := 0;
        Unique_Right_Count : Natural := 0;
        Total_Left_Count : Natural := 0;
        Total_Right_Count : Natural := 0;
        Left_Index : Bag := Left;
        Right_Index : Bag;
    begin
        while Left_Index /= null loop
            Right_Index := Right;
            while Right_Index /= null loop
                if Left_Index.The_Item = Right_Index.The_Item then
                    exit;
                else
                    Right_Index := Right_Index.Next;
                end if;
            end loop;
            if Right_Index = null then
                return False;
            elsif Left_Index.The_Count > Right_Index.The_Count then
                return False;
            else
                Unique_Left_Count := Unique_Left_Count + 1;
                Total_Left_Count := Total_Left_Count + Left_Index.The_Count;
                Left_Index := Left_Index.Next;
            end if;
        end loop;
        Right_Index := Right;
        while Right_Index /= null loop
            Unique_Right_Count := Unique_Right_Count + 1;
            Total_Right_Count := Total_Right_Count + Right_Index.The_Count;
            Right_Index := Right_Index.Next;
        end loop;
        if Unique_Left_Count < Unique_Right_Count then
            return True;
        elsif Unique_Left_Count > Unique_Right_Count then
            return False;
        else
            return (Total_Left_Count < Total_Right_Count);
        end if;
    end Is_A_Proper_Subset;

end Bag_Simple_Sequential_Unbounded_Unmanaged_Noniterator;
