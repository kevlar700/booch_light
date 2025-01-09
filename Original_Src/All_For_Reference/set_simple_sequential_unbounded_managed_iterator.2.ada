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

with Storage_Manager_Sequential;
package body Set_Simple_Sequential_Unbounded_Managed_Iterator is

    type Node is
        record
            The_Item : Item;
            Next : Set;
        end record;

    procedure Free (The_Node : in out Node) is
    begin
        null;
    end Free;

    procedure Set_Next (The_Node : in out Node; To_Next : in Set) is
    begin
        The_Node.Next := To_Next;
    end Set_Next;

    function Next_Of (The_Node : in Node) return Set is
    begin
        return The_Node.Next;
    end Next_Of;

    package Node_Manager is
       new Storage_Manager_Sequential (Item => Node,
                                       Pointer => Set,
                                       Free => Free,
                                       Set_Pointer => Set_Next,
                                       Pointer_Of => Next_Of);

    procedure Copy (From_The_Set : in Set; To_The_Set : in out Set) is
        From_Index : Set := From_The_Set;
        To_Index : Set;
    begin
        Node_Manager.Free (To_The_Set);
        if From_The_Set /= null then
            To_The_Set := Node_Manager.New_Item;
            To_The_Set.The_Item := From_Index.The_Item;
            To_Index := To_The_Set;
            From_Index := From_Index.Next;
            while From_Index /= null loop
                To_Index.Next := Node_Manager.New_Item;
                To_Index := To_Index.Next;
                To_Index.The_Item := From_Index.The_Item;
                From_Index := From_Index.Next;
            end loop;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Set : in out Set) is
    begin
        Node_Manager.Free (The_Set);
    end Clear;

    procedure Add (The_Item : in Item; To_The_Set : in out Set) is
        Temporary_Node : Set;
        Index : Set := To_The_Set;
    begin
        while Index /= null loop
            if Index.The_Item = The_Item then
                raise Item_Is_In_Set;
            else
                Index := Index.Next;
            end if;
        end loop;
        Temporary_Node := Node_Manager.New_Item;
        Temporary_Node.The_Item := The_Item;
        Temporary_Node.Next := To_The_Set;
        To_The_Set := Temporary_Node;
    exception
        when Storage_Error =>
            raise Overflow;
    end Add;

    procedure Remove (The_Item : in Item; From_The_Set : in out Set) is
        Previous : Set;
        Index : Set := From_The_Set;
    begin
        while Index /= null loop
            if Index.The_Item = The_Item then
                if Previous = null then
                    From_The_Set := From_The_Set.Next;
                else
                    Previous.Next := Index.Next;
                end if;
                Index.Next := null;
                Node_Manager.Free (Index);
                return;
            else
                Previous := Index;
                Index := Index.Next;
            end if;
        end loop;
        raise Item_Is_Not_In_Set;
    end Remove;

    procedure Union (Of_The_Set : in Set;
                     And_The_Set : in Set;
                     To_The_Set : in out Set) is
        From_Index : Set := Of_The_Set;
        To_Index : Set;
        To_Top : Set;
        Temporary_Node : Set;
    begin
        Node_Manager.Free (To_The_Set);
        while From_Index /= null loop
            Temporary_Node := Node_Manager.New_Item;
            Temporary_Node.The_Item := From_Index.The_Item;
            Temporary_Node.Next := To_The_Set;
            To_The_Set := Temporary_Node;
            From_Index := From_Index.Next;
        end loop;
        From_Index := And_The_Set;
        To_Top := To_The_Set;
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
                Temporary_Node := Node_Manager.New_Item;
                Temporary_Node.The_Item := From_Index.The_Item;
                Temporary_Node.Next := To_The_Set;
                To_The_Set := Temporary_Node;
            end if;
            From_Index := From_Index.Next;
        end loop;
    exception
        when Storage_Error =>
            raise Overflow;
    end Union;

    procedure Intersection (Of_The_Set : in Set;
                            And_The_Set : in Set;
                            To_The_Set : in out Set) is
        Of_Index : Set := Of_The_Set;
        And_Index : Set;
        Temporary_Node : Set;
    begin
        Node_Manager.Free (To_The_Set);
        while Of_Index /= null loop
            And_Index := And_The_Set;
            while And_Index /= null loop
                if Of_Index.The_Item = And_Index.The_Item then
                    Temporary_Node := Node_Manager.New_Item;
                    Temporary_Node.The_Item := Of_Index.The_Item;
                    Temporary_Node.Next := To_The_Set;
                    To_The_Set := Temporary_Node;
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

    procedure Difference (Of_The_Set : in Set;
                          And_The_Set : in Set;
                          To_The_Set : in out Set) is
        Of_Index : Set := Of_The_Set;
        And_Index : Set;
        Temporary_Node : Set;
    begin
        Node_Manager.Free (To_The_Set);
        while Of_Index /= null loop
            And_Index := And_The_Set;
            while And_Index /= null loop
                if Of_Index.The_Item = And_Index.The_Item then
                    exit;
                else
                    And_Index := And_Index.Next;
                end if;
            end loop;
            if And_Index = null then
                Temporary_Node := Node_Manager.New_Item;
                Temporary_Node.The_Item := Of_Index.The_Item;
                Temporary_Node.Next := To_The_Set;
                To_The_Set := Temporary_Node;
            end if;
            Of_Index := Of_Index.Next;
        end loop;
    exception
        when Storage_Error =>
            raise Overflow;
    end Difference;

    function Is_Equal (Left : in Set; Right : in Set) return Boolean is
        Left_Count : Natural := 0;
        Right_Count : Natural := 0;
        Left_Index : Set := Left;
        Right_Index : Set;
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

    function Extent_Of (The_Set : in Set) return Natural is
        Count : Natural := 0;
        Index : Set := The_Set;
    begin
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        return Count;
    end Extent_Of;

    function Is_Empty (The_Set : in Set) return Boolean is
    begin
        return (The_Set = null);
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Set : in Set) return Boolean is
        Index : Set := Of_The_Set;
    begin
        while Index /= null loop
            if The_Item = Index.The_Item then
                return True;
            end if;
            Index := Index.Next;
        end loop;
        return False;
    end Is_A_Member;

    function Is_A_Subset (Left : in Set; Right : in Set) return Boolean is
        Left_Index : Set := Left;
        Right_Index : Set;
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
            else
                Left_Index := Left_Index.Next;
            end if;
        end loop;
        return True;
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Set; Right : in Set) return Boolean is
        Left_Count : Natural := 0;
        Right_Count : Natural := 0;
        Left_Index : Set := Left;
        Right_Index : Set;
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
        return (Left_Count < Right_Count);
    end Is_A_Proper_Subset;

    procedure Iterate (Over_The_Set : in Set) is
        The_Iterator : Set := Over_The_Set;
        Continue : Boolean;
    begin
        while The_Iterator /= null loop
            Process (The_Iterator.The_Item, Continue);
            exit when not Continue;
            The_Iterator := The_Iterator.Next;
        end loop;
    end Iterate;

end Set_Simple_Sequential_Unbounded_Managed_Iterator;
