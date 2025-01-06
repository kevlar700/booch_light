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

package body List_Double_Unbounded_Unmanaged is

    type Node is
        record
            Previous : List;
            The_Item : Item;
            Next : List;
        end record;

    procedure Copy (From_The_List : in List; To_The_List : in out List) is
        From_Index : List := From_The_List;
        To_Index : List;
    begin
        if From_The_List = null then
            To_The_List := null;
        else
            To_The_List := new Node'(Previous => null, 
                                     The_Item => From_Index.The_Item, 
                                     Next => null);
            To_Index := To_The_List;
            From_Index := From_Index.Next;
            while From_Index /= null loop
                To_Index.Next := new Node'(Previous => To_Index, 
                                           The_Item => From_Index.The_Item, 
                                           Next => null);
                To_Index := To_Index.Next;
                From_Index := From_Index.Next;
            end loop;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_List : in out List) is
    begin
        The_List := null;
    end Clear;

    procedure Construct (The_Item : in Item; And_The_List : in out List) is
    begin
        if And_The_List = null then
            And_The_List := 
               new Node'(Previous => null, The_Item => The_Item, Next => null);
        elsif And_The_List.Previous = null then
            And_The_List := new Node'(Previous => null, 
                                      The_Item => The_Item, 
                                      Next => And_The_List);
            And_The_List.Next.Previous := And_The_List;
        else
            raise Not_At_Head;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Construct;

    procedure Set_Head (Of_The_List : in out List; To_The_Item : in Item) is
    begin
        Of_The_List.The_Item := To_The_Item;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Set_Head;

    procedure Swap_Tail (Of_The_List : in out List; 
                         And_The_List : in out List) is
        Temporary_Node : List;
    begin
        if And_The_List = null then
            if Of_The_List.Next /= null then
                Temporary_Node := Of_The_List.Next;
                Temporary_Node.Previous := null;
                Of_The_List.Next := null;
                And_The_List := Temporary_Node;
            end if;
        elsif And_The_List.Previous = null then
            if Of_The_List.Next /= null then
                Temporary_Node := Of_The_List.Next;
                Temporary_Node.Previous := null;
                Of_The_List.Next := And_The_List;
                And_The_List.Previous := Of_The_List;
                And_The_List := Temporary_Node;
            else
                And_The_List.Previous := Of_The_List;
                Of_The_List.Next := And_The_List;
                And_The_List := null;
            end if;
        else
            raise Not_At_Head;
        end if;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Swap_Tail;

    function Is_Equal (Left : in List; Right : in List) return Boolean is
        Left_Index : List := Left;
        Right_Index : List := Right;
    begin
        while Left_Index /= null loop
            if Left_Index.The_Item /= Right_Index.The_Item then
                return False;
            end if;
            Left_Index := Left_Index.Next;
            Right_Index := Right_Index.Next;
        end loop;
        return (Right_Index = null);
    exception
        when Constraint_Error =>
            return False;
    end Is_Equal;

    function Length_Of (The_List : in List) return Natural is
        Count : Natural := 0;
        Index : List := The_List;
    begin
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        return Count;
    end Length_Of;

    function Is_Null (The_List : in List) return Boolean is
    begin
        return (The_List = null);
    end Is_Null;

    function Head_Of (The_List : in List) return Item is
    begin
        return The_List.The_Item;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Head_Of;

    function Tail_Of (The_List : in List) return List is
    begin
        return The_List.Next;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Tail_Of;

    function Predecessor_Of (The_List : in List) return List is
    begin
        return The_List.Previous;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Predecessor_Of;

end List_Double_Unbounded_Unmanaged;
