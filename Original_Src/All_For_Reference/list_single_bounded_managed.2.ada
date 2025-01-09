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

package body List_Single_Bounded_Managed is

    type Node is
        record
            The_Item : Item;
            Next : List;
        end record;

    Heap : array (Positive range 1 .. The_Size) of Node;

    Free_List : List;

    procedure Free (The_List : in out List) is
        Temporary_Node : List;
    begin
        while The_List /= Null_List loop
            Temporary_Node := The_List;
            The_List := Heap (The_List.The_Head).Next;
            Heap (Temporary_Node.The_Head).Next := Free_List;
            Free_List := Temporary_Node;
        end loop;
    end Free;

    function New_Item return List is
        Temporary_Node : List;
    begin
        if Free_List = Null_List then
            raise Storage_Error;
        else
            Temporary_Node := Free_List;
            Free_List := Heap (Free_List.The_Head).Next;
            Heap (Temporary_Node.The_Head).Next := Null_List;
            return Temporary_Node;
        end if;
    end New_Item;

    procedure Copy (From_The_List : in List; To_The_List : in out List) is
        From_Index : List := From_The_List;
        To_Index : List;
    begin
        Free (To_The_List);
        if From_The_List /= Null_List then
            To_The_List := New_Item;
            Heap (To_The_List.The_Head).The_Item :=
               Heap (From_Index.The_Head).The_Item;
            To_Index := To_The_List;
            From_Index := Heap (From_Index.The_Head).Next;
            while From_Index /= Null_List loop
                Heap (To_Index.The_Head).Next := New_Item;
                To_Index := Heap (To_Index.The_Head).Next;
                Heap (To_Index.The_Head).The_Item :=
                   Heap (From_Index.The_Head).The_Item;
                From_Index := Heap (From_Index.The_Head).Next;
            end loop;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_List : in out List) is
    begin
        Free (The_List);
    end Clear;

    procedure Construct (The_Item : in Item; And_The_List : in out List) is
        Temporary_Node : List;
    begin
        Temporary_Node := New_Item;
        Heap (Temporary_Node.The_Head).The_Item := The_Item;
        Heap (Temporary_Node.The_Head).Next := And_The_List;
        And_The_List := Temporary_Node;
    exception
        when Storage_Error =>
            raise Overflow;
    end Construct;

    procedure Set_Head (Of_The_List : in out List; To_The_Item : in Item) is
    begin
        Heap (Of_The_List.The_Head).The_Item := To_The_Item;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Set_Head;

    procedure Swap_Tail (Of_The_List : in out List;
                         And_The_List : in out List) is
        Temporary_Node : List;
    begin
        Temporary_Node := Heap (Of_The_List.The_Head).Next;
        Heap (Of_The_List.The_Head).Next := And_The_List;
        And_The_List := Temporary_Node;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Swap_Tail;

    function Is_Equal (Left : in List; Right : in List) return Boolean is
        Left_Index : List := Left;
        Right_Index : List := Right;
    begin
        while Left_Index /= Null_List loop
            if Heap (Left_Index.The_Head).The_Item /=
               Heap (Right_Index.The_Head).The_Item then
                return False;
            end if;
            Left_Index := Heap (Left_Index.The_Head).Next;
            Right_Index := Heap (Right_Index.The_Head).Next;
        end loop;
        return (Right_Index = Null_List);
    exception
        when Constraint_Error =>
            return False;
    end Is_Equal;

    function Length_Of (The_List : in List) return Natural is
        Count : Natural := 0;
        Index : List := The_List;
    begin
        while Index /= Null_List loop
            Count := Count + 1;
            Index := Heap (Index.The_Head).Next;
        end loop;
        return Count;
    end Length_Of;

    function Is_Null (The_List : in List) return Boolean is
    begin
        return (The_List = Null_List);
    end Is_Null;

    function Head_Of (The_List : in List) return Item is
    begin
        return Heap (The_List.The_Head).The_Item;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Head_Of;

    function Tail_Of (The_List : in List) return List is
    begin
        return Heap (The_List.The_Head).Next;
    exception
        when Constraint_Error =>
            raise List_Is_Null;
    end Tail_Of;

begin
    Free_List.The_Head := 1;
    for Index in 1 .. (The_Size - 1) loop
        Heap (Index).Next := List'(The_Head => (Index + 1));
    end loop;
    Heap (The_Size).Next := Null_List;
end List_Single_Bounded_Managed;
