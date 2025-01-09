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

with Storage_Manager_Concurrent;
package body Deque_Nonpriority_Nonbalking_Sequential_Unbounded_Controlled_Iterator is

    type Node is
        record
            The_Item : Item;
            Next : Structure;
        end record;

    procedure Free (The_Node : in out Node) is
    begin
        null;
    end Free;

    procedure Set_Next (The_Node : in out Node; To_Next : in Structure) is
    begin
        The_Node.Next := To_Next;
    end Set_Next;

    function Next_Of (The_Node : in Node) return Structure is
    begin
        return The_Node.Next;
    end Next_Of;

    package Node_Manager is
       new Storage_Manager_Concurrent (Item => Node,
                                       Pointer => Structure,
                                       Free => Free,
                                       Set_Pointer => Set_Next,
                                       Pointer_Of => Next_Of);

    procedure Copy (From_The_Deque : in Deque; To_The_Deque : in out Deque) is
        From_Index : Structure := From_The_Deque.The_Front;
        To_Index : Structure;
    begin
        Node_Manager.Free (To_The_Deque.The_Front);
        To_The_Deque.The_Back := null;
        if From_The_Deque.The_Front /= null then
            To_The_Deque.The_Front := Node_Manager.New_Item;
            To_The_Deque.The_Back := To_The_Deque.The_Front;
            To_The_Deque.The_Front.The_Item := From_Index.The_Item;
            To_Index := To_The_Deque.The_Front;
            From_Index := From_Index.Next;
            while From_Index /= null loop
                To_Index.Next := Node_Manager.New_Item;
                To_Index.Next.The_Item := From_Index.The_Item;
                To_Index := To_Index.Next;
                From_Index := From_Index.Next;
                To_The_Deque.The_Back := To_Index;
            end loop;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Deque : in out Deque) is
    begin
        Node_Manager.Free (The_Deque.The_Front);
        The_Deque.The_Back := null;
    end Clear;

    procedure Add (The_Item : in Item;
                   To_The_Deque : in out Deque;
                   At_The_Location : in Location) is
        Temporary_Node : Structure;
    begin
        if To_The_Deque.The_Front = null then
            To_The_Deque.The_Front := Node_Manager.New_Item;
            To_The_Deque.The_Front.The_Item := The_Item;
            To_The_Deque.The_Back := To_The_Deque.The_Front;
        elsif At_The_Location = Front then
            Temporary_Node := To_The_Deque.The_Front;
            To_The_Deque.The_Front := Node_Manager.New_Item;
            To_The_Deque.The_Front.The_Item := The_Item;
            To_The_Deque.The_Front.Next := Temporary_Node;
        else
            To_The_Deque.The_Back.Next := Node_Manager.New_Item;
            To_The_Deque.The_Back.Next.The_Item := The_Item;
            To_The_Deque.The_Back := To_The_Deque.The_Back.Next;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Add;

    procedure Pop (The_Deque : in out Deque; At_The_Location : in Location) is
        Temporary_Node : Structure;
        Index : Structure := The_Deque.The_Front;
    begin
        if Index.Next = null then
            Node_Manager.Free (The_Deque.The_Front);
            The_Deque.The_Back := null;
        elsif At_The_Location = Front then
            Temporary_Node := The_Deque.The_Front;
            The_Deque.The_Front := The_Deque.The_Front.Next;
            Temporary_Node.Next := null;
            Node_Manager.Free (Temporary_Node);
        else
            while Index.Next /= The_Deque.The_Back loop
                Index := Index.Next;
            end loop;
            Temporary_Node := The_Deque.The_Back;
            The_Deque.The_Back := Index;
            The_Deque.The_Back.Next := null;
            Node_Manager.Free (Temporary_Node);
        end if;
    exception
        when Constraint_Error =>
            raise Underflow;
    end Pop;

    function Is_Equal (Left : in Deque; Right : in Deque) return Boolean is
        Left_Index : Structure := Left.The_Front;
        Right_Index : Structure := Right.The_Front;
    begin
        while Left_Index /= null loop
            if Left_Index.The_Item /= Right_Index.The_Item then
                return False;
            else
                Left_Index := Left_Index.Next;
                Right_Index := Right_Index.Next;
            end if;
        end loop;
        return (Right_Index = null);
    exception
        when Constraint_Error =>
            return False;
    end Is_Equal;

    function Length_Of (The_Deque : in Deque) return Natural is
        Count : Natural := 0;
        Index : Structure := The_Deque.The_Front;
    begin
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        return Count;
    end Length_Of;

    function Is_Empty (The_Deque : in Deque) return Boolean is
    begin
        return (The_Deque.The_Front = null);
    end Is_Empty;

    function Front_Of (The_Deque : in Deque) return Item is
    begin
        return The_Deque.The_Front.The_Item;
    exception
        when Constraint_Error =>
            raise Underflow;
    end Front_Of;

    function Back_Of (The_Deque : in Deque) return Item is
    begin
        return The_Deque.The_Back.The_Item;
    exception
        when Constraint_Error =>
            raise Underflow;
    end Back_Of;

    procedure Iterate (Over_The_Deque : in Deque) is
        The_Iterator : Structure := Over_The_Deque.The_Front;
        Continue : Boolean;
    begin
        while not (The_Iterator = null) loop
            Process (The_Iterator.The_Item, Continue);
            exit when not Continue;
            The_Iterator := The_Iterator.Next;
        end loop;
    end Iterate;

end Deque_Nonpriority_Nonbalking_Sequential_Unbounded_Controlled_Iterator;
