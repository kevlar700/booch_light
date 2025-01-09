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
package body Stack_Concurrent_Unbounded_Managed_Noniterator is

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

    procedure Seize (The_Stack : in Stack) is
    begin
        Semaphore.Seize (The_Stack.Guard);
    end Seize;

    procedure Release (The_Stack : in Stack) is
    begin
        Semaphore.Release (The_Stack.Guard);
    end Release;

    procedure Copy (From_The_Stack : in Stack; To_The_Stack : in out Stack) is
        From_Index : Structure;
        To_Index : Structure;
    begin
        Seize (From_The_Stack);
        Seize (To_The_Stack);
        From_Index := From_The_Stack.The_Items;
        Node_Manager.Free (To_The_Stack.The_Items);
        if From_The_Stack.The_Items /= null then
            To_The_Stack.The_Items := Node_Manager.New_Item;
            To_The_Stack.The_Items.The_Item := From_Index.The_Item;
            To_Index := To_The_Stack.The_Items;
            From_Index := From_Index.Next;
            while From_Index /= null loop
                To_Index.Next := Node_Manager.New_Item;
                To_Index := To_Index.Next;
                To_Index.The_Item := From_Index.The_Item;
                From_Index := From_Index.Next;
            end loop;
        end if;
        Release (From_The_Stack);
        Release (To_The_Stack);
    exception
        when Storage_Error =>
            Release (From_The_Stack);
            Release (To_The_Stack);
            raise Overflow;
    end Copy;

    procedure Clear (The_Stack : in out Stack) is
    begin
        Seize (The_Stack);
        Node_Manager.Free (The_Stack.The_Items);
        Release (The_Stack);
    end Clear;

    procedure Push (The_Item : in Item; On_The_Stack : in out Stack) is
        Temporary_Node : Structure;
    begin
        Seize (On_The_Stack);
        Temporary_Node := Node_Manager.New_Item;
        Temporary_Node.The_Item := The_Item;
        Temporary_Node.Next := On_The_Stack.The_Items;
        On_The_Stack.The_Items := Temporary_Node;
        Release (On_The_Stack);
    exception
        when Storage_Error =>
            Release (On_The_Stack);
            raise Overflow;
    end Push;

    procedure Pop (The_Stack : in out Stack) is
        Temporary_Node : Structure;
    begin
        Seize (The_Stack);
        Temporary_Node := The_Stack.The_Items;
        The_Stack.The_Items := Temporary_Node.Next;
        Temporary_Node.Next := null;
        Node_Manager.Free (Temporary_Node);
        Release (The_Stack);
    exception
        when Constraint_Error =>
            Release (The_Stack);
            raise Underflow;
    end Pop;

    function Is_Equal (Left : in Stack; Right : in Stack) return Boolean is
        Left_Index : Structure;
        Right_Index : Structure;
    begin
        Seize (Left);
        Seize (Right);
        Left_Index := Left.The_Items;
        Right_Index := Right.The_Items;
        while Left_Index /= null loop
            if Left_Index.The_Item /= Right_Index.The_Item then
                Release (Left);
                Release (Right);
                return False;
            end if;
            Left_Index := Left_Index.Next;
            Right_Index := Right_Index.Next;
        end loop;
        Release (Left);
        Release (Right);
        return (Right_Index = null);
    exception
        when Constraint_Error =>
            Release (Left);
            Release (Right);
            return False;
    end Is_Equal;

    function Depth_Of (The_Stack : in Stack) return Natural is
        Count : Natural := 0;
        Index : Structure;
    begin
        Seize (The_Stack);
        Index := The_Stack.The_Items;
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        Release (The_Stack);
        return Count;
    end Depth_Of;

    function Is_Empty (The_Stack : in Stack) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Stack);
        Result := (The_Stack.The_Items = null);
        Release (The_Stack);
        return Result;
    end Is_Empty;

    function Top_Of (The_Stack : in Stack) return Item is
        Temporary_Item : Item;
    begin
        Seize (The_Stack);
        Temporary_Item := The_Stack.The_Items.The_Item;
        Release (The_Stack);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release (The_Stack);
            raise Underflow;
    end Top_Of;

end Stack_Concurrent_Unbounded_Managed_Noniterator;
