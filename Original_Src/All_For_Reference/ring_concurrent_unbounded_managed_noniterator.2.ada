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
package body Ring_Concurrent_Unbounded_Managed_Noniterator is

    type Node is
        record
            Previous : Structure;
            The_Item : Item;
            Next : Structure;
        end record;

    procedure Free (The_Node : in out Node) is
    begin
        The_Node.Previous := null;
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

    procedure Seize (The_Ring : in Ring) is
    begin
        Semaphore.Seize (The_Ring.Guard);
    end Seize;

    procedure Release (The_Ring : in Ring) is
    begin
        Semaphore.Release (The_Ring.Guard);
    end Release;

    procedure Copy (From_The_Ring : in Ring; To_The_Ring : in out Ring) is
        From_Index : Structure;
        To_Index : Structure;
    begin
        Seize (From_The_Ring);
        Seize (To_The_Ring);
        From_Index := From_The_Ring.The_Top;
        if To_The_Ring.The_Top /= null then
            To_The_Ring.The_Top.Previous.Next := null;
            Node_Manager.Free (To_The_Ring.The_Top);
        end if;
        if From_The_Ring.The_Top = null then
            To_The_Ring.The_Mark := null;
        else
            To_The_Ring.The_Top := Node_Manager.New_Item;
            To_The_Ring.The_Top.The_Item := From_Index.The_Item;
            To_Index := To_The_Ring.The_Top;
            if From_The_Ring.The_Mark = From_Index then
                To_The_Ring.The_Mark := To_Index;
            end if;
            From_Index := From_Index.Next;
            while From_Index /= From_The_Ring.The_Top loop
                To_Index.Next := Node_Manager.New_Item;
                To_Index.Next.Previous := To_Index;
                To_Index.Next.The_Item := From_Index.The_Item;
                To_Index := To_Index.Next;
                if From_The_Ring.The_Mark = From_Index then
                    To_The_Ring.The_Mark := To_Index;
                end if;
                From_Index := From_Index.Next;
            end loop;
            To_The_Ring.The_Top.Previous := To_Index;
            To_Index.Next := To_The_Ring.The_Top;
        end if;
        Release (From_The_Ring);
        Release (To_The_Ring);
    exception
        when Storage_Error =>
            Release (From_The_Ring);
            Release (To_The_Ring);
            raise Overflow;
    end Copy;

    procedure Clear (The_Ring : in out Ring) is
    begin
        Seize (The_Ring);
        if The_Ring.The_Top /= null then
            The_Ring.The_Top.Previous.Next := null;
            Node_Manager.Free (The_Ring.The_Top);
            The_Ring.The_Mark := null;
        end if;
        Release (The_Ring);
    end Clear;

    procedure Insert (The_Item : in Item; In_The_Ring : in out Ring) is
        Temporary_Node : Structure;
    begin
        Seize (In_The_Ring);
        if In_The_Ring.The_Top = null then
            In_The_Ring.The_Top := Node_Manager.New_Item;
            In_The_Ring.The_Top.Previous := In_The_Ring.The_Top;
            In_The_Ring.The_Top.The_Item := The_Item;
            In_The_Ring.The_Top.Next := In_The_Ring.The_Top;
            In_The_Ring.The_Mark := In_The_Ring.The_Top;
        else
            Temporary_Node := Node_Manager.New_Item;
            Temporary_Node.Previous := In_The_Ring.The_Top.Previous;
            Temporary_Node.The_Item := The_Item;
            Temporary_Node.Next := In_The_Ring.The_Top;
            In_The_Ring.The_Top := Temporary_Node;
            In_The_Ring.The_Top.Next.Previous := In_The_Ring.The_Top;
            In_The_Ring.The_Top.Previous.Next := In_The_Ring.The_Top;
        end if;
        Release (In_The_Ring);
    exception
        when Storage_Error =>
            Release (In_The_Ring);
            raise Overflow;
    end Insert;

    procedure Pop (The_Ring : in out Ring) is
        Temporary_Node : Structure;
    begin
        Seize (The_Ring);
        Temporary_Node := The_Ring.The_Top;
        if The_Ring.The_Top = The_Ring.The_Top.Next then
            The_Ring.The_Top := null;
            The_Ring.The_Mark := null;
        else
            The_Ring.The_Top.Previous.Next := The_Ring.The_Top.Next;
            The_Ring.The_Top.Next.Previous := The_Ring.The_Top.Previous;
            if The_Ring.The_Mark = The_Ring.The_Top then
                The_Ring.The_Mark := The_Ring.The_Top.Next;
            end if;
            The_Ring.The_Top := The_Ring.The_Top.Next;
        end if;
        Temporary_Node.Next := null;
        Node_Manager.Free (Temporary_Node);
        Release (The_Ring);
    exception
        when Constraint_Error =>
            Release (The_Ring);
            raise Underflow;
    end Pop;

    procedure Rotate (The_Ring : in out Ring;
                      In_The_Direction : in Direction) is
    begin
        Seize (The_Ring);
        if In_The_Direction = Forward then
            The_Ring.The_Top := The_Ring.The_Top.Next;
        else
            The_Ring.The_Top := The_Ring.The_Top.Previous;
        end if;
        Release (The_Ring);
    exception
        when Constraint_Error =>
            Release (The_Ring);
            raise Rotate_Error;
    end Rotate;

    procedure Mark (The_Ring : in out Ring) is
    begin
        Seize (The_Ring);
        The_Ring.The_Mark := The_Ring.The_Top;
        Release (The_Ring);
    end Mark;

    procedure Rotate_To_Mark (The_Ring : in out Ring) is
    begin
        Seize (The_Ring);
        The_Ring.The_Top := The_Ring.The_Mark;
        Release (The_Ring);
    end Rotate_To_Mark;

    function Is_Equal (Left : in Ring; Right : in Ring) return Boolean is
        Left_Index : Structure;
        Right_Index : Structure;
        Result : Boolean;
    begin
        Seize (Left);
        Seize (Right);
        Left_Index := Left.The_Top;
        Right_Index := Right.The_Top;
        if Left_Index.The_Item /= Right_Index.The_Item then
            Release (Left);
            Release (Right);
            return False;
        elsif (Left.The_Mark = Left_Index) and then
              (Right.The_Mark /= Right_Index) then
            Release (Left);
            Release (Right);
            return False;
        else
            Left_Index := Left_Index.Next;
            Right_Index := Right_Index.Next;
            while Left_Index /= Left.The_Top loop
                if Left_Index.The_Item /= Right_Index.The_Item then
                    Release (Left);
                    Release (Right);
                    return False;
                elsif (Left.The_Mark = Left_Index) and then
                      (Right.The_Mark /= Right_Index) then
                    Release (Left);
                    Release (Right);
                    return False;
                end if;
                Left_Index := Left_Index.Next;
                Right_Index := Right_Index.Next;
            end loop;
            Result := Right_Index = Right.The_Top;
            Release (Left);
            Release (Right);
            return Result;
        end if;
    exception
        when Constraint_Error =>
            Result := Left.The_Top = Right.The_Top;
            Release (Left);
            Release (Right);
            return Result;
    end Is_Equal;

    function Extent_Of (The_Ring : in Ring) return Natural is
        Count : Natural := 0;
        Index : Structure;
    begin
        Seize (The_Ring);
        Index := The_Ring.The_Top;
        Index := Index.Next;
        Count := Count + 1;
        while Index /= The_Ring.The_Top loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        Release (The_Ring);
        return Count;
    exception
        when Constraint_Error =>
            Release (The_Ring);
            return 0;
    end Extent_Of;

    function Is_Empty (The_Ring : in Ring) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Ring);
        Result := The_Ring.The_Top = null;
        Release (The_Ring);
        return Result;
    end Is_Empty;

    function Top_Of (The_Ring : in Ring) return Item is
        Temporary_Item : Item;
    begin
        Seize (The_Ring);
        Temporary_Item := The_Ring.The_Top.The_Item;
        Release (The_Ring);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release (The_Ring);
            raise Underflow;
    end Top_Of;

    function At_Mark (The_Ring : in Ring) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Ring);
        Result := The_Ring.The_Top = The_Ring.The_Mark;
        Release (The_Ring);
        return Result;
    end At_Mark;

end Ring_Concurrent_Unbounded_Managed_Noniterator;
