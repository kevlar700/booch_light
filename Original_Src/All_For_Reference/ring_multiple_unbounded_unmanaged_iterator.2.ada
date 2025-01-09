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

package body Ring_Multiple_Unbounded_Unmanaged_Iterator is

    type Node is
        record
            Previous : Structure;
            The_Item : Item;
            Next : Structure;
        end record;

    procedure Seize_For_Reading (The_Ring : in Ring) is
    begin
        Monitor.Start_Reading (The_Ring.Guard);
    end Seize_For_Reading;

    procedure Release_For_Reading (The_Ring : in Ring) is
    begin
        Monitor.Stop_Reading (The_Ring.Guard);
    end Release_For_Reading;

    procedure Seize_For_Writing (The_Ring : in Ring) is
    begin
        Monitor.Start_Writing (The_Ring.Guard);
    end Seize_For_Writing;

    procedure Release_For_Writing (The_Ring : in Ring) is
    begin
        Monitor.Stop_Writing (The_Ring.Guard);
    end Release_For_Writing;

    procedure Copy (From_The_Ring : in Ring; To_The_Ring : in out Ring) is
        From_Index : Structure;
        To_Index : Structure;
    begin
        Seize_For_Reading (From_The_Ring);
        Seize_For_Writing (To_The_Ring);
        From_Index := From_The_Ring.The_Top;
        if From_The_Ring.The_Top = null then
            To_The_Ring.The_Top := null;
            To_The_Ring.The_Mark := null;
        else
            To_The_Ring.The_Top := new Node'(Previous => null,
                                             The_Item => From_Index.The_Item,
                                             Next => null);
            To_Index := To_The_Ring.The_Top;
            if From_The_Ring.The_Mark = From_Index then
                To_The_Ring.The_Mark := To_Index;
            end if;
            From_Index := From_Index.Next;
            while From_Index /= From_The_Ring.The_Top loop
                To_Index.Next := new Node'(Previous => To_Index,
                                           The_Item => From_Index.The_Item,
                                           Next => null);
                To_Index := To_Index.Next;
                if From_The_Ring.The_Mark = From_Index then
                    To_The_Ring.The_Mark := To_Index;
                end if;
                From_Index := From_Index.Next;
            end loop;
            To_The_Ring.The_Top.Previous := To_Index;
            To_Index.Next := To_The_Ring.The_Top;
        end if;
        Release_For_Reading (From_The_Ring);
        Release_For_Writing (To_The_Ring);
    exception
        when Storage_Error =>
            Release_For_Reading (From_The_Ring);
            Release_For_Writing (To_The_Ring);
            raise Overflow;
    end Copy;

    procedure Clear (The_Ring : in out Ring) is
    begin
        Seize_For_Writing (The_Ring);
        The_Ring.The_Top := null;
        The_Ring.The_Mark := null;
        Release_For_Writing (The_Ring);
    end Clear;

    procedure Insert (The_Item : in Item; In_The_Ring : in out Ring) is
    begin
        Seize_For_Writing (In_The_Ring);
        if In_The_Ring.The_Top = null then
            In_The_Ring.The_Top :=
               new Node'(Previous => null, The_Item => The_Item, Next => null);
            In_The_Ring.The_Top.Previous := In_The_Ring.The_Top;
            In_The_Ring.The_Top.Next := In_The_Ring.The_Top;
            In_The_Ring.The_Mark := In_The_Ring.The_Top;
        else
            In_The_Ring.The_Top :=
               new Node'(Previous => In_The_Ring.The_Top.Previous,
                         The_Item => The_Item,
                         Next => In_The_Ring.The_Top);
            In_The_Ring.The_Top.Next.Previous := In_The_Ring.The_Top;
            In_The_Ring.The_Top.Previous.Next := In_The_Ring.The_Top;
        end if;
        Release_For_Writing (In_The_Ring);
    exception
        when Storage_Error =>
            Release_For_Writing (In_The_Ring);
            raise Overflow;
    end Insert;

    procedure Pop (The_Ring : in out Ring) is
    begin
        Seize_For_Writing (The_Ring);
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
        Release_For_Writing (The_Ring);
    exception
        when Constraint_Error =>
            Release_For_Writing (The_Ring);
            raise Underflow;
    end Pop;

    procedure Rotate (The_Ring : in out Ring;
                      In_The_Direction : in Direction) is
    begin
        Seize_For_Writing (The_Ring);
        if In_The_Direction = Forward then
            The_Ring.The_Top := The_Ring.The_Top.Next;
        else
            The_Ring.The_Top := The_Ring.The_Top.Previous;
        end if;
        Release_For_Writing (The_Ring);
    exception
        when Constraint_Error =>
            Release_For_Writing (The_Ring);
            raise Rotate_Error;
    end Rotate;

    procedure Mark (The_Ring : in out Ring) is
    begin
        Seize_For_Writing (The_Ring);
        The_Ring.The_Mark := The_Ring.The_Top;
        Release_For_Writing (The_Ring);
    end Mark;

    procedure Rotate_To_Mark (The_Ring : in out Ring) is
    begin
        Seize_For_Writing (The_Ring);
        The_Ring.The_Top := The_Ring.The_Mark;
        Release_For_Writing (The_Ring);
    end Rotate_To_Mark;

    function Is_Equal (Left : in Ring; Right : in Ring) return Boolean is
        Left_Index : Structure;
        Right_Index : Structure;
        Result : Boolean;
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        Left_Index := Left.The_Top;
        Right_Index := Right.The_Top;
        if Left_Index.The_Item /= Right_Index.The_Item then
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return False;
        elsif (Left.The_Mark = Left_Index) and then
              (Right.The_Mark /= Right_Index) then
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return False;
        else
            Left_Index := Left_Index.Next;
            Right_Index := Right_Index.Next;
            while Left_Index /= Left.The_Top loop
                if Left_Index.The_Item /= Right_Index.The_Item then
                    Release_For_Reading (Left);
                    Release_For_Reading (Right);
                    return False;
                elsif (Left.The_Mark = Left_Index) and then
                      (Right.The_Mark /= Right_Index) then
                    Release_For_Reading (Left);
                    Release_For_Reading (Right);
                    return False;
                else
                    Left_Index := Left_Index.Next;
                    Right_Index := Right_Index.Next;
                end if;
            end loop;
            Result := Right_Index = Right.The_Top;
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return Result;
        end if;
    exception
        when Constraint_Error =>
            Result := Left.The_Top = Right.The_Top;
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return Result;
    end Is_Equal;

    function Extent_Of (The_Ring : in Ring) return Natural is
        Count : Natural := 0;
        Index : Structure;
    begin
        Seize_For_Reading (The_Ring);
        Index := The_Ring.The_Top;
        Index := Index.Next;
        Count := Count + 1;
        while Index /= The_Ring.The_Top loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        Release_For_Reading (The_Ring);
        return Count;
    exception
        when Constraint_Error =>
            Release_For_Reading (The_Ring);
            return 0;
    end Extent_Of;

    function Is_Empty (The_Ring : in Ring) return Boolean is
        Result : Boolean;
    begin
        Seize_For_Reading (The_Ring);
        Result := The_Ring.The_Top = null;
        Release_For_Reading (The_Ring);
        return Result;
    end Is_Empty;

    function Top_Of (The_Ring : in Ring) return Item is
        Temporary_Item : Item;
    begin
        Seize_For_Reading (The_Ring);
        Temporary_Item := The_Ring.The_Top.The_Item;
        Release_For_Reading (The_Ring);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release_For_Reading (The_Ring);
            raise Underflow;
    end Top_Of;

    function At_Mark (The_Ring : in Ring) return Boolean is
        Result : Boolean;
    begin
        Seize_For_Reading (The_Ring);
        Result := The_Ring.The_Top = The_Ring.The_Mark;
        Release_For_Reading (The_Ring);
        return Result;
    end At_Mark;

    procedure Iterate (Over_The_Ring : in Ring) is
        The_Iterator : Structure;
        Continue : Boolean;
    begin
        Seize_For_Reading (Over_The_Ring);
        The_Iterator := Over_The_Ring.The_Top;
        if The_Iterator /= null then
            Process (The_Iterator.The_Item, Continue);
            if Continue then
                The_Iterator := The_Iterator.Next;
                while not (The_Iterator = Over_The_Ring.The_Top) loop
                    Process (The_Iterator.The_Item, Continue);
                    exit when not Continue;
                    The_Iterator := The_Iterator.Next;
                end loop;
            end if;
        end if;
        Release_For_Reading (Over_The_Ring);
    end Iterate;

end Ring_Multiple_Unbounded_Unmanaged_Iterator;
