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

package body Queue_Nonpriority_Balking_Multiple_Unbounded_Unmanaged_Noniterator is

    type Node is
        record
            The_Item : Item;
            Next : Structure;
        end record;

    procedure Seize_For_Reading (The_Queue : in Queue) is
    begin
        Monitor.Start_Reading (The_Queue.Guard);
    end Seize_For_Reading;

    procedure Release_For_Reading (The_Queue : in Queue) is
    begin
        Monitor.Stop_Reading (The_Queue.Guard);
    end Release_For_Reading;

    procedure Seize_For_Writing (The_Queue : in Queue) is
    begin
        Monitor.Start_Writing (The_Queue.Guard);
    end Seize_For_Writing;

    procedure Release_For_Writing (The_Queue : in Queue) is
    begin
        Monitor.Stop_Writing (The_Queue.Guard);
    end Release_For_Writing;

    procedure Copy (From_The_Queue : in Queue; To_The_Queue : in out Queue) is
        From_Index : Structure;
        To_Index : Structure;
    begin
        Seize_For_Reading (From_The_Queue);
        Seize_For_Writing (To_The_Queue);
        From_Index := From_The_Queue.The_Front;
        if From_The_Queue.The_Front = null then
            To_The_Queue.The_Front := null;
            To_The_Queue.The_Back := null;
        else
            To_The_Queue.The_Front :=
               new Node'(The_Item => From_Index.The_Item, Next => null);
            To_The_Queue.The_Back := To_The_Queue.The_Front;
            To_Index := To_The_Queue.The_Front;
            From_Index := From_Index.Next;
            while From_Index /= null loop
                To_Index.Next :=
                   new Node'(The_Item => From_Index.The_Item, Next => null);
                To_Index := To_Index.Next;
                From_Index := From_Index.Next;
                To_The_Queue.The_Back := To_Index;
            end loop;
        end if;
        Release_For_Reading (From_The_Queue);
        Release_For_Writing (To_The_Queue);
    exception
        when Storage_Error =>
            Release_For_Reading (From_The_Queue);
            Release_For_Writing (To_The_Queue);
            raise Overflow;
    end Copy;

    procedure Clear (The_Queue : in out Queue) is
    begin
        Seize_For_Writing (The_Queue);
        The_Queue.The_Front := null;
        The_Queue.The_Back := null;
        Release_For_Writing (The_Queue);
    end Clear;

    procedure Add (The_Item : in Item; To_The_Queue : in out Queue) is
    begin
        Seize_For_Writing (To_The_Queue);
        if To_The_Queue.The_Front = null then
            To_The_Queue.The_Front :=
               new Node'(The_Item => The_Item, Next => null);
            To_The_Queue.The_Back := To_The_Queue.The_Front;
        else
            To_The_Queue.The_Back.Next :=
               new Node'(The_Item => The_Item, Next => null);
            To_The_Queue.The_Back := To_The_Queue.The_Back.Next;
        end if;
        Release_For_Writing (To_The_Queue);
    exception
        when Storage_Error =>
            Release_For_Writing (To_The_Queue);
            raise Overflow;
    end Add;

    procedure Pop (The_Queue : in out Queue) is
    begin
        Seize_For_Writing (The_Queue);
        The_Queue.The_Front := The_Queue.The_Front.Next;
        if The_Queue.The_Front = null then
            The_Queue.The_Back := null;
        end if;
        Release_For_Writing (The_Queue);
    exception
        when Constraint_Error =>
            Release_For_Writing (The_Queue);
            raise Underflow;
    end Pop;

    procedure Remove_Item (From_The_Queue : in out Queue;
                           At_The_Position : in Positive) is
        Count : Natural := 1;
        Previous : Structure;
        Index : Structure;
    begin
        Seize_For_Writing (From_The_Queue);
        Index := From_The_Queue.The_Front;
        while Index /= null loop
            if Count = At_The_Position then
                exit;
            else
                Count := Count + 1;
                Previous := Index;
                Index := Index.Next;
            end if;
        end loop;
        if Index = null then
            Release_For_Writing (From_The_Queue);
            raise Position_Error;
        elsif Previous = null then
            From_The_Queue.The_Front := Index.Next;
        else
            Previous.Next := Index.Next;
        end if;
        if From_The_Queue.The_Back = Index then
            From_The_Queue.The_Back := Previous;
        end if;
        Release_For_Writing (From_The_Queue);
    end Remove_Item;

    function Is_Equal (Left : in Queue; Right : in Queue) return Boolean is
        Left_Index : Structure;
        Right_Index : Structure;
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        Left_Index := Left.The_Front;
        Right_Index := Right.The_Front;
        while Left_Index /= null loop
            if Left_Index.The_Item /= Right_Index.The_Item then
                Release_For_Reading (Left);
                Release_For_Reading (Right);
                return False;
            else
                Left_Index := Left_Index.Next;
                Right_Index := Right_Index.Next;
            end if;
        end loop;
        Release_For_Reading (Left);
        Release_For_Reading (Right);
        return (Right_Index = null);
    exception
        when Constraint_Error =>
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return False;
    end Is_Equal;

    function Length_Of (The_Queue : in Queue) return Natural is
        Count : Natural := 0;
        Index : Structure;
    begin
        Seize_For_Reading (The_Queue);
        Index := The_Queue.The_Front;
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        Release_For_Reading (The_Queue);
        return Count;
    end Length_Of;

    function Is_Empty (The_Queue : in Queue) return Boolean is
        Result : Boolean;
    begin
        Seize_For_Reading (The_Queue);
        Result := The_Queue.The_Front = null;
        Release_For_Reading (The_Queue);
        return Result;
    end Is_Empty;

    function Front_Of (The_Queue : in Queue) return Item is
        Temporary_Item : Item;
    begin
        Seize_For_Reading (The_Queue);
        Temporary_Item := The_Queue.The_Front.The_Item;
        Release_For_Reading (The_Queue);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release_For_Reading (The_Queue);
            raise Underflow;
    end Front_Of;

    function Position_Of
                (The_Item : in Item; In_The_Queue : in Queue) return Natural is
        Position : Natural := 1;
        Index : Structure;
    begin
        Seize_For_Reading (In_The_Queue);
        Index := In_The_Queue.The_Front;
        while Index /= null loop
            if Index.The_Item = The_Item then
                Release_For_Reading (In_The_Queue);
                return Position;
            else
                Position := Position + 1;
                Index := Index.Next;
            end if;
        end loop;
        Release_For_Reading (In_The_Queue);
        return 0;
    end Position_Of;

end Queue_Nonpriority_Balking_Multiple_Unbounded_Unmanaged_Noniterator;
