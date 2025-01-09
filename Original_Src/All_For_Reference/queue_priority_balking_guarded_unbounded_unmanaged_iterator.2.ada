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

package body Queue_Priority_Balking_Guarded_Unbounded_Unmanaged_Iterator is

    type Node is
        record
            The_Item : Item;
            Next : Structure;
        end record;

    procedure Seize (The_Queue : in Queue) is
    begin
        Semaphore.Seize (The_Queue.Guard);
    end Seize;

    procedure Release (The_Queue : in Queue) is
    begin
        Semaphore.Release (The_Queue.Guard);
    end Release;

    procedure Copy (From_The_Queue : in Queue; To_The_Queue : in out Queue) is
        From_Index : Structure := From_The_Queue.The_Front;
        To_Index : Structure;
    begin
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
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Queue : in out Queue) is
    begin
        The_Queue.The_Front := null;
        The_Queue.The_Back := null;
    end Clear;

    procedure Add (The_Item : in Item; To_The_Queue : in out Queue) is
        Previous : Structure;
        Index : Structure := To_The_Queue.The_Front;
    begin
        if To_The_Queue.The_Front = null then
            To_The_Queue.The_Front :=
               new Node'(The_Item => The_Item, Next => null);
            To_The_Queue.The_Back := To_The_Queue.The_Front;
        else
            while (Index /= null) and then (Priority_Of (The_Item) <=
                                            Priority_Of (Index.The_Item)) loop
                Previous := Index;
                Index := Index.Next;
            end loop;
            if Previous = null then
                To_The_Queue.The_Front :=
                   new Node'(The_Item => The_Item, Next => Index);
                if To_The_Queue.The_Back = null then
                    To_The_Queue.The_Back := To_The_Queue.The_Front;
                end if;
            elsif Index = null then
                To_The_Queue.The_Back.Next :=
                   new Node'(The_Item => The_Item, Next => null);
                To_The_Queue.The_Back := To_The_Queue.The_Back.Next;
            else
                Previous.Next := new Node'(The_Item => The_Item, Next => Index);
            end if;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Add;

    procedure Pop (The_Queue : in out Queue) is
    begin
        The_Queue.The_Front := The_Queue.The_Front.Next;
        if The_Queue.The_Front = null then
            The_Queue.The_Back := null;
        end if;
    exception
        when Constraint_Error =>
            raise Underflow;
    end Pop;

    procedure Remove_Item (From_The_Queue : in out Queue;
                           At_The_Position : in Positive) is
        Count : Natural := 1;
        Previous : Structure;
        Index : Structure := From_The_Queue.The_Front;
    begin
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
            raise Position_Error;
        elsif Previous = null then
            From_The_Queue.The_Front := Index.Next;
        else
            Previous.Next := Index.Next;
        end if;
        if From_The_Queue.The_Back = Index then
            From_The_Queue.The_Back := Previous;
        end if;
    end Remove_Item;

    function Is_Equal (Left : in Queue; Right : in Queue) return Boolean is
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

    function Length_Of (The_Queue : in Queue) return Natural is
        Count : Natural := 0;
        Index : Structure := The_Queue.The_Front;
    begin
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        return Count;
    end Length_Of;

    function Is_Empty (The_Queue : in Queue) return Boolean is
    begin
        return (The_Queue.The_Front = null);
    end Is_Empty;

    function Front_Of (The_Queue : in Queue) return Item is
    begin
        return The_Queue.The_Front.The_Item;
    exception
        when Constraint_Error =>
            raise Underflow;
    end Front_Of;

    function Position_Of
                (The_Item : in Item; In_The_Queue : in Queue) return Natural is
        Position : Natural := 1;
        Index : Structure := In_The_Queue.The_Front;
    begin
        while Index /= null loop
            if Index.The_Item = The_Item then
                return Position;
            else
                Position := Position + 1;
                Index := Index.Next;
            end if;
        end loop;
        return 0;
    end Position_Of;

    procedure Iterate (Over_The_Queue : in Queue) is
        The_Iterator : Structure := Over_The_Queue.The_Front;
        Continue : Boolean;
    begin
        while not (The_Iterator = null) loop
            Process (The_Iterator.The_Item, Continue);
            exit when not Continue;
            The_Iterator := The_Iterator.Next;
        end loop;
    end Iterate;

end Queue_Priority_Balking_Guarded_Unbounded_Unmanaged_Iterator;
