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

package body Deque_Nonpriority_Nonbalking_Concurrent_Unbounded_Unmanaged_Noniterator is

    type Node is
        record
            The_Item : Item;
            Next : Structure;
        end record;

    procedure Seize (The_Deque : in Deque) is
    begin
        Semaphore.Seize (The_Deque.Guard);
    end Seize;

    procedure Release (The_Deque : in Deque) is
    begin
        Semaphore.Release (The_Deque.Guard);
    end Release;

    procedure Copy (From_The_Deque : in Deque; To_The_Deque : in out Deque) is
        From_Index : Structure;
        To_Index : Structure;
    begin
        Seize (From_The_Deque);
        Seize (To_The_Deque);
        From_Index := From_The_Deque.The_Front;
        if From_The_Deque.The_Front = null then
            To_The_Deque.The_Front := null;
            To_The_Deque.The_Back := null;
        else
            To_The_Deque.The_Front :=
               new Node'(The_Item => From_Index.The_Item, Next => null);
            To_The_Deque.The_Back := To_The_Deque.The_Front;
            To_Index := To_The_Deque.The_Front;
            From_Index := From_Index.Next;
            while From_Index /= null loop
                To_Index.Next :=
                   new Node'(The_Item => From_Index.The_Item, Next => null);
                To_Index := To_Index.Next;
                From_Index := From_Index.Next;
                To_The_Deque.The_Back := To_Index;
            end loop;
        end if;
        Release (From_The_Deque);
        Release (To_The_Deque);
    exception
        when Storage_Error =>
            Release (From_The_Deque);
            Release (To_The_Deque);
            raise Overflow;
    end Copy;

    procedure Clear (The_Deque : in out Deque) is
    begin
        Seize (The_Deque);
        The_Deque.The_Front := null;
        The_Deque.The_Back := null;
        Release (The_Deque);
    end Clear;

    procedure Add (The_Item : in Item;
                   To_The_Deque : in out Deque;
                   At_The_Location : in Location) is
    begin
        Seize (To_The_Deque);
        if To_The_Deque.The_Front = null then
            To_The_Deque.The_Front :=
               new Node'(The_Item => The_Item, Next => null);
            To_The_Deque.The_Back := To_The_Deque.The_Front;
        elsif At_The_Location = Front then
            To_The_Deque.The_Front := new Node'(The_Item => The_Item,
                                                Next => To_The_Deque.The_Front);
        else
            To_The_Deque.The_Back.Next :=
               new Node'(The_Item => The_Item, Next => null);
            To_The_Deque.The_Back := To_The_Deque.The_Back.Next;
        end if;
        Release (To_The_Deque);
    exception
        when Storage_Error =>
            Release (To_The_Deque);
            raise Overflow;
    end Add;

    procedure Pop (The_Deque : in out Deque; At_The_Location : in Location) is
        Index : Structure;
    begin
        Seize (The_Deque);
        Index := The_Deque.The_Front;
        if Index.Next = null then
            The_Deque.The_Front := null;
            The_Deque.The_Back := null;
        elsif At_The_Location = Front then
            The_Deque.The_Front := The_Deque.The_Front.Next;
        else
            while Index.Next /= The_Deque.The_Back loop
                Index := Index.Next;
            end loop;
            The_Deque.The_Back := Index;
            The_Deque.The_Back.Next := null;
        end if;
        Release (The_Deque);
    exception
        when Constraint_Error =>
            Release (The_Deque);
            raise Underflow;
    end Pop;

    function Is_Equal (Left : in Deque; Right : in Deque) return Boolean is
        Left_Index : Structure;
        Right_Index : Structure;
    begin
        Seize (Left);
        Seize (Right);
        Left_Index := Left.The_Front;
        Right_Index := Right.The_Front;
        while Left_Index /= null loop
            if Left_Index.The_Item /= Right_Index.The_Item then
                Release (Left);
                Release (Right);
                return False;
            else
                Left_Index := Left_Index.Next;
                Right_Index := Right_Index.Next;
            end if;
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

    function Length_Of (The_Deque : in Deque) return Natural is
        Count : Natural := 0;
        Index : Structure;
    begin
        Seize (The_Deque);
        Index := The_Deque.The_Front;
        while Index /= null loop
            Count := Count + 1;
            Index := Index.Next;
        end loop;
        Release (The_Deque);
        return Count;
    end Length_Of;

    function Is_Empty (The_Deque : in Deque) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Deque);
        Result := The_Deque.The_Front = null;
        Release (The_Deque);
        return Result;
    end Is_Empty;

    function Front_Of (The_Deque : in Deque) return Item is
        Temporary_Item : Item;
    begin
        Seize (The_Deque);
        Temporary_Item := The_Deque.The_Front.The_Item;
        Release (The_Deque);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release (The_Deque);
            raise Underflow;
    end Front_Of;

    function Back_Of (The_Deque : in Deque) return Item is
        Temporary_Item : Item;
    begin
        Seize (The_Deque);
        Temporary_Item := The_Deque.The_Back.The_Item;
        Release (The_Deque);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release (The_Deque);
            raise Underflow;
    end Back_Of;

end Deque_Nonpriority_Nonbalking_Concurrent_Unbounded_Unmanaged_Noniterator;
