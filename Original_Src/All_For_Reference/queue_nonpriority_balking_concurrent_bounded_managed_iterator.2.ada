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

package body Queue_Nonpriority_Balking_Concurrent_Bounded_Managed_Iterator is

    procedure Seize (The_Queue : in Queue) is
    begin
        Semaphore.Seize (The_Queue.Guard);
    end Seize;

    procedure Release (The_Queue : in Queue) is
    begin
        Semaphore.Release (The_Queue.Guard);
    end Release;

    procedure Copy (From_The_Queue : in Queue; To_The_Queue : in out Queue) is
    begin
        Seize (From_The_Queue);
        Seize (To_The_Queue);
        if From_The_Queue.The_Back > To_The_Queue.The_Size then
            Release (From_The_Queue);
            Release (To_The_Queue);
            raise Overflow;
        elsif From_The_Queue.The_Back = 0 then
            To_The_Queue.The_Back := 0;
        else
            To_The_Queue.The_Items (1 .. From_The_Queue.The_Back) :=
               From_The_Queue.The_Items (1 .. From_The_Queue.The_Back);
            To_The_Queue.The_Back := From_The_Queue.The_Back;
        end if;
        Release (From_The_Queue);
        Release (To_The_Queue);
    end Copy;

    procedure Clear (The_Queue : in out Queue) is
    begin
        Seize (The_Queue);
        The_Queue.The_Back := 0;
        Release (The_Queue);
    end Clear;

    procedure Add (The_Item : in Item; To_The_Queue : in out Queue) is
    begin
        Seize (To_The_Queue);
        To_The_Queue.The_Items (To_The_Queue.The_Back + 1) := The_Item;
        To_The_Queue.The_Back := To_The_Queue.The_Back + 1;
        Release (To_The_Queue);
    exception
        when Constraint_Error =>
            Release (To_The_Queue);
            raise Overflow;
    end Add;

    procedure Pop (The_Queue : in out Queue) is
    begin
        Seize (The_Queue);
        if The_Queue.The_Back = 0 then
            Release (The_Queue);
            raise Underflow;
        elsif The_Queue.The_Back = 1 then
            The_Queue.The_Back := 0;
        else
            The_Queue.The_Items (1 .. (The_Queue.The_Back - 1)) :=
               The_Queue.The_Items (2 .. The_Queue.The_Back);
            The_Queue.The_Back := The_Queue.The_Back - 1;
        end if;
        Release (The_Queue);
    end Pop;

    procedure Remove_Item (From_The_Queue : in out Queue;
                           At_The_Position : in Positive) is
    begin
        Seize (From_The_Queue);
        if From_The_Queue.The_Back < At_The_Position then
            Release (From_The_Queue);
            raise Position_Error;
        elsif From_The_Queue.The_Back /= At_The_Position then
            From_The_Queue.The_Items (At_The_Position ..
                                         (From_The_Queue.The_Back - 1)) :=
               From_The_Queue.The_Items ((At_The_Position + 1) ..
                                            From_The_Queue.The_Back);
        end if;
        From_The_Queue.The_Back := From_The_Queue.The_Back - 1;
        Release (From_The_Queue);
    end Remove_Item;

    function Is_Equal (Left : in Queue; Right : in Queue) return Boolean is
    begin
        Seize (Left);
        Seize (Right);
        if Left.The_Back /= Right.The_Back then
            Release (Left);
            Release (Right);
            return False;
        else
            for Index in 1 .. Left.The_Back loop
                if Left.The_Items (Index) /= Right.The_Items (Index) then
                    Release (Left);
                    Release (Right);
                    return False;
                end if;
            end loop;
            Release (Left);
            Release (Right);
            return True;
        end if;
    end Is_Equal;

    function Length_Of (The_Queue : in Queue) return Natural is
        Count : Natural;
    begin
        Seize (The_Queue);
        Count := The_Queue.The_Back;
        Release (The_Queue);
        return Count;
    end Length_Of;

    function Is_Empty (The_Queue : in Queue) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Queue);
        Result := The_Queue.The_Back = 0;
        Release (The_Queue);
        return Result;
    end Is_Empty;

    function Front_Of (The_Queue : in Queue) return Item is
        Temporary_Item : Item;
    begin
        Seize (The_Queue);
        if The_Queue.The_Back = 0 then
            Release (The_Queue);
            raise Underflow;
        else
            Temporary_Item := The_Queue.The_Items (1);
            Release (The_Queue);
            return Temporary_Item;
        end if;
    end Front_Of;

    function Position_Of
                (The_Item : in Item; In_The_Queue : in Queue) return Natural is
    begin
        Seize (In_The_Queue);
        for Index in 1 .. In_The_Queue.The_Back loop
            if In_The_Queue.The_Items (Index) = The_Item then
                Release (In_The_Queue);
                return Index;
            end if;
        end loop;
        Release (In_The_Queue);
        return 0;
    end Position_Of;

    procedure Iterate (Over_The_Queue : in Queue) is
        Continue : Boolean;
    begin
        Seize (Over_The_Queue);
        for The_Iterator in 1 .. Over_The_Queue.The_Back loop
            Process (Over_The_Queue.The_Items (The_Iterator), Continue);
            exit when not Continue;
        end loop;
        Release (Over_The_Queue);
    end Iterate;

end Queue_Nonpriority_Balking_Concurrent_Bounded_Managed_Iterator;
