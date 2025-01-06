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

package body Stack_Concurrent_Bounded_Managed_Iterator is

    procedure Seize (The_Stack : in Stack) is
    begin
        Semaphore.Seize (The_Stack.Guard);
    end Seize;

    procedure Release (The_Stack : in Stack) is
    begin
        Semaphore.Release (The_Stack.Guard);
    end Release;

    procedure Copy (From_The_Stack : in Stack; To_The_Stack : in out Stack) is
    begin
        Seize (From_The_Stack);
        Seize (To_The_Stack);
        if From_The_Stack.The_Top > To_The_Stack.The_Size then
            Release (From_The_Stack);
            Release (To_The_Stack);
            raise Overflow;
        else
            To_The_Stack.The_Items (1 .. From_The_Stack.The_Top) :=
               From_The_Stack.The_Items (1 .. From_The_Stack.The_Top);
            To_The_Stack.The_Top := From_The_Stack.The_Top;
            Release (From_The_Stack);
            Release (To_The_Stack);
        end if;
    end Copy;

    procedure Clear (The_Stack : in out Stack) is
    begin
        Seize (The_Stack);
        The_Stack.The_Top := 0;
        Release (The_Stack);
    end Clear;

    procedure Push (The_Item : in Item; On_The_Stack : in out Stack) is
    begin
        Seize (On_The_Stack);
        On_The_Stack.The_Items (On_The_Stack.The_Top + 1) := The_Item;
        On_The_Stack.The_Top := On_The_Stack.The_Top + 1;
        Release (On_The_Stack);
    exception
        when Constraint_Error =>
            Release (On_The_Stack);
            raise Overflow;
    end Push;

    procedure Pop (The_Stack : in out Stack) is
    begin
        Seize (The_Stack);
        The_Stack.The_Top := The_Stack.The_Top - 1;
        Release (The_Stack);
    exception
        when Constraint_Error =>
            Release (The_Stack);
            raise Underflow;
    end Pop;

    function Is_Equal (Left : in Stack; Right : in Stack) return Boolean is
    begin
        Seize (Left);
        Seize (Right);
        if Left.The_Top /= Right.The_Top then
            Release (Left);
            Release (Right);
            return False;
        else
            for Index in 1 .. Left.The_Top loop
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

    function Depth_Of (The_Stack : in Stack) return Natural is
        Count : Natural;
    begin
        Seize (The_Stack);
        Count := The_Stack.The_Top;
        Release (The_Stack);
        return Count;
    end Depth_Of;

    function Is_Empty (The_Stack : in Stack) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Stack);
        Result := (The_Stack.The_Top = 0);
        Release (The_Stack);
        return Result;
    end Is_Empty;

    function Top_Of (The_Stack : in Stack) return Item is
        Temporary_Item : Item;
    begin
        Seize (The_Stack);
        Temporary_Item := The_Stack.The_Items (The_Stack.The_Top);
        Release (The_Stack);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release (The_Stack);
            raise Underflow;
    end Top_Of;

    procedure Iterate (Over_The_Stack : in Stack) is
        Continue : Boolean;
    begin
        Seize (Over_The_Stack);
        for The_Iterator in reverse 1 .. Over_The_Stack.The_Top loop
            Process (Over_The_Stack.The_Items (The_Iterator), Continue);
            exit when not Continue;
        end loop;
        Release (Over_The_Stack);
    end Iterate;

end Stack_Concurrent_Bounded_Managed_Iterator;
