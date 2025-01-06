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

package body Stack_Guarded_Bounded_Managed_Iterator is

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
        if From_The_Stack.The_Top > To_The_Stack.The_Size then
            raise Storage_Error;
        else
            To_The_Stack.The_Items (1 .. From_The_Stack.The_Top) :=
               From_The_Stack.The_Items (1 .. From_The_Stack.The_Top);
            To_The_Stack.The_Top := From_The_Stack.The_Top;
        end if;
    end Copy;

    procedure Clear (The_Stack : in out Stack) is
    begin
        The_Stack.The_Top := 0;
    end Clear;

    procedure Push (The_Item : in Item; On_The_Stack : in out Stack) is
    begin
        On_The_Stack.The_Items (On_The_Stack.The_Top + 1) := The_Item;
        On_The_Stack.The_Top := On_The_Stack.The_Top + 1;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Push;

    procedure Pop (The_Stack : in out Stack) is
    begin
        The_Stack.The_Top := The_Stack.The_Top - 1;
    exception
        when Constraint_Error =>
            raise Underflow;
    end Pop;

    function Is_Equal (Left : in Stack; Right : in Stack) return Boolean is
    begin
        if Left.The_Top /= Right.The_Top then
            return False;
        else
            for Index in 1 .. Left.The_Top loop
                if Left.The_Items (Index) /= Right.The_Items (Index) then
                    return False;
                end if;
            end loop;
            return True;
        end if;
    end Is_Equal;

    function Depth_Of (The_Stack : in Stack) return Natural is
    begin
        return The_Stack.The_Top;
    end Depth_Of;

    function Is_Empty (The_Stack : in Stack) return Boolean is
    begin
        return (The_Stack.The_Top = 0);
    end Is_Empty;

    function Top_Of (The_Stack : in Stack) return Item is
    begin
        return The_Stack.The_Items (The_Stack.The_Top);
    exception
        when Constraint_Error =>
            raise Underflow;
    end Top_Of;

    procedure Iterate (Over_The_Stack : in Stack) is
        Continue : Boolean;
    begin
        for The_Iterator in reverse 1 .. Over_The_Stack.The_Top loop
            Process (Over_The_Stack.The_Items (The_Iterator), Continue);
            exit when not Continue;
        end loop;
    end Iterate;

end Stack_Guarded_Bounded_Managed_Iterator;
