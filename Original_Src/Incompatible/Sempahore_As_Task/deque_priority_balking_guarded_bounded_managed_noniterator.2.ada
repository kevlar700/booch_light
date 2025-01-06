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

package body Deque_Priority_Balking_Guarded_Bounded_Managed_Noniterator is

    procedure Seize (The_Deque : in Deque) is
    begin
        Semaphore.Seize (The_Deque.Guard);
    end Seize;

    procedure Release (The_Deque : in Deque) is
    begin
        Semaphore.Release (The_Deque.Guard);
    end Release;

    procedure Copy (From_The_Deque : in Deque; To_The_Deque : in out Deque) is
    begin
        if From_The_Deque.The_Back > To_The_Deque.The_Size then
            raise Overflow;
        elsif From_The_Deque.The_Back = 0 then
            To_The_Deque.The_Back := 0;
        else
            To_The_Deque.The_Items (1 .. From_The_Deque.The_Back) :=
               From_The_Deque.The_Items (1 .. From_The_Deque.The_Back);
            To_The_Deque.The_Back := From_The_Deque.The_Back;
        end if;
    end Copy;

    procedure Clear (The_Deque : in out Deque) is
    begin
        The_Deque.The_Back := 0;
    end Clear;

    procedure Add (The_Item : in Item;
                   To_The_Deque : in out Deque;
                   At_The_Location : in Location) is
        Index : Natural := 1;
    begin
        if To_The_Deque.The_Back = 0 then
            To_The_Deque.The_Items (To_The_Deque.The_Back + 1) := The_Item;
            To_The_Deque.The_Back := To_The_Deque.The_Back + 1;
        else
            if At_The_Location = Front then
                while (Index <= To_The_Deque.The_Back) and then
                         (Priority_Of (The_Item) <
                          Priority_Of (To_The_Deque.The_Items (Index))) loop
                    Index := Index + 1;
                end loop;
            else
                while (Index <= To_The_Deque.The_Back) and then
                         (Priority_Of (The_Item) <=
                          Priority_Of (To_The_Deque.The_Items (Index))) loop
                    Index := Index + 1;
                end loop;
            end if;
            if Index > To_The_Deque.The_Back then
                To_The_Deque.The_Items (To_The_Deque.The_Back + 1) := The_Item;
                To_The_Deque.The_Back := To_The_Deque.The_Back + 1;
            else
                To_The_Deque.The_Items ((Index + 1) ..
                                           (To_The_Deque.The_Back + 1)) :=
                   To_The_Deque.The_Items (Index .. To_The_Deque.The_Back);
                To_The_Deque.The_Items (Index) := The_Item;
                To_The_Deque.The_Back := To_The_Deque.The_Back + 1;
            end if;
        end if;
    exception
        when Constraint_Error =>
            raise Overflow;
    end Add;

    procedure Pop (The_Deque : in out Deque; At_The_Location : in Location) is
    begin
        if The_Deque.The_Back = 0 then
            raise Underflow;
        elsif The_Deque.The_Back = 1 then
            The_Deque.The_Back := 0;
        elsif At_The_Location = Front then
            The_Deque.The_Items (1 .. (The_Deque.The_Back - 1)) :=
               The_Deque.The_Items (2 .. The_Deque.The_Back);
            The_Deque.The_Back := The_Deque.The_Back - 1;
        else
            The_Deque.The_Back := The_Deque.The_Back - 1;
        end if;
    end Pop;

    procedure Remove_Item (From_The_Deque : in out Deque;
                           At_The_Position : in Positive) is
    begin
        if From_The_Deque.The_Back < At_The_Position then
            raise Position_Error;
        elsif From_The_Deque.The_Back /= At_The_Position then
            From_The_Deque.The_Items (At_The_Position ..
                                         (From_The_Deque.The_Back - 1)) :=
               From_The_Deque.The_Items ((At_The_Position + 1) ..
                                            From_The_Deque.The_Back);
        end if;
        From_The_Deque.The_Back := From_The_Deque.The_Back - 1;
    end Remove_Item;

    function Is_Equal (Left : in Deque; Right : in Deque) return Boolean is
    begin
        if Left.The_Back /= Right.The_Back then
            return False;
        end if;
        for Index in 1 .. Left.The_Back loop
            if Left.The_Items (Index) /= Right.The_Items (Index) then
                return False;
            end if;
        end loop;
        return True;
    end Is_Equal;

    function Length_Of (The_Deque : in Deque) return Natural is
    begin
        return The_Deque.The_Back;
    end Length_Of;

    function Is_Empty (The_Deque : in Deque) return Boolean is
    begin
        return (The_Deque.The_Back = 0);
    end Is_Empty;

    function Front_Of (The_Deque : in Deque) return Item is
    begin
        if The_Deque.The_Back = 0 then
            raise Underflow;
        end if;
        return The_Deque.The_Items (1);
    end Front_Of;

    function Back_Of (The_Deque : in Deque) return Item is
    begin
        return The_Deque.The_Items (The_Deque.The_Back);
    exception
        when Constraint_Error =>
            raise Underflow;
    end Back_Of;

    function Position_Of
                (The_Item : in Item; In_The_Deque : in Deque) return Natural is
    begin
        for Index in 1 .. In_The_Deque.The_Back loop
            if In_The_Deque.The_Items (Index) = The_Item then
                return Index;
            end if;
        end loop;
        return 0;
    end Position_Of;

end Deque_Priority_Balking_Guarded_Bounded_Managed_Noniterator;
