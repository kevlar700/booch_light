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

package body Pattern_Match_Regular_Expression is

    function Location_Of (The_Pattern : in Items; In_The_Items : in Items)
                         return Index is

        type Kind is (Literal, Class, Any, Stop, Unknown);
        type Literals is array (Positive range 1 .. The_Pattern'Length) of Item;
        type Pattern (The_Kind : Kind := Unknown) is
            record
                True_Pattern : Boolean := True;
                Has_Closure : Boolean := False;
                case The_Kind is
                    when Literal =>
                        The_Item : Item;
                    when Class =>
                        Number_Of_Items : Natural := 0;
                        The_Items : Literals;
                    when Any | Stop | Unknown =>
                        null;
                end case;
            end record;
        type Patterns is array (Positive range <>) of Pattern;

        Full_Pattern : Patterns (1 .. The_Pattern'Length + 1);

        procedure Preprocess (The_Pattern : in Items;
                              Full_Pattern : in out Patterns) is
            type State is (Building_Pattern, Building_Class,
                           Building_Escape_Pattern, Building_Escape_Class);
            The_State : State := Building_Pattern;
            Pattern_Index : Index := The_Pattern'First;
            Full_Index : Positive := Full_Pattern'First;
            Last_Pattern : Natural := 0;
        begin
            loop
                case The_State is
                    when Building_Pattern =>
                        if Is_Equal (The_Pattern (Pattern_Index), Any_Item) then
                            if Full_Pattern (Full_Index).True_Pattern then
                                Full_Pattern (Full_Index) :=
                                   (The_Kind => Any,
                                    True_Pattern =>
                                       Full_Pattern (Full_Index).True_Pattern,
                                    Has_Closure => False);
                                Last_Pattern := Full_Index;
                                Full_Index := Full_Index + 1;
                            else
                                raise Illegal_Pattern;
                            end if;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Escape_Item) then
                            The_State := Building_Escape_Pattern;
                        elsif Is_Equal
                                 (The_Pattern (Pattern_Index), Not_Item) then
                            if Full_Pattern (Full_Index).True_Pattern then
                                Full_Pattern (Full_Index).True_Pattern := False;
                            else
                                raise Illegal_Pattern;
                            end if;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Closure_Item) then
                            if not Full_Pattern (Last_Pattern).Has_Closure then
                                Full_Pattern (Last_Pattern).Has_Closure := True;
                            else
                                raise Illegal_Pattern;
                            end if;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Start_Class) then
                            Full_Pattern (Full_Index) :=
                               (The_Kind => Class,
                                True_Pattern =>
                                   Full_Pattern (Full_Index).True_Pattern,
                                Has_Closure => False,
                                Number_Of_Items => 0,
                                The_Items => (others => Any_Item));
                            Last_Pattern := Full_Index;
                            Full_Index := Full_Index + 1;
                            The_State := Building_Class;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Stop_Class) then
                            raise Illegal_Pattern;
                        else
                            Full_Pattern (Full_Index) :=
                               (The_Kind => Literal,
                                True_Pattern =>
                                   Full_Pattern (Full_Index).True_Pattern,
                                Has_Closure => False,
                                The_Item => The_Pattern (Pattern_Index));
                            Last_Pattern := Full_Index;
                            Full_Index := Full_Index + 1;
                        end if;
                    when Building_Class =>
                        if Is_Equal (The_Pattern (Pattern_Index), Any_Item) then
                            raise Illegal_Pattern;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Escape_Item) then
                            The_State := Building_Escape_Class;
                        elsif Is_Equal
                                 (The_Pattern (Pattern_Index), Not_Item) then
                            raise Illegal_Pattern;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Closure_Item) then
                            raise Illegal_Pattern;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Start_Class) then
                            raise Illegal_Pattern;
                        elsif Is_Equal (The_Pattern (Pattern_Index),
                                        Stop_Class) then
                            if Full_Pattern (Last_Pattern).Number_Of_Items >
                               0 then
                                The_State := Building_Pattern;
                            else
                                raise Illegal_Pattern;
                            end if;
                        else
                            Full_Pattern (Last_Pattern).Number_Of_Items :=
                               Full_Pattern (Last_Pattern).Number_Of_Items + 1;
                            Full_Pattern (Last_Pattern).The_Items
                               (Full_Pattern (Last_Pattern).Number_Of_Items) :=
                               The_Pattern (Pattern_Index);
                        end if;
                    when Building_Escape_Pattern =>
                        Full_Pattern (Full_Index) :=
                           (The_Kind => Literal,
                            True_Pattern =>
                               Full_Pattern (Full_Index).True_Pattern,
                            Has_Closure => False,
                            The_Item => The_Pattern (Pattern_Index));
                        Last_Pattern := Full_Index;
                        Full_Index := Full_Index + 1;
                        The_State := Building_Pattern;
                    when Building_Escape_Class =>
                        Full_Pattern (Last_Pattern).Number_Of_Items :=
                           Full_Pattern (Last_Pattern).Number_Of_Items + 1;
                        Full_Pattern (Last_Pattern).The_Items
                           (Full_Pattern (Last_Pattern).Number_Of_Items) :=
                           The_Pattern (Pattern_Index);
                        The_State := Building_Class;
                end case;
                if Pattern_Index = The_Pattern'Last then
                    if (The_State = Building_Pattern) and
                       (Full_Pattern (Full_Index).True_Pattern) then
                        Full_Pattern (Full_Index) :=
                           (The_Kind => Stop,
                            True_Pattern =>
                               Full_Pattern (Full_Index).True_Pattern,
                            Has_Closure => False);
                        return;
                    else
                        raise Illegal_Pattern;
                    end if;
                else
                    Pattern_Index := Index'Succ (Pattern_Index);
                end if;
            end loop;
        exception
            when Constraint_Error =>
                raise Illegal_Pattern;
        end Preprocess;

        function Is_Match (The_Pattern : in Pattern; The_Item : in Item)
                          return Boolean is
        begin
            case The_Pattern.The_Kind is
                when Literal =>
                    if The_Pattern.True_Pattern then
                        return Is_Equal (The_Pattern.The_Item, The_Item);
                    else
                        return not Is_Equal (The_Pattern.The_Item, The_Item);
                    end if;
                when Class =>
                    if The_Pattern.True_Pattern then
                        for Index in 1 .. The_Pattern.Number_Of_Items loop
                            if Is_Equal (The_Pattern.The_Items (Index),
                                         The_Item) then
                                return True;
                            end if;
                        end loop;
                        return False;
                    else
                        for Index in 1 .. The_Pattern.Number_Of_Items loop
                            if Is_Equal (The_Pattern.The_Items (Index),
                                         The_Item) then
                                return False;
                            end if;
                        end loop;
                        return True;
                    end if;
                when Any =>
                    return True;
                when others =>
                    raise Illegal_Pattern;
            end case;
        end Is_Match;

        function Location_Of (Full_Pattern : in Patterns;
                              In_The_Items : in Items;
                              The_Start : in Index) return Index is
            Items_Index : Index := The_Start;
            Total_Closures : Natural := 0;
            Temporary_Location : Index;
            Temporary_Index : Index;
        begin
            for Full_Index in Full_Pattern'Range loop
                if Full_Pattern (Full_Index).The_Kind = Stop then
                    return The_Start;
                elsif Full_Pattern (Full_Index).Has_Closure then
                    for Index in Items_Index .. In_The_Items'Last loop
                        if Is_Match (Full_Pattern (Full_Index),
                                     In_The_Items (Index)) then
                            Total_Closures := Total_Closures + 1;
                        else
                            exit;
                        end if;
                    end loop;
                    while Total_Closures > 0 loop
                        begin
                            Temporary_Index :=
                               Index'Val (Index'Pos (Items_Index) +
                                          Total_Closures);
                            Temporary_Location :=
                               Location_Of (Full_Pattern (Full_Index + 1 ..
                                                             Full_Pattern'Last),
                                            In_The_Items (Temporary_Index ..
                                                             In_The_Items'Last),
                                            Temporary_Index);
                            Items_Index := Temporary_Index;
                            exit;
                        exception
                            when Pattern_Not_Found =>
                                Total_Closures := Total_Closures - 1;
                        end;
                    end loop;
                elsif Is_Match (Full_Pattern (Full_Index),
                                In_The_Items (Items_Index)) then
                    Items_Index := Index'Succ (Items_Index);
                else
                    raise Pattern_Not_Found;
                end if;
            end loop;
        exception
            when Constraint_Error =>
                raise Pattern_Not_Found;
        end Location_Of;
    begin
        Preprocess (The_Pattern, Full_Pattern);
        for Start in In_The_Items'Range loop
            begin
                return Location_Of (Full_Pattern, In_The_Items, Start);
            exception
                when Pattern_Not_Found =>
                    null;
            end;
        end loop;
        raise Pattern_Not_Found;
    end Location_Of;

end Pattern_Match_Regular_Expression;
