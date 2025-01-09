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

package body Pattern_Match_Knuth_Morris_Pratt is

    function Location_Of (The_Pattern : in Items; In_The_Items : in Items)
                         return Index is

        type Skip_Table is array (The_Pattern'Range) of Natural;

        Skip_All : Boolean := False;
        Pattern_Skip : Skip_Table;
        Pattern_Index : Index := The_Pattern'First;
        Items_Index : Index := In_The_Items'First;

        procedure Preprocess (The_Pattern : in Items;
                              Pattern_Skip : in out Skip_Table) is
            Pattern_Index : Index := The_Pattern'First;
            Shift_Amount : Natural := 0;
        begin
            Pattern_Skip (Pattern_Skip'First) := 0;
            while Pattern_Index < Pattern_Skip'Last loop
                while (Shift_Amount > 0) and then
                         (The_Pattern (Index'Val
                                          (Index'Pos (The_Pattern'First) +
                                           Shift_Amount - 1)) /=
                          The_Pattern (Pattern_Index)) loop
                    Shift_Amount := Pattern_Skip
                                       (Index'Val (Index'Pos
                                                      (Pattern_Skip'First) +
                                                   Shift_Amount - 1));
                end loop;
                Shift_Amount := Shift_Amount + 1;
                Pattern_Index := Index'Succ (Pattern_Index);
                if The_Pattern (Index'Val (Index'Pos (The_Pattern'First) +
                                           Shift_Amount - 1)) =
                   The_Pattern (Pattern_Index) then
                    Pattern_Skip (Pattern_Index) :=
                       Pattern_Skip (Index'Val (Index'Pos (Pattern_Skip'First) +
                                                Shift_Amount - 1));
                else
                    Pattern_Skip (Pattern_Index) := Shift_Amount;
                end if;
            end loop;
        end Preprocess;

    begin
        Preprocess (The_Pattern, Pattern_Skip);
        while ((Pattern_Index <= The_Pattern'Last) and
               (Items_Index <= In_The_Items'Last)) loop
            while In_The_Items (Items_Index) /= The_Pattern (Pattern_Index) loop
                if Pattern_Skip (Pattern_Index) = 0 then
                    Skip_All := True;
                    exit;
                else
                    Pattern_Index := Index'Val
                                        (Index'Pos (The_Pattern'First) +
                                         Pattern_Skip (Pattern_Index) - 1);
                end if;
            end loop;
            if (Pattern_Index = The_Pattern'Last) and not Skip_All then
                return Index'Val (Index'Pos (Items_Index) -
                                  The_Pattern'Length + 1);
            else
                if Skip_All then
                    Pattern_Index := The_Pattern'First;
                    Skip_All := False;
                else
                    Pattern_Index := Index'Succ (Pattern_Index);
                end if;
                Items_Index := Index'Succ (Items_Index);
            end if;
        end loop;
        raise Pattern_Not_Found;
    exception
        when Constraint_Error =>
            raise Pattern_Not_Found;
    end Location_Of;

end Pattern_Match_Knuth_Morris_Pratt;
