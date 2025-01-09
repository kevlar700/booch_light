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

with Integer_Utilities;
package body Pattern_Match_Boyer_Moore is

    package Natural_Utilities is new Integer_Utilities (Number => Natural);

    function Location_Of (The_Pattern : in Items; In_The_Items : in Items)
                         return Index is

        type Skip_Table is array (The_Pattern'Range) of Natural;

        Pattern_Skip : Skip_Table;
        Pattern_Index : Index;
        Items_Index : Index := Index'Val (Index'Pos (In_The_Items'First) +
                                          The_Pattern'Length - 1);

        function Items_Skip (The_Item : in Item) return Natural is
        begin
            for Temporary_Index in reverse The_Pattern'Range loop
                if The_Item = The_Pattern (Temporary_Index) then
                    return (The_Pattern'Length - Index'Pos (Temporary_Index));
                end if;
            end loop;
            return The_Pattern'Length;
        end Items_Skip;

        procedure Preprocess (The_Pattern : in Items;
                              Pattern_Skip : in out Skip_Table) is
            Next : Skip_Table;
            Pattern_Index : Index := The_Pattern'Last;
            Shift_Amount : Natural := The_Pattern'Length + 1;
        begin
            for Temporary_Index in The_Pattern'Range loop
                Pattern_Skip (Temporary_Index) :=
                   2 * The_Pattern'Length - Index'Pos (Temporary_Index);
            end loop;
            loop
                Next (Pattern_Index) := Shift_Amount;
                while (Shift_Amount <= The_Pattern'Length) and then
                         (The_Pattern (Pattern_Index) /=
                          The_Pattern (Index'Val
                                          (Index'Pos (The_Pattern'First) +
                                           Shift_Amount - 1))) loop
                    Pattern_Skip (Index'Val (Index'Pos (The_Pattern'First) +
                                             Shift_Amount - 1)) :=
                       Natural_Utilities.Min
                          (Pattern_Skip (Index'Val
                                            (Index'Pos (The_Pattern'First) +
                                             Shift_Amount - 1)),
                           The_Pattern'Length - Index'Pos (Pattern_Index));
                    Shift_Amount := Next (Index'Val (Index'Pos
                                                        (The_Pattern'First) +
                                                     Shift_Amount - 1));
                end loop;
                exit when (Pattern_Index = The_Pattern'First);
                Shift_Amount := Shift_Amount - 1;
                Pattern_Index := Index'Pred (Pattern_Index);
            end loop;
            for Temporary_Index in The_Pattern'First ..
                                      Index'Val (Shift_Amount - 1) loop
                Pattern_Skip (Temporary_Index) :=
                   Natural_Utilities.Min (Pattern_Skip (Temporary_Index),
                                          The_Pattern'Length + Shift_Amount -
                                             Index'Pos (Temporary_Index) - 1);
            end loop;
        end Preprocess;

    begin
        Preprocess (The_Pattern, Pattern_Skip);
        while (Items_Index <= In_The_Items'Last) loop
            Pattern_Index := The_Pattern'Last;
            while In_The_Items (Items_Index) = The_Pattern (Pattern_Index) loop
                if Pattern_Index = The_Pattern'First then
                    return Items_Index;
                else
                    Pattern_Index := Index'Pred (Pattern_Index);
                    Items_Index := Index'Pred (Items_Index);
                end if;
            end loop;
            Items_Index := Index'Val
                              (Index'Pos (Items_Index) +
                               Natural_Utilities.Max
                                  (Items_Skip (In_The_Items (Items_Index)),
                                   Pattern_Skip (Pattern_Index)));
        end loop;
        raise Pattern_Not_Found;
    exception
        when Constraint_Error =>
            raise Pattern_Not_Found;
    end Location_Of;

end Pattern_Match_Boyer_Moore;
