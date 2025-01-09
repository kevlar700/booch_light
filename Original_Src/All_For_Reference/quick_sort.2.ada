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

package body Quick_Sort is

    procedure Exchange (Left : in out Item; Right : in out Item) is
        Temporary_Item : Item;
    begin
        Temporary_Item := Left;
        Left := Right;
        Right := Temporary_Item;
    end Exchange;

    procedure Sort (The_Items : in out Items) is
        procedure Sort_Recursive (Left_Index : in Index;
                                  Right_Index : in Index) is
            Pivot_Item : Item;
            The_Front : Index;
            The_Back : Index;
            Middle_Index : Index;
        begin
            if Left_Index < Right_Index then
                Middle_Index := Index'Val ((Index'Pos (Left_Index) +
                                            Index'Pos (Right_Index)) / 2);
                if The_Items (Middle_Index) < The_Items (Left_Index) then
                    Exchange (The_Items (Middle_Index), The_Items (Left_Index));
                end if;
                if The_Items (Right_Index) < The_Items (Left_Index) then
                    Exchange (The_Items (Right_Index), The_Items (Left_Index));
                end if;
                if The_Items (Right_Index) < The_Items (Middle_Index) then
                    Exchange (The_Items (Right_Index),
                              The_Items (Middle_Index));
                end if;
                Pivot_Item := The_Items (Middle_Index);
                Exchange (The_Items (Middle_Index),
                          The_Items (Index'Pred (Right_Index)));
                The_Front := Index'Succ (Left_Index);
                The_Back := Index'Pred (Right_Index);
                if The_Back /= The_Items'First then
                    The_Back := Index'Pred (The_Back);
                end if;
                loop
                    while The_Items (The_Front) < Pivot_Item loop
                        The_Front := Index'Succ (The_Front);
                    end loop;
                    while Pivot_Item < The_Items (The_Back) loop
                        The_Back := Index'Pred (The_Back);
                    end loop;
                    if The_Front <= The_Back then
                        if (The_Front = The_Items'Last) or else
                           (The_Back = The_Items'First) then
                            return;
                        else
                            Exchange (The_Items (The_Front),
                                      The_Items (The_Back));
                            The_Front := Index'Succ (The_Front);
                            The_Back := Index'Pred (The_Back);
                        end if;
                    end if;
                    exit when (The_Front > The_Back);
                end loop;
                Sort_Recursive (Left_Index, The_Back);
                Sort_Recursive (The_Front, Right_Index);
            end if;
        end Sort_Recursive;
    begin
        Sort_Recursive (The_Items'First, The_Items'Last);
    end Sort;

end Quick_Sort;
