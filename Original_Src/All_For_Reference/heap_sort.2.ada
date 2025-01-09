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

package body Heap_Sort is

    procedure Sort (The_Items : in out Items) is

        Temporary_Item : Item;
        Left_Index : Index;
        Right_Index : Index;

        procedure Sift (Left_Index : Index; Right_Index : Index) is
            Temporary_Item : Item := The_Items (Left_Index);
            The_Front : Index := Left_Index;
            The_Back : Index := Index'Val (Index'Pos (The_Front) * 2);
        begin
            while The_Back <= Right_Index loop
                if The_Back < Right_Index then
                    if The_Items (The_Back) < The_Items
                                                 (Index'Succ (The_Back)) then
                        The_Back := Index'Succ (The_Back);
                    end if;
                end if;
                exit when not (Temporary_Item < The_Items (The_Back));
                The_Items (The_Front) := The_Items (The_Back);
                The_Front := The_Back;
                exit when (Index'Pos (The_Front) * 2 >
                           Index'Pos (The_Items'Last));
                The_Back := Index'Val (Index'Pos (The_Front) * 2);
            end loop;
            The_Items (The_Front) := Temporary_Item;
        end Sift;

    begin
        Left_Index :=
           Index'Val
              (((Index'Pos (The_Items'Last) - Index'Pos (The_Items'First) + 1) /
                2) +
               1);
        Right_Index := The_Items'Last;
        while Left_Index > The_Items'First loop
            Left_Index := Index'Pred (Left_Index);
            Sift (Left_Index, Right_Index);
        end loop;
        while Right_Index > The_Items'First loop
            Temporary_Item := The_Items (The_Items'First);
            The_Items (The_Items'First) := The_Items (Right_Index);
            The_Items (Right_Index) := Temporary_Item;
            Right_Index := Index'Pred (Right_Index);
            Sift (Left_Index, Right_Index);
        end loop;
    end Sort;

end Heap_Sort;
