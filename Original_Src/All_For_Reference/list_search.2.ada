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

package body List_Search is

    function Position_Of 
                (The_Item : in Item; In_The_List : in List) return Positive is
        Index : List := In_The_List;
        Position : Positive := 1;
    begin
        while not Is_Null (Index) loop
            if The_Item = Head_Of (Index) then
                return Position;
            else
                Position := Position + 1;
                Index := Tail_Of (Index);
            end if;
        end loop;
        raise Item_Not_Found;
    end Position_Of;

    function Location_Of (The_Position : in Positive; In_The_List : in List) 
                         return List is
        Index : List;
    begin
        if Is_Null (In_The_List) then
            raise Position_Error;
        else
            Index := In_The_List;
            for Count in 2 .. The_Position loop
                Index := Tail_Of (Index);
                if Is_Null (Index) then
                    raise Position_Error;
                end if;
            end loop;
            return Index;
        end if;
    end Location_Of;

    function Location_Of 
                (The_Item : in Item; In_The_List : in List) return List is
        Index : List := In_The_List;
    begin
        while not Is_Null (Index) loop
            if The_Item = Head_Of (Index) then
                return Index;
            else
                Index := Tail_Of (Index);
            end if;
        end loop;
        raise Item_Not_Found;
    end Location_Of;

end List_Search;
