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

package body Storage_Manager_Concurrent is

    task Manager is
        entry Free (The_Pointer : in out Pointer);
        entry New_Item (The_Pointer : out Pointer);
    end Manager;

    task body Manager is
        Free_List : Pointer := null;
        Temporary_Pointer : Pointer;
    begin
        loop
            begin
                select
                    accept Free (The_Pointer : in out Pointer) do
                        while The_Pointer /= null loop
                            Temporary_Pointer := The_Pointer;
                            The_Pointer := Pointer_Of (The_Pointer.all);
                            Free (Temporary_Pointer.all);
                            Set_Pointer (Temporary_Pointer.all,
                                         The_Pointer => Free_List);
                            Free_List := Temporary_Pointer;
                        end loop;
                    end Free;
                or
                    accept New_Item (The_Pointer : out Pointer) do
                        if Free_List = null then
                            The_Pointer := new Item;
                        else
                            Temporary_Pointer := Free_List;
                            Free_List := Pointer_Of (Temporary_Pointer.all);
                            Set_Pointer (Temporary_Pointer.all,
                                         The_Pointer => null);
                            The_Pointer := Temporary_Pointer;
                        end if;
                    end New_Item;
                or
                    terminate;
                end select;
            exception
                when Storage_Error =>
                    null;
            end;
        end loop;
    end Manager;

    procedure Free (The_Pointer : in out Pointer) is
    begin
        Manager.Free (The_Pointer);
    end Free;

    function New_Item return Pointer is
        Temporary_Pointer : Pointer;
    begin
        Manager.New_Item (Temporary_Pointer);
        return Temporary_Pointer;
    end New_Item;

end Storage_Manager_Concurrent;
