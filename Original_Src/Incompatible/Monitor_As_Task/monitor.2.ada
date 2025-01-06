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

package body Monitor is

    task body Kind is
        Number_Of_Readers : Natural := 0;
    begin
        loop
            select
                accept Start (The_Service : in Service) do
                    case The_Service is
                        when Read =>
                            Number_Of_Readers := Number_Of_Readers + 1;
                        when Write =>
                            while Number_Of_Readers > 0 loop
                                accept Stop_Reading;
                                Number_Of_Readers := Number_Of_Readers - 1;
                            end loop;
                    end case;
                end Start;
                if Number_Of_Readers = 0 then
                    accept Stop_Writing;
                end if;
            or
                accept Stop_Reading;
                Number_Of_Readers := Number_Of_Readers - 1;
            or
                terminate;
            end select;
        end loop;
    end Kind;

    procedure Start_Reading (The_Monitor : in Kind) is
    begin
        The_Monitor.Start (Read);
    end Start_Reading;

    procedure Stop_Reading (The_Monitor : in Kind) is
    begin
        The_Monitor.Stop_Reading;
    end Stop_Reading;

    procedure Start_Writing (The_Monitor : in Kind) is
    begin
        The_Monitor.Start (Write);
    end Start_Writing;

    procedure Stop_Writing (The_Monitor : in Kind) is
    begin
        The_Monitor.Stop_Writing;
    end Stop_Writing;

end Monitor;
