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

package body Map_Discrete_Noncached_Sequential_Bounded_Managed_Noniterator is

    procedure Copy (From_The_Map : in Map; To_The_Map : in out Map) is
    begin
        To_The_Map := From_The_Map;
    end Copy;

    procedure Clear (The_Map : in out Map) is
    begin
        for Index in The_Map'Range loop
            The_Map (Index).Is_Bound := False;
        end loop;
    end Clear;

    procedure Bind (The_Domain : in Domain;
                    And_The_Range : in Ranges;
                    In_The_Map : in out Map) is
    begin
        if In_The_Map (The_Domain).Is_Bound then
            raise Multiple_Binding;
        else
            In_The_Map (The_Domain) := Node'(Is_Bound => True,
                                             The_Range => And_The_Range);
        end if;
    end Bind;

    procedure Unbind (The_Domain : in Domain; In_The_Map : in out Map) is
    begin
        if not In_The_Map (The_Domain).Is_Bound then
            raise Domain_Is_Not_Bound;
        else
            In_The_Map (The_Domain).Is_Bound := False;
        end if;
    end Unbind;

    function Is_Equal (Left : in Map; Right : in Map) return Boolean is
    begin
        for Index in Left'Range loop
            if Left (Index).Is_Bound /= Right (Index).Is_Bound then
                return False;
            elsif Left (Index).Is_Bound and then
                  Left (Index).The_Range /= Right (Index).The_Range then
                return False;
            end if;
        end loop;
        return True;
    end Is_Equal;

    function Extent_Of (The_Map : in Map) return Natural is
        Count : Natural := 0;
    begin
        for Index in The_Map'Range loop
            if The_Map (Index).Is_Bound then
                Count := Count + 1;
            end if;
        end loop;
        return Count;
    end Extent_Of;

    function Is_Empty (The_Map : in Map) return Boolean is
    begin
        for Index in The_Map'Range loop
            if The_Map (Index).Is_Bound then
                return False;
            end if;
        end loop;
        return True;
    end Is_Empty;

    function Is_Bound
                (The_Domain : in Domain; In_The_Map : in Map) return Boolean is
    begin
        return In_The_Map (The_Domain).Is_Bound;
    end Is_Bound;

    function Range_Of
                (The_Domain : in Domain; In_The_Map : in Map) return Ranges is
    begin
        if not In_The_Map (The_Domain).Is_Bound then
            raise Domain_Is_Not_Bound;
        else
            return In_The_Map (The_Domain).The_Range;
        end if;
    end Range_Of;

end Map_Discrete_Noncached_Sequential_Bounded_Managed_Noniterator;
