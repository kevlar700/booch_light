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

with Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator;
package body Pipe_Bounded_Managed is

    package Item_Queue is
       new Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator
              (Item);

    task body Pipe is
        The_Queue : Item_Queue.Queue (The_Size);
    begin
        loop
            begin
                select
                    when not Item_Queue.Is_Empty (The_Queue) =>
                        accept Read (The_Item : out Item) do
                            The_Item := Item_Queue.Front_Of (The_Queue);
                            Item_Queue.Pop (The_Queue);
                        end Read;
                or
                    accept Write (The_Item : in Item) do
                        Item_Queue.Add (The_Item, To_The_Queue => The_Queue);
                    end Write;
                or
                    accept Get_Extent (The_Value : out Natural) do
                        The_Value := Item_Queue.Length_Of (The_Queue);
                    end Get_Extent;
                or
                    terminate;
                end select;
            exception
                when Item_Queue.Overflow =>
                    null;
            end;
        end loop;
    end Pipe;

    procedure Read (The_Item : out Item;
                    From_The_Pipe : in out Pipe;
                    Wait : in Duration) is
    begin
        select
            From_The_Pipe.Read (The_Item);
        or
            delay Wait;
            raise Timeout;
        end select;
    end Read;

    procedure Write (The_Item : in Item;
                     To_The_Pipe : in out Pipe;
                     Wait : in Duration) is
    begin
        select
            To_The_Pipe.Write (The_Item);
        or
            delay Wait;
            raise Timeout;
        end select;
    exception
        when Item_Queue.Overflow =>
            raise Overflow;
    end Write;

    function Extent_Of (The_Pipe : in Pipe) return Natural is
        The_Result : Natural;
    begin
        The_Pipe.Get_Extent (The_Result);
        return The_Result;
    end Extent_Of;

end Pipe_Bounded_Managed;
