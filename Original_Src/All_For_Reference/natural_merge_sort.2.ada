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

package body Natural_Merge_Sort is

    procedure Sort (The_File : in out File;
                    Temporary_File_1 : in out File;
                    Temporary_File_2 : in out File) is

        Number_Of_Runs : Natural;

        procedure Copy (From_The_File : in out File;
                        To_The_File : in out File;
                        End_Of_Run : out Boolean) is
            Temporary_Item : Item;
        begin
            Get (From_The_File, Temporary_Item);
            Put (To_The_File, Temporary_Item);
            if Is_End_Of_File (From_The_File) then
                End_Of_Run := True;
            else
                End_Of_Run := (Next_Item (From_The_File) < Temporary_Item);
            end if;
        end Copy;

        procedure Copy_Run (From_The_File : in out File;
                            To_The_File : in out File) is
            End_Of_Run : Boolean;
        begin
            loop
                Copy (From_The_File, To_The_File, End_Of_Run);
                exit when End_Of_Run;
            end loop;
        end Copy_Run;

        procedure Merge_Run (From_The_File : in out File;
                             And_The_File : in out File;
                             To_The_File : in out File) is
            End_Of_Run : Boolean;
        begin
            loop
                if not (Next_Item (And_The_File) <
                        Next_Item (From_The_File)) then
                    Copy (From_The_File, To_The_File, End_Of_Run);
                    if End_Of_Run then
                        Copy_Run (And_The_File, To_The_File);
                        exit;
                    end if;
                else
                    Copy (And_The_File, To_The_File, End_Of_Run);
                    if End_Of_Run then
                        Copy_Run (From_The_File, To_The_File);
                        exit;
                    end if;
                end if;
            end loop;
        end Merge_Run;

    begin
        loop
            Open_For_Reading (The_File);
            if Is_End_Of_File (The_File) then
                Close (The_File);
                Close (Temporary_File_1);
                Close (Temporary_File_2);
                raise File_Is_Empty;
            else
                Open_For_Writing (Temporary_File_1);
                Open_For_Writing (Temporary_File_2);
                loop
                    Copy_Run (The_File, To_The_File => Temporary_File_1);
                    if not Is_End_Of_File (The_File) then
                        Copy_Run (The_File, To_The_File => Temporary_File_2);
                    end if;
                    exit when Is_End_Of_File (The_File);
                end loop;
                Open_For_Writing (The_File);
                Open_For_Reading (Temporary_File_1);
                Open_For_Reading (Temporary_File_2);
                Number_Of_Runs := 0;
                while (not Is_End_Of_File (Temporary_File_1)) and
                         (not Is_End_Of_File (Temporary_File_2)) loop
                    Merge_Run (Temporary_File_1, Temporary_File_2,
                               To_The_File => The_File);
                    Number_Of_Runs := Number_Of_Runs + 1;
                end loop;
                while not Is_End_Of_File (Temporary_File_1) loop
                    Copy_Run (Temporary_File_1, To_The_File => The_File);
                    Number_Of_Runs := Number_Of_Runs + 1;
                end loop;
                while not Is_End_Of_File (Temporary_File_2) loop
                    Copy_Run (Temporary_File_2, To_The_File => The_File);
                    Number_Of_Runs := Number_Of_Runs + 1;
                end loop;
                exit when (Number_Of_Runs = 1);
            end if;
        end loop;
        Close (The_File);
        Close (Temporary_File_1);
        Close (Temporary_File_2);
    end Sort;

end Natural_Merge_Sort;
