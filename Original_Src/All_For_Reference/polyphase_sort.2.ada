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

package body Polyphase_Sort is

    procedure Sort (The_File : in out File;
                    Temporary_Files : in out Files;
                    Sorted_File : out Positive) is

        Number_Of_Runs : array (1 .. Number_Of_Files) of Natural;
        Number_Of_Dummy_Runs : array (1 .. Number_Of_Files) of Natural;
        Last_Item : array (1 .. Number_Of_Files) of Item;
        File_Map : array (1 .. Number_Of_Files) of Positive;
        Available_Files : array (1 .. Number_Of_Files) of Positive;
        Level : Natural := 1;
        Output_File : Natural := 1;
        Number_Of_Available_Files : Natural;
        Last_File : Positive;
        Last_Runs : Natural;
        Last_Dummy_Runs : Natural;

        procedure Select_File is
            Temporary_Run : Natural;
        begin
            if Number_Of_Dummy_Runs (Output_File) <
               Number_Of_Dummy_Runs (Output_File + 1) then
                Output_File := Output_File + 1;
            else
                if Number_Of_Dummy_Runs (Output_File) = 0 then
                    Level := Level + 1;
                    Temporary_Run := Number_Of_Runs (1);
                    for Index in 1 .. (Number_Of_Files - 1) loop
                        Number_Of_Dummy_Runs (Index) :=
                           Temporary_Run + Number_Of_Runs (Index + 1) -
                              Number_Of_Runs (Index);
                        Number_Of_Runs (Index) := Temporary_Run +
                                                     Number_Of_Runs (Index + 1);
                    end loop;
                end if;
                Output_File := 1;
            end if;
            Number_Of_Dummy_Runs (Output_File) :=
               Number_Of_Dummy_Runs (Output_File) - 1;
        end Select_File;

        procedure Copy_Run is
            Temporary_Item : Item;
        begin
            loop
                Get (The_File, Temporary_Item);
                Put (Temporary_Files (Output_File), Temporary_Item);
                exit when (Is_End_Of_File (The_File) or else
                           (Next_Item (The_File) < Temporary_Item));
            end loop;
            Last_Item (Output_File) := Temporary_Item;
        end Copy_Run;

        procedure Merge_Run is
            File_Index : Positive;
            Smallest_Item : Item;
            Smallest_File : Positive;
            Temporary_Item : Item;
            End_Of_File : Boolean;
        begin
            loop
                Number_Of_Available_Files := 0;
                for Index in 1 .. (Number_Of_Files - 1) loop
                    if Number_Of_Dummy_Runs (Index) > 0 then
                        Number_Of_Dummy_Runs (Index) :=
                           Number_Of_Dummy_Runs (Index) - 1;
                    else
                        Number_Of_Available_Files :=
                           Number_Of_Available_Files + 1;
                        Available_Files (Number_Of_Available_Files) :=
                           File_Map (Index);
                    end if;
                end loop;
                if Number_Of_Available_Files = 0 then
                    Number_Of_Dummy_Runs (Number_Of_Files) :=
                       Number_Of_Dummy_Runs (Number_Of_Files) + 1;
                else
                    loop
                        File_Index := 1;
                        Smallest_File := 1;
                        Smallest_Item :=
                           Next_Item (Temporary_Files (Available_Files (1)));
                        while File_Index < Number_Of_Available_Files loop
                            File_Index := File_Index + 1;
                            Temporary_Item :=
                               Next_Item (Temporary_Files
                                             (Available_Files (File_Index)));
                            if Temporary_Item < Smallest_Item then
                                Smallest_Item := Temporary_Item;
                                Smallest_File := File_Index;
                            end if;
                        end loop;
                        Get (Temporary_Files (Available_Files (Smallest_File)),
                             Temporary_Item);
                        End_Of_File := Is_End_Of_File (Temporary_Files
                                                          (Available_Files
                                                              (Smallest_File)));
                        Put (Temporary_Files (File_Map (Number_Of_Files)),
                             Temporary_Item);
                        if End_Of_File or else
                           (Next_Item (Temporary_Files
                                          (Available_Files (Smallest_File))) <
                            Temporary_Item) then
                            Available_Files (Smallest_File) :=
                               Available_Files (Number_Of_Available_Files);
                            Number_Of_Available_Files :=
                               Number_Of_Available_Files - 1;
                        end if;
                        exit when (Number_Of_Available_Files = 0);
                    end loop;
                end if;
                Last_Runs := Last_Runs - 1;
                exit when (Last_Runs = 0);
            end loop;
        end Merge_Run;

    begin
        for Index in 1 .. (Number_Of_Files - 1) loop
            Number_Of_Runs (Index) := 1;
            Number_Of_Dummy_Runs (Index) := 1;
            Open_For_Writing (Temporary_Files (Index));
        end loop;
        Number_Of_Runs (Number_Of_Files) := 0;
        Number_Of_Dummy_Runs (Number_Of_Files) := 0;
        Open_For_Reading (The_File);
        if Is_End_Of_File (The_File) then
            for Index in 1 .. Number_Of_Files loop
                Close (Temporary_Files (Index));
            end loop;
            Close (The_File);
            raise File_Is_Empty;
        else
            loop
                Select_File;
                Copy_Run;
                exit when (Is_End_Of_File (The_File) or
                           (Output_File = (Number_Of_Files - 1)));
            end loop;
            while not Is_End_Of_File (The_File) loop
                Select_File;
                if not (Next_Item (The_File) < Last_Item (Output_File)) then
                    Copy_Run;
                    if Is_End_Of_File (The_File) then
                        Number_Of_Dummy_Runs (Output_File) :=
                           Number_Of_Dummy_Runs (Output_File) + 1;
                    else
                        Copy_Run;
                    end if;
                else
                    Copy_Run;
                end if;
            end loop;
            Close (The_File);
            for Index in 1 .. (Number_Of_Files - 1) loop
                Open_For_Reading (Temporary_Files (Index));
            end loop;
            for Index in 1 .. Number_Of_Files loop
                File_Map (Index) := Index;
            end loop;
            loop
                Last_Runs := Number_Of_Runs (Number_Of_Files - 1);
                Number_Of_Dummy_Runs (Number_Of_Files) := 0;
                Open_For_Writing (Temporary_Files (File_Map (Number_Of_Files)));
                Merge_Run;
                Open_For_Reading (Temporary_Files (File_Map (Number_Of_Files)));
                Last_File := File_Map (Number_Of_Files);
                Last_Dummy_Runs := Number_Of_Dummy_Runs (Number_Of_Files);
                Last_Runs := Number_Of_Runs (Number_Of_Files - 1);
                for Index in reverse 2 .. Number_Of_Files loop
                    File_Map (Index) := File_Map (Index - 1);
                    Number_Of_Runs (Index) :=
                       Number_Of_Runs (Index - 1) - Last_Runs;
                    Number_Of_Dummy_Runs (Index) :=
                       Number_Of_Dummy_Runs (Index - 1);
                end loop;
                File_Map (1) := Last_File;
                Number_Of_Runs (1) := Last_Runs;
                Number_Of_Dummy_Runs (1) := Last_Dummy_Runs;
                Level := Level - 1;
                exit when (Level = 0);
            end loop;
            for Index in 1 .. Number_Of_Files loop
                Close (Temporary_Files (Index));
            end loop;
            Sorted_File := File_Map (1);
        end if;
    end Sort;

end Polyphase_Sort;
