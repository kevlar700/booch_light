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

package body String_Concurrent_Bounded_Managed_Noniterator is

    procedure Seize (The_String : in String) is
    begin
        Semaphore.Seize (The_String.Guard);
    end Seize;

    procedure Release (The_String : in String) is
    begin
        Semaphore.Release (The_String.Guard);
    end Release;

    procedure Copy (From_The_String : in String;
                    To_The_String : in out String) is
    begin
        Seize (From_The_String);
        Seize (To_The_String);
        if From_The_String.The_Length > To_The_String.The_Size then
            Release (From_The_String);
            Release (To_The_String);
            raise Overflow;
        else
            To_The_String.The_Items (1 .. From_The_String.The_Length) :=
               From_The_String.The_Items (1 .. From_The_String.The_Length);
            To_The_String.The_Length := From_The_String.The_Length;
            Release (From_The_String);
            Release (To_The_String);
        end if;
    end Copy;

    procedure Copy (From_The_Substring : in Substring;
                    To_The_String : in out String) is
    begin
        Seize (To_The_String);
        if From_The_Substring'Length > To_The_String.The_Size then
            Release (To_The_String);
            raise Overflow;
        else
            To_The_String.The_Items (1 .. From_The_Substring'Length) :=
               From_The_Substring;
            To_The_String.The_Length := From_The_Substring'Length;
            Release (To_The_String);
        end if;
    end Copy;

    procedure Clear (The_String : in out String) is
    begin
        Seize (The_String);
        The_String.The_Length := 0;
        Release (The_String);
    end Clear;

    procedure Prepend (The_String : in String; To_The_String : in out String) is
        New_Length : Natural;
    begin
        Seize (The_String);
        Seize (To_The_String);
        New_Length := To_The_String.The_Length + The_String.The_Length;
        if New_Length > To_The_String.The_Size then
            Release (The_String);
            Release (To_The_String);
            raise Overflow;
        else
            To_The_String.The_Items
               ((The_String.The_Length + 1) .. New_Length) :=
               To_The_String.The_Items (1 .. To_The_String.The_Length);
            To_The_String.The_Items (1 .. The_String.The_Length) :=
               The_String.The_Items (1 .. The_String.The_Length);
            To_The_String.The_Length := New_Length;
            Release (The_String);
            Release (To_The_String);
        end if;
    end Prepend;

    procedure Prepend (The_Substring : in Substring;
                       To_The_String : in out String) is
        New_Length : Natural;
    begin
        Seize (To_The_String);
        New_Length := To_The_String.The_Length + The_Substring'Length;
        if New_Length > To_The_String.The_Size then
            Release (To_The_String);
            raise Overflow;
        else
            To_The_String.The_Items
               ((The_Substring'Length + 1) .. New_Length) :=
               To_The_String.The_Items (1 .. To_The_String.The_Length);
            To_The_String.The_Items (1 .. The_Substring'Length) :=
               The_Substring;
            To_The_String.The_Length := New_Length;
            Release (To_The_String);
        end if;
    end Prepend;

    procedure Append (The_String : in String; To_The_String : in out String) is
        New_Length : Natural;
    begin
        Seize (The_String);
        Seize (To_The_String);
        New_Length := To_The_String.The_Length + The_String.The_Length;
        if New_Length > To_The_String.The_Size then
            Release (The_String);
            Release (To_The_String);
            raise Overflow;
        else
            To_The_String.The_Items
               ((To_The_String.The_Length + 1) .. New_Length) :=
               The_String.The_Items (1 .. The_String.The_Length);
            To_The_String.The_Length := New_Length;
            Release (The_String);
            Release (To_The_String);
        end if;
    end Append;

    procedure Append (The_Substring : in Substring;
                      To_The_String : in out String) is
        New_Length : Natural;
    begin
        Seize (To_The_String);
        New_Length := To_The_String.The_Length + The_Substring'Length;
        if New_Length > To_The_String.The_Size then
            Release (To_The_String);
            raise Overflow;
        else
            To_The_String.The_Items
               ((To_The_String.The_Length + 1) .. New_Length) := The_Substring;
            To_The_String.The_Length := New_Length;
            Release (To_The_String);
        end if;
    end Append;

    procedure Insert (The_String : in String;
                      In_The_String : in out String;
                      At_The_Position : in Positive) is
        New_Length : Natural;
        End_Position : Natural;
    begin
        Seize (The_String);
        Seize (In_The_String);
        New_Length := In_The_String.The_Length + The_String.The_Length;
        End_Position := At_The_Position + The_String.The_Length;
        if At_The_Position > In_The_String.The_Length then
            Release (The_String);
            Release (In_The_String);
            raise Position_Error;
        else
            if New_Length > In_The_String.The_Size then
                Release (The_String);
                Release (In_The_String);
                raise Overflow;
            else
                In_The_String.The_Items (End_Position .. New_Length) :=
                   In_The_String.The_Items (At_The_Position ..
                                               In_The_String.The_Length);
                In_The_String.The_Items
                   (At_The_Position .. (End_Position - 1)) :=
                   The_String.The_Items (1 .. The_String.The_Length);
                In_The_String.The_Length := New_Length;
                Release (The_String);
                Release (In_The_String);
            end if;
        end if;
    end Insert;

    procedure Insert (The_Substring : in Substring;
                      In_The_String : in out String;
                      At_The_Position : in Positive) is
        New_Length : Natural;
        End_Position : Natural;
    begin
        Seize (In_The_String);
        New_Length := In_The_String.The_Length + The_Substring'Length;
        End_Position := At_The_Position + The_Substring'Length;
        if At_The_Position > In_The_String.The_Length then
            Release (In_The_String);
            raise Position_Error;
        elsif New_Length > In_The_String.The_Size then
            Release (In_The_String);
            raise Overflow;
        else
            In_The_String.The_Items (End_Position .. New_Length) :=
               In_The_String.The_Items (At_The_Position ..
                                           In_The_String.The_Length);
            In_The_String.The_Items (At_The_Position .. (End_Position - 1)) :=
               The_Substring;
            In_The_String.The_Length := New_Length;
            Release (In_The_String);
        end if;
    end Insert;

    procedure Delete (In_The_String : in out String;
                      From_The_Position : in Positive;
                      To_The_Position : in Positive) is
        New_Length : Natural;
    begin
        Seize (In_The_String);
        if (From_The_Position > In_The_String.The_Length) or else
           (To_The_Position > In_The_String.The_Length) or else
           (From_The_Position > To_The_Position) then
            Release (In_The_String);
            raise Position_Error;
        else
            New_Length := In_The_String.The_Length -
                             (To_The_Position - From_The_Position + 1);
            In_The_String.The_Items (From_The_Position .. New_Length) :=
               In_The_String.The_Items ((To_The_Position + 1) ..
                                           In_The_String.The_Length);
            In_The_String.The_Length := New_Length;
            Release (In_The_String);
        end if;
    end Delete;

    procedure Replace (In_The_String : in out String;
                       At_The_Position : in Positive;
                       With_The_String : in String) is
        End_Position : Natural;
    begin
        Seize (In_The_String);
        Seize (With_The_String);
        End_Position := At_The_Position + With_The_String.The_Length - 1;
        if (At_The_Position > In_The_String.The_Length) or else
           (End_Position > In_The_String.The_Length) then
            Release (In_The_String);
            Release (With_The_String);
            raise Position_Error;
        else
            In_The_String.The_Items (At_The_Position .. End_Position) :=
               With_The_String.The_Items (1 .. With_The_String.The_Length);
            Release (In_The_String);
            Release (With_The_String);
        end if;
    end Replace;

    procedure Replace (In_The_String : in out String;
                       At_The_Position : in Positive;
                       With_The_Substring : in Substring) is
        End_Position : Natural;
    begin
        Seize (In_The_String);
        End_Position := At_The_Position + With_The_Substring'Length - 1;
        if (At_The_Position > In_The_String.The_Length) or else
           (End_Position > In_The_String.The_Length) then
            Release (In_The_String);
            raise Position_Error;
        else
            In_The_String.The_Items (At_The_Position .. End_Position) :=
               With_The_Substring;
            Release (In_The_String);
        end if;
    end Replace;

    procedure Set_Item (In_The_String : in out String;
                        At_The_Position : in Positive;
                        With_The_Item : in Item) is
    begin
        Seize (In_The_String);
        if At_The_Position > In_The_String.The_Length then
            Release (In_The_String);
            raise Position_Error;
        else
            In_The_String.The_Items (At_The_Position) := With_The_Item;
            Release (In_The_String);
        end if;
    end Set_Item;

    function Is_Equal (Left : in String; Right : in String) return Boolean is
    begin
        Seize (Left);
        Seize (Right);
        if Left.The_Length /= Right.The_Length then
            Release (Left);
            Release (Right);
            return False;
        else
            for Index in 1 .. Left.The_Length loop
                if Left.The_Items (Index) /= Right.The_Items (Index) then
                    Release (Left);
                    Release (Right);
                    return False;
                end if;
            end loop;
        end if;
        Release (Left);
        Release (Right);
        return True;
    end Is_Equal;

    function Is_Equal (Left : in Substring; Right : in String) return Boolean is
    begin
        Seize (Right);
        if Left'Length /= Right.The_Length then
            Release (Right);
            return False;
        else
            for Index in 1 .. Left'Length loop
                if Left (Left'First + Index - 1) /= Right.The_Items (Index) then
                    Release (Right);
                    return False;
                end if;
            end loop;
            Release (Right);
            return True;
        end if;
    end Is_Equal;

    function Is_Equal (Left : in String; Right : in Substring) return Boolean is
    begin
        Seize (Left);
        if Left.The_Length /= Right'Length then
            Release (Left);
            return False;
        else
            for Index in 1 .. Left.The_Length loop
                if Left.The_Items (Index) /= Right
                                                (Right'First + Index - 1) then
                    Release (Left);
                    return False;
                end if;
            end loop;
            Release (Left);
            return True;
        end if;
    end Is_Equal;

    function Is_Less_Than
                (Left : in String; Right : in String) return Boolean is
        Result : Boolean;
    begin
        Seize (Left);
        Seize (Right);
        for Index in 1 .. Left.The_Length loop
            if Index > Right.The_Length then
                Release (Left);
                Release (Right);
                return False;
            elsif Left.The_Items (Index) < Right.The_Items (Index) then
                Release (Left);
                Release (Right);
                return True;
            elsif Right.The_Items (Index) < Left.The_Items (Index) then
                Release (Left);
                Release (Right);
                return False;
            end if;
        end loop;
        Result := (Left.The_Length < Right.The_Length);
        Release (Left);
        Release (Right);
        return Result;
    end Is_Less_Than;

    function Is_Less_Than
                (Left : in Substring; Right : in String) return Boolean is
        Result : Boolean;
    begin
        Seize (Right);
        for Index in 1 .. Left'Length loop
            if Index > Right.The_Length then
                Release (Right);
                return False;
            elsif Left (Left'First + Index - 1) < Right.The_Items (Index) then
                Release (Right);
                return True;
            elsif Right.The_Items (Index) < Left (Left'First + Index - 1) then
                Release (Right);
                return False;
            end if;
        end loop;
        Result := (Left'Length < Right.The_Length);
        Release (Right);
        return Result;
    end Is_Less_Than;

    function Is_Less_Than
                (Left : in String; Right : in Substring) return Boolean is
        Result : Boolean;
    begin
        Seize (Left);
        for Index in 1 .. Left.The_Length loop
            if Index > Right'Length then
                Release (Left);
                return False;
            elsif Left.The_Items (Index) < Right (Right'First + Index - 1) then
                Release (Left);
                return True;
            elsif Right (Right'First + Index - 1) < Left.The_Items (Index) then
                Release (Left);
                return False;
            end if;
        end loop;
        Result := (Left.The_Length < Right'Length);
        Release (Left);
        return Result;
    end Is_Less_Than;

    function Is_Greater_Than
                (Left : in String; Right : in String) return Boolean is
    begin
        Seize (Left);
        Seize (Right);
        for Index in 1 .. Left.The_Length loop
            if Index > Right.The_Length then
                Release (Left);
                Release (Right);
                return True;
            elsif Left.The_Items (Index) < Right.The_Items (Index) then
                Release (Left);
                Release (Right);
                return False;
            elsif Right.The_Items (Index) < Left.The_Items (Index) then
                Release (Left);
                Release (Right);
                return True;
            end if;
        end loop;
        Release (Left);
        Release (Right);
        return False;
    end Is_Greater_Than;

    function Is_Greater_Than
                (Left : in Substring; Right : in String) return Boolean is
    begin
        Seize (Right);
        for Index in 1 .. Left'Length loop
            if Index > Right.The_Length then
                Release (Right);
                return True;
            elsif Left (Left'First + Index - 1) < Right.The_Items (Index) then
                Release (Right);
                return False;
            elsif Right.The_Items (Index) < Left (Left'First + Index - 1) then
                Release (Right);
                return True;
            end if;
        end loop;
        Release (Right);
        return False;
    end Is_Greater_Than;

    function Is_Greater_Than
                (Left : in String; Right : in Substring) return Boolean is
    begin
        Seize (Left);
        for Index in 1 .. Left.The_Length loop
            if Index > Right'Length then
                Release (Left);
                return True;
            elsif Left.The_Items (Index) < Right (Right'First + Index - 1) then
                Release (Left);
                return False;
            elsif Right (Right'First + Index - 1) < Left.The_Items (Index) then
                Release (Left);
                return True;
            end if;
        end loop;
        Release (Left);
        return False;
    end Is_Greater_Than;

    function Length_Of (The_String : in String) return Natural is
        Result : Natural;
    begin
        Seize (The_String);
        Result := The_String.The_Length;
        Release (The_String);
        return Result;
    end Length_Of;

    function Is_Null (The_String : in String) return Boolean is
        Result : Boolean;
    begin
        Seize (The_String);
        Result := The_String.The_Length = 0;
        Release (The_String);
        return Result;
    end Is_Null;

    function Item_Of (The_String : in String; At_The_Position : in Positive)
                     return Item is
        Temporary_Item : Item;
    begin
        Seize (The_String);
        if At_The_Position > The_String.The_Length then
            Release (The_String);
            raise Position_Error;
        else
            Temporary_Item := The_String.The_Items (At_The_Position);
            Release (The_String);
            return Temporary_Item;
        end if;
    end Item_Of;

    function Substring_Of (The_String : in String) return Substring is
    begin
        Seize (The_String);
        declare
            Temporary_Items : Substring (1 .. The_String.The_Length) :=
               The_String.The_Items (1 .. The_String.The_Length);
        begin
            Release (The_String);
            return Temporary_Items;
        end;
    end Substring_Of;

    function Substring_Of (The_String : in String;
                           From_The_Position : in Positive;
                           To_The_Position : in Positive) return Substring is
    begin
        Seize (The_String);
        if (From_The_Position > The_String.The_Length) or else
           (To_The_Position > The_String.The_Length) or else
           (From_The_Position > To_The_Position) then
            Release (The_String);
            raise Position_Error;
        else
            declare
                Temporary_Items : Substring
                                     (From_The_Position .. To_The_Position) :=
                   The_String.The_Items (From_The_Position .. To_The_Position);
            begin
                Release (The_String);
                return Temporary_Items;
            end;
        end if;
    end Substring_Of;

end String_Concurrent_Bounded_Managed_Noniterator;
