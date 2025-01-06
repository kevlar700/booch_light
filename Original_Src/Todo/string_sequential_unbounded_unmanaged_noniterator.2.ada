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

package body String_Sequential_Unbounded_Unmanaged_Noniterator is

    procedure Set (The_String : in out String;
                   To_The_Size : in Natural;
                   Preserve_The_Value : in Boolean) is
        Temporary_Structure : Structure;
    begin
        if To_The_Size = 0 then
            The_String.The_Items := null;
        elsif The_String.The_Items = null then
            The_String.The_Items := new Substring (1 .. To_The_Size);
        elsif To_The_Size > The_String.The_Items'Length then
            if Preserve_The_Value then
                Temporary_Structure := new Substring (1 .. To_The_Size);
                Temporary_Structure (1 .. The_String.The_Length) :=
                   The_String.The_Items (1 .. The_String.The_Length);
                The_String.The_Items := Temporary_Structure;
            else
                The_String.The_Items := new Substring (1 .. To_The_Size);
            end if;
        end if;
        The_String.The_Length := To_The_Size;
    end Set;

    procedure Copy (From_The_String : in String;
                    To_The_String : in out String) is
    begin
        Set (To_The_String,
             To_The_Size => From_The_String.The_Length,
             Preserve_The_Value => False);
        To_The_String.The_Items (1 .. From_The_String.The_Length) :=
           From_The_String.The_Items (1 .. From_The_String.The_Length);
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Copy (From_The_Substring : in Substring;
                    To_The_String : in out String) is
    begin
        Set (To_The_String,
             To_The_Size => From_The_Substring'Length,
             Preserve_The_Value => False);
        To_The_String.The_Items (1 .. From_The_Substring'Length) :=
           From_The_Substring;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_String : in out String) is
    begin
        Set (The_String, To_The_Size => 0, Preserve_The_Value => False);
    end Clear;

    procedure Prepend (The_String : in String; To_The_String : in out String) is
        Old_Length : Natural := To_The_String.The_Length;
        New_Length : Natural :=
           To_The_String.The_Length + The_String.The_Length;
    begin
        Set (To_The_String,
             To_The_Size => New_Length,
             Preserve_The_Value => True);
        To_The_String.The_Items ((The_String.The_Length + 1) .. New_Length) :=
           To_The_String.The_Items (1 .. Old_Length);
        To_The_String.The_Items (1 .. The_String.The_Length) :=
           The_String.The_Items (1 .. The_String.The_Length);
    exception
        when Storage_Error =>
            raise Overflow;
    end Prepend;

    procedure Prepend (The_Substring : in Substring;
                       To_The_String : in out String) is
        Old_Length : Natural := To_The_String.The_Length;
        New_Length : Natural := To_The_String.The_Length + The_Substring'Length;
    begin
        Set (To_The_String,
             To_The_Size => New_Length,
             Preserve_The_Value => True);
        To_The_String.The_Items ((The_Substring'Length + 1) .. New_Length) :=
           To_The_String.The_Items (1 .. Old_Length);
        To_The_String.The_Items (1 .. The_Substring'Length) := The_Substring;
    exception
        when Storage_Error =>
            raise Overflow;
    end Prepend;

    procedure Append (The_String : in String; To_The_String : in out String) is
        Old_Length : Natural := To_The_String.The_Length;
        New_Length : Natural :=
           To_The_String.The_Length + The_String.The_Length;
    begin
        Set (To_The_String,
             To_The_Size => New_Length,
             Preserve_The_Value => True);
        To_The_String.The_Items ((Old_Length + 1) .. New_Length) :=
           The_String.The_Items (1 .. The_String.The_Length);
    exception
        when Storage_Error =>
            raise Overflow;
    end Append;

    procedure Append (The_Substring : in Substring;
                      To_The_String : in out String) is
        Old_Length : Natural := To_The_String.The_Length;
        New_Length : Natural := To_The_String.The_Length + The_Substring'Length;
    begin
        Set (To_The_String,
             To_The_Size => New_Length,
             Preserve_The_Value => True);
        To_The_String.The_Items ((Old_Length + 1) .. New_Length) :=
           The_Substring;
    exception
        when Storage_Error =>
            raise Overflow;
    end Append;

    procedure Insert (The_String : in String;
                      In_The_String : in out String;
                      At_The_Position : in Positive) is
        Old_Length : Natural := In_The_String.The_Length;
        New_Length : Natural :=
           In_The_String.The_Length + The_String.The_Length;
        End_Position : Natural := At_The_Position + The_String.The_Length;
    begin
        if At_The_Position > In_The_String.The_Length then
            raise Position_Error;
        else
            Set (In_The_String,
                 To_The_Size => New_Length,
                 Preserve_The_Value => True);
            In_The_String.The_Items (End_Position .. New_Length) :=
               In_The_String.The_Items (At_The_Position .. Old_Length);
            In_The_String.The_Items (At_The_Position .. (End_Position - 1)) :=
               The_String.The_Items (1 .. The_String.The_Length);
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Insert;

    procedure Insert (The_Substring : in Substring;
                      In_The_String : in out String;
                      At_The_Position : in Positive) is
        Old_Length : Natural := In_The_String.The_Length;
        New_Length : Natural := In_The_String.The_Length + The_Substring'Length;
        End_Position : Natural := At_The_Position + The_Substring'Length;
    begin
        if At_The_Position > In_The_String.The_Length then
            raise Position_Error;
        else
            Set (In_The_String,
                 To_The_Size => New_Length,
                 Preserve_The_Value => True);
            In_The_String.The_Items (End_Position .. New_Length) :=
               In_The_String.The_Items (At_The_Position .. Old_Length);
            In_The_String.The_Items (At_The_Position .. (End_Position - 1)) :=
               The_Substring;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Insert;

    procedure Delete (In_The_String : in out String;
                      From_The_Position : in Positive;
                      To_The_Position : in Positive) is
        New_Length : Natural;
    begin
        if (From_The_Position > In_The_String.The_Length) or else
           (To_The_Position > In_The_String.The_Length) or else
           (From_The_Position > To_The_Position) then
            raise Position_Error;
        else
            New_Length := In_The_String.The_Length -
                             (To_The_Position - From_The_Position + 1);
            In_The_String.The_Items (From_The_Position .. New_Length) :=
               In_The_String.The_Items ((To_The_Position + 1) ..
                                           In_The_String.The_Length);
            Set (In_The_String,
                 To_The_Size => New_Length,
                 Preserve_The_Value => True);
        end if;
    end Delete;

    procedure Replace (In_The_String : in out String;
                       At_The_Position : in Positive;
                       With_The_String : in String) is
        End_Position : Natural := At_The_Position +
                                     With_The_String.The_Length - 1;
    begin
        if (At_The_Position > In_The_String.The_Length) or else
           (End_Position > In_The_String.The_Length) then
            raise Position_Error;
        else
            In_The_String.The_Items (At_The_Position .. End_Position) :=
               With_The_String.The_Items (1 .. With_The_String.The_Length);
        end if;
    end Replace;

    procedure Replace (In_The_String : in out String;
                       At_The_Position : in Positive;
                       With_The_Substring : in Substring) is
        End_Position : Natural := At_The_Position +
                                     With_The_Substring'Length - 1;
    begin
        if (At_The_Position > In_The_String.The_Length) or else
           (End_Position > In_The_String.The_Length) then
            raise Position_Error;
        else
            In_The_String.The_Items (At_The_Position .. End_Position) :=
               With_The_Substring;
        end if;
    end Replace;

    procedure Set_Item (In_The_String : in out String;
                        At_The_Position : in Positive;
                        With_The_Item : in Item) is
    begin
        if At_The_Position > In_The_String.The_Length then
            raise Position_Error;
        else
            In_The_String.The_Items (At_The_Position) := With_The_Item;
        end if;
    end Set_Item;

    function Is_Equal (Left : in String; Right : in String) return Boolean is
    begin
        if Left.The_Length /= Right.The_Length then
            return False;
        else
            for Index in 1 .. Left.The_Length loop
                if Left.The_Items (Index) /= Right.The_Items (Index) then
                    return False;
                end if;
            end loop;
            return True;
        end if;
    end Is_Equal;

    function Is_Equal (Left : in Substring; Right : in String) return Boolean is
    begin
        if Left'Length /= Right.The_Length then
            return False;
        else
            for Index in 1 .. Left'Length loop
                if Left (Left'First + Index - 1) /= Right.The_Items (Index) then
                    return False;
                end if;
            end loop;
            return True;
        end if;
    end Is_Equal;

    function Is_Equal (Left : in String; Right : in Substring) return Boolean is
    begin
        if Left.The_Length /= Right'Length then
            return False;
        else
            for Index in 1 .. Left.The_Length loop
                if Left.The_Items (Index) /= Right
                                                (Right'First + Index - 1) then
                    return False;
                end if;
            end loop;
            return True;
        end if;
    end Is_Equal;

    function Is_Less_Than
                (Left : in String; Right : in String) return Boolean is
    begin
        for Index in 1 .. Left.The_Length loop
            if Index > Right.The_Length then
                return False;
            elsif Left.The_Items (Index) < Right.The_Items (Index) then
                return True;
            elsif Right.The_Items (Index) < Left.The_Items (Index) then
                return False;
            end if;
        end loop;
        return (Left.The_Length < Right.The_Length);
    end Is_Less_Than;

    function Is_Less_Than
                (Left : in Substring; Right : in String) return Boolean is
    begin
        for Index in 1 .. Left'Length loop
            if Index > Right.The_Length then
                return False;
            elsif Left (Left'First + Index - 1) < Right.The_Items (Index) then
                return True;
            elsif Right.The_Items (Index) < Left (Left'First + Index - 1) then
                return False;
            end if;
        end loop;
        return (Left'Length < Right.The_Length);
    end Is_Less_Than;

    function Is_Less_Than
                (Left : in String; Right : in Substring) return Boolean is
    begin
        for Index in 1 .. Left.The_Length loop
            if Index > Right'Length then
                return False;
            elsif Left.The_Items (Index) < Right (Right'First + Index - 1) then
                return True;
            elsif Right (Right'First + Index - 1) < Left.The_Items (Index) then
                return False;
            end if;
        end loop;
        return (Left.The_Length < Right'Length);
    end Is_Less_Than;

    function Is_Greater_Than
                (Left : in String; Right : in String) return Boolean is
    begin
        for Index in 1 .. Left.The_Length loop
            if Index > Right.The_Length then
                return True;
            elsif Left.The_Items (Index) < Right.The_Items (Index) then
                return False;
            elsif Right.The_Items (Index) < Left.The_Items (Index) then
                return True;
            end if;
        end loop;
        return False;
    end Is_Greater_Than;

    function Is_Greater_Than
                (Left : in Substring; Right : in String) return Boolean is
    begin
        for Index in 1 .. Left'Length loop
            if Index > Right.The_Length then
                return True;
            elsif Left (Left'First + Index - 1) < Right.The_Items (Index) then
                return False;
            elsif Right.The_Items (Index) < Left (Left'First + Index - 1) then
                return True;
            end if;
        end loop;
        return False;
    end Is_Greater_Than;

    function Is_Greater_Than
                (Left : in String; Right : in Substring) return Boolean is
    begin
        for Index in 1 .. Left.The_Length loop
            if Index > Right'Length then
                return True;
            elsif Left.The_Items (Index) < Right (Right'First + Index - 1) then
                return False;
            elsif Right (Right'First + Index - 1) < Left.The_Items (Index) then
                return True;
            end if;
        end loop;
        return False;
    end Is_Greater_Than;

    function Length_Of (The_String : in String) return Natural is
    begin
        return The_String.The_Length;
    end Length_Of;

    function Is_Null (The_String : in String) return Boolean is
    begin
        return (The_String.The_Length = 0);
    end Is_Null;

    function Item_Of (The_String : in String; At_The_Position : in Positive)
                     return Item is
    begin
        if At_The_Position > The_String.The_Length then
            raise Position_Error;
        else
            return The_String.The_Items (At_The_Position);
        end if;
    end Item_Of;

    function Substring_Of (The_String : in String) return Substring is
        Temporary_Structure : Substring (1 .. 1);
    begin
        return The_String.The_Items (1 .. The_String.The_Length);
    exception
        when Constraint_Error =>
            return Temporary_Structure (1 .. 0);
    end Substring_Of;

    function Substring_Of (The_String : in String;
                           From_The_Position : in Positive;
                           To_The_Position : in Positive) return Substring is
    begin
        if (From_The_Position > The_String.The_Length) or else
           (To_The_Position > The_String.The_Length) or else
           (From_The_Position > To_The_Position) then
            raise Position_Error;
        else
            return The_String.The_Items (From_The_Position .. To_The_Position);
        end if;
    end Substring_Of;

end String_Sequential_Unbounded_Unmanaged_Noniterator;
