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

with Integer_Utilities, Fixed_Point_Utilities, String_Utilities;
package body Calendar_Utilities is

    type Month_Day is array (Month) of Day;

    Century_Offset : constant := 1900;
    Days_Per_Year : constant := 365;
    Days_Per_Month : constant Month_Day := (1 => 31, 
                                            2 => 28, 
                                            3 => 31, 
                                            4 => 30, 
                                            5 => 31, 
                                            6 => 30, 
                                            7 => 31, 
                                            8 => 31, 
                                            9 => 30, 
                                            10 => 31, 
                                            11 => 30, 
                                            12 => 31);
    First_Day : constant Day_Name := Tuesday;
    Seconds_Per_Minute : constant := 60;
    Seconds_Per_Hour : constant := 60 * Seconds_Per_Minute;
    Seconds_Per_Day : constant := 24 * Seconds_Per_Hour;
    Milliseconds_Per_Second : constant := 1000;
    Noon : constant Hour := 12;
    Time_Separator : constant Character := ':';
    Date_Separator : constant Character := '/';
    Blank : constant Character := ' ';
    Comma : constant Character := ',';
    Zero : constant Character := '0';

    package Natural_Utilities is new Integer_Utilities (Number => Natural);

    package Duration_Utilities is 
       new Fixed_Point_Utilities (Number => Duration);

    function Image_Of (The_Number : in Natural) return String is
    begin
        if The_Number < 10 then
            return String_Utilities.Replaced 
                      (The_Character => Blank, 
                       With_The_Character => Zero, 
                       In_The_String => 
                          Natural_Utilities.Image_Of (The_Number));
        else
            return String_Utilities.Stripped_Leading 
                      (The_Character => Blank, 
                       From_The_String => 
                          Natural_Utilities.Image_Of (The_Number));
        end if;
    end Image_Of;

    function Is_Leap_Year (The_Year : in Year) return Boolean is
    begin
        if The_Year mod 100 = 0 then
            return (The_Year mod 400 = 0);
        else
            return (The_Year mod 4 = 0);
        end if;
    end Is_Leap_Year;

    function Days_In (The_Year : in Year) return Year_Day is
    begin
        if Is_Leap_Year (The_Year) then
            return (Days_Per_Year + 1);
        else
            return Days_Per_Year;
        end if;
    end Days_In;

    function Days_In (The_Month : in Month; The_Year : in Year) return Day is
    begin
        if (The_Month = Month_Name'Pos (February) + 1) and then 
           Is_Leap_Year (The_Year) then
            return (Days_Per_Month (Month_Name'Pos (February) + 1) + 1);
        else
            return Days_Per_Month (The_Month);
        end if;
    end Days_In;

    function Month_Of (The_Month : in Month) return Month_Name is
    begin
        return Month_Name'Val (The_Month - 1);
    end Month_Of;

    function Month_Of (The_Month : in Month_Name) return Month is
    begin
        return (Month_Name'Pos (The_Month) + 1);
    end Month_Of;

    function Day_Of (The_Year : in Year; The_Day : in Year_Day) 
                    return Day_Name is
        February_28Th : constant Year_Day := 59;
        Result : Day_Name := First_Day;
        procedure Increment (The_Day : in out Day_Name; 
                             Offset : in Natural := 1) is
        begin
            The_Day := Day_Name'Val (Day_Name'Pos (The_Day) + Offset);
        exception
            when Constraint_Error =>
                The_Day := Day_Name'Val (Day_Name'Pos (The_Day) + Offset - 7);
        end Increment;
    begin
        for Index in (Year'First + 1) .. The_Year loop
            if Is_Leap_Year (Index) then
                Increment (Result, Offset => 2);
            else
                Increment (Result);
            end if;
        end loop;  
        if Is_Leap_Year (The_Year) and (The_Day <= February_28Th) then
            Increment (Result, Offset => 6);
        end if;
        Increment (Result, Offset => Natural (((The_Day mod 7) - 1)));
        return Result;
    end Day_Of;

    function Day_Of (The_Time : in Time) return Year_Day is
        Result : Natural := 0;
    begin
        for Index in Month'First .. (The_Time.The_Month - 1) loop
            Result := Result + Natural (Days_In (Index, The_Time.The_Year));
        end loop;
        return Year_Day (Result + Natural (The_Time.The_Day));
    end Day_Of;

    function Time_Of (The_Year : in Year;  
                      The_Day : in Year_Day) return Time is
        Result : Year_Day := The_Day;
    begin
        for Index in Month'First .. Month'Last loop
            if Result <= Year_Day (Days_In (Index, The_Year)) then
                return Time'(The_Year => The_Year, 
                             The_Month => Index, 
                             The_Day => Day (Result), 
                             The_Hour => Hour'First, 
                             The_Minute => Minute'First, 
                             The_Second => Second'First, 
                             The_Millisecond => Millisecond'First);
            else
                Result := Result - Year_Day (Days_In (Index, The_Year));
            end if;
        end loop;
        raise Lexical_Error;
    end Time_Of;

    function Period_Of (The_Time : in Time) return Period is
    begin
        if The_Time.The_Hour >= Noon then
            return Pm;
        else
            return Am;
        end if;
    end Period_Of;

    function Time_Of (The_Time : in Time) return Calendar.Time is
    begin
        return Calendar.Time_Of 
                  (Year => Calendar.Year_Number (The_Time.The_Year), 
                   Month => Calendar.Month_Number (The_Time.The_Month), 
                   Day => Calendar.Day_Number (The_Time.The_Day), 
                   Seconds => Calendar.Day_Duration (The_Time.The_Hour) * 
                                 Seconds_Per_Hour + 
                              Calendar.Day_Duration (The_Time.The_Minute) * 
                                 Seconds_Per_Minute + 
                              Calendar.Day_Duration (The_Time.The_Second) + 
                              Calendar.Day_Duration (The_Time.The_Millisecond) / 
                                 Milliseconds_Per_Second);
    end Time_Of;

    function Time_Of (The_Time : in Calendar.Time) return Time is
        Result : Time;  
        Total_Duration : Calendar.Day_Duration;
        Seconds : Natural;
    begin
        Calendar.Split (The_Time, 
                        Year => Calendar.Year_Number (Result.The_Year), 
                        Month => Calendar.Month_Number (Result.The_Month), 
                        Day => Calendar.Day_Number (Result.The_Day), 
                        Seconds => Total_Duration);
        Seconds := Duration_Utilities.Floor (Total_Duration);
        Result.The_Hour := Hour (Seconds / Seconds_Per_Hour);
        Seconds := Seconds mod Seconds_Per_Hour;
        Result.The_Minute := Minute (Seconds / Seconds_Per_Minute);
        Result.The_Second := Second (Seconds mod Seconds_Per_Minute);
        Result.The_Millisecond := 
           Millisecond (Duration_Utilities.Real_Part (Total_Duration) * 
                        Milliseconds_Per_Second);
        return Result;
    end Time_Of;

    function Time_Image_Of 
                (The_Time : in Time; Time_Form : in Time_Format := Full) 
                return String is
    begin
        case Time_Form is
            when Full =>
                if The_Time.The_Hour > Noon then
                    return (Image_Of (Natural (The_Time.The_Hour - 12)) & 
                            Time_Separator & 
                            Image_Of (Natural (The_Time.The_Minute)) & 
                            Time_Separator & 
                            Image_Of (Natural (The_Time.The_Second)) & 
                            Time_Separator & 
                            Image_Of (Natural (The_Time.The_Millisecond) / 10) & 
                            " PM");
                else
                    return (Image_Of (Natural (The_Time.The_Hour)) & 
                            Time_Separator & 
                            Image_Of (Natural (The_Time.The_Minute)) & 
                            Time_Separator & 
                            Image_Of (Natural (The_Time.The_Second)) & 
                            Time_Separator & 
                            Image_Of (Natural (The_Time.The_Millisecond) / 10) & 
                            " AM");
                end if;
            when Military =>
                return (Image_Of (Natural (The_Time.The_Hour)) & 
                        Time_Separator & 
                        Image_Of (Natural (The_Time.The_Minute)) & 
                        Time_Separator & 
                        Image_Of (Natural (The_Time.The_Second)) & 
                        Time_Separator & 
                        Image_Of (Natural (The_Time.The_Millisecond) / 10));
        end case;
    end Time_Image_Of;

    function Date_Image_Of 
                (The_Time : in Time; Date_Form : in Date_Format := Full) 
                return String is
    begin
        case Date_Form is
            when Full =>
                return (Month_Name'Image (Month_Name'Val 
                                             (The_Time.The_Month - 1)) & 
                        Natural_Utilities.Image_Of 
                           (Natural (The_Time.The_Day)) & Comma & 
                        Natural_Utilities.Image_Of 
                           (Natural (The_Time.The_Year)));
            when Month_Day_Year =>
                return (Image_Of (Integer (The_Time.The_Month)) & 
                        Date_Separator & Image_Of (Integer (The_Time.The_Day)) & 
                        Date_Separator & 
                        Image_Of (Integer (The_Time.The_Year)) (4 .. 5));
        end case;
    end Date_Image_Of;

    function Value_Of (The_Date : in String; 
                       The_Time : in String; 
                       Date_Form : in Date_Format := Full; 
                       Time_Form : in Time_Format := Full) return Time is
        Result : Time;
        Left_Index : Positive;
        Right_Index : Positive;
    begin
        case Date_Form is
            when Full =>
                Right_Index := String_Utilities.Location_Of 
                                  (The_Character => Blank, 
                                   In_The_String => The_Date);
                Result.The_Month := 
                   Month 
                      (Month_Name'Pos 
                          (Month_Name'Value 
                              (The_Date 
                                  (The_Date'First .. (Right_Index - 1)))) + 1);
                Left_Index := Right_Index + 1;
                Right_Index := String_Utilities.Location_Of 
                                  (The_Character => Comma, 
                                   In_The_String => 
                                      The_Date (Left_Index .. The_Date'Last));
                Result.The_Day := Day (Natural_Utilities.Value_Of 
                                          (The_Date (Left_Index .. 
                                                        (Right_Index - 1))));
                Left_Index := Right_Index + 1;
                Result.The_Year := 
                   Year (Natural_Utilities.Value_Of 
                            (The_Date (Left_Index .. The_Date'Last)));
            when Month_Day_Year =>
                Right_Index := String_Utilities.Location_Of 
                                  (The_Character => Date_Separator, 
                                   In_The_String => The_Date);
                Result.The_Month := Month 
                                       (Natural_Utilities.Value_Of 
                                           (The_Date (The_Date'First .. 
                                                         (Right_Index - 1))));
                Left_Index := Right_Index + 1;
                Right_Index := String_Utilities.Location_Of 
                                  (The_Character => Date_Separator, 
                                   In_The_String => 
                                      The_Date (Left_Index .. The_Date'Last));
                Result.The_Day := Day (Natural_Utilities.Value_Of 
                                          (The_Date (Left_Index .. 
                                                        (Right_Index - 1))));
                Left_Index := Right_Index + 1;
                Result.The_Year := 
                   Year (Natural_Utilities.Value_Of 
                            (The_Date (Left_Index .. The_Date'Last)) + 
                         Natural (Century_Offset));
        end case;
        Right_Index := String_Utilities.Location_Of 
                          (The_Character => Time_Separator, 
                           In_The_String => The_Time);
        Result.The_Hour := Hour (Natural_Utilities.Value_Of 
                                    (The_Time (The_Time'First .. 
                                                  (Right_Index - 1))));
        Left_Index := Right_Index + 1;
        Right_Index := String_Utilities.Location_Of 
                          (The_Character => Time_Separator, 
                           In_The_String => The_Time 
                                               (Left_Index .. The_Time'Last));
        Result.The_Minute := Minute (Natural_Utilities.Value_Of 
                                        (The_Time (Left_Index .. 
                                                      (Right_Index - 1))));
        Left_Index := Right_Index + 1;
        Right_Index := String_Utilities.Location_Of 
                          (The_Character => Time_Separator, 
                           In_The_String => The_Time 
                                               (Left_Index .. The_Time'Last));
        Result.The_Second := Second (Natural_Utilities.Value_Of 
                                        (The_Time (Left_Index .. 
                                                      (Right_Index - 1))));
        Left_Index := Right_Index + 1;
        case Time_Form is
            when Full =>
                Right_Index := String_Utilities.Location_Of 
                                  (The_Character => Blank, 
                                   In_The_String => 
                                      The_Time (Left_Index .. The_Time'Last));
                Result.The_Millisecond := 
                   Millisecond (Natural_Utilities.Value_Of 
                                   (The_Time (Left_Index .. 
                                                 (Right_Index - 1))));
                Left_Index := Right_Index + 1;
                if Period'Value (The_Time (Left_Index .. The_Time'Last)) = 
                   Pm then
                    if Result.The_Hour /= Noon then
                        Result.The_Hour := Result.The_Hour + Noon;
                    end if;
                end if;
            when Military =>
                Result.The_Millisecond := 
                   Millisecond (Natural_Utilities.Value_Of 
                                   (The_Time (Left_Index .. The_Time'Last)));
        end case;
        return Result;
    exception
        when Constraint_Error =>
            raise Lexical_Error;
    end Value_Of;

    function Duration_Of (The_Interval : in Interval) return Duration is
    begin
        return (Duration (The_Interval.Elapsed_Days) * Seconds_Per_Day + 
                Duration (The_Interval.Elapsed_Hours) * Seconds_Per_Hour + 
                Duration (The_Interval.Elapsed_Minutes) * Seconds_Per_Minute + 
                Duration (The_Interval.Elapsed_Seconds) + 
                Duration (The_Interval.Elapsed_Milliseconds) / 
                   Milliseconds_Per_Second);
    end Duration_Of;

    function Interval_Of (The_Duration : in Duration) return Interval is
        Result : Interval;
        The_Seconds : Duration := The_Duration;
    begin
        Result.Elapsed_Days := Duration_Utilities.Floor 
                                  (The_Seconds / Seconds_Per_Day);
        The_Seconds := The_Seconds - 
                          Duration (Result.Elapsed_Days) * Seconds_Per_Day;
        Result.Elapsed_Hours := Hour (Duration_Utilities.Floor 
                                         (The_Seconds / Seconds_Per_Hour));
        The_Seconds := The_Seconds - 
                          Duration (Result.Elapsed_Hours) * Seconds_Per_Hour;
        Result.Elapsed_Minutes := 
           Minute (Duration_Utilities.Floor (The_Seconds / Seconds_Per_Minute));
        The_Seconds := The_Seconds - Duration (Result.Elapsed_Minutes) * 
                                        Seconds_Per_Minute;
        Result.Elapsed_Seconds := Second 
                                     (Duration_Utilities.Floor (The_Seconds));
        The_Seconds := The_Seconds - Duration (Result.Elapsed_Seconds);
        Result.Elapsed_Milliseconds := 
           Millisecond (Duration_Utilities.Floor 
                           (The_Seconds * Milliseconds_Per_Second));
        return Result;
    end Interval_Of;

    function Image_Of (The_Interval : in Interval) return String is
    begin
        return (Image_Of (Natural (The_Interval.Elapsed_Days)) & 
                Time_Separator & 
                Image_Of (Natural (The_Interval.Elapsed_Hours)) & 
                Time_Separator & 
                Image_Of (Natural (The_Interval.Elapsed_Minutes)) & 
                Time_Separator & 
                Image_Of (Natural (The_Interval.Elapsed_Seconds)) & 
                Time_Separator & 
                Image_Of (Natural (The_Interval.Elapsed_Milliseconds)));
    end Image_Of;

    function Value_Of (The_Interval : in String) return Interval is
        Result : Interval;
        Left_Index : Positive;
        Right_Index : Positive;
    begin
        Right_Index := String_Utilities.Location_Of 
                          (The_Character => Time_Separator, 
                           In_The_String => The_Interval);
        Result.Elapsed_Days := 
           Natural_Utilities.Value_Of 
              (The_Interval (The_Interval'First .. (Right_Index - 1)));
        Left_Index := Right_Index + 1;
        Right_Index := String_Utilities.Location_Of 
                          (The_Character => Time_Separator, 
                           In_The_String => 
                              The_Interval (Left_Index .. The_Interval'Last));
        Result.Elapsed_Hours := 
           Hour (Natural_Utilities.Value_Of 
                    (The_Interval (Left_Index .. (Right_Index - 1))));
        Left_Index := Right_Index + 1;
        Right_Index := String_Utilities.Location_Of 
                          (The_Character => Time_Separator, 
                           In_The_String => 
                              The_Interval (Left_Index .. The_Interval'Last));
        Result.Elapsed_Minutes := 
           Minute (Natural_Utilities.Value_Of 
                      (The_Interval (Left_Index .. (Right_Index - 1))));
        Left_Index := Right_Index + 1;
        Right_Index := String_Utilities.Location_Of 
                          (The_Character => Time_Separator, 
                           In_The_String => 
                              The_Interval (Left_Index .. The_Interval'Last));
        Result.Elapsed_Seconds := 
           Second (Natural_Utilities.Value_Of 
                      (The_Interval (Left_Index .. (Right_Index - 1))));
        Left_Index := Right_Index + 1;
        Result.Elapsed_Milliseconds := 
           Millisecond (Natural_Utilities.Value_Of 
                           (The_Interval (Left_Index .. The_Interval'Last)));
        return Result;
    exception
        when Constraint_Error =>
            raise Lexical_Error;
    end Value_Of;

end Calendar_Utilities;
