--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;
with Booch_Light.Fixed_Point_Utilities;
with Booch_Light.Integer_Utilities;
with Booch_Light.String_Utilities;

package body Booch_Light.Calendar_Utilities is

   type Month_Day is array (Month) of Day;

   Century_Offset          : constant           := 1_900;
   Days_Per_Year           : constant           := 365;
   Days_Per_Month          : constant Month_Day :=
     [1 => 31,
     2  => 28,
     3  => 31,
     4  => 30,
     5  => 31,
     6  => 30,
     7  => 31,
     8  => 31,
     9  => 30,
     10 => 31,
     11 => 30,
     12 => 31];
   First_Day               : constant Day_Name  := Tuesday;
   Seconds_Per_Minute      : constant           := 60;
   Seconds_Per_Hour        : constant           := 60 * Seconds_Per_Minute;
   Seconds_Per_Day         : constant           := 24 * Seconds_Per_Hour;
   Milliseconds_Per_Second : constant           := 1_000;
   Noon                    : constant Hour      := 12;
   Time_Separator          : constant Character := ':';
   Date_Separator          : constant Character := '/';
   Blank                   : constant Character := ' ';
   Comma                   : constant Character := ',';
   Zero                    : constant Character := '0';

   package Natural_Utilities is new Integer_Utilities (Number => Natural);

   package Duration_Utilities is new Fixed_Point_Utilities
     (Number => Duration);

   function Image_Of
     (The_Number : in Natural)
      return String
   is
   begin
      if The_Number < 10 then
         return String_Utilities.Replaced
             (The_Character      => Blank,
              With_The_Character => Zero,
              In_The_String      => Natural_Utilities.Image_Of (The_Number));
      else
         return String_Utilities.Stripped_Leading
             (The_Character   => Blank,
              From_The_String => Natural_Utilities.Image_Of (The_Number));
      end if;
   end Image_Of;

   function Is_Leap_Year
     (The_Year : in Year)
      return Boolean
   is
   begin
      if The_Year mod 100 = 0 then
         return (The_Year mod 400 = 0);
      else
         return (The_Year mod 4 = 0);
      end if;
   end Is_Leap_Year;

   function Days_In
     (The_Year : in Year)
      return Year_Day
   is
   begin
      if Is_Leap_Year (The_Year) then
         return (Days_Per_Year + 1);
      else
         return Days_Per_Year;
      end if;
   end Days_In;

   function Days_In
     (The_Month : in Month;
      The_Year  : in Year)
      return Day
   is
   begin
      if (The_Month = Month_Name'Pos (February) + 1)
        and then Is_Leap_Year (The_Year)
      then
         return (Days_Per_Month (Month_Name'Pos (February) + 1) + 1);
      else
         return Days_Per_Month (The_Month);
      end if;
   end Days_In;

   function Month_Of
     (The_Month : in Month)
      return Month_Name
   is
   begin
      return Month_Name'Val (The_Month - 1);
   end Month_Of;

   function Month_Of
     (The_Month : in Month_Name)
      return Month
   is
   begin
      return (Month_Name'Pos (The_Month) + 1);
   end Month_Of;

   function Day_Of
     (The_Year : in Year;
      The_Day  : in Year_Day)
      return Day_Name
   is
      February_28Th : constant Year_Day := 59;
      Result        : Day_Name          := First_Day;
      procedure Increment
        (The_Day_Nested : in out Day_Name;
         Offset         : in     Natural := 1)
      is
      begin
         The_Day_Nested :=
           Day_Name'Val (Day_Name'Pos (The_Day_Nested) + Offset);
      exception
         when Constraint_Error =>
            The_Day_Nested :=
              Day_Name'Val (Day_Name'Pos (The_Day_Nested) + Offset - 7);
      end Increment;
   begin
      for Index in (Year'First + 1) .. The_Year loop
         if Is_Leap_Year (Index) then
            Increment
              (Result,
               Offset => 2);
         else
            Increment (Result);
         end if;
      end loop;
      if Is_Leap_Year (The_Year) and then (The_Day <= February_28Th) then
         Increment
           (Result,
            Offset => 6);
      end if;
      Increment
        (Result,
         Offset => Natural (((The_Day mod 7) - 1)));
      return Result;
   end Day_Of;

   function Day_Of
     (The_Time : in Time)
      return Year_Day
   is
      Result : Natural := 0;
   begin
      for Index in Month'First .. (The_Time.The_Month - 1) loop
         Result := Result + Natural (Days_In (Index, The_Time.The_Year));
      end loop;
      return Year_Day (Result + Natural (The_Time.The_Day));
   end Day_Of;

   procedure Time_Of
     (The_Year     : in     Year;
      The_Day      : in     Year_Day;
      Result       :    out Time;
      Booch_Status :    out Locus.Time_Of)
   is
      No_Time : constant Time :=
        (The_Year        => Year'Invalid_Value,
         The_Month       => Month'Invalid_Value,
         The_Day         => Day'Invalid_Value,
         The_Hour        => Hour'Invalid_Value,
         The_Minute      => Minute'Invalid_Value,
         The_Second      => Second'Invalid_Value,
         The_Millisecond => Millisecond'Invalid_Value);
      Tmp_Day : Year_Day      := The_Day;
   begin
      for Index in Month'First .. Month'Last loop
         if Tmp_Day <= Year_Day (Days_In (Index, The_Year)) then

            Booch_Status := OK;
            Result       :=
              Time'
                (The_Year        => The_Year,
                 The_Month       => Index,
                 The_Day         => Day (Tmp_Day),
                 The_Hour        => Hour'First,
                 The_Minute      => Minute'First,
                 The_Second      => Second'First,
                 The_Millisecond => Millisecond'First);
            return;

         else
            Tmp_Day := Tmp_Day - Year_Day (Days_In (Index, The_Year));
         end if;
      end loop;

      Result       := No_Time;
      Booch_Status := Lexical_Error;
      Alterable_Log.Log
        (Log_ID  => "C7F9DDA704DD1668",
         Message => "Lexical_Error: Time_Of failed");
      return;

   end Time_Of;

   function Period_Of
     (The_Time : in Time)
      return Period
   is
   begin
      if The_Time.The_Hour >= Noon then
         return Pm;
      else
         return Am;
      end if;
   end Period_Of;

   --  function Time_Of
   --    (The_Time : in Time)
   --     return Ada.Calendar.Time
   --  is
   --  begin
   --     return Ada.Calendar.Time_Of
   --         (Year    => Ada.Calendar.Year_Number (The_Time.The_Year),
   --          Month   => Ada.Calendar.Month_Number (The_Time.The_Month),
   --          Day     => Ada.Calendar.Day_Number (The_Time.The_Day),
   --          Seconds =>
   --            Ada.Calendar.Day_Duration (The_Time.The_Hour) * Seconds_Per_Hour +
   --            Ada.Calendar.Day_Duration (The_Time.The_Minute) *
   --              Seconds_Per_Minute +
   --            Ada.Calendar.Day_Duration (The_Time.The_Second) +
   --            Ada.Calendar.Day_Duration (The_Time.The_Millisecond) /
   --              Milliseconds_Per_Second);
   --  end Time_Of;
   --
   --  function Time_Of
   --    (The_Time : in Ada.Calendar.Time)
   --     return Time
   --  is
   --     Result         : Time;
   --     Total_Duration : Ada.Calendar.Day_Duration;
   --     Seconds        : Natural;
   --  begin
   --     Ada.Calendar.Split
   --       (The_Time,
   --        Year    => Ada.Calendar.Year_Number (Result.The_Year),
   --        Month   => Ada.Calendar.Month_Number (Result.The_Month),
   --        Day     => Ada.Calendar.Day_Number (Result.The_Day),
   --        Seconds => Total_Duration);
   --     Seconds                := Duration_Utilities.Floor (Total_Duration);
   --     Result.The_Hour        := Hour (Seconds / Seconds_Per_Hour);
   --     Seconds                := Seconds mod Seconds_Per_Hour;
   --     Result.The_Minute      := Minute (Seconds / Seconds_Per_Minute);
   --     Result.The_Second      := Second (Seconds mod Seconds_Per_Minute);
   --     Result.The_Millisecond :=
   --       Millisecond
   --         (Duration_Utilities.Real_Part (Total_Duration) *
   --          Milliseconds_Per_Second);
   --     return Result;
   --  end Time_Of;

   function Time_Image_Of
     (The_Time  : in Time;
      Time_Form : in Time_Format := Full)
      return String
   is
   begin
      case Time_Form is
         when Full =>
            if The_Time.The_Hour > Noon then
               return
                 (Image_Of (Natural (The_Time.The_Hour - 12)) &
                  Time_Separator & Image_Of (Natural (The_Time.The_Minute)) &
                  Time_Separator & Image_Of (Natural (The_Time.The_Second)) &
                  Time_Separator &
                  Image_Of (Natural (The_Time.The_Millisecond) / 10) & " PM");
            else
               return
                 (Image_Of (Natural (The_Time.The_Hour)) & Time_Separator &
                  Image_Of (Natural (The_Time.The_Minute)) & Time_Separator &
                  Image_Of (Natural (The_Time.The_Second)) & Time_Separator &
                  Image_Of (Natural (The_Time.The_Millisecond) / 10) & " AM");
            end if;
         when Military =>
            return
              (Image_Of (Natural (The_Time.The_Hour)) & Time_Separator &
               Image_Of (Natural (The_Time.The_Minute)) & Time_Separator &
               Image_Of (Natural (The_Time.The_Second)) & Time_Separator &
               Image_Of (Natural (The_Time.The_Millisecond) / 10));
      end case;
   end Time_Image_Of;

   function Date_Image_Of
     (The_Time  : in Time;
      Date_Form : in Date_Format := Full)
      return String
   is
   begin
      case Date_Form is
         when Full =>
            return
              (Month_Name'Image (Month_Name'Val (The_Time.The_Month - 1)) &
               Natural_Utilities.Image_Of (Natural (The_Time.The_Day)) &
               Comma &
               Natural_Utilities.Image_Of (Natural (The_Time.The_Year)));
         when Month_Day_Year =>
            return
              (Image_Of (Integer (The_Time.The_Month)) & Date_Separator &
               Image_Of (Integer (The_Time.The_Day)) & Date_Separator &
               Image_Of (Integer (The_Time.The_Year)) (4 .. 5));
      end case;
   end Date_Image_Of;

   procedure Value_Of
     (The_Date     : in     String;
      The_Time     : in     String;
      Date_Form    : in     Date_Format := Full;
      Time_Form    : in     Time_Format := Full;
      Result       :    out Time;
      Booch_Status :    out Locus.Value_Of)
   is
      No_Time     : constant Time :=
        (The_Year        => Year'Invalid_Value,
         The_Month       => Month'Invalid_Value,
         The_Day         => Day'Invalid_Value,
         The_Hour        => Hour'Invalid_Value,
         The_Minute      => Minute'Invalid_Value,
         The_Second      => Second'Invalid_Value,
         The_Millisecond => Millisecond'Invalid_Value);
      Left_Index  : Positive;
      Right_Index : Positive;
      Tmp_Status  : Natural_Utilities.Locus.Value_Of;
      Tmp_Number  : Natural;
   begin

      Result := No_Time;

      case Date_Form is
         when Full =>
            Right_Index      := String_Utilities.Location_Of
                (The_Character => Blank,
                 In_The_String => The_Date);
            Result.The_Month :=
              Month
                (Month_Name'Pos
                   (Month_Name'Value
                      (The_Date (The_Date'First .. (Right_Index - 1)))) +
                 1);
            Left_Index       := Right_Index + 1;
            Right_Index      := String_Utilities.Location_Of
                (The_Character => Comma,
                 In_The_String => The_Date (Left_Index .. The_Date'Last));

            Natural_Utilities.Value_Of
              (The_Image    => The_Date (Left_Index .. (Right_Index - 1)),
               Result       => Tmp_Number,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Lexical_Error =>
                  Alterable_Log.Log
                    (Log_ID  => "189E09AD2B32D108",
                     Message => "Lexical_Error: Value_Of Time failed");
                  Booch_Status := Tmp_Status;
                  return;

               when OK =>
                  Result.The_Day := Day (Tmp_Number);

            end case;

            Left_Index := Right_Index + 1;

            Natural_Utilities.Value_Of
              (The_Image    => The_Date (Left_Index .. The_Date'Last),
               Result       => Tmp_Number,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Lexical_Error =>
                  Alterable_Log.Log
                    (Log_ID  => "6DDF508FB5D9BCB3",
                     Message => "Lexical_Error: Value_Of Time failed");
                  Booch_Status := Tmp_Status;
                  return;

               when OK =>
                  Result.The_Year := Year (Tmp_Number);

            end case;

         when Month_Day_Year =>
            Right_Index := String_Utilities.Location_Of
                (The_Character => Date_Separator,
                 In_The_String => The_Date);

            Natural_Utilities.Value_Of
              (The_Image    => The_Date (The_Date'First .. (Right_Index - 1)),
               Result       => Tmp_Number,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Lexical_Error =>
                  Alterable_Log.Log
                    (Log_ID  => "9C686C45B94FD310",
                     Message => "Lexical_Error: Value_Of Time failed");
                  Booch_Status := Tmp_Status;
                  return;

               when OK =>
                  Result.The_Month := Month (Tmp_Number);
            end case;

            Left_Index := Right_Index + 1;

            Right_Index := String_Utilities.Location_Of
                (The_Character => Date_Separator,
                 In_The_String => The_Date (Left_Index .. The_Date'Last));

            Natural_Utilities.Value_Of
              (The_Image    => The_Date (Left_Index .. (Right_Index - 1)),
               Result       => Tmp_Number,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Lexical_Error =>
                  Alterable_Log.Log
                    (Log_ID  => "D8AD6D7432A297D9",
                     Message => "Lexical_Error: Value_Of Time failed");
                  Booch_Status := Tmp_Status;
                  return;

               when OK =>
                  Result.The_Day := Day (Tmp_Number);

            end case;

            Left_Index := Right_Index + 1;

            Natural_Utilities.Value_Of
              (The_Image    => (The_Date (Left_Index .. The_Date'Last)),
               Result       => Tmp_Number,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Lexical_Error =>

                  Alterable_Log.Log
                    (Log_ID  => "BC7F114563C9C5E3",
                     Message => "Lexical_Error: Value_Of Time failed");
                  Booch_Status := Tmp_Status;
                  return;

               when OK =>
                  Result.The_Year :=
                    Year ((Tmp_Number) + Natural (Century_Offset));

            end case;
      end case;

      Right_Index := String_Utilities.Location_Of
          (The_Character => Time_Separator,
           In_The_String => The_Time);

      Natural_Utilities.Value_Of
        (The_Image    => The_Time (The_Time'First .. (Right_Index - 1)),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "45AAE0C7916E96C9",
               Message => "Lexical_Error: Value_Of Time failed");
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            Result.The_Hour := Hour (Tmp_Number);

      end case;

      Left_Index  := Right_Index + 1;
      Right_Index := String_Utilities.Location_Of
          (The_Character => Time_Separator,
           In_The_String => The_Time (Left_Index .. The_Time'Last));

      Natural_Utilities.Value_Of
        (The_Image    => The_Time (Left_Index .. (Right_Index - 1)),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "5F5050A07B115913",
               Message => "Lexical_Error: Value_Of Time failed");
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            Result.The_Minute := Minute (Tmp_Number);
      end case;

      Left_Index  := Right_Index + 1;
      Right_Index := String_Utilities.Location_Of
          (The_Character => Time_Separator,
           In_The_String => The_Time (Left_Index .. The_Time'Last));

      Natural_Utilities.Value_Of
        (The_Image    => The_Time (Left_Index .. (Right_Index - 1)),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "5FEAD5065E3E21B5",
               Message => "Lexical_Error: Value_Of Time failed");
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            Result.The_Second := Second (Tmp_Number);
      end case;

      Left_Index := Right_Index + 1;

      case Time_Form is
         when Full =>
            Right_Index := String_Utilities.Location_Of
                (The_Character => Blank,
                 In_The_String => The_Time (Left_Index .. The_Time'Last));

            Natural_Utilities.Value_Of
              (The_Image    => The_Time (Left_Index .. (Right_Index - 1)),
               Result       => Tmp_Number,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Lexical_Error =>
                  Alterable_Log.Log
                    (Log_ID  => "E62C74B71D0761E5",
                     Message => "Lexical_Error: Value_Of Time failed");
                  Booch_Status := Tmp_Status;
                  return;

               when OK =>
                  Result.The_Millisecond := Millisecond (Tmp_Number);
            end case;

            Left_Index := Right_Index + 1;

            if Period'Value (The_Time (Left_Index .. The_Time'Last)) = Pm then
               if Result.The_Hour /= Noon then
                  Result.The_Hour := Result.The_Hour + Noon;
               end if;
            end if;

         when Military =>

            Natural_Utilities.Value_Of
              (The_Image    => The_Time (Left_Index .. The_Time'Last),
               Result       => Tmp_Number,
               Booch_Status => Tmp_Status);

            case Tmp_Status is
               when Lexical_Error =>
                  Alterable_Log.Log
                    (Log_ID  => "DA2978378BC22E9D",
                     Message => "Lexical_Error: Value_Of Time failed");
                  Booch_Status := Tmp_Status;
                  return;

               when OK =>
                  Result.The_Millisecond := Millisecond (Tmp_Number);

            end case;

      end case;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "B62F6874C6736DD6",
            Message => "Lexical_Error: Value_Of Time failed");
         Booch_Status := Lexical_Error;
         return;

   end Value_Of;

   function Duration_Of
     (The_Interval : in Interval)
      return Duration
   is
   begin
      return
        (Duration (The_Interval.Elapsed_Days) * Seconds_Per_Day +
         Duration (The_Interval.Elapsed_Hours) * Seconds_Per_Hour +
         Duration (The_Interval.Elapsed_Minutes) * Seconds_Per_Minute +
         Duration (The_Interval.Elapsed_Seconds) +
         Duration (The_Interval.Elapsed_Milliseconds) /
           Milliseconds_Per_Second);
   end Duration_Of;

   function Interval_Of
     (The_Duration : in Duration)
      return Interval
   is
      Result      : Interval;
      The_Seconds : Duration := The_Duration;
   begin
      Result.Elapsed_Days         :=
        Duration_Utilities.Floor (The_Seconds / Seconds_Per_Day);
      The_Seconds                 :=
        The_Seconds - Duration (Result.Elapsed_Days) * Seconds_Per_Day;
      Result.Elapsed_Hours        :=
        Hour (Duration_Utilities.Floor (The_Seconds / Seconds_Per_Hour));
      The_Seconds                 :=
        The_Seconds - Duration (Result.Elapsed_Hours) * Seconds_Per_Hour;
      Result.Elapsed_Minutes      :=
        Minute (Duration_Utilities.Floor (The_Seconds / Seconds_Per_Minute));
      The_Seconds                 :=
        The_Seconds - Duration (Result.Elapsed_Minutes) * Seconds_Per_Minute;
      Result.Elapsed_Seconds      :=
        Second (Duration_Utilities.Floor (The_Seconds));
      The_Seconds := The_Seconds - Duration (Result.Elapsed_Seconds);
      Result.Elapsed_Milliseconds :=
        Millisecond
          (Duration_Utilities.Floor (The_Seconds * Milliseconds_Per_Second));
      return Result;
   end Interval_Of;

   function Image_Of
     (The_Interval : in Interval)
      return String
   is
   begin
      return
        (Image_Of (The_Interval.Elapsed_Days) & Time_Separator &
         Image_Of (Natural (The_Interval.Elapsed_Hours)) & Time_Separator &
         Image_Of (Natural (The_Interval.Elapsed_Minutes)) & Time_Separator &
         Image_Of (Natural (The_Interval.Elapsed_Seconds)) & Time_Separator &
         Image_Of (Natural (The_Interval.Elapsed_Milliseconds)));
   end Image_Of;

   procedure Value_Of
     (The_Interval : in     String;
      Result       :    out Interval;
      Booch_Status :    out Locus.Value_Of)
   is
      No_Interval : constant Interval :=
        (Elapsed_Days         => Natural'Invalid_Value,
         Elapsed_Hours        => Hour'Invalid_Value,
         Elapsed_Minutes      => Minute'Invalid_Value,
         Elapsed_Seconds      => Second'Invalid_Value,
         Elapsed_Milliseconds => Millisecond'Invalid_Value);
      Left_Index  : Positive;
      Right_Index : Positive;
      Tmp_Status  : Natural_Utilities.Locus.Value_Of;
      Tmp_Number  : Natural;

   begin
      Result := No_Interval;

      Right_Index := String_Utilities.Location_Of
          (The_Character => Time_Separator,
           In_The_String => The_Interval);

      Natural_Utilities.Value_Of
        (The_Image => The_Interval (The_Interval'First .. (Right_Index - 1)),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "6D2FDFAB1C3BE263",
               Message => "Lexical_Error: Value_Of Interval failed");
            Booch_Status := Tmp_Status;
            Result       := No_Interval;
            return;

         when OK =>
            Result.Elapsed_Days := Tmp_Number;
      end case;

      Left_Index  := Right_Index + 1;
      Right_Index := String_Utilities.Location_Of
          (The_Character => Time_Separator,
           In_The_String => The_Interval (Left_Index .. The_Interval'Last));

      Natural_Utilities.Value_Of
        (The_Image    => The_Interval (Left_Index .. (Right_Index - 1)),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "24F1847BAFC2D78C",
               Message => "Lexical_Error: Value_Of Interval failed");
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            Result.Elapsed_Hours := Hour (Tmp_Number);
      end case;

      Left_Index  := Right_Index + 1;
      Right_Index := String_Utilities.Location_Of
          (The_Character => Time_Separator,
           In_The_String => The_Interval (Left_Index .. The_Interval'Last));

      Natural_Utilities.Value_Of
        (The_Image    => The_Interval (Left_Index .. (Right_Index - 1)),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>

            Alterable_Log.Log
              (Log_ID  => "1399261FFD58D47B",
               Message => "Lexical_Error: Value_Of Interval failed");
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            Result.Elapsed_Minutes := Minute (Tmp_Number);
      end case;

      Left_Index  := Right_Index + 1;
      Right_Index := String_Utilities.Location_Of
          (The_Character => Time_Separator,
           In_The_String => The_Interval (Left_Index .. The_Interval'Last));

      Natural_Utilities.Value_Of
        (The_Image    => The_Interval (Left_Index .. (Right_Index - 1)),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "1F5161333F3E9872",
               Message => "Lexical_Error: Value_Of Interval failed");
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            Result.Elapsed_Seconds := Second (Tmp_Number);
      end case;

      Left_Index := Right_Index + 1;

      Natural_Utilities.Value_Of
        (The_Image    => The_Interval (Left_Index .. The_Interval'Last),
         Result       => Tmp_Number,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "2AF2DE8239A853DF",
               Message => "Lexical_Error: Value_Of Interval failed");
            Booch_Status := Tmp_Status;
            Result       := No_Interval;
            return;

         when OK =>
            Result.Elapsed_Milliseconds := Millisecond (Tmp_Number);
      end case;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "E443669A9D5388A6",
            Message => "Lexical_Error: Value_Of Interval failed");
         Booch_Status := Lexical_Error;
         Result       := No_Interval;
         return;

   end Value_Of;

end Booch_Light.Calendar_Utilities;

--              Original Booch Components (Ada 83 version)
--  License: MIT
--  Copyright (C) 1987 Grady Booch Copyright (C) 2024 Kevin Chadwick (Light
--  runtime compatibility)
--
--  Permission is hereby granted, free of charge, to any person obtaining
--  a copy of this software and associated documentation files (the
--  “Software”), to deal in the Software without restriction, including
--  without limitation the rights to use, copy, modify, merge, publish,
--  distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to
--  the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
