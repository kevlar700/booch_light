--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

--  with Ada.Calendar;
package Booch_Light.Calendar_Utilities is

   package Locus is

      subtype Value_Of is Status_Code with
          Static_Predicate => Value_Of in Lexical_Error | OK;

      subtype Time_Of is Status_Code with
          Static_Predicate => Time_Of in Lexical_Error | OK;

   end Locus;

   --  type Year is new Ada.Calendar.Year_Number;
   type Year is new Integer range 1_901 .. 2_399;
   type Month is range 1 .. 12;
   type Day is range 1 .. 31;
   type Hour is range 0 .. 23;
   type Minute is range 0 .. 59;
   type Second is range 0 .. 59;
   type Millisecond is range 0 .. 999;

   type Time is record
      The_Year        : Year;
      The_Month       : Month;
      The_Day         : Day;
      The_Hour        : Hour;
      The_Minute      : Minute;
      The_Second      : Second;
      The_Millisecond : Millisecond;
   end record;

   type Interval is record
      Elapsed_Days         : Natural;
      Elapsed_Hours        : Hour;
      Elapsed_Minutes      : Minute;
      Elapsed_Seconds      : Second;
      Elapsed_Milliseconds : Millisecond;
   end record;

   type Year_Day is range 1 .. 366;

   type Month_Name is
     (January,
      February,
      March,
      April,
      May,
      June,
      July,
      August,
      September,
      October,
      November,
      December);

   type Day_Name is
     (Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday,
      Sunday);

   type Period is
     (Am,
      Pm);

   type Time_Format is
     (Full,            -- 01:21:06:30 PM
      Military);       -- 13:21:06:30

   type Date_Format is
     (Full,            -- FEBRUARY 27, 1955
      Month_Day_Year); -- 02/27/55

   function Is_Leap_Year
     (The_Year : Year)
      return Boolean;

   function Days_In
     (The_Year : Year)
      return Year_Day;

   function Days_In
     (The_Month : Month;
      The_Year  : Year)
      return Day;

   function Month_Of
     (The_Month : Month)
      return Month_Name;

   function Month_Of
     (The_Month : Month_Name)
      return Month;

   function Day_Of
     (The_Year : Year;
      The_Day  : Year_Day)
      return Day_Name;

   function Day_Of
     (The_Time : Time)
      return Year_Day;

   procedure Time_Of
     (The_Year     :     Year;
      The_Day      :     Year_Day;
      Result       : out Time;
      Booch_Status : out Locus.Time_Of);

   function Period_Of
     (The_Time : Time)
      return Period;

   --  function Time_Of
   --    (The_Time : Time)
   --     return Ada.Calendar.Time;
   --
   --  function Time_Of
   --    (The_Time : Ada.Calendar.Time)
   --     return Time;

   --  TODO: Replace as recursion is not permitted in this repo
   --  function Time_Image_Of
   --       (The_Time  : Time;
   --        Time_Form : Time_Format := Full)
   --        return String;

   --  TODO: Replace as recursion is not permitted in this repo
   --  function Date_Image_Of
   --    (The_Time  : Time;
   --     Date_Form : Date_Format := Full)
   --     return String;

   procedure Value_Of
     (The_Date     :     String;
      The_Time     :     String;
      Date_Form    :     Date_Format := Full;
      Time_Form    :     Time_Format := Full;
      Result       : out Time;
      Booch_Status : out Locus.Value_Of);

   function Duration_Of
     (The_Interval : Interval)
      return Duration;

   function Interval_Of
     (The_Duration : Duration)
      return Interval;

   --  TODO: Replace as recursion is not permitted in this repo
   --  function Image_Of
   --    (The_Interval : Interval)
   --     return String;

   procedure Value_Of
     (The_Interval :     String;
      Result       : out Interval;
      Booch_Status : out Locus.Value_Of);

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
