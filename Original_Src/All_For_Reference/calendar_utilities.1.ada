--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

with Calendar;
package Calendar_Utilities is

    type Year        is new Calendar.Year_Number;
    type Month       is range 1 ..  12;
    type Day         is range 1 ..  31;
    type Hour        is range 0 ..  23;
    type Minute      is range 0 ..  59;
    type Second      is range 0 ..  59;
    type Millisecond is range 0 .. 999;

    type Time is
        record
            The_Year        : Year;
            The_Month       : Month;
            The_Day         : Day;
            The_Hour        : Hour;
            The_Minute      : Minute;
            The_Second      : Second;
            The_Millisecond : Millisecond;
        end record;

    type Interval is
        record
            Elapsed_Days         : Natural;
            Elapsed_Hours        : Hour;
            Elapsed_Minutes      : Minute;
            Elapsed_Seconds      : Second;
            Elapsed_Milliseconds : Millisecond;
        end record;

    type Year_Day    is range 1 .. 366;

    type Month_Name  is (January, February, March, April, May, June, July, 
                         August, September, October, November, December);
    type Day_Name    is (Monday, Tuesday, Wednesday, 
                         Thursday, Friday, Saturday, Sunday);

    type Period      is (Am, Pm);

    type Time_Format is (Full,            -- 01:21:06:30 PM
                         Military);       -- 13:21:06:30
    type Date_Format is (Full,            -- FEBRUARY 27, 1955
                         Month_Day_Year); -- 02/27/55

    function Is_Leap_Year  (The_Year  : in Year)          return Boolean;
    function Days_In       (The_Year  : in Year)          return Year_Day;
    function Days_In       (The_Month : in Month;         
                            The_Year  : in Year)          return Day;
    function Month_Of      (The_Month : in Month)         return Month_Name;
    function Month_Of      (The_Month : in Month_Name)    return Month;
    function Day_Of        (The_Year  : in Year;          
                            The_Day   : in Year_Day)      return Day_Name;
    function Day_Of        (The_Time  : in Time)          return Year_Day;
    function Time_Of       (The_Year  : in Year;          
                            The_Day   : in Year_Day)      return Time;
    function Period_Of     (The_Time  : in Time)          return Period;
    function Time_Of       (The_Time  : in Time)          return Calendar.Time;
    function Time_Of       (The_Time  : in Calendar.Time) return Time;

    function Time_Image_Of (The_Time  : in Time; 
                            Time_Form : in Time_Format := Full) 
                                                          return String;
    function Date_Image_Of (The_Time  : in Time; 
                            Date_Form : in Date_Format := Full) 
                                                          return String;
    function Value_Of      (The_Date  : in String; 
                            The_Time  : in String; 
                            Date_Form : in Date_Format := Full; 
                            Time_Form : in Time_Format := Full) 
                                                          return Time;

    function Duration_Of (The_Interval : in Interval)     return Duration;
    function Interval_Of (The_Duration : in Duration)     return Interval;
    function Image_Of    (The_Interval : in Interval)     return String;
    function Value_Of    (The_Interval : in String)       return Interval;

    Lexical_Error : exception;

end Calendar_Utilities;

--              Original Booch Components (Ada 83 version)
--                               
-- Copyright (C) 2000 Grady Booch
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
