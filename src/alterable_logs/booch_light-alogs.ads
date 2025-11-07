with Elogs;
with Elogs_Config;

--  This child package provides a convenient place to facilitate adding your
--  own procedures into the logging system without modifying every elog call
--  or maintaining a variation of the elog package itself. You will need to
--  maintain your own changes. Suggestions of a better way of facilitating
--  this are welcome.
package Booch_Light.Alogs is

   procedure Log
     (Log_ID  : Elogs.Log_ID_Type;
      Message : String) with
     Pre => Message'Length < Elogs.Max_Message_Length;

   procedure Status_Exception
     (Log_ID  : Elogs.Log_ID_Type;
      Message : String) with
     Pre =>
      Message'Length <
      (Elogs.Max_Message_Length - Elogs.Exceptive_Prepend'Length);

end Booch_Light.Alogs;

--  License: MIT
--  Copyright (C) 2024 Kevin Chadwick (Elansys Limited)
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
