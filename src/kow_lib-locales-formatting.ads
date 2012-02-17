------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Library                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWLib is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWLib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWLib; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- This is the KOW_Lib.Locales.Formatting package                                   --
--                                                                          --
-- Here is where all the locale formatting methods are implemented          --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Calendar;			use Ada.Calendar; 
with Ada.Calendar.Time_Zones;
with Ada.Strings.Unbounded; 		use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Locales; 			use KOW_Lib.Locales;

package KOW_Lib.Locales.Formatting is


	------------------------------
	-- Date and Time formatting --
	------------------------------

	Pattern_Error : exception;
  	--  Exception raised for incorrect pattern

	--  The pattern is a string to describe date and time output format. The string is
	--  a set of standard character and special tag that are replaced by the
	--  corresponding values. It follows the GNU Date specification. Here are
	--  the recognized directives :
	--
	--          Time fields:
	--
	--          %H   hour (00..23)
	--          %I   hour (01..12)
	--          %k   hour ( 0..23)
	--          %l   hour ( 1..12)
	--          %M   minute (00..59)
	--          %p   locale's AM or PM
	--          %r   time, 12-hour (hh:mm:ss [AP]M)
	--          %s   seconds  since 1970-01-01  00:00:00 UTC
	--                (a nonstandard extension)
	--          %S   second (00..59)
	--          %T   time, 24-hour (hh:mm:ss)
	--
	--          Date fields:
	--
	--          %a   locale's abbreviated weekday name (Sun..Sat)
	--          %A   locale's    full   weekday   name,    variable   length
	--                  (Sunday..Saturday)
	--          %b   locale's abbreviated month name (Jan..Dec)
	--          %B   locale's    full    month    name,   variable    length
	--                  (January..December)
	--          %c   locale's date and time (Sat Nov 04 12:02:33 EST 1989)
	--          %d   day of month (01..31)
	--          %D   date (mm/dd/yy)
	--          %h   same as %b
	--          %m   month (01..12)
	--          %w   day of week (0..6) with 0 corresponding to Sunday
	--          %x   locale's date representation (mm/dd/yy)
	--          %y   last two digits of year (00..99)
	--          %Y   year (1970...)



	function Format(
				Pattern		: in Locales.String_Access;
				T		: in Time
			) return String;
	-- format the time variable using the T's time offset and given pattern


	function Format(
				Pattern		: in String;
				T		: in Time;
				Time_Zone	: in Time_Zones.Time_Offset 
			) return String;
	-- format the time variable using the given time offset and pattern

	
	
end KOW_Lib.Locales.Formatting;


