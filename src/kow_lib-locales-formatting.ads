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


-- TODO :: numeric and money formatting

--------------
-- Ada 2005 --
--------------
with Ada.Calendar;			use Ada.Calendar; 
with Ada.Calendar.Time_Zones;
with Ada.Strings.Unbounded; 		use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;


package KOW_Lib.Locales.Formatting is



	----------------
	-- Exceptions --
	----------------
	Pattern_Error : exception;
	-- raised whenever the pattern is not in the expected format


	------------------------------
	-- Date and Time formatting --
	------------------------------

	--  The pattern is a string to describe date and time output format. The string is
	--  a set of standard character and special tag that are replaced by the
	--  corresponding values. It follows an extended subset of the GNU Date specification.
	--  Here are the recognized directives :
	--
	--          Time fields:
	--
	--          %H   hour (00..23)
	--          %I   hour (01..12)
	--          %M   minute (00..59)
	--          %p   AM or PM
	--          %S   second (00..59)
	--          %T   locale's default time
	--
	--          Date fields:
	--
	--          %a   locale's abbreviated weekday name (Sun..Sat)
	--          %A   locale's full weekday name, variable length (Sunday..Saturday)
	--          %b   locale's abbreviated month name (Jan..Dec)
	--          %B   locale's full month name, variable length (January..December)
	--          %c   default locale's date and time (Sat Nov 04 12:02:33 EST 1989)
	--          %d   day of month (01..31)
	--          %D   default locale's date (in english it'd be mm/dd/yy)
	--          %m   month (01..12)
	--          %w   day of week (0..6) with 0 corresponding to Sunday
	--          %y   last two digits of year (00..99)
	--          %Y   year (1970...)
	--
	--  Each pattern involving numbers also accepts the padding modifier, which consists of:
	--          -    don't prepend with anything
	--          _    append blank spaces to the left
	--          0    append zeros to the left (default)
	--  For example:
	--          %-m  in jan will return "1".
	--          %_m  in jan will return " 1".
	--          %0m  in jan will return "01".
	--          %m   in jan will return "01".



	function Format(
				L		: in Locale_Type;
				T		: in Time;
				Pattern		: in String
			) return String;
	-- format the time variable using the T's time offset and given pattern


	function Format(
				L		: in Locale_Type;
				T		: in Time;
				Pattern		: in String;
				Time_Zone	: in Time_Zones.Time_Offset 
			) return String;
	-- format the time variable using the given time offset and pattern

	



	---------------------
	-- Name Formatting --
	---------------------

	function Full_Name(
				L		: in Locale_Type;
				First_Name	: in String;
				Last_Name	: in String
			) return String;
	-- format the given name
	

	-----------------------
	-- Number Formatting --
	-----------------------

	function To_String(
				L	: in Locale_Type;
				F	: in Long_Float;
				Dec	: in Natural := 2
			) return String;
	-- Format the number acording to the given locale, using
	-- Dec decimal digits.

	
	-------------------------------
	-- Generic Formatting Engine --
	-------------------------------

	generic 
		with function Value_Of( Key : in String ) return String;
	function Generic_Format( Pattern : in String ) return String;
	-- looks for instances of %SOMEKEY, and call Value_Of for those values.
	-- Then replace it in the resulting string.
	--
	--
	-- The current implementation relies on Unbounded_String internally
	
end KOW_Lib.Locales.Formatting;


