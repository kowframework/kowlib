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
-- This is the KOW_Lib.Locales package                                      --
--                                                                          --
-- Provides functions to get patterns that describe dates or times and      --
-- functions that return a formatted number, date or name according to      --
-- a specific Locale                                                        --
------------------------------------------------------------------------------


with Ada.Calendar;			use Ada.Calendar;
with Ada.Calendar.Formatting;		use Ada.Calendar.Formatting;
with Ada.Containers;			use Ada.Containers;
with Ada.Containers.Hashed_Maps;


package KOW_Lib.Locales is 
	pragma Elaborate_Body;


	INVALID_LOCALE_CODE		: exception;
	INVALID_DEFAULT_LOCALE_CODE	: exception;


	---------------------
	-- The Locale Code --
	---------------------

	type ISO_Code_Type is new String( 1 .. 2 );
	type Locale_Code_Type is record
		Language	: ISO_Code_Type := ( others => ' ' );
		Country		: ISO_Code_Type := ( others => ' ' );
	end record;


	function From_String( Str : in String ) return Locale_Code_Type;
	function To_String( Locale_Code : in Locale_Code_Type ) return String;
	-- convert from/to the ISO locale code format 

	ISO_Locale_Code : Locale_Code_Type;
	-- the ISO locale is a meta locale; it's for formatting the date/time variables into
	-- the ISO 8601 standard
	--
	-- it's also the default locale when no LOCALE environment variable is set.


	function Get_Default_Locale_Code return Locale_Code_Type;
	-- get the default locale code from the locale variable

	--------------------
	-- Auxiliar Types --
	--------------------

	type String_Access is access String;
	function To_String( Str : in String_Access ) return String;
	-- gets the value (being "" when str is null)

	subtype Separator_Type is Character;

	type Month_String_Array is Array( Month_Number'Range ) of String_Access;
	type Day_String_Array is Array( Day_Name'Range ) of String_Access;

	---------------------
	-- The Locale Type --
	---------------------

	type Locale_Type is record
		Code		: Locale_Code_Type;

		Label		: String_Access;

		Default_Date	: String_Access;
		Default_Time	: String_Access;
		Default_Datetime: String_Access;

		Short_Date	: String_Access;
		Long_Date	: String_Access;


		
		Currency	: String_Access;
		-- prefix for the currency
		
		Thousands_Separator	: Separator_Type;
		Decimal_Separator	: Separator_Type;

		
		Week_Days	: Day_String_Array;

		Short_Week_Days	: Day_String_Array;
		
		
		Months		: Month_String_Array;
		Short_Months	: Month_String_Array;


		Person_Name_Img	: String_Access;
	end record;


	---------------------
	-- Locale Registry --
	---------------------

	procedure Iterate(
				Iterator	: not null access procedure( Locale : in Locale_Type )
			);
	-- iterate over all supported locales
	
	function Get_Locale(
				Locale_Code	: in Locale_Code_Type := Get_Default_Locale_Code 
			) return Locale_Type;
	-- get the given locale


	procedure Register(
				Locale	: in Locale_Type
			);
	-- register a new locale


private

	function Hash( Key : in Locale_Code_Type ) return Hash_Type;
	package Locale_Maps is new Ada.Containers.Hashed_Maps(
						Key_Type	=> Locale_Code_Type,
						Element_Type	=> Locale_Type,
						Hash		=> Hash,
      						Equivalent_Keys	=> "="
					);

	Supported_Locales : Locale_Maps.Map;
	-- Map with all Supported Locales.



	LOCALE_VARIABLE : constant String := "LOCALE";

end KOW_Lib.Locales;

