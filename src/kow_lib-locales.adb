------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- KOW_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- This is the KOW_Lib.Locales package                                  	    --
--                                                                          --
-- Provides functions to get patterns that describe dates or times and      --
-- functions that return a formatted number, date or name according to      --
-- a specific Locale                                                        --
------------------------------------------------------------------------------


with Ada.Containers.Hashed_Maps;	use Ada.Containers; 
with Ada.Calendar; 			use Ada.Calendar;
with Ada.Calendar.Formatting;		use Ada.Calendar.Formatting;
with Ada.Strings;  			use Ada.Strings;
with Ada.Strings.Fixed;  
with Ada.Characters.Handling;		use Ada.Characters.Handling;

with KOW_Lib.String_Util;	
with Ada.Text_IO;			use Ada.Text_IO;

package body KOW_Lib.Locales is
	

	function Get_Locale_Envvar return String is
	begin
		return "LOCALE"; 
	end Get_Locale_Envvar;
	
	function Get_Locale_Envvar_Value return String is
		use Ada.Environment_Variables;
	begin
		if Exists( Get_Locale_Envvar ) then
		 	return Value(Get_Locale_Envvar);
		else
			raise LOCALE_ENVVAR_INVALID with
				"Locale Environment Variable """ &
				Get_Locale_Envvar & """does not exist.";
		end if;
	end Get_Locale_Envvar_Value;
	
	function Get_Environment_Locale return Locale is
	begin
		return Get_Locale( Get_Locale_Envvar_Value ); 
	end Get_Environment_Locale; 

	
	function Get_Locale( Code: String ) return Locale is
		pragma Inline(Get_Locale);
	begin
		return Get_Locale(To_Unbounded_String(Code));
	end Get_Locale;
	
	
	function Get_Locale( Code: Locale_Code ) return Locale is
	-- gets a code like:
	-- 	ll_CC_LL
	-- where
	-- 	ll => language
	-- 	CC => country code
	-- 	LL => locale code inside country
	--
	-- Find in order:
	-- 	ll_CC_LL
	-- 	ll_CC
	-- 	ll
	-- 
	-- raise exception LOCALE_NOT_SUPPORTED when not found results.

	Tmp: Unbounded_String := Code;
	
	use Locale_Tables;

	begin
		while Find(Supported_Locales, Tmp) = No_Element 
		loop
			if (Length(Tmp) - 3) > 1 then
				Tmp := Head(Tmp, (Length(Tmp)- 3));
			else
				raise LOCALE_NOT_SUPPORTED with To_String( Code );
			end if;		
		end loop;

		return Locale_Tables.Element(Supported_Locales, Tmp);
	end Get_Locale;


	procedure Set_Default_Locale(L: in Locale ) is
	begin
		DEFAULT_LOCALE := L;
	end Set_Default_Locale;


	function Get_Default_Locale return Locale is
	begin
		return DEFAULT_LOCALE;
	end Get_Default_Locale;



	function Image( L: in Locale; D: in Day_Name; short: in Boolean)
		return String is
	begin
		if short = true then
			return To_String(L.DAY_OF_WEEK_SHORT_NAMES(D));
		else
			return To_String(L.DAY_OF_WEEK_NAMES(D));
		end if;
	end Image;

	
	function Image( D: in Day_Name; short: in Boolean)
		return String is


	begin
		return Image(Get_Default_Locale, D, short);
	end Image;

	

	function Image( L: in Locale; D: in Month_Number; short: in Boolean)
		return String is
	
	begin
		if short = true then
		 	return To_String(L.MONTH_SHORT_NAMES(D));
		else
		 	return To_String(L.MONTH_NAMES(D));
		end if;
	end Image;

	function Image( D: in Month_Number; short: in Boolean)
		return String is
	
	begin
		return Image(Get_Default_Locale, D, short);
	end Image;
	


	function Get_Short_Date_Pattern(L: in Locale)
		return String is
	begin
		return To_String(L.DEFAULT_SHORT_DATE);
	end Get_Short_Date_Pattern;
	
	function Get_Long_Date_Pattern(L: in Locale) return
		String is
	begin
		return To_String(L.DEFAULT_LONG_DATE);
	end Get_Long_Date_Pattern;
	
	function Get_Short_Date_Pattern	return String is
	begin
		return To_String(Get_Default_Locale.DEFAULT_SHORT_DATE);
	end Get_Short_Date_Pattern;

	function Get_Long_Date_Pattern return String is
	begin
		return To_String(Get_Default_Locale.DEFAULT_LONG_DATE);
	end Get_Long_Date_Pattern;


	function Get_Default_Time_Pattern( L: in Locale )
		return String is
	begin
		return To_String(L.DEFAULT_TIME);
	end Get_Default_Time_Pattern;


	function Get_Default_Date_Pattern( L: in Locale )
		return String is
	begin
		return To_String(L.DEFAULT_DATE);
	end Get_Default_Date_Pattern;
	


	
	function Get_Formatted_Number(L: in Locale; n: in Long_Float)
		return String is
	begin
		-- format with precision of 2 decimal places
		return Get_Formatted_Number(L, n, 2);
	end Get_Formatted_Number;
	
	function Get_Formatted_Number(	L: in Locale;
					n: in Long_Float;
					decimal_places : in Integer) return String is
		-- format according to Locale L, inserting decimal and
		-- thousand separators on n and with precision of 
		-- decimal_places decimal places.

		Integer_Value : Integer := Integer(Long_Float'Truncation(n));
		
		Decimal_Value : Integer := Integer(Long_Float'Truncation((n
			- (Long_Float'Truncation(n))) * 10.0 ** decimal_places )); 

		
		function Trim (str : in String) return String is
		begin	
			return Ada.Strings.Fixed.Trim (str, Both);
		end Trim;
		
		Integer_Part : String := Trim(Integer'Image(Integer_Value));
		Decimal_Part : String := Trim(Integer'Image(Decimal_Value));		
		
		Separs 			: Integer;
		Integer_Length 	: Integer;
		Res_Length 		: Integer;
	
	
	begin
		Separs := Integer_Part'Length/3 -1;
		if Integer_Part'Length rem 3 > 0 then
			Separs := Separs + 1;
		end if;

		Integer_Length := Integer_Part'Length + Separs;
		Res_Length := Integer_Length;
		if decimal_places > 0 then
			Res_Length := Res_Length + Decimal_Places + 1;
			--Decimal_Part'Length + 1; 
		end if;

		declare
			Result : String( 1 .. Res_Length );
			Index : Integer := 1;
			Separs_Idx : Integer := 0;
		begin
	
			-- inserting the thousands separators
			while Index <= Integer_Length loop
				if Index mod 4 = 0 then
					Result(Integer_Length + 1 - Index) :=
						L.THOUSANDS_SEPARATOR;
					Separs_Idx := Separs_Idx + 1;
				else
					Result(Integer_Length + 1 - Index) :=
						Integer_Part(
						Integer_Part'Length + 1 -
						Index + Separs_Idx);
				end if;
				
				Index := Index + 1;
			end loop;
			
			-- inserting the decimal separators
			if decimal_places > 0 then
				Result(Index) := L.DECIMAL_SEPARATOR;
				Index := Index + 1;
				
				for k in Decimal_Part'Range loop
					Result(Index) := Decimal_Part(k);	
					Index := Index + 1;
				end loop;

				while Index <= Result'Last loop
					Result(Index) := '0';
					Index := Index + 1;
				end loop;
			end if;
	
			return Result;
		end;

	end Get_Formatted_Number;



	function Get_Formatted_Currency(L: in Locale; n: in Long_Float)
		return String is
	begin
		return To_String(L.CURRENCY_PREFIX) & " " &
			Get_Formatted_Number(L, n);
	end Get_Formatted_Currency;


	function Get_Formatted_Currency(L: in Locale; n: in Long_Float;
		decimal_places : in Integer) return String is
	begin
		return To_String(L.CURRENCY_PREFIX) & " " &
			Get_Formatted_Number(L, n, decimal_places);
	end Get_Formatted_Currency;

		

	function Get_Formatted_Percentage(L: in Locale; n: in Long_Float)
		return String is
	begin
		return Get_Formatted_Number(L, n * 100.0) & "%";
	end Get_Formatted_Percentage;

	
	function Get_Formatted_Percentage(	L: in Locale;
						n: in Long_Float;
						decimal_places : in Integer) return String is
	begin
		return Get_Formatted_Number(L, n * 100.0, decimal_places) & "%";
	end Get_Formatted_Percentage;

	

	function Get_Formated_Full_Name(	L: in Locale; 
						First_Name: in String; 
						Last_Name: in String := "" ) return String is
		Temporary_Name: Unbounded_String;
	begin
		if Last_Name'Length = 0 then
			-- There is no need to format if the last name is empty.
			--
			-- Then we expect the full name is stored in the first name field.
			return First_Name;
		end if;

		Temporary_Name := 
			KOW_Lib.String_Util.Str_Replace(	From	=> "%f",
							To		=> First_Name,
							Str		=> To_String( L.FULL_NAMES ));

		Temporary_Name := 
			KOW_Lib.String_Util.Str_Replace(	From	=> "%l",
							To	=> Last_Name,
							Str	=> To_String( Temporary_Name ) );

		return To_String( Temporary_Name );
	end Get_Formated_Full_Name;


	procedure Add_Locale(L: Locale) is
		code : Unbounded_String := L.CODE;

		LL : Locale := L;
	begin
		-- add all possible code for a Locale. For a code ll_CC_LL,
		-- ll, ll_CC and ll_CC_LL are added to Supported_Locales.
		loop
			if not Locale_Tables.Contains( Supported_Locales, code )  then
				Locale_Tables.Insert( Supported_Locales, code, LL);
			end if;
			
			if (Length(code) - 3) > 1 then
				code := Head(code, (Length(code)- 3));
				LL.Auto_Generalized := True;
			else
				return;
			end if;		
		end loop;

	end Add_Locale;

	procedure Add_All_Supported_Locales is
	begin
		Add_Locale(LOCALE_pt_BR);
		Add_Locale(LOCALE_en_US);
		Add_Locale(LOCALE_es_ES);
		Add_Locale(LOCALE_fr_FR);
		Add_Locale(LOCALE_jp_JP);
		Add_Locale(LOCALE_ISO);
	end Add_All_Supported_Locales;
	
begin
	Add_All_Supported_Locales;
end KOW_Lib.Locales;
