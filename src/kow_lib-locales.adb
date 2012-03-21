------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Library                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
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


--------------
-- Ada 2005 --
--------------
with Ada.Containers;			use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Environment_Variables;
with Ada.Strings.Hash;


package body KOW_Lib.Locales is 

	function From_String( Str : in String ) return Locale_Code_Type is
		function Language return ISO_Code_Type is
		begin
			return ISO_Code_Type( Str( Str'First .. Str'First + 1 ) );
		exception
			when CONSTRAINT_ERROR =>
				raise CONSTRAINT_ERROR with '"' & Str & """ is not a valid locale code";
		end Language;

		function Country return ISO_Code_Type is
		begin
			if Str'Length = 2 then
				return No_Country;
			elsif Str'Length = 5 then
				return ISO_Code_Type( Str( Str'Last - 1 .. Str'Last ) );
			else
				raise INVALID_LOCALE_CODE with Str;
			end if;
		end Country;
	begin
		if Str = "ISO" then
			return ISO_Locale_Code;
		else
			return Locale_Code_Type'(
						Language	=> Language,
						Country		=> Country
					);
		end if;
	end From_String;


	function To_String( Locale_Code : in Locale_Code_Type ) return String is
		-- convert from/to the ISO locale code format 
	begin
		if Locale_Code.Country /= No_Country then
			return String( Locale_Code.Language ) & '_' & String( Locale_Code.Country );
		elsif Locale_Code.Language /= No_Language then
			return String( Locale_Code.Language );
		else
			return "ISO";
		end if;
	end To_String;


	function Get_Default_Locale_Code return Locale_Code_Type is
		-- get the default locale code from the locale variable
	begin
		return From_String( Ada.Environment_Variables.Value( Locale_Variable ) );
	end Get_Default_Locale_Code;


	function Hash( Locale_Code : in Locale_Code_Type ) return Ada.Containers.Hash_Type is
		-- a faster hash function for Locale_Codes
		function Compute( Element : in ISO_Code_Type ) return Hash_Type is
			pragma Inline( Compute );
		begin
			return Hash_Type( Character'Pos( Element( ISO_Code_Type'First ) ) + Character'Pos( Element( ISO_Code_Type'Last ) ) );
		end Compute;
	begin
		if Locale_Code.Country = No_Country then
			return Compute( Locale_Code.Language ) * 10;
		else
			return Compute( Locale_Code.Language ) * 10 + Compute( Locale_Code.Country );
		end if;
	end Hash;


	--------------------
	-- Auxiliar Types --
	--------------------

	function To_String( Str : in String_Access ) return String is
	begin
		if Str = null then
			return "";
		else
			return Str.all;
		end if;
	end To_String;







	---------------------
	-- The Locale Type --
	---------------------


	function Get_Default_Locale return Locale_Type is
	begin
		return Get_Locale( Get_Default_Locale_Code );
	end Get_Default_Locale;

	---------------------
	-- Locale Registry --
	---------------------

	procedure Iterate(
				Iterator	: not null access procedure( Locale : in Locale_Type )
			) is
		-- iterate over all supported locales

		use Locale_Maps;
		procedure Inner_Iterator( C : in Cursor ) is
		begin
			Iterator.all( Element( C ) );
		end Inner_Iterator;
	begin
		Iterate( Supported_Locales, Inner_Iterator'Access );
	end Iterate;


	function Get_Locale(
				Locale_Code	: in Locale_Code_Type := Get_Default_Locale_Code 
			) return Locale_Type is
		-- get the given locale
	begin
		return Locale_Maps.Element( Supported_Locales, Locale_Code );
	end Get_Locale;

	procedure Register( Locale : in Locale_Type ) is
	begin
		Locale_Maps.Include( Supported_Locales, Locale.Code, Locale );
	end Register;


begin
	Register( Default_Locales.ISO );

	Register( Default_Locales.en );
	Register( Default_Locales.en_US );

	Register( Default_Locales.es_ES );

	Register( Default_Locales.pt );
	Register( Default_Locales.pt_BR );
end KOW_Lib.Locales;

