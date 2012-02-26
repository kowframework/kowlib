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



	package Default_Locales is

		---------
		-- ISO --
		---------
		ISO : constant Locale_Type := (
					Code			=> ISO_Locale_Code,

					Label			=> new String'( "ISO" ),

					Thousands_Separator	=> ',',
					Decimal_Separator	=> '.',

					Currency		=> new String'( "$" ),

					Default_Date		=> new String'( "%Y-%m-%d" ),
					Default_Time		=> new String'( "%H:%M:%S" ),
					Default_Datetime	=> new String'( "%Y-%m-%d %H:%M:%S" ),


					Short_Date		=> new String'("%Y/%m/%d"),
			
					Long_Date		=> new String'("%Y-%m-%d"),
			
				
					Week_Days		=> ( others => null ),
					Short_Week_Days		=> ( others => null ),

					Months			=> ( others => null ),
					Short_Months		=> ( others => null ),

					Person_Name_Img		=> new String'( "%l, %f" )
				);


		-------------
		-- English --
		-------------
		en : constant Locale_Type := (
					Code			=> ( "en", "  " ),
					Label			=> new String'( "English" ),
					Thousands_Separator	=> ',',
					Decimal_Separator	=> '.',
					Currency		=> new String'( "$"),
			
					Default_Date		=> new String'( "%m/%d/%Y" ),
					Default_Time		=> new String'( "%H:%M %p" ),
					Default_Datetime	=> new String'( "%m/%d/%Y %H:%M %p" ),

					Short_Date		=> new String'( "%m/%d/%Y" ),
					Long_Date		=> new String'("%A, %B %d, %Y at %H:%M %p"),


					Week_Days		=> (
									Monday		=> new String'("monday"),
									Tuesday		=> new String'("tuesday"),
									Wednesday	=> new String'("wednesday"),
									Thursday	=> new String'("thursday"),
									Friday		=> new String'("friday"),
									Saturday	=> new String'("saturday"),
									Sunday		=> new String'("sunday")
								),

					Short_Week_Days		=> (
									Monday		=> new String'("mon"),
									Tuesday		=> new String'("tue"),
									Wednesday	=> new String'("wed"),
									Thursday	=> new String'("thu"),
									Friday		=> new String'("fri"),
									Saturday	=> new String'("sat"),
									Sunday		=> new String'("sun")
								),

			

					Months			=> (
									01	=> new String'( "January" ),
									02	=> new String'( "February" ),
									03	=> new String'( "March" ),
									04	=> new String'( "April" ),
									05	=> new String'( "May" ),
									06	=> new String'( "June" ),
									07	=> new String'( "July" ),
									08	=> new String'( "August" ), 
									09	=> new String'( "September" ),
									10	=> new String'( "October" ),
									11	=> new String'( "November" ),
									12	=> new String'( "December" )
								),
			

					Short_Months		=> (
									01	=> new String'( "Jan" ),
									02	=> new String'( "Feb" ),
									03	=> new String'( "Mar" ),
									04	=> new String'( "Apr" ),
									05	=> new String'( "May" ),
									06	=> new String'( "Jun" ),
									07	=> new String'( "Jul" ),
									08	=> new String'( "Aug" ), 
									09	=> new String'( "Sep" ),
									10	=> new String'( "Oct" ),
									11	=> new String'( "Nov" ),
									12	=> new String'( "Dec" )
								),

					Person_Name_Img		=> new String'( "%l, %f" )
				);

		en_US : constant Locale_Type := (
					Code			=> ( "en", "US" ),
					Label			=> new String'( "US English" ),
					Thousands_Separator	=> ',',
					Decimal_Separator	=> '.',
					Currency		=> new String'( "$"),
			
					Default_Date		=> new String'( "%m/%d/%Y" ),
					Default_Time		=> new String'( "%H:%M %p" ),
					Default_Datetime	=> new String'( "%m/%d/%Y %H:%M %p" ),

					Short_Date		=> new String'( "%m/%d/%Y" ),
					Long_Date		=> new String'("%A, %B %d, %Y at %H:%M %p"),


					Week_Days		=> (
									Monday		=> new String'("monday"),
									Tuesday		=> new String'("tuesday"),
									Wednesday	=> new String'("wednesday"),
									Thursday	=> new String'("thursday"),
									Friday		=> new String'("friday"),
									Saturday	=> new String'("saturday"),
									Sunday		=> new String'("sunday")
								),

					Short_Week_Days		=> (
									Monday		=> new String'("mon"),
									Tuesday		=> new String'("tue"),
									Wednesday	=> new String'("wed"),
									Thursday	=> new String'("thu"),
									Friday		=> new String'("fri"),
									Saturday	=> new String'("sat"),
									Sunday		=> new String'("sun")
								),

			

					Months			=> (
									01	=> new String'( "January" ),
									02	=> new String'( "February" ),
									03	=> new String'( "March" ),
									04	=> new String'( "April" ),
									05	=> new String'( "May" ),
									06	=> new String'( "June" ),
									07	=> new String'( "July" ),
									08	=> new String'( "August" ), 
									09	=> new String'( "September" ),
									10	=> new String'( "October" ),
									11	=> new String'( "November" ),
									12	=> new String'( "December" )
								),
			

					Short_Months		=> (
									01	=> new String'( "Jan" ),
									02	=> new String'( "Feb" ),
									03	=> new String'( "Mar" ),
									04	=> new String'( "Apr" ),
									05	=> new String'( "May" ),
									06	=> new String'( "Jun" ),
									07	=> new String'( "Jul" ),
									08	=> new String'( "Aug" ), 
									09	=> new String'( "Sep" ),
									10	=> new String'( "Oct" ),
									11	=> new String'( "Nov" ),
									12	=> new String'( "Dec" )
								),

					Person_Name_Img		=> new String'( "%l, %f" )
				);



		-------------
		-- Spanish --
		-------------

		es_ES : constant Locale_Type := (

						Code			=> ( "es", "ES" ),
						Label			=> new String'( "Español" ),

						Thousands_Separator	=> '.',
						Decimal_Separator	=> ',',
						Currency		=> new String'( "€" ),

						Default_Date		=> new String'( "%d/%m/%Y" ),
						Default_Time		=> new String'( "%H:%M:%S" ),
						Default_Datetime	=> new String'( "%d/%m/%Y %H:%M:%S" ),

						Short_Date		=> new String'( "%d/%m/%Y" ),
						Long_Date		=> new String'( "%d de %B de %Y %H:%M:%S" ),
						
				
						Week_Days		=> (
										Monday		=> new String'( "lunes" ),
										Tuesday		=> new String'( "martes" ),
										Wednesday	=> new String'( "miércoles" ),
										Thursday	=> new String'( "jueves" ),
										Friday		=> new String'( "viernes" ),
										Saturday	=> new String'( "sábado" ),
										Sunday		=> new String'( "domingo" )
									),

						Short_Week_Days		=> (
										Monday		=> new String'( "lun" ),
										Tuesday		=> new String'( "mar" ),
										Wednesday	=> new String'( "mié" ),
										Thursday	=> new String'( "jue" ),
										Friday		=> new String'( "vie" ),
										Saturday	=> new String'( "sáb" ),
										Sunday		=> new String'( "dom" )
									),

						Months			=> (
										01	=> new String'( "enero" ),
										02	=> new String'( "febrero" ),
										03	=> new String'( "marzo" ),
										04	=> new String'( "abril" ),
										05	=> new String'( "mayo" ),
										06	=> new String'( "junio" ),
										07	=> new String'( "julio" ),
										08	=> new String'( "agosto" ), 
										09	=> new String'( "septiembre" ),
										10	=> new String'( "octubre" ),
										11	=> new String'( "noviembre" ),
										12	=> new String'( "diciembrer" )
									),
						Short_Months		=> (
										01	=> new String'( "ene" ),
										02	=> new String'( "feb" ),
										03	=> new String'( "mar" ),
										04	=> new String'( "abr" ),
										05	=> new String'( "may" ),
										06	=> new String'( "jun" ),
										07	=> new String'( "jul" ),
										08	=> new String'( "ago" ), 
										09	=> new String'( "sep" ),
										10	=> new String'( "oct" ),
										11	=> new String'( "nov" ),
										12	=> new String'( "dic" )
									),

						Person_Name_Img		=> new String'( "%f %l" )
					);


		----------------
		-- Portuguese --
		----------------



		pt : constant Locale_Type := (
					Code		=> ( "pt", "  " ),
					Label		=> new String'( "Português" ),

					Default_Date	=> new String'( "%d/%m/%Y" ),
					Default_Time	=> new String'( "%H:%M" ),
					Default_Datetime=> new String'( "%d/%m/%Y %H:%M" ),

					Short_Date	=> new String'( "%d/%m/%Y" ),
					Long_Date	=> new String'( "%A, %d de %B de %Y %H:%M" ),

					Currency	=> new String'( "R$" ),

					Thousands_Separator	=> '.',
					Decimal_Separator	=> ',',

					Week_Days	=> (
								Monday		=> new String'( "segunda-feira" ),
								Tuesday		=> new String'( "terça-feira" ),
								Wednesday	=> new String'( "quarta-feira" ),
								Thursday	=> new String'( "quinta-feira" ),
								Friday		=> new String'( "sexta-feira" ),
								Saturday	=> new String'( "sábado" ),
								Sunday		=> new String'( "domingo" )
							),
					Short_Week_Days	=> (
								Monday		=> new String'( "seg" ),
								Tuesday		=> new String'( "ter" ),
								Wednesday	=> new String'( "qua" ),
								Thursday	=> new String'( "qui" ),
								Friday		=> new String'( "sex" ),
								Saturday	=> new String'( "sab" ),
								Sunday		=> new String'( "dom" )
							),

					Months		=> (
								 1		=> new String'( "Janeiro" ),
								 2		=> new String'( "Fevereiro" ),
								 3		=> new String'( "Março" ),
								 4		=> new String'( "Abril" ),
								 5		=> new String'( "Maio" ),
								 6		=> new String'( "Junho" ),
								 7		=> new String'( "Julho" ),
								 8		=> new String'( "Agosto" ),
								 9		=> new String'( "Setembro" ),
								10		=> new String'( "Outubro" ),
								11		=> new String'( "Novembro" ),
								12		=> new String'( "Dezembro" )
							),

					Short_Months	=> (
								 1		=> new String'( "Jan" ),
								 2		=> new String'( "Fev" ),
								 3		=> new String'( "Mar" ),
								 4		=> new String'( "Abr" ),
								 5		=> new String'( "Mai" ),
								 6		=> new String'( "Jun" ),
								 7		=> new String'( "Jul" ),
								 8		=> new String'( "Ago" ),
								 9		=> new String'( "Set" ),
								10		=> new String'( "Out" ),
								11		=> new String'( "Nov" ),
								12		=> new String'( "Dez" )
							),
					Person_name_Img	=> new String'( "%f %l" )
				);

		pt_BR : constant Locale_Type := (
					Code		=> ( "pt", "BR" ),
					Label		=> new String'( "Português Brasileiro" ),

					Default_Date	=> new String'( "%d/%m/%Y" ),
					Default_Time	=> new String'( "%H:%M" ),
					Default_Datetime=> new String'( "%d/%m/%Y %H:%M" ),

					Short_Date	=> new String'( "%d/%m/%Y" ),
					Long_Date	=> new String'( "%A, %d de %B de %Y %H:%M" ),

					Currency	=> new String'( "R$" ),

					Thousands_Separator	=> '.',
					Decimal_Separator	=> ',',

					Week_Days	=> (
								Monday		=> new String'( "segunda-feira" ),
								Tuesday		=> new String'( "terça-feira" ),
								Wednesday	=> new String'( "quarta-feira" ),
								Thursday	=> new String'( "quinta-feira" ),
								Friday		=> new String'( "sexta-feira" ),
								Saturday	=> new String'( "sábado" ),
								Sunday		=> new String'( "domingo" )
							),
					Short_Week_Days	=> (
								Monday		=> new String'( "seg" ),
								Tuesday		=> new String'( "ter" ),
								Wednesday	=> new String'( "qua" ),
								Thursday	=> new String'( "qui" ),
								Friday		=> new String'( "sex" ),
								Saturday	=> new String'( "sab" ),
								Sunday		=> new String'( "dom" )
							),

					Months		=> (
								 1		=> new String'( "Janeiro" ),
								 2		=> new String'( "Fevereiro" ),
								 3		=> new String'( "Março" ),
								 4		=> new String'( "Abril" ),
								 5		=> new String'( "Maio" ),
								 6		=> new String'( "Junho" ),
								 7		=> new String'( "Julho" ),
								 8		=> new String'( "Agosto" ),
								 9		=> new String'( "Setembro" ),
								10		=> new String'( "Outubro" ),
								11		=> new String'( "Novembro" ),
								12		=> new String'( "Dezembro" )
							),

					Short_Months	=> (
								 1		=> new String'( "Jan" ),
								 2		=> new String'( "Fev" ),
								 3		=> new String'( "Mar" ),
								 4		=> new String'( "Abr" ),
								 5		=> new String'( "Mai" ),
								 6		=> new String'( "Jun" ),
								 7		=> new String'( "Jul" ),
								 8		=> new String'( "Ago" ),
								 9		=> new String'( "Set" ),
								10		=> new String'( "Out" ),
								11		=> new String'( "Nov" ),
								12		=> new String'( "Dez" )
							),
					Person_name_Img	=> new String'( "%f %l" )
				);

	end Default_Locales;

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

