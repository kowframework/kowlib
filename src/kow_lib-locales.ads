------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
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
-- This is the KOW_Lib.Locales package                                      	--
--                                                                          --
-- Provides functions to get patterns that describe dates or times and      --
-- functions that return a formatted number, date or name according to      --
-- a specific Locale                                                       --
------------------------------------------------------------------------------


with Ada.Calendar;			use Ada.Calendar;
with Ada.Calendar.Formatting;		use Ada.Calendar.Formatting;
with Ada.Containers.Hashed_Maps;	use Ada.Containers;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Environment_Variables;


package KOW_Lib.Locales is 
	pragma Elaborate_Body;


	LOCALE_ENVVAR_INVALID : exception;


	subtype Locale_Code is Unbounded_String;
	-- Represents a code of a specific Locale
	
	
	subtype Time_Image is Unbounded_String;
	-- Represents hours, minutes, seconds
	-- or any time. 

	
	subtype Currency_Image is Unbounded_String;
	-- Represents a form of money. 
	
	subtype Separator is Character; 
	-- Type for representing file separators. 


	subtype Name_Image is Unbounded_String;
	-- Type for representing a pattern for people's names.
	-- The last name and first name order depends according to
	-- Locale. 

	type Day_Of_Week_Array is Array( Day_Name'Range )
		of Unbounded_String;
	-- Type for representing the days of the week. 
	
	type Month_Array is Array( Month_Number'Range )
		of Unbounded_String;
	-- Type for representing the months of the year. 

	type Locale is record

		CODE: Locale_Code;
		-- the code for this locale

		Name: Unbounded_String;
		-- the label/name for this locale (US English, Portuguẽs Brasileiro, etc)
		
		Auto_Generalized : Boolean := False;
		-- this is to track if this locale has been automatically generalized
		-- IE: when we add pt_BR into an enpty registry, it becomes default for the pt locale as well
		-- when it hapens, the return value for getting the pt locale has Auto_Generalized = True


		DEFAULT_DATE: Time_Image;
		-- default format to display dates
		DEFAULT_TIME: Time_Image;
		-- default format to display time (hh:mm)
		DEFAULT_SHORT_DATE: Time_Image;
		-- default format to display short dates (dd/mm/aa)
		DEFAULT_LONG_DATE: Time_Image;
		-- default format to display long dates
		-- (dd de mmmmmm de aaaa)
		
		
		CURRENCY_PREFIX : Currency_Image;
		-- prefix in abbreviated currency.
		THOUSANDS_SEPARATOR : Separator;
		-- separator used to divide usually into groups of three, starting
		-- from the decimal separator in both directions
		DECIMAL_SEPARATOR : Separator;
		-- separator used to mark the boundary between the integral and
		-- the fractional parts of a decimal numeral
		
		
		DAY_OF_WEEK_NAMES: Day_Of_Week_Array;
		-- array with days of the week names.
		
		DAY_OF_WEEK_SHORT_NAMES: Day_Of_Week_Array;
		-- array with abbreviated days of the week names.
		
		MONTH_NAMES: Month_Array;
		-- array with months of the year names.
		
		MONTH_SHORT_NAMES: Month_Array;
		-- array with abbreviated month names.

		FULL_NAMES: Name_Image;
		-- full name pattern.
	end record;


	-- Brazilian Portuguese Locale
	LOCALE_pt_BR: Locale := (
		CODE => To_Unbounded_String("pt_BR"),
		Name => To_Unbounded_String( "Português Brasileiro" ),
		Auto_Generalized => False,
			
		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_String("R$"),
		
		DEFAULT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d/%m/%y"),
		DEFAULT_LONG_DATE => To_Unbounded_String("%A, %d de %B de %Y %H:%M"),
		
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("segunda-feira"),
								To_Unbounded_String("terça-feira"),
								To_Unbounded_String("quarta-feira"),
								To_Unbounded_String("quinta-feira"),
								To_Unbounded_String("sexta-feira"),
								To_Unbounded_String("sábado"),
								To_Unbounded_String("domingo")	),

		DAY_OF_WEEK_SHORT_NAMES => (	To_Unbounded_String("seg"),
										To_Unbounded_String("ter"),
										To_Unbounded_String("qua"),
										To_Unbounded_String("qui"),
										To_Unbounded_String("sex"),
										To_Unbounded_String("sáb"),
										To_Unbounded_String("dom")	),

		MONTH_NAMES => (	To_Unbounded_String("janeiro"),
							To_Unbounded_String("fevereiro"),
							To_Unbounded_String("março"),
							To_Unbounded_String("abril"),
							To_Unbounded_String("maio"),
							To_Unbounded_String("junho"),
							To_Unbounded_String("julho"),
							To_Unbounded_String("agosto"),
							To_Unbounded_String("setembro"),
							To_Unbounded_String("outubro"),
							To_Unbounded_String("novembro"),
							To_Unbounded_String("dezembro")	),

		MONTH_SHORT_NAMES => (	To_Unbounded_String("jan"),
								To_Unbounded_String("fev"),
								To_Unbounded_String("mar"),
								To_Unbounded_String("abr"),
								To_Unbounded_String("mai"),
								To_Unbounded_String("jun"),
								To_Unbounded_String("jul"),
								To_Unbounded_String("ago"),
								To_Unbounded_String("set"),
								To_Unbounded_String("out"),
								To_Unbounded_String("nov"),
								To_Unbounded_String("dez")	),

		FULL_NAMES => To_Unbounded_String( "%f %l" )

	);

			
	-- American English Locale, also known United States
	-- English or U.S.English Locale 
	LOCALE_en_US: Locale := (
		CODE => To_Unbounded_String("en_US"),
		Name => To_Unbounded_String( "US English" ),
		Auto_Generalized => False,
			
		THOUSANDS_SEPARATOR => ',',
		DECIMAL_SEPARATOR => '.',
		CURRENCY_PREFIX => To_Unbounded_String("$"),
		
		DEFAULT_DATE => To_Unbounded_String("%m/%d/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M %p"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%m/%d/%Y"),
		DEFAULT_LONG_DATE => To_Unbounded_String("%A, %B %d , %Y at %H:%M %p"),
		
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("monday"),
								To_Unbounded_String("tuesday"),
								To_Unbounded_String("wednesday"),
								To_Unbounded_String("thursday"),
								To_Unbounded_String("friday"),
								To_Unbounded_String("saturday"),
								To_Unbounded_String("sunday")	),
		
		DAY_OF_WEEK_SHORT_NAMES => (	To_Unbounded_String("mon"),
										To_Unbounded_String("tue"),
										To_Unbounded_String("wedy"),
										To_Unbounded_String("thu"),
										To_Unbounded_String("fri"),
										To_Unbounded_String("sat"),
										To_Unbounded_String("sun")	),

		MONTH_NAMES => (	To_Unbounded_String("January"),
							To_Unbounded_String("February"),
							To_Unbounded_String("March"),
				  			To_Unbounded_String("April"),
							To_Unbounded_String("May"),
				 			To_Unbounded_String("June") ,
							To_Unbounded_String("July") ,
						  	To_Unbounded_String("August") , 
							To_Unbounded_String("September"),
						  	To_Unbounded_String("October"),
							To_Unbounded_String("November"),
				 			To_Unbounded_String("December")	),
		
	 	MONTH_SHORT_NAMES => (	To_Unbounded_String("Jan"),
								To_Unbounded_String("Feb"),
						 		To_Unbounded_String("Mar"),
						 		To_Unbounded_String("Apr"),
						  		To_Unbounded_String("May"),
						  		To_Unbounded_String("Jun") ,
							 	To_Unbounded_String("Jul") ,
								To_Unbounded_String("Aug") , 
								To_Unbounded_String("Sep"),
								To_Unbounded_String("Oct"),
								To_Unbounded_String("Nov"),
								To_Unbounded_String("Dec")	),
		
		FULL_NAMES => To_Unbounded_String( "%l, %f" )
	
	);


	-- Spanish Locale of Spain
	LOCALE_es_ES: Locale := (
		CODE => To_Unbounded_String("es_ES"),
		Name => To_Unbounded_String( "TODO :: es_ES" ),
		Auto_Generalized => False,

		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_String("€"),

		DEFAULT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_LONG_DATE => To_Unbounded_String("%d de %B de %Y %H:%M:%S "),

		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("lunes"),
								To_Unbounded_String("martes"),
								To_Unbounded_String("miércoles"),
								To_Unbounded_String("jueves"),
								To_Unbounded_String("viernes"),
								To_Unbounded_String("sábado"),
								To_Unbounded_String("domingo")	),
		
		DAY_OF_WEEK_SHORT_NAMES => (	To_Unbounded_String("lun"),
										To_Unbounded_String("mar"),
										To_Unbounded_String("mié"),
										To_Unbounded_String("jue"),
										To_Unbounded_String("vie"),
										To_Unbounded_String("sáb"),
										To_Unbounded_String("dom")	),
		
		MONTH_NAMES =>	(	To_Unbounded_String("enero"),
						 	To_Unbounded_String("febrero"),
						 	To_Unbounded_String("marzo"),
						 	To_Unbounded_String("abril"),
						 	To_Unbounded_String("mayo"),
						 	To_Unbounded_String("junio") ,
						  	To_Unbounded_String("julio") ,
					 	 	To_Unbounded_String("agosto") , 
							To_Unbounded_String("septiembre"),
						  	To_Unbounded_String("octubre"),
						 	To_Unbounded_String("noviembre"),
					  		To_Unbounded_String("diciembrer")	),
		
		MONTH_SHORT_NAMES => (	To_Unbounded_String("ene"),
								To_Unbounded_String("feb"),
								To_Unbounded_String("mar"),
								To_Unbounded_String("abr"),
								To_Unbounded_String("may"),
								To_Unbounded_String("jun") ,
								To_Unbounded_String("jul") ,
								To_Unbounded_String("ago") , 
								To_Unbounded_String("sep"),
								To_Unbounded_String("oct"),
								To_Unbounded_String("nov"),
								To_Unbounded_String("dic")	),

		FULL_NAMES => To_Unbounded_String( "%f %l" )
	);
	

	--French Locale of France
	LOCALE_fr_FR: Locale := (
		CODE => To_Unbounded_String("fr_FR"),
		Name => To_Unbounded_String( "TODO :: fr_FR" ),
		Auto_Generalized => False,
		
		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_String("€"),
		
		DEFAULT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_LONG_DATE => To_Unbounded_String("%d %B %Y %H:%M:%S"),
		
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("lundi"),
								To_Unbounded_String("mardi"),
								To_Unbounded_String("mercredi"),
								To_Unbounded_String("jeudi"),
								To_Unbounded_String("vendredi"),
								To_Unbounded_String("samedi"),
								To_Unbounded_String("dimanche")	),

		DAY_OF_WEEK_SHORT_NAMES => (	To_Unbounded_String("lun"),
										To_Unbounded_String("mar"),
										To_Unbounded_String("mer"),
										To_Unbounded_String("jeu"),
										To_Unbounded_String("ven"),
										To_Unbounded_String("sam"),
										To_Unbounded_String("dim")	),

		MONTH_NAMES => (	To_Unbounded_String("janvier"),
							To_Unbounded_String("février"),
							To_Unbounded_String("mars"),
							To_Unbounded_String("avril"),
							To_Unbounded_String("mai"),
							To_Unbounded_String("juin"),
							To_Unbounded_String("juillet"),
							To_Unbounded_String("août"), 
							To_Unbounded_String("septembre"),
							To_Unbounded_String("octobre"),
							To_Unbounded_String("novembre"),
							To_Unbounded_String("décembre")	),
		
		MONTH_SHORT_NAMES => (	To_Unbounded_String("janv"),
								To_Unbounded_String("févr"),
								To_Unbounded_String("mars"),
								To_Unbounded_String("avr"),
								To_Unbounded_String("mai"),
								To_Unbounded_String("juin"),
								To_Unbounded_String("juil"),
								To_Unbounded_String("août"), 
								To_Unbounded_String("sept"),
								To_Unbounded_String("oct"),
								To_Unbounded_String("nov"),
								To_Unbounded_String("déc")	),
		
		FULL_NAMES => To_Unbounded_String( "%f %l" )
		
	);

	
	--German LOCALE for Germany
	LOCALE_de_DE: Locale := (
		CODE => To_Unbounded_String("de_DE"),
		Name => To_Unbounded_String( "TODO :: de_DE" ),
		Auto_Generalized => False,

		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_String("€"),

		DEFAULT_DATE => To_Unbounded_String("%d.%m.%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d.%m.%Y"),
		DEFAULT_LONG_DATE => To_Unbounded_String("%d. %B %Y %H:%M:%S"),
		
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("Montag"),
								To_Unbounded_String("Dienstag"),
								To_Unbounded_String("Mittwoch"),
								To_Unbounded_String("Donnerstag"),
								To_Unbounded_String("Freitag"),
								To_Unbounded_String("Samstag"),
								To_Unbounded_String("Sonntag")	),

		DAY_OF_WEEK_SHORT_NAMES => (	To_Unbounded_String("Mo"),
										To_Unbounded_String("Di"),
										To_Unbounded_String("Mi"),
										To_Unbounded_String("Do"),
										To_Unbounded_String("Fr"),
										To_Unbounded_String("Sa"),
										To_Unbounded_String("So")	),

		MONTH_NAMES =>	(	To_Unbounded_String("Januar"),
							To_Unbounded_String("Februar"),
							To_Unbounded_String("März"),
							To_Unbounded_String("April"),
							To_Unbounded_String("Mai"),
							To_Unbounded_String("Juni"),
							To_Unbounded_String("Juli"),
							To_Unbounded_String("August"), 
							To_Unbounded_String("September"),
							To_Unbounded_String("Oktober"),
							To_Unbounded_String("November"),
							To_Unbounded_String("Dezember")	),

		MONTH_SHORT_NAMES => (	To_Unbounded_String("Jan"),
								To_Unbounded_String("Feb"),
								To_Unbounded_String("Mrz"),
								To_Unbounded_String("Apr"),
								To_Unbounded_String("Mai"),
								To_Unbounded_String("Jun"),
								To_Unbounded_String("Jul"),
								To_Unbounded_String("Aug"), 
								To_Unbounded_String("Sep"),
								To_Unbounded_String("Okt"),
								To_Unbounded_String("Nov"),
								To_Unbounded_String("Dez")	),

		
		FULL_NAMES => To_Unbounded_String( "%f %l" )
	);	

	-- Japanese Locale of Japan
	LOCALE_jp_JP: Locale := (
		CODE => To_Unbounded_String("jp_JP"),
		Name => To_Unbounded_String( "TODO :: jp_JP" ),
		Auto_Generalized => False,
		
		THOUSANDS_SEPARATOR => ',',
		DECIMAL_SEPARATOR => '.',
		CURRENCY_PREFIX => To_Unbounded_String("￥"),
		
		DEFAULT_DATE => To_Unbounded_String("%Y/%m/%d"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%Y/%M/%d"),
		DEFAULT_LONG_DATE => To_Unbounded_String (	"%Y" &
													Character'Val(16#e5#) &
													Character'Val(16#b9#) &
													Character'Val(16#b4#) &
													"%B%d" &
													Character'Val(16#e6#) &
													Character'Val(16#97#) &
													Character'Val(16#a5#) &
													"%H:%M:%S"),
		
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("月曜日"),
								To_Unbounded_String("火曜日"),
								To_Unbounded_String("水曜日"),
								To_Unbounded_String("木曜日"),
								To_Unbounded_String("金曜日"),
								To_Unbounded_String("土曜日"),
								To_Unbounded_String("日曜日")	),
		

		DAY_OF_WEEK_SHORT_NAMES => (	To_Unbounded_String(	Character'Val(16#e6#) &
																Character'Val(16#9c#) &
																Character'Val(16#88#)	), --月曜日
										
										To_Unbounded_String(	Character'Val(16#e7#) &
																Character'Val(16#81#) &
																Character'Val(16#ab#)	), --火曜日
			
										To_Unbounded_String(	Character'Val(16#e6#) &
																Character'Val(16#b0#) &
																Character'Val(16#b4#)	), --水曜日
			
										To_Unbounded_String(	Character'Val(16#e6#) &
																Character'Val(16#9c#) &
																Character'Val(16#a8#)	), --木曜日
			
										To_Unbounded_String(	Character'Val(16#e9#) &
																Character'Val(16#87#) &
																Character'Val(16#91#)	), --金曜日
			
										To_Unbounded_String(	Character'Val(16#e5#) &
																Character'Val(16#9c#) &
																Character'Val(16#9f#)	), -- 土曜日
				
										To_Unbounded_String(	Character'Val(16#e6#) &
																Character'Val(16#97#) &
																Character'Val(16#a5#)	)	), --日曜日	
		
		
		MONTH_NAMES => (	To_Unbounded_String(	'1' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 1月
		
							To_Unbounded_String(	'2' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 2月
					
							To_Unbounded_String(	'3' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 3月
					
							To_Unbounded_String(	'4' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 4月
					
							To_Unbounded_String(	'5' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 5月
					
							To_Unbounded_String(	'6' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 6月
					
							To_Unbounded_String(	'7' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 7月
					
							To_Unbounded_String(	'8' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 8月
					
							To_Unbounded_String(	'9' &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 9月
					
							To_Unbounded_String(	"10" &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	), -- 10月
					
							To_Unbounded_String(	"11" &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)), -- 11月
					
							To_Unbounded_String(	"12" &
													Character'Val(16#e6#) &
													Character'Val(16#9c#) &
													Character'Val(16#88#)	)	),-- 12月


		MONTH_SHORT_NAMES => (	To_Unbounded_String("1"), -- 1月
								To_Unbounded_String("2"), -- 2月
								To_Unbounded_String("3"), -- 3月
								To_Unbounded_String("4"), -- 4月
								To_Unbounded_String("5"), -- 5月
								To_Unbounded_String("6"), -- 6月
								To_Unbounded_String("7"), -- 7月
								To_Unbounded_String("8"), -- 8月
								To_Unbounded_String("9"), -- 9月
								To_Unbounded_String("10"), -- 10月
								To_Unbounded_String("11"), -- 11月
								To_Unbounded_String("12")	), -- 12月


		FULL_NAMES => To_Unbounded_String( "%f %l" )
	);

	-- ISO Locale.
	-- ISO 8601 is an international standard for date
	-- and time representations 
	LOCALE_ISO: Locale := (
		CODE => To_Unbounded_String("ISO"),
		Name => To_Unbounded_String( "ISO" ),
		Auto_Generalized => False,

		THOUSANDS_SEPARATOR => ' ',
		DECIMAL_SEPARATOR => '.',
		CURRENCY_PREFIX =>  To_Unbounded_String("$"),

		DEFAULT_DATE => To_Unbounded_String("%Y-%m-%d"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%Y/%m/%d"),
		
		-- This pattern is not right because we haven't the
		-- week of the year and the day of the year.
		DEFAULT_LONG_DATE =>	To_Unbounded_String("%Y-%m-%d, %H:%M:%S"),
		
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("monday"),
								To_Unbounded_String("tuesday"),
								To_Unbounded_String("wednesday"),
								To_Unbounded_String("thursday"),
								To_Unbounded_String("friday"),
								To_Unbounded_String("saturday"),
								To_Unbounded_String("sunday")	),
		
		DAY_OF_WEEK_SHORT_NAMES => (	To_Unbounded_String("mon"),
										To_Unbounded_String("tue"),
										To_Unbounded_String("wedy"),
										To_Unbounded_String("thu"),
										To_Unbounded_String("fri"),
										To_Unbounded_String("sat"),
										To_Unbounded_String("sun")	),
		
		MONTH_NAMES =>	(	To_Unbounded_String("January"),
						 	To_Unbounded_String("February"),
						  	To_Unbounded_String("March"),
						  	To_Unbounded_String("April"),
						  	To_Unbounded_String("May"),
						  	To_Unbounded_String("June") ,
						  	To_Unbounded_String("July") ,
						  	To_Unbounded_String("August") , 
						  	To_Unbounded_String("September"),
						  	To_Unbounded_String("October"),
						  	To_Unbounded_String("November"),
						  	To_Unbounded_String("December")	),
		
		MONTH_SHORT_NAMES => (	To_Unbounded_String("Jan"),
								To_Unbounded_String("Feb"),
								To_Unbounded_String("Mar"),
								To_Unbounded_String("Apr"),
								To_Unbounded_String("May"),
								To_Unbounded_String("Jun") ,
								To_Unbounded_String("Jul") ,
								To_Unbounded_String("Aug") , 
								To_Unbounded_String("Sep"),
								To_Unbounded_String("Oct"),
								To_Unbounded_String("Nov"),
								To_Unbounded_String("Dec")	),

		
		FULL_NAMES => To_Unbounded_String( "%f %l" )
	);

	DEFAULT_LOCALE: Locale := LOCALE_pt_BR;


	package Locale_Tables is new
		 Ada.Containers.Hashed_Maps(	Key_Type => Unbounded_String,
						Element_Type => Locale,
						Hash => Ada.Strings.Unbounded.Hash,
      						Equivalent_Keys => "="	);

	Supported_Locales: Locale_Tables.Map;
	-- Map with all Supported Locales.
	
	function Get_Locale( Code: String) return Locale;

	function Get_Locale( Code: Locale_Code ) return Locale;
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


	LOCALE_NOT_SUPPORTED : exception;

	procedure Set_Default_Locale(L: in Locale );
	function Get_Default_Locale return Locale;

	function Get_Locale_Envvar return String;
	-- returns Locale environment variable name;
	
	function Get_Locale_Envvar_Value return String;
	-- returns Locale environment variable value;
	
	function Get_Environment_Locale return Locale;
	-- returns the Locale to current Locale environmnet variable value


	function Image( L: in Locale; D: in Ada.Calendar.Formatting.Day_Name;
		short: in Boolean ) return String;
	-- returns the abbreviated day of the week image (e.g. mon) if short is true or full
	-- day of the week image (e.g. monday) if short is false according to L. 
	
	function Image( D: in Ada.Calendar.Formatting.Day_Name; 
		short: in Boolean ) return String;
	-- returns the abbreviated day of the week image (e.g. mon) if short is true or full
	-- day of the week image (e.g. monday) if short is false according to Default_Locale. 
	
	

	function Image( L: in Locale; D: in Month_Number; short: in Boolean )
		return String;
	-- returns the abbreviated month of the year image (e.g. jan) if short is true or full
	-- month of the year image (e.g. january) if short is false according to L. 

	
	function Image( D: in Month_Number; short: in Boolean )
		return String;
	-- returns the abbreviated month of the year image (e.g. jan) if short is true or full
	-- month of the year image (e.g. january) if short is false according to Default_Locale. 

	

	function Get_Short_Date_Pattern(L: in Locale) return String;
	-- returns the pattern for short date (e.g. "%m/%d/%Y") according to L.
	
	function Get_Long_Date_Pattern(L: in Locale) return String;
	-- returns the pattern for long date (e.g. "%A, %B %d , %Y at %H:%M %p")
	-- according to L.
	
	function Get_Short_Date_Pattern	return String;
	-- returns the pattern for short date (e.g. "%m/%d/%Y") according to Default_Locale.
	
	function Get_Long_Date_Pattern	return String;
	-- returns the pattern for long date (e.g. "%A, %B %d , %Y at %H:%M %p")
	-- according to Default_Locale.


	function Get_Default_Time_Pattern( L: in Locale )
		return String;
	-- returns the default pattern for time (e.g. "%H:%M %p").
	
	function Get_Default_Date_Pattern( L: in Locale )
		return String;
	-- returns the default pattern for date (e.g. "%m/%d/%Y").
	
	
	function Get_Formatted_Number(L: in Locale; n: in Long_Float)
		return String;
	-- returns the number n formatted according to Locale L, 
	-- inserting decimal and thousand separators on n and with
	-- precision of two decimal places.
	
	function Get_Formatted_Number(L: in Locale; n: in Long_Float;
		decimal_places : in Integer) return String;
	-- returns the number n formatted according to Default_Locale, 
	-- inserting decimal and thousand separators on n and with
	-- precision of decimal_places decimal places.


	function Get_Formatted_Percentage(L: in Locale; n: in Long_Float)
		return String;
	-- returns the percentage n formatted with precision of two decimal
	-- places according to Locale L.
		
	function Get_Formatted_Percentage(L: in Locale; n: in Long_Float;
		decimal_places : in Integer) return String;
	-- returns the percentage n formatted with precision of decimal_places
	-- decimal places according to Locale L.
		

	function Get_Formatted_Currency(L: in Locale; n: in Long_Float)
		return String;
	-- returns the currency n formatted with precision of two decimal
	-- places according to Locale L.

	function Get_Formatted_Currency(L: in Locale; n: in Long_Float;
		decimal_places : in Integer) return String;
	-- returns the currency n formatted with precision of decimal_places
	-- decimal places according to Locale L.


	function Get_Formated_Full_Name(	L: in Locale;
						First_Name: in String;
						Last_Name: in String := "" ) return String;
	-- returns the full name with first and last Name order according to
	-- Locale L.
	
	procedure Add_All_Supported_Locales;

end KOW_Lib.Locales;
