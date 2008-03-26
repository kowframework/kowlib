with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;   use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Unbounded.Hash;

package Aw_Lib.Locales is 
	pragma Elaborate_Body;

	subtype Time_Image is Unbounded_Wide_String;

	subtype Locale_Code is Unbounded_String;
	
	subtype Currency_Image is Unbounded_Wide_String;
	
	subtype Separator is Character; 

	type Day_Of_Week_Array is Array( Day_Name'Range ) of Unbounded_Wide_String;
	type Month_Array is Array( Month_Number'Range ) of Unbounded_Wide_String;

	type Locale is record

		CODE: Locale_Code;
		
		DEFAULT_DATE: Time_Image;
		-- default form to display dates
		DEFAULT_TIME: Time_Image;
		-- default form to display time
		DEFAULT_SHORT_DATE: Time_Image;
		-- default form to display short dates (dd/mm/aa)
		DEFAULT_LONG_DATE: Time_Image;
		-- default form to display long dates
		-- (dd de mmmmmm de aaaa)
		
		
		CURRENCY_PREFIX : Currency_Image;
		-- prefix in abbreviated currency.
		THOUSANDS_SEPARATOR : Separator ;
		DECIMAL_SEPARATOR : Separator;

		DAY_OF_WEEK_NAMES: Day_Of_Week_Array;
		-- array with day of week names.
		
		DAY_OF_WEEK_SHORT_NAMES: Day_Of_Week_Array;
		-- array with day of week abbreviated names
		
		MONTH_NAMES: Month_Array;
		-- array with month names.
		
		MONTH_SHORT_NAMES: Month_Array;
		-- array with abbreviated month names.

	end record;

	LOCALE_pt_BR: Locale := (
		CODE => To_Unbounded_String("pt_BR"),
			
		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_Wide_String("R$"),
		
		DEFAULT_DATE => To_Unbounded_Wide_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_Wide_String("%H:%M"),
		DEFAULT_SHORT_DATE => To_Unbounded_Wide_String("%d/%m/%y"),
		DEFAULT_LONG_DATE => 
			To_Unbounded_Wide_String("%A, %d de %B de %Y %H:%M"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_Wide_String("segunda-feira"),
					To_Unbounded_Wide_String("terça-feira"),
					To_Unbounded_Wide_String("quarta-feira"),
					To_Unbounded_Wide_String("quinta-feira"),
					To_Unbounded_Wide_String("sexta-feira"),
					To_Unbounded_Wide_String("sábado"),
					To_Unbounded_Wide_String("domingo")
					),
		DAY_OF_WEEK_SHORT_NAMES => (
					To_Unbounded_Wide_String("seg"),
					To_Unbounded_Wide_String("ter"),
					To_Unbounded_Wide_String("qua"),
					To_Unbounded_Wide_String("qui"),
					To_Unbounded_Wide_String("sex"),
					To_Unbounded_Wide_String("sáb"),
					To_Unbounded_Wide_String("dom")
					),
		MONTH_NAMES => (To_Unbounded_Wide_String("janeiro"),
				To_Unbounded_Wide_String("fevereiro"),
				To_Unbounded_Wide_String("março"),
				To_Unbounded_Wide_String("abril"),
				To_Unbounded_Wide_String("maio"),
				To_Unbounded_Wide_String("junho"),
				To_Unbounded_Wide_String("julho"),
				To_Unbounded_Wide_String("agosto"),
				To_Unbounded_Wide_String("setembro"),
				To_Unbounded_Wide_String("outubro"),
				To_Unbounded_Wide_String("novembro"),
				To_Unbounded_Wide_String("dezembro")
				),
		MONTH_SHORT_NAMES => (To_Unbounded_Wide_String("jan"),
				To_Unbounded_Wide_String("fev"),
				To_Unbounded_Wide_String("mar"),
				To_Unbounded_Wide_String("abr"),
				To_Unbounded_Wide_String("mai"),
				To_Unbounded_Wide_String("jun"),
				To_Unbounded_Wide_String("jul"),
				To_Unbounded_Wide_String("ago"),
				To_Unbounded_Wide_String("set"),
				To_Unbounded_Wide_String("out"),
				To_Unbounded_Wide_String("nov"),
				To_Unbounded_Wide_String("dez"))
	);
	
	LOCALE_en_US: Locale := (
		CODE => To_Unbounded_String("en_US"),
		
		THOUSANDS_SEPARATOR => ',',
		DECIMAL_SEPARATOR => '.',
		CURRENCY_PREFIX => To_Unbounded_Wide_String("$"),
		
		DEFAULT_DATE => To_Unbounded_Wide_String("%m/%d/%Y"),
		DEFAULT_TIME => To_Unbounded_Wide_String("%H:%M %p"),
		DEFAULT_SHORT_DATE => To_Unbounded_Wide_String("%m/%d/%Y"),
		DEFAULT_LONG_DATE =>
			To_Unbounded_Wide_String("%A, %B %d , %Y at %H:%M %p"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_Wide_String("monday"),
					To_Unbounded_Wide_String("tuesday"),
					To_Unbounded_Wide_String("wednesday"),
					To_Unbounded_Wide_String("thursday"),
					To_Unbounded_Wide_String("friday"),
					To_Unbounded_Wide_String("saturday"),
					To_Unbounded_Wide_String("sunday")
					),
		DAY_OF_WEEK_SHORT_NAMES => ( To_Unbounded_Wide_String("mon"),
					To_Unbounded_Wide_String("tue"),
					To_Unbounded_Wide_String("wedy"),
					To_Unbounded_Wide_String("thu"),
					To_Unbounded_Wide_String("fri"),
					To_Unbounded_Wide_String("sat"),
					To_Unbounded_Wide_String("sun")
					),
		MONTH_NAMES =>	( To_Unbounded_Wide_String("January"),
				  To_Unbounded_Wide_String("February"),
				  To_Unbounded_Wide_String("March"),
				  To_Unbounded_Wide_String("April"),
				  To_Unbounded_Wide_String("May"),
				  To_Unbounded_Wide_String("June") ,
				  To_Unbounded_Wide_String("July") ,
				  To_Unbounded_Wide_String("August") , 
				  To_Unbounded_Wide_String("September"),
				  To_Unbounded_Wide_String("October"),
				  To_Unbounded_Wide_String("November"),
				  To_Unbounded_Wide_String("December")
				  ),
		 MONTH_SHORT_NAMES => ( To_Unbounded_Wide_String("Jan"),
				  To_Unbounded_Wide_String("Feb"),
				  To_Unbounded_Wide_String("Mar"),
				  To_Unbounded_Wide_String("Apr"),
				  To_Unbounded_Wide_String("May"),
				  To_Unbounded_Wide_String("Jun") ,
				  To_Unbounded_Wide_String("Jul") ,
				  To_Unbounded_Wide_String("Aug") , 
				  To_Unbounded_Wide_String("Sep"),
				  To_Unbounded_Wide_String("Oct"),
				  To_Unbounded_Wide_String("Nov"),
				  To_Unbounded_Wide_String("Dec"))
	);

	LOCALE_es_ES: Locale := (
		CODE => To_Unbounded_String("es_ES"),

		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_Wide_String("€"),

		DEFAULT_DATE => To_Unbounded_Wide_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_Wide_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_Wide_String("%d/%m/%Y"),
		DEFAULT_LONG_DATE => To_Unbounded_Wide_String(
			"%d de %B de %Y %H:%M:%S "),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_Wide_String("lunes"),
					To_Unbounded_Wide_String("martes"),
					To_Unbounded_Wide_String("miércoles"),
					To_Unbounded_Wide_String("jueves"),
					To_Unbounded_Wide_String("viernes"),
					To_Unbounded_Wide_String("sábado"),
					To_Unbounded_Wide_String("domingo")
					),
		DAY_OF_WEEK_SHORT_NAMES => (To_Unbounded_Wide_String("lun"),
					To_Unbounded_Wide_String("mar"),
					To_Unbounded_Wide_String("mié"),
					To_Unbounded_Wide_String("jue"),
					To_Unbounded_Wide_String("vie"),
					To_Unbounded_Wide_String("sáb"),
					To_Unbounded_Wide_String("dom")
					),
		MONTH_NAMES =>	( To_Unbounded_Wide_String("enero"),
				  To_Unbounded_Wide_String("febrero"),
				  To_Unbounded_Wide_String("marzo"),
				  To_Unbounded_Wide_String("abril"),
				  To_Unbounded_Wide_String("mayo"),
				  To_Unbounded_Wide_String("junio") ,
				  To_Unbounded_Wide_String("julio") ,
				  To_Unbounded_Wide_String("agosto") , 
				  To_Unbounded_Wide_String("septiembre"),
				  To_Unbounded_Wide_String("octubre"),
				  To_Unbounded_Wide_String("noviembre"),
				  To_Unbounded_Wide_String("diciembrer")
				  ),
		MONTH_SHORT_NAMES => ( To_Unbounded_Wide_String("ene"),
				  To_Unbounded_Wide_String("feb"),
				  To_Unbounded_Wide_String("mar"),
				  To_Unbounded_Wide_String("abr"),
				  To_Unbounded_Wide_String("may"),
				  To_Unbounded_Wide_String("jun") ,
				  To_Unbounded_Wide_String("jul") ,
				  To_Unbounded_Wide_String("ago") , 
				  To_Unbounded_Wide_String("sep"),
				  To_Unbounded_Wide_String("oct"),
				  To_Unbounded_Wide_String("nov"),
				  To_Unbounded_Wide_String("dic"))
				);
	

	LOCALE_fr_FR: Locale := (
		CODE => To_Unbounded_String("fr_FR"),

		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_Wide_String("€"),
		
		DEFAULT_DATE => To_Unbounded_Wide_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_Wide_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_Wide_String("%d/%m/%Y"),
		DEFAULT_LONG_DATE =>
			To_Unbounded_Wide_String("%d %B %Y %H:%M:%S"),
		
		DAY_OF_WEEK_NAMES => (	To_Unbounded_Wide_String("lundi"),
					To_Unbounded_Wide_String("mardi"),
					To_Unbounded_Wide_String("mercredi"),
					To_Unbounded_Wide_String("jeudi"),
					To_Unbounded_Wide_String("vendredi"),
					To_Unbounded_Wide_String("samedi"),
					To_Unbounded_Wide_String("dimanche")
					),
		DAY_OF_WEEK_SHORT_NAMES => ( To_Unbounded_Wide_String("lun"),
					To_Unbounded_Wide_String("mar"),
					To_Unbounded_Wide_String("mer"),
					To_Unbounded_Wide_String("jeu"),
					To_Unbounded_Wide_String("ven"),
					To_Unbounded_Wide_String("sam"),
					To_Unbounded_Wide_String("dim")
					),
		MONTH_NAMES =>	( To_Unbounded_Wide_String("janvier"),
				  To_Unbounded_Wide_String("février"),
				  To_Unbounded_Wide_String("mars"),
				  To_Unbounded_Wide_String("avril"),
				  To_Unbounded_Wide_String("mai"),
				  To_Unbounded_Wide_String("juin"),
				  To_Unbounded_Wide_String("juillet"),
				  To_Unbounded_Wide_String("août"), 
				  To_Unbounded_Wide_String("septembre"),
				  To_Unbounded_Wide_String("octobre"),
				  To_Unbounded_Wide_String("novembre"),
				  To_Unbounded_Wide_String("décembre")
				  ),
		MONTH_SHORT_NAMES =>	( To_Unbounded_Wide_String("janv"),
				  To_Unbounded_Wide_String("févr"),
				  To_Unbounded_Wide_String("mars"),
				  To_Unbounded_Wide_String("avr"),
				  To_Unbounded_Wide_String("mai"),
				  To_Unbounded_Wide_String("juin"),
				  To_Unbounded_Wide_String("juil"),
				  To_Unbounded_Wide_String("août"), 
				  To_Unbounded_Wide_String("sept"),
				  To_Unbounded_Wide_String("oct"),
				  To_Unbounded_Wide_String("nov"),
				  To_Unbounded_Wide_String("déc")
				  ));
	LOCALE_de_DE: Locale := (
		CODE => To_Unbounded_String("de_DE"),

		THOUSANDS_SEPARATOR => '.',
		DECIMAL_SEPARATOR => ',',
		CURRENCY_PREFIX => To_Unbounded_Wide_String("€"),

		DEFAULT_DATE => To_Unbounded_Wide_String("%d.%m.%Y"),
		DEFAULT_TIME => To_Unbounded_Wide_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_Wide_String("%d.%m.%Y"),
		DEFAULT_LONG_DATE =>
		To_Unbounded_Wide_String("%d. %B %Y %H:%M:%S"),
		DAY_OF_WEEK_NAMES => (To_Unbounded_Wide_String("Montag"),
					To_Unbounded_Wide_String("Dienstag"),
					To_Unbounded_Wide_String("Mittwoch"),
					To_Unbounded_Wide_String("Donnerstag"),
					To_Unbounded_Wide_String("Freitag"),
					To_Unbounded_Wide_String("Samstag"),
					To_Unbounded_Wide_String("Sonntag")
					),
		DAY_OF_WEEK_SHORT_NAMES => (To_Unbounded_Wide_String("Mo"),
					To_Unbounded_Wide_String("Di"),
					To_Unbounded_Wide_String("Mi"),
					To_Unbounded_Wide_String("Do"),
					To_Unbounded_Wide_String("Fr"),
					To_Unbounded_Wide_String("Sa"),
					To_Unbounded_Wide_String("So")) ,
		MONTH_NAMES =>	( To_Unbounded_Wide_String("Januar"),
				  To_Unbounded_Wide_String("Februar"),
				  To_Unbounded_Wide_String("März"),
				  To_Unbounded_Wide_String("April"),
				  To_Unbounded_Wide_String("Mai"),
				  To_Unbounded_Wide_String("Juni"),
				  To_Unbounded_Wide_String("Juli"),
				  To_Unbounded_Wide_String("August"), 
				  To_Unbounded_Wide_String("September"),
				  To_Unbounded_Wide_String("Oktober"),
				  To_Unbounded_Wide_String("November"),
				  To_Unbounded_Wide_String("Dezember")
				  ),
		MONTH_SHORT_NAMES => ( To_Unbounded_Wide_String("Jan"),
				  To_Unbounded_Wide_String("Feb"),
				  To_Unbounded_Wide_String("Mrz"),
				  To_Unbounded_Wide_String("Apr"),
				  To_Unbounded_Wide_String("Mai"),
				  To_Unbounded_Wide_String("Jun"),
				  To_Unbounded_Wide_String("Jul"),
				  To_Unbounded_Wide_String("Aug"), 
				  To_Unbounded_Wide_String("Sep"),
				  To_Unbounded_Wide_String("Okt"),
				  To_Unbounded_Wide_String("Nov"),
				  To_Unbounded_Wide_String("Dez"))
		);	

	LOCALE_jp_JP: Locale := (
		CODE => To_Unbounded_String("jp_JP"),
		
		THOUSANDS_SEPARATOR => ',',
		DECIMAL_SEPARATOR => '.',
		CURRENCY_PREFIX => To_Unbounded_Wide_String("￥"),
		
		DEFAULT_DATE => To_Unbounded_Wide_String("%Y/%m/%d"),
		DEFAULT_TIME => To_Unbounded_Wide_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_Wide_String("%Y/%M/%d"),
		DEFAULT_LONG_DATE => To_Unbounded_Wide_String (
		"%Y" & 	Wide_Character'Val(16#e5#) & Wide_Character'Val(16#b9#) &
			Wide_Character'Val(16#b4#) &
		"%B%d" & Wide_Character'Val(16#e6#) & Wide_Character'Val(16#97#) &
			Wide_Character'Val(16#a5#) &
		"%H:%M:%S"),
		
		DAY_OF_WEEK_NAMES => (
			To_Unbounded_Wide_String("月曜日"),
			To_Unbounded_Wide_String("火曜日"),
			To_Unbounded_Wide_String("水曜日"),
			To_Unbounded_Wide_String("木曜日"),
			To_Unbounded_Wide_String("金曜日"),
			To_Unbounded_Wide_String("土曜日"),
			To_Unbounded_Wide_String("日曜日")
		),
		

		DAY_OF_WEEK_SHORT_NAMES => (
			To_Unbounded_Wide_String(Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)),
			--月曜日
					
			To_Unbounded_Wide_String(Wide_Character'Val(16#e7#) &
				Wide_Character'Val(16#81#) &  Wide_Character'Val(16#ab#)), 
			--火曜日
			
			To_Unbounded_Wide_String(Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#b0#) &  Wide_Character'Val(16#b4#)), 
			--水曜日
			
			To_Unbounded_Wide_String(Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#a8#)), 
			--木曜日
			
			To_Unbounded_Wide_String(Wide_Character'Val(16#e9#) &
				Wide_Character'Val(16#87#) &  Wide_Character'Val(16#91#)), 
			--金曜日
			
			To_Unbounded_Wide_String(Wide_Character'Val(16#e5#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#9f#)), 
			-- 土曜日
			
			To_Unbounded_Wide_String(Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#97#) &  Wide_Character'Val(16#a5#)) 
			--日曜日	
		),
		
		
		MONTH_NAMES => (
		To_Unbounded_Wide_String('1' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 1月
	
		To_Unbounded_Wide_String('2' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)),
		-- 2月
	
		To_Unbounded_Wide_String('3' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 3月
	
		To_Unbounded_Wide_String('4' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 4月
	
		To_Unbounded_Wide_String('5' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 5月
	
		To_Unbounded_Wide_String('6' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 6月
	
		To_Unbounded_Wide_String('7' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 7月
	
		To_Unbounded_Wide_String('8' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 8月
	
		To_Unbounded_Wide_String('9' & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 9月
	
		To_Unbounded_Wide_String("10" & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 10月
	
		To_Unbounded_Wide_String("11" & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)), 
		-- 11月
	
		To_Unbounded_Wide_String("12" & Wide_Character'Val(16#e6#) &
				Wide_Character'Val(16#9c#) &  Wide_Character'Val(16#88#)) 
		-- 12月
		),


		MONTH_SHORT_NAMES => (
		To_Unbounded_Wide_String("1"), -- 1月
		To_Unbounded_Wide_String("2"), -- 2月
		To_Unbounded_Wide_String("3"), -- 3月
		To_Unbounded_Wide_String("4"), -- 4月
		To_Unbounded_Wide_String("5"), -- 5月
		To_Unbounded_Wide_String("6"), -- 6月
		To_Unbounded_Wide_String("7"), -- 7月
		To_Unbounded_Wide_String("8"), -- 8月
		To_Unbounded_Wide_String("9"), -- 9月
		To_Unbounded_Wide_String("10"), -- 10月
		To_Unbounded_Wide_String("11"), -- 11月
		To_Unbounded_Wide_String("12") -- 12月
		)
	);

	
	LOCALE_ISO: Locale := (
		CODE => To_Unbounded_String("ISO"),

		THOUSANDS_SEPARATOR => ',',
		DECIMAL_SEPARATOR => '.',
		CURRENCY_PREFIX =>  To_Unbounded_Wide_String("$"),

		DEFAULT_DATE => To_Unbounded_Wide_String("%Y-%m-%d"),
		DEFAULT_TIME => To_Unbounded_Wide_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_Wide_String("%Y/%m/%d"),
		
		--Precisa da semanda do ano e do dia do ano
		DEFAULT_LONG_DATE =>
			To_Unbounded_Wide_String("%Y-%m-%d, %H:%M:%S"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_Wide_String("monday"),
					To_Unbounded_Wide_String("tuesday"),
					To_Unbounded_Wide_String("wednesday"),
					To_Unbounded_Wide_String("thursday"),
					To_Unbounded_Wide_String("friday"),
					To_Unbounded_Wide_String("saturday"),
					To_Unbounded_Wide_String("sunday")
					),
		DAY_OF_WEEK_SHORT_NAMES => ( To_Unbounded_Wide_String("mon"),
					To_Unbounded_Wide_String("tue"),
					To_Unbounded_Wide_String("wedy"),
					To_Unbounded_Wide_String("thu"),
					To_Unbounded_Wide_String("fri"),
					To_Unbounded_Wide_String("sat"),
					To_Unbounded_Wide_String("sun")
					),
		MONTH_NAMES =>	( To_Unbounded_Wide_String("January"),
				  To_Unbounded_Wide_String("February"),
				  To_Unbounded_Wide_String("March"),
				  To_Unbounded_Wide_String("April"),
				  To_Unbounded_Wide_String("May"),
				  To_Unbounded_Wide_String("June") ,
				  To_Unbounded_Wide_String("July") ,
				  To_Unbounded_Wide_String("August") , 
				  To_Unbounded_Wide_String("September"),
				  To_Unbounded_Wide_String("October"),
				  To_Unbounded_Wide_String("November"),
				  To_Unbounded_Wide_String("December")
				  ),
		 MONTH_SHORT_NAMES => ( To_Unbounded_Wide_String("Jan"),
				  To_Unbounded_Wide_String("Feb"),
				  To_Unbounded_Wide_String("Mar"),
				  To_Unbounded_Wide_String("Apr"),
				  To_Unbounded_Wide_String("May"),
				  To_Unbounded_Wide_String("Jun") ,
				  To_Unbounded_Wide_String("Jul") ,
				  To_Unbounded_Wide_String("Aug") , 
				  To_Unbounded_Wide_String("Sep"),
				  To_Unbounded_Wide_String("Oct"),
				  To_Unbounded_Wide_String("Nov"),
				  To_Unbounded_Wide_String("Dec"))
	);

	DEFAULT_LOCALE: Locale := LOCALE_pt_BR;

	package Locale_Tables is new Ada.Containers.Hashed_Maps(
		Key_Type => Unbounded_String,
		Element_Type => Locale,
		Hash => Ada.Strings.Unbounded.Hash,
      		Equivalent_Keys => "=");

	Supported_Locales: Locale_Tables.Map;

	
	function Get_Locale( Code: String) return Locale;

	function Get_Locale( Code: Locale_Code ) return Locale;
	-- recebe um código de Locale da forma:
	-- 	ll_CC_LL
	-- onde
	-- 	ll => idioma
	-- 	CC => código do país
	-- 	LL => código de localidade dentro do país
	--
	-- Procura na ordem:
	-- 	ll_CC_LL
	-- 	ll_CC
	-- 	ll
	-- 
	-- dando exception LOCALE_NOT_SUPPORTED quando nenhum dos 3
	-- deu resultados.
	
	LOCALE_NOT_SUPPORTED : exception;

	procedure Set_Default_Locale(L: in Locale );
	function Get_Default_Locale return Locale;

	
	function Image( L: in Locale; D: in Ada.Calendar.Formatting.Day_Name;
		short: in Boolean ) return Wide_String;

	function Image( D: in Ada.Calendar.Formatting.Day_Name; 
		short: in Boolean ) return Wide_String;

	
	function Image( L: in Locale; D: in Month_Number; short: in Boolean )
		return Wide_String;

	function Image( D: in Month_Number; short: in Boolean )
		return Wide_String;


	function Get_Short_Date_Pattern(L: in Locale) return Wide_String;

	function Get_Long_Date_Pattern(L: in Locale) return Wide_String;

	
	function Get_Short_Date_Pattern	return Wide_String;

	function Get_Long_Date_Pattern	return Wide_String;
			

	function Get_Default_Time_Pattern( L: in Locale )
		return Wide_String;

	function Get_Default_Date_Pattern( L: in Locale )
		return Wide_String;

	
	
	function Get_Formatted_Number(L: in Locale; n: in Long_Float)
		return Wide_String;

	function Get_Formatted_Number(L: in Locale; n: in Long_Float;
		dec_digits : in Integer) return Wide_String;

	
	function Get_Formatted_Percentage(L: in Locale; n: in Long_Float)
		return Wide_String;
		
	function Get_Formatted_Percentage(L: in Locale; n: in Long_Float;
		dec_digits : in Integer) return Wide_String;
		
		
	function Get_Formatted_Currency(L: in Locale; n: in Long_Float)
		return Wide_String;

	function Get_Formatted_Currency(L: in Locale; n: in Long_Float;
		dec_digits : in Integer) return Wide_String;
	

end Aw_Lib.Locales;

