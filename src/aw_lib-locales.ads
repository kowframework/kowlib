with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

package Aw_Lib.Locales is

	subtype Time_Image is Unbounded_String;

	subtype Locale_Code is Unbounded_String;

	type Day_Of_Week_Array is Array( Day_Name'Range ) of Unbounded_String;
	type Month_Array is Array( Month_Number'Range ) of Unbounded_String;


	type Locale is record

		CODE: Locale_Code;
		
		DEFAULT_DATE: Time_Image;
		-- forma padrão de apresentar datas
		DEFAULT_TIME: Time_Image;
		-- forma padrão de apresentar horas
		DEFAULT_SHORT_DATE: Time_Image;
		-- forma padrão de apresentar datas curtas(dd/mm/aa)
		DEFAULT_LONG_DATE: Time_Image;
		-- forma padrão de apresentar datas longas 
		-- (dd de mmmmmm de aaaa)
		
		DAY_OF_WEEK_NAMES: Day_Of_Week_Array;

		MONTH_NAMES: Month_Array;

	end record;

	LOCALE_pt_BR: Locale := (
		CODE => To_Unbounded_String("pt_BR"),
		DEFAULT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d/%m/%y"),
		DEFAULT_LONG_DATE => 
			To_Unbounded_String("%A, %d de %B de %Y %H:%M"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("segunda-feira"),
					To_Unbounded_String("terça-feira"),
					To_Unbounded_String("quarta-feira"),
					To_Unbounded_String("quinta-feira"),
					To_Unbounded_String("sexta-feira"),
					To_Unbounded_String("sábado"),
					To_Unbounded_String("domingo")
					),
		MONTH_NAMES => (To_Unbounded_String("janeiro"),
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
				To_Unbounded_String("dezembro"))
	);
	
	LOCALE_en_US: Locale := (
		CODE => To_Unbounded_String("en_US"),
		DEFAULT_DATE => To_Unbounded_String("%m/%d/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M %p"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%m/%d/%Y"),
		DEFAULT_LONG_DATE =>
			To_Unbounded_String("%A, %B %d , %Y at %H:%M %p"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("monday"),
					To_Unbounded_String("tuesday"),
					To_Unbounded_String("wednesday"),
					To_Unbounded_String("thursday"),
					To_Unbounded_String("friday"),
					To_Unbounded_String("saturday"),
					To_Unbounded_String("sunday")) ,
		MONTH_NAMES =>	( To_Unbounded_String("January"),
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
				  To_Unbounded_String("December"))
	);

	LOCALE_es_ES: Locale := (
		CODE => To_Unbounded_String("es_ES"),
		DEFAULT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_LONG_DATE =>

		To_Unbounded_String("%d de %B de %Y %H:%M:%S "),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("lunes"),
					To_Unbounded_String("martes"),
					To_Unbounded_String("miércoles"),
					To_Unbounded_String("jueves"),
					To_Unbounded_String("viernes"),
					To_Unbounded_String("sábado"),
					To_Unbounded_String("domingo")) ,
		MONTH_NAMES =>	( To_Unbounded_String("enero"),
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
				  To_Unbounded_String("diciembrer"))
	);
	

	LOCALE_fr_FR: Locale := (
		CODE => To_Unbounded_String("fr_FR"),
		DEFAULT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d/%m/%Y"),
		DEFAULT_LONG_DATE =>

		To_Unbounded_String("%d %B %Y %H:%M:%S"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("lundi"),
					To_Unbounded_String("mardi"),
					To_Unbounded_String("mercredi"),
					To_Unbounded_String("jeudi"),
					To_Unbounded_String("vendredi"),
					To_Unbounded_String("samedi"),
					To_Unbounded_String("dimanche")) ,
		MONTH_NAMES =>	( To_Unbounded_String("janvier"),
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
				  To_Unbounded_String("décembre"))
	);

	LOCALE_de_DE: Locale := (
		CODE => To_Unbounded_String("de_DE"),
		DEFAULT_DATE => To_Unbounded_String("%d.%m.%Y"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%d.%m.%Y"),
		DEFAULT_LONG_DATE =>

		To_Unbounded_String("%d. %B %Y %H:%M:%S"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("Montag"),
					To_Unbounded_String("Dienstag"),
					To_Unbounded_String("Mittwoch"),
					To_Unbounded_String("Donnerstag"),
					To_Unbounded_String("Freitag"),
					To_Unbounded_String("Samstag"),
					To_Unbounded_String("Sonntag")) ,
		MONTH_NAMES =>	( To_Unbounded_String("Januar"),
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
				  To_Unbounded_String("Dezember"))
	);

	LOCALE_ISO: Locale := (
		CODE => To_Unbounded_String("ISO"),
		DEFAULT_DATE => To_Unbounded_String("%Y-%m-%d"),
		DEFAULT_TIME => To_Unbounded_String("%H:%M:%S"),
		DEFAULT_SHORT_DATE => To_Unbounded_String("%Y/%m/%d"),
		
		--Precisa da semanda do ano e do dia do ano
		DEFAULT_LONG_DATE =>
			To_Unbounded_String("%Y-%m-%d, %H:%M:%S"),
		DAY_OF_WEEK_NAMES => (	To_Unbounded_String("monday"),
					To_Unbounded_String("tuesday"),
					To_Unbounded_String("wednesday"),
					To_Unbounded_String("thursday"),
					To_Unbounded_String("friday"),
					To_Unbounded_String("saturday"),
					To_Unbounded_String("sunday")) ,
		MONTH_NAMES =>	( To_Unbounded_String("January"),
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
				  To_Unbounded_String("December"))

	);


	DEFAULT_LOCALE: Locale := LOCALE_pt_BR;

 	function Hash (id: Unbounded_String) return Hash_Type;
  	 --  you need to provide this to every hashed container

	package Locale_Tables is new Ada.Containers.Hashed_Maps(
		Key_Type => Unbounded_String,
		Element_Type => Locale,
		Hash => Hash,
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

	
	function Image( L: in Locale; D: in Ada.Calendar.Formatting.Day_Name)
		return String;

	function Image( D: in Ada.Calendar.Formatting.Day_Name)
		return String;

	
	function Image( L: in Locale; D: in Month_Number)
		return String;

	function Image( D: in Month_Number) return String;


	function Get_Short_Date_Pattern(L: in Locale) return String;

	function Get_Long_Date_Pattern(L: in Locale) return String;

	
	function Get_Short_Date_Pattern	return String;

	function Get_Long_Date_Pattern	return String;
			

	function Get_Default_Time_Pattern( L: in Locale )
		return String;

	function Get_Default_Date_Pattern( L: in Locale )
		return String;

end Aw_Lib.Locales;

