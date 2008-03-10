with Ada.Containers.Hashed_Maps; use Ada.Containers; 
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

package body Aw_Lib.Locales is
	
	function To_Hash is 
		new Ada.Unchecked_Conversion (Unbounded_String,
			Ada.Containers.Hash_Type);
  	

	function Hash (id : Unbounded_String) return Ada.Containers.Hash_Type is
  	begin
    		return To_Hash (id);
   	end;


	function Get_Locale( Code: String) return Locale is
		pragma Inline (Get_Locale);
	begin

		return Get_Locale(To_Unbounded_String(Code));
	end Get_Locale;
	
	
	function Get_Locale( Code: Locale_Code ) return Locale is
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

	Tmp: Unbounded_String := Code;
	
	use Locale_Tables;

	begin
		while Find(Supported_Locales, Tmp) = No_Element 
		loop
			if (Length(Tmp) - 3) > 1 then
				Tmp := Head(Tmp, (Length(Tmp)- 3));
			else
				raise LOCALE_NOT_SUPPORTED;
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



	function Image( L: in Locale; D: in Day_Name)
		return String is
	begin
		return To_String(L.DAY_OF_WEEK_NAMES(D));
	end Image;

	function Image( D: in Day_Name) return String is
	begin
		return Image(Get_Default_Locale, D);
	end Image;

	

	function Image( L: in Locale; D: in Month_Number)
		return String is
	begin
	 	return To_String(L.MONTH_NAMES(D));
	end Image;

	function Image( D: in Month_Number) return String is
	begin
		return Image(Get_Default_Locale, D);
	end Image;
	


	function Get_Short_Date_Pattern(L: in Locale) return String is
	begin
		return To_String(L.DEFAULT_SHORT_DATE);
	end Get_Short_Date_Pattern;
	
	function Get_Long_Date_Pattern(L: in Locale) return String is
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


	function Get_Default_Time_Pattern( L: in Locale ) return String is
	begin
		return To_String(L.DEFAULT_TIME);

	end Get_Default_Time_Pattern;


	function Get_Default_Date_Pattern( L: in Locale ) return String is
	begin
		return To_String(L.DEFAULT_DATE);

	end Get_Default_Date_Pattern;


begin
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("pt_BR"),
				LOCALE_pt_BR );

	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("pt"),
				LOCALE_pt_BR );
	
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("en_US"),
				LOCALE_en_US );
	
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("en"),
				LOCALE_en_US );


	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("es_ES"),
				LOCALE_es_ES );
	
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("es"),
				LOCALE_es_ES );

	
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("fr_FR"),
				LOCALE_fr_FR );
	
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("fr"),
				LOCALE_fr_FR );

	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("de_DE"),
				LOCALE_de_DE );
	
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("de"),
				LOCALE_de_DE );


	
	Locale_Tables.Insert( 	Supported_Locales,
				To_Unbounded_String("ISO"),
				LOCALE_ISO );

end Aw_Lib.Locales;
