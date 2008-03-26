with Ada.Containers.Hashed_Maps; use Ada.Containers; 
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Strings;   use Ada.Strings;
with Ada.Strings.Fixed;  
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Aw_Lib.Locales is
	

	function Get_Locale( Code: String) return Locale is
		pragma Inline (Get_Locale);
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



	function Image( L: in Locale; D: in Day_Name; short: in Boolean)
		return Wide_String is
	begin
		if short = true then
			return To_Wide_String(L.DAY_OF_WEEK_SHORT_NAMES(D));
		else
			return To_Wide_String(L.DAY_OF_WEEK_NAMES(D));
		end if;
	end Image;

	function Image( D: in Day_Name; short: in Boolean)
		return Wide_String is
	begin
		return Image(Get_Default_Locale, D, short);
	end Image;

	

	function Image( L: in Locale; D: in Month_Number; short: in Boolean)
		return Wide_String is
	begin
		if short = true then
		 	return To_Wide_String(L.MONTH_SHORT_NAMES(D));
		else
		 	return To_Wide_String(L.MONTH_NAMES(D));
		end if;
	end Image;

	function Image( D: in Month_Number; short: in Boolean)
		return Wide_String is
	begin
		return Image(Get_Default_Locale, D, short);
	end Image;
	


	function Get_Short_Date_Pattern(L: in Locale)
		return Wide_String is
	begin
		return To_Wide_String(L.DEFAULT_SHORT_DATE);
	end Get_Short_Date_Pattern;
	
	function Get_Long_Date_Pattern(L: in Locale) return
		Wide_String is
	begin
		return To_Wide_String(L.DEFAULT_LONG_DATE);
	end Get_Long_Date_Pattern;
	
	function Get_Short_Date_Pattern	return Wide_String is
	begin
		return To_Wide_String(Get_Default_Locale.DEFAULT_SHORT_DATE);
	end Get_Short_Date_Pattern;

	function Get_Long_Date_Pattern return Wide_String is
	begin
		return To_Wide_String(Get_Default_Locale.DEFAULT_LONG_DATE);
	end Get_Long_Date_Pattern;


	function Get_Default_Time_Pattern( L: in Locale )
		return Wide_String is
	begin
		return To_Wide_String(L.DEFAULT_TIME);
	end Get_Default_Time_Pattern;


	function Get_Default_Date_Pattern( L: in Locale )
		return Wide_String is
	begin
		return To_Wide_String(L.DEFAULT_DATE);
	end Get_Default_Date_Pattern;
	
	
	function Get_Formatted_Number(L: in Locale; n: in Long_Float)
		return Wide_String is
	begin
		return Get_Formatted_Number(L, n, 2);
	end Get_Formatted_Number;
	
	function Get_Formatted_Number(L: in Locale; n: in Long_Float;
		dec_digits : in Integer) return Wide_String is

		Integer_Value : Integer := Integer(Long_Float'Truncation(n));
		Decimal_Value : Integer := Integer(Long_Float'Truncation((n
			- (Long_Float'Truncation(n))) * 10.0 ** dec_digits )); 

		function Trim (str : in String) return String is
		begin	
			return Ada.Strings.Fixed.Trim (str, Both);
		end Trim;
		
		Integer_Part : String := Trim(Integer'Image(Integer_Value));
		Decimal_Part : String := Trim(Integer'Image(Decimal_Value));		
		Separs : Integer;
		Integer_Length : Integer;
		Res_Length : Integer;
	begin
		Separs := Integer_Part'Length/3 -1;
		if Integer_Part'Length rem 3 > 0 then
			Separs := Separs + 1;
		end if;

		Integer_Length := Integer_Part'Length + Separs;
		Res_Length := Integer_Length;
		if dec_digits > 0 then
			Res_Length := Res_Length + Decimal_Part'Length + 1; 
		end if;

		declare
			Result : String( 1 .. Res_Length );
			Index : Integer := 1;
			Separs_Idx : Integer := 0;
		begin
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
			
			if dec_digits > 0 then
				Result(Index) := L.DECIMAL_SEPARATOR;
				Index := Index + 1;
		
			
				for k in Decimal_Part'Range loop
					Result(Index) := Decimal_Part(k);	
					Index := Index + 1;
				end loop;
			end if;
	
			return To_Wide_String(Result);
		end;

	end Get_Formatted_Number;


	function Get_Formatted_Currency(L: in Locale; n: in Long_Float)
		return Wide_String is
	begin
		return To_Wide_String(L.CURRENCY_PREFIX) & " " &
			Get_Formatted_Number(L, n);
	end Get_Formatted_Currency;


	function Get_Formatted_Currency(L: in Locale; n: in Long_Float;
		dec_digits : in Integer) return Wide_String is
	begin
		return To_Wide_String(L.CURRENCY_PREFIX) & " " &
			Get_Formatted_Number(L, n, dec_digits);
	end Get_Formatted_Currency;

		
	function Get_Formatted_Percentage(L: in Locale; n: in Long_Float)
		return Wide_String is
	begin
		return Get_Formatted_Number(L, n * 100.0) & "%";
	end Get_Formatted_Percentage;

	
	function Get_Formatted_Percentage(L: in Locale; n: in Long_Float;
			dec_digits : in Integer) return Wide_String is
	begin
		return Get_Formatted_Number(L, n * 100.0, dec_digits) & "%";
	end Get_Formatted_Percentage;

	
	procedure Add_Locale(L: Locale) is
		code : Unbounded_String := L.CODE;
	begin
		loop
			Locale_Tables.Insert( Supported_Locales, code, L);

			if (Length(code) - 3) > 1 then
				code := Head(code, (Length(code)- 3));
			else
				return;
			end if;		
		end loop;

	end Add_Locale;


begin
	Add_Locale(LOCALE_pt_BR);
	Add_Locale(LOCALE_en_US);
	Add_Locale(LOCALE_es_ES);
	Add_Locale(LOCALE_fr_FR);
	Add_Locale(LOCALE_jp_JP);
	Add_Locale(LOCALE_ISO);

end Aw_Lib.Locales;
