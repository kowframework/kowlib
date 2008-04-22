------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
--                                                                          --
--                                                                          --
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
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
-- This is the Aw_Lib.Calendar package                                      --
--                                                                          --
-- Provides functions to handle dates and time, especially to format        --
-- according to Locale and Formatter's Pattern.                             --
------------------------------------------------------------------------------

with Ada.Calendar;		use Ada.Calendar; 
with Ada.Calendar.Formatting;	use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; 	use Ada.Calendar.Time_Zones;

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with Ada.Characters.Handling;	use Ada.Characters.Handling;

with Aw_Lib.Locales;		use Aw_Lib.Locales;
with Aw_Lib.Replacer;		use Aw_Lib.Replacer;



package body Aw_Lib.Calendar is

	-------------------------------
	---------- TIMESTAMP ----------
	-------------------------------

	MomentoUnix: Time := Ada.Calendar.Time_Of(1970, 1,1);
	-- Momento inicial do Timestamp
	
	function Get(This: in Timestamp; Key: in Character)
		return Unbounded_String is 
	-- Funcao que receba uma chave e retorna o valor;
		Str : Unbounded_String;
		Hora: Time := To_Time(This);


	function To_Unb(Num: Integer; Tamanho: Integer)
		return Unbounded_String is
	
	-- Transforma numero em Unb String com tamanho especifico;
	
	Str	: Unbounded_String;
	T 	: Integer := Tamanho;
	N 	: Integer := Num;
	
	begin
		while T >= 1 loop
			Str := Str & To_Unbounded_String(N/T);

			Put("[  ");Put(N);Put(To_String(Str));Put_Line("]");
			N := N mod T;
			T := T/10;
		end loop;
		
		return Str;
	end To_Unb;

	begin
		Put("Entrou um => ");
		Put(Key);
		New_Line;

		case Key is
		
			when 'd' => 
				Str := To_Unb(Integer(Ada.Calendar.Day(Hora)),10);
				Put(Integer(Ada.Calendar.Day(Hora)));
			when 'm' => 
				Str := To_Unb(Integer(Ada.Calendar.Month(Hora)),10);
			when 'y' =>
				Str := To_Unb(Integer(Ada.Calendar.Year(Hora))
					mod 100, 10);
			when 'Y' => 
				Str := To_Unb(Integer(Ada.Calendar.Year(Hora)), 1000);
			
			when others => Str := Str;
		end case;

		Put("Saiu um [");
		Put(To_String(Str));
		Put_Line("]");

		return Str ;
	end Get;


	function To_Timestamp( Hora: in Ada.Calendar.Time) return Timestamp is
		-- Retorna o timestamp dado um Time;
		tempo: Timestamp;
	begin
		tempo := Timestamp(Hora - MomentoUnix);
		return tempo;
	end To_Timestamp;

	function To_Time( Hora: in Timestamp) return Time is
		-- Retorna o Time dado um Timestamp;
		Tempo: Time;
		
	begin
		tempo := MomentoUnix + Duration(Hora);
		return tempo;
	end To_Time;


	function Date( Formato: in Unbounded_String; Hora: in Timestamp)
		return Unbounded_String is

		Str : Unbounded_String := Formato;
	begin
		Date(Str, Hora);
		return Str;
	end Date;


	procedure Date( Formato: in out Unbounded_String; Hora: in Timestamp) is
		Str_Pos : Integer;
		Str		: Unbounded_String;
		Str_New : Unbounded_String;
		Str_Len : Integer;
		Comeco  : Unbounded_String;
		Fim		: Unbounded_String;
	begin
		loop
			Get_Variable(Formato, 1, Str_Pos, Str_Len, Str);
			
			exit when Str_Len = 0;
			--Put(Str_Pos); Put(Str_Len); Put(To_String(Str));
		
			Str_New := get(Hora, Element(Str,1));
			New_Line;
			Put(To_String(Str_New));
			Replace_Slice(	Formato, Positive(Str_Pos),
			Natural(Str_Len + Str_Pos),
			To_String(Str_New));
			Str_Len := 0;

			exit when Str_Len = 0;
		end loop;		

	end Date;




	-------------------------------
	---------- FORMATTER ----------
	-------------------------------
	
	function Get_Date return Time is
		
		A_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
		-- Gets a value indicating the current time

		UTC_Adjustment : Ada.Calendar.Time_Zones.Time_Offset :=
			Ada.Calendar.Time_Zones.UTC_Time_Offset (A_Time); 
		-- Gets the difference between the time zone of the function call and
		-- UTC (Coordinate Universal Time) - this is close but not quite the
		-- same as GMT (Greenwich Mean Time). This difference includes local
		-- time changes as daylight saving.
	begin	
		return A_Time + Duration(UTC_Adjustment * 60);
	end Get_Date;


	function Get_Formatter(Pattern : Unbounded_String) return Formatter is
		F : Formatter;
	begin
		F := (Pattern => Pattern);
		return F;
	end Get_Formatter;


	function Get_Formatter(Pattern : String) return Formatter is
		pragma Inline(Get_Formatter);
	begin
		return Get_Formatter(To_Unbounded_String(Pattern));
	end Get_Formatter;


	type Padding_Mode is (None, Zero, Space);

	type Sec_Number is mod 2 ** 64;
	--  Type used to compute the number of seconds since 01/01/1970. A 32 bit
	--  number will cover only a period of 136 years. This means that for date
	--  past 2106 the computation is not possible. A 64 bits number should be
	--  enough for a very large period of time.





	--------------------------------
	-- BEGIN OF LOCAL SUBPROGRAMS --
	--------------------------------

	function Am_Pm (H : Natural) return String;
	--  Return AM or PM depending on the hour H

	function Hour_12 (H : Natural) return Positive;
	--  Convert a 1-24h format to a 0-12 hour format

	function Image (Str : String; Length : Natural := 0)
		return String;
 	--  Return Str capitalized and cut to length number of characters. If
	--  length is set to 0 it does not cut it.

	function Image (	N : Sec_Number;
						Padding : Padding_Mode := Zero;
						Length  : Natural := 0) return String;
	--  Return image of N. This number is eventually padded with
	--  zeros or spaces depending of the length required.
	--  If length is 0 then no padding occurs.

	function Image (	N		: Natural;
						Padding : Padding_Mode := Zero;
						Length  : Natural := 0) return String;
	
	--  As above with N provided in Integer format

	-----------
	-- Am_Pm --
	-----------
	function Am_Pm (H : Natural) return String is
	begin
	 if H = 0 or else H > 12 then
		 return "PM";
	 else
		 return "AM";
	 end if;
	end Am_Pm;

	-------------
	-- Hour_12 --
	-------------

	function Hour_12 (H : Natural) return Positive is
	begin
		if H = 0 then
			 return 12;
		 elsif H <= 12 then
			 return H;
		 else --  H > 12
			 return H - 12;
		end if;
	end Hour_12;

	-----------
	-- Image --
	-----------
	
	function Image (	Str : String;
						Length : Natural := 0) return String
	is
		Local : constant String := Str;
	begin
		if Length = 0 then
			return Local;
		else
			return Local (1 .. Length);
		end if;
	end Image;

	-----------
	-- Image --
	-----------

	function Image (	N	: Natural;
						Padding : Padding_Mode := Zero;
						Length  : Natural := 0) return String is
	begin
		return Image (Sec_Number (N), Padding, Length);
	end Image;

 	-----------
	-- Image --
	-----------
	
	function Image (	N	 	: Sec_Number;
						Padding : Padding_Mode := Zero;
						Length  : Natural := 0) return String is
		
		function Pad_Char return String;

		function Pad_Char return String is
		begin
			case Padding is
				when None  => return "";
				when Zero  => return "00";
				when Space => return "  ";
			end case;
		end Pad_Char;

		NI  : constant String := Sec_Number'Image (N);
		NIP : constant String := Pad_Char & NI (2 .. NI'Last);

		--  Start of processing for Image
	begin
		if Length = 0 or else Padding = None then
			return NI (2 .. NI'Last);
		else
			return NIP (NIP'Last - Length + 1 .. NIP'Last);
		end if;
	end Image;

	------------------------------
	-- END OF LOCAL SUBPROGRAMS --
	------------------------------



	function Format( F: Formatter; Date : Time ) return String is 
	begin
		-- Call Format using the date and the ISO Locale.  
		return Format(Get_Locale("ISO"), F, date);
	end format;


	function Format (	L		: Locale;
						F 		: Formatter;
						Date 	: Ada.Calendar.Time ) return String is
		-- returns the date formatted according to Formatter's Pattern and Locale.

		Picture : String := To_String(F.Pattern);
		
		Padding : Padding_Mode := Zero;
		--  Padding is set for one directive

		Result		: Unbounded_String;

		Year		: Year_Number;
		Month		: Month_Number;
		Day			: Day_Number;
		Hour		: Hour_Number;
		Minute		: Minute_Number;
		Second		: Second_Number;
		Sub_Second	: Second_Duration;

		P : Positive := Picture'First;

		begin
			Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);

		loop
			--  A directive has the following format "%[-_]."

			if Picture (P) = '%' then

				Padding := Zero;

				if P = Picture'Last then
					raise Pattern_Error;
				end if;

				--  Check for GNU extension to change the padding

				if Picture (P + 1) = '-' then
					Padding := None;
					P := P + 1;
				elsif Picture (P + 1) = '_' then
					Padding := Space;
					P := P + 1;
				end if;

				if P = Picture'Last then
					raise Pattern_Error;
				end if;

				case Picture (P + 1) is

					--  Literal %
					when '%' =>
						Result := Result & '%';

					--  A newline
					when 'n' =>
						Result := Result & ASCII.LF;

					--  A horizontal tab
					when 't' =>
						Result := Result & ASCII.HT;

					--  Hour (00..23)
					when 'H' =>
						Result := Result & Image (Hour, Padding, 2);

					--  Hour (01..12)
					when 'I' =>
						Result := Result & Image (Hour_12 (Hour), Padding, 2);

					--  Hour ( 0..23)
					when 'k' =>
						Result := Result & Image (Hour, Space, 2);

					--  Hour ( 1..12)
					when 'l' =>
						Result := Result & Image (Hour_12 (Hour), Space, 2);

					--  Minute (00..59)
					when 'M' =>
						Result := Result & Image (Minute, Padding, 2);

					--  AM/PM
					when 'p' =>
						Result := Result & Am_Pm (Hour);

					--  Time, 12-hour (hh:mm:ss [AP]M)
					when 'r' =>
						Result := Result &
							Image (Hour_12 (Hour), Padding, Length => 2) & ':' &
							Image (Minute, Padding, Length => 2) & ':' &
							Image (Second, Padding, Length => 2) & ' ' &
							Am_Pm (Hour);
	
					--	Seconds  since 1970-01-01  00:00:00 UTC
					--	(a nonstandard extension)
					--	 when 's' =>
					--
				 	--	 declare
					--		Sec : constant Sec_Number :=
					--			Sec_Number (Julian_Day (Year, Month, Day) -
					--			Julian_Day (1970, 1, 1)) * 86_400 +
					--			Sec_Number (Hour) * 3_600 +
					--			Sec_Number (Minute) * 60 +
					--			Sec_Number (Second);
					--	 begin
					--		 Result := Result & Image (Sec, None);
					--	end;
					-- 
					
					
					--	 Second (00..59)
		 			when 'S' =>
							Result := Result & Image (Second, Padding, Length => 2);
	
						--  Milliseconds (3 digits)
						--  Microseconds (6 digits)
						--  Nanoseconds  (9 digits)

					when 'i' | 'e' | 'o' =>
						declare
							Sub_Sec : constant Long_Integer :=
								Long_Integer (Sub_Second * 1_000_000_000);

							Img1  : constant String := Sub_Sec'Img;
							Img2  : constant String := "00000000" &
							 	Img1 (Img1'First + 1 .. Img1'Last);
							Nanos : constant String :=
								 Img2 (Img2'Last - 8 .. Img2'Last);
		
						begin
							case Picture (P + 1) is
								when 'i' =>
									Result := Result &
										Nanos (Nanos'First .. Nanos'First + 2);

								when 'e' =>
									Result := Result &
										Nanos (Nanos'First .. Nanos'First + 5);

								when 'o' =>
									Result := Result & Nanos;

								when others =>
									null;
							end case;
						end;

					--  Time, 24-hour (hh:mm:ss)
					when 'T' =>
						Result := Result &
						  Image (Hour, Padding, Length => 2) & ':' &
						  Image (Minute, Padding, Length => 2) & ':' &
						  Image (Second, Padding, Length => 2);


					--  Locale's abbreviated weekday name (Sun..Sat)
					when 'a' =>
					declare
						D: Day_Name :=
							Ada.Calendar.Formatting.Day_Of_Week(Date);
						S: String :=  Aw_lib.Locales.Image(L, D, true);
					begin
						Result := Result & Image (S);
					end;


					--  Locale's full weekday name, variable length
					--  (Sunday..Saturday)
					when 'A' =>
						declare
							D: Day_Name := 
								Ada.Calendar.Formatting.Day_Of_Week(Date);
							S: String :=  Aw_lib.Locales.Image(L, D, false);
		
						begin
							Result := Result & Image (S);
						end;


					--  Locale's abbreviated month name (Jan..Dec)
					when 'b' | 'h' =>
						Result := Result &
						  Image (Image(L, Month, true));


					--  Locale's full month name, variable length
					--  (January..December)
					when 'B' =>
						Result := Result &
						  Image (Image(L, Month, false));


					--  Locale's date and time (Sat Nov 04 12:02:33 EST 1989)
					when 'c' =>
						declare
							Zero_Formatter : Formatter := 
								Get_Formatter("%a %b %d %T %Y");
							Space_Formatter : Formatter :=
								Get_Formatter("%a %b %_d %_T %Y");
							None_Formatter : Formatter :=
								Get_Formatter("%a %b %-d %-T %Y");
						begin
							case Padding is
								when Zero => Result := Result &
									Format(L, Zero_Formatter, Date);
								when Space => Result := Result &
									Format(L, Space_Formatter, Date);
								when None => Result := Result &
									Format(L, None_Formatter, Date);
								end case;
						end;


					--	Day of month (01..31)
					when 'd' =>
						Result := Result & Image (Day, Padding, 2);


					--  Date (mm/dd/yy)
					when 'D' | 'x' =>
						Result := Result &
							Image (Month, Padding, 2) & '/' &
							Image (Day, Padding, 2) & '/' &
							Image (Year, Padding, 2);


					--  Month (01..12)
					when 'm' =>
						Result := Result & Image (Month, Padding, 2);


					--  Day of week (0..6) with 0 corresponding to Sunday
					when 'w' =>
						declare
							DOW : Natural range 0 .. 6;

						begin
							if Day_Of_Week (Date) = Sunday then
								DOW := 0;
							else
								DOW := Day_Name'Pos (Day_Of_Week (Date));
							end if;

							Result := Result & Image (DOW, Length => 1);
						end;

					-- Year (70...)	
					when 'y' =>
						declare
							Y : constant Natural := Year - (Year / 100) * 100;
						begin
							Result := Result & Image (Y, Padding, 2);
						end;

					--	Year (1970...)
					when 'Y' =>
						Result := Result & Image (Year, None, 4);

					when others =>
						raise Pattern_Error;
				end case;

				P := P + 2;

			else
				Result := Result & Picture (P);
				P := P + 1;
			end if;

			exit when P > Picture'Last;

		end loop;

		return To_String (Result);
	end Format;
	
end Aw_Lib.Calendar;

