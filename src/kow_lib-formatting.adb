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


with Ada.Calendar;		use Ada.Calendar; 
with Ada.Calendar.Formatting;	use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; 	use Ada.Calendar.Time_Zones;

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with Ada.Characters.Handling;	use Ada.Characters.Handling;

with KOW_Lib.Locales;		use KOW_Lib.Locales;



package body KOW_Lib.Calendar is


	function Format(
				Pattern		: in Locales.String_Access;
				T		: in Time
			) return String is
		-- format the time variable using the T's time offset and given pattern
	begin
		return Format( Pattern, T, Time_Zones.UTC_Time_Offset( T ) );
	end Format;

	function Format(
				Pattern		: in String;
				T		: in Time;
				Time_Zone	: in Time_Zones.Time_Offset 
			) return String is
		-- format the time variable using the given time offset and pattern




	
	function Get(Hora : Time; Key: in Character) return Unbounded_String is 
		-- Funcao que receba uma chave e retorna o valor;
		Str : Unbounded_String;
		function To_Unb(Num: Integer; Tamanho: Integer) return Unbounded_String is
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


	type Padding_Mode is (None, Zero, Space);



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


	------------------------------
	-- END OF LOCAL SUBPROGRAMS --
	------------------------------

	function Format(
				L		: in Locale;
				F		: in Formatter; 
				date		: in Time;
				Time_Zone	: in Time_Zones.Time_Offset := Time_Zones.UTC_Time_Offset( Clock )
			) return String is
		-- returns the date formatted according to Formatter's Pattern, Locale and time zone. 

		Pattern : String := To_String(F.Pattern);
		
		Fill		: Boolean := True;
		Fill_Char	: Character := '0';
		Result		: Unbounded_String;

		Year		: Year_Number;
		Month		: Month_Number;
		Day		: Day_Number;
		Hour		: Hour_Number;
		Minute		: Minute_Number;
		Second		: Second_Number;
		Sub_Second	: Second_Duration;

		P : Positive := Pattern'First;


		function C return Character is
		begin
			return Pattern( P );
		end C;

		procedure Next is
		begin
			P := P + 1;
		end Next;



		procedure Append_Value is
			-- append the value corresponding to the current value of P
		begin
			case C is
				--  Literal %
				when '%' =>
					Append( '%' );

				--  A newline
				when 'n' =>
					Append( ASCII.LF );

				--  A horizontal tab
				when 't' =>
					Append( ASCII.HT );

				--  Hour (00..23)
				when 'H' =>
					Append( Image( Hour, Fill_Char, 2 ) );

				--  Hour (01..12)
				when 'I' =>
					Append( Image( Hour_12( Date ), Fill, Fill_Char, 2 ) );
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
						case Pattern (P + 1) is
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
					S: String :=  KOW_lib.Locales.Image(L, D, true);
				begin
					Result := Result & Image (S);
				end;


				--  Locale's full weekday name, variable length
				--  (Sunday..Saturday)
				when 'A' =>
					declare
						D: Day_Name := 
							Ada.Calendar.Formatting.Day_Of_Week(Date);
						S: String :=  KOW_lib.Locales.Image(L, D, false);
	
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
		end Append_Value;


		begin
			Ada.Calendar.Formatting.Split(
					Date		=> Date,
					Year		=> Year,
					Month		=> Month,
					Day		=> Day,
					Hour		=> Hour,
					Minute		=> Minute,
					Second		=> Second,
					Sub_Second	=> Sub_Second,
					Time_Zone	=> Time_Zone
				);

		loop
			--  A directive has the following format "%[-_]."
			if Pattern( P ) = '%' then


				if P = Pattern'Last then
					-- the pattern shouldn't end with %
					raise Pattern_Error;
				end if;

				--  Check for GNU extension to change the padding

				if Pattern (P + 1) = '-' then
					Fill = False;
					P := P + 1;
				elsif Pattern (P + 1) = '_' then
					Fill := True;
					Fill_Charr = ' ';
					P := P + 1;
				else
					Fill := True;
					Fill_Char := '0';
				end if;

				Next;
				Append_Value( P );
				Next;
			else
				Result := Result & Pattern (P);
				Next;
			end if;

			exit when P > Pattern'Last;

		end loop;

		return To_String (Result);
	end Format;





end KOW_Lib.Calendar;

