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



package body KOW_Lib.Locales.Formatting is


	type Padding_Type is (
			No_Padding,	-- don't fill spaces
			Padding_Zero,	-- fill with zeros
			Padding_Space	-- fill with spaces
		);


	----------------------
	-- Helper Functions --
	----------------------


	function Am_Pm (H : Natural) return String is
	begin
	 if H = 0 or else H > 12 then
		 return "PM";
	 else
		 return "AM";
	 end if;
	end Am_Pm;


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

	function Image (
				Str	: String;
				Length	: Natural := 0
			) return String	is
	begin
		if Length = 0 then
			return Str;
		else
			return Str( Str'First .. Str'First + Length - 1 );
		end if;
	end Image;



	------------------------------
	-- Date and Time formatting --
	------------------------------

	function Format(
				L		: in Locale_Type;
				T		: in Time;
				Pattern		: in String
			) return String is
		-- format the time variable using the T's time offset and given pattern
	begin
		return Format( L, T, Pattern, Time_Zones.UTC_Time_Offset( T ) );
	end Format;

	function Format(
				L		: in Locale_Type;
				T		: in Time;
				Pattern		: in String;
				Time_Zone	: in Time_Zones.Time_Offset 
			) return String is
		-- format the time variable using the given time offset and pattern

		Year		: Year_Number;
		Month		: Month_Number;
		Day		: Day_Number;
		Hour		: Hour_Number;
		Minute		: Minute_Number;
		Second		: Second_Number;
		Sub_Second	: Second_Duration;



		function Value_Of( Key : in String ) return String is
			C	: constant Character := Key( Key'First );

			function Padding return Padding_Type is
			begin
				if Key'Length = 1 then
					return Padding_Zero;
				else
					if Key( Key'Last ) = '_' then
						return Padding_Spaces;
					elsif Key( Key'Last ) = '-' then
						return No_Padding;
					elsif Key( Key'Last ) = '0' then
						return Padding_Zero;
					else
						raise Pattern_Error with "Invalid modifier in key " & Key;
					end if;
				end if;
			end Padding;
		begin
			case C is
				--  A newline
				when 'n' =>
					return ( 1 => ASCII.LF );

				--  A horizontal tab
				when 't' =>
					return (  1 => ASCII.HT );

				--  Hour (00..23)
				when 'H' =>
					return Image( Hour, Padding, 2 );

				--  Hour (01..12)
				when 'I' =>
					return Image( Hour_12 (Hour), Padding, 2 );

				--  Minute (00..59)
				when 'M' =>
					return Image( Minute, Padding, 2 );

				--  AM/PM
				when 'p' =>
					return Am_Pm( Hour );

				--	 Second (00..59)
				when 'S' =>
					return Image( Second, Padding, Length => 2 );

				-- Locale's default time
				when 'T' =>
					return Format( L, T, L.Default_Time.all, Time_Zone );


				--  Locale's abbreviated weekday name (Sun..Sat)
				when 'a' =>
					declare
						D : constant Day_Name := Ada.Calendar.Formatting.Day_Of_Week(Date);
					begin
						return To_String( L.Short_Week_Days( D ) );
					end;


				--  Locale's full weekday name, variable length
				--  (Sunday..Saturday)
				when 'A' =>
					declare
						D : constant Day_Name := Ada.Calendar.Formatting.Day_Of_Week(Date);
					begin
						return To_String( L.Week_Days( D ) );
					end;


				--  Locale's abbreviated month name (Jan..Dec)
				when 'b'  =>
					return To_String( L.Short_Months( Month ) );


				--  Locale's full month name, variable length
				--  (January..December)
				when 'B' =>
					  return To_String( L.Months( Month ) );


				--  Locale's date and time (Sat Nov 04 12:02:33 EST 1989)
				when 'c' =>
					return Format( L, T, L.Default_Datetime.all, Time_Zone );

				-- Day of month (01..31)
				when 'd' =>
					return Image( Day, Padding, 2 );


				--  Date (mm/dd/yy)
				when 'D'  =>
					return Format( L, T, L.Default_Date.all, Time_Zone );

				--  Month (01..12)
				when 'm' =>
					return Image( Month, Padding, 2 );


				--  Day of week (0..6) with 0 corresponding to Sunday
				when 'w' =>
					declare
						DOW : Natural range 0 .. 6;
					begin
						if Day_Of_Week( Date ) = Sunday then
							DOW := 0;
						else
							DOW := Day_Name'Pos( Day_Of_Week( Date ) );
						end if;
						return Image (DOW, No_Padding, Length => 1);
					end;

				-- Year (70...)	
				when 'y' =>
					declare
						Y : constant Natural := Year - (Year / 100) * 100;
					begin
						return Image( Y, Padding, 2 );
					end;

				--	Year (1970...)
				when 'Y' =>
					return Image( Year, Padding, 4 );

				when others =>
					raise Pattern_Error with "Unknown Time Key " & Key;
			end case;
		end Value_Of;


		function Inner_Format is new Generic_Format( Value_Of => Value_Of );
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

		return Inner_Format( Pattern );
	end Format;



	---------------------
	-- Name Formatting --
	---------------------

	function Full_Name(
				L		: in Locale_Type;
				First_Name	: in String;
				Last_Name	: in String
			) return String is
		-- format the given name


		function Value_of( Key : in String ) return String is
		begin
			case Key( Key'Last ) is
				when 'f' =>
					return First_Name;
				when 'l' =>
					return Last_Name;
				when others =>
					raise Pattern_Error with "Invalid key for name formatting " & Key;
			end case;
		end Value_of;

		function Inner_Format is new Generic_Format( Value_of );
	begin
		return Inner_Format( To_String( L.Person_Name_Img ) );
	end Full_Name;


	-------------------------------
	-- Generic Formatting Engine --
	-------------------------------


	function Generic_Format( Pattern : in String ) return String is
		-- looks for instances of %SOMEKEY, and call Value_Of for those values.
		-- Then replace it in the resulting string.
		--
		--
		-- The current implementation relies on Unbounded_String internally
	

		Buffer	: Unbounded_String;
		i	: Positive := Pattern'First;


		function In_Range return Boolean is
		begin
			return i in Pattern'Range;
		end In_Range;

		function C return Character is
		begin
			return Pattern( i );
		end C;

		procedure Next is
		begin
			i := i + 1;
		end Next;

		procedure Previous is
		begin
			i := i - 1;
		end Previous;

		procedure Append_Value is
			function Get_Key return String is
				Key_Buffer : Unbounded_String;

				First : Boolean := True;

				function Is_Key_Char return Boolean is
					function Is_Modifier return Boolean is
					begin
						return C in ( '0', '-', '_' );
					end Is_Modifier;

					function Is_Key return Boolean is
					begin
						return C in 'a' .. 'z' or else C in 'A' .. 'Z';
					end Is_Key;
				begin
					if First then
						First := False;
						return Is_Modifier or else Is_Key;
					else
						return Length( Key_Buffer < 2 ) and then Is_Key;
					end if;
				end Is_Key_Char;
			begin
				loop
					exit when not In_Range or else not Is_Key_Char;
					Append( Key_Buffer, C );
					Next;
				end loop;

				if In_Range then
					-- because it will move forward again later on
					Previous;
				end if;

				return To_String( Key_Buffer );
			end Get_Key;
		begin
			Next;
			if In_Range then
				if C = '%' then
					Append( Buffer, '%' );
				else
					Append( Buffer, Value_Of( Get_Key ) );
				end if;
			end if;
		end Append_Value;

	begin

		loop
			exit when not In_Range;
			if C = '%' then
				Append_Value;
			else
				Append( Buffer, C );
			end if;
			Next;
		end loop;

		return To_String( Buffer );
	end Generic_Format;
end KOW_Lib.Locales.Formatting;

