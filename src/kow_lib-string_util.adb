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

--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;	use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers;		use Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.UString_Vectors;



package body KOW_Lib.String_Util is

	function implode(	Sep: Character;
				Vect: KOW_Lib.UString_Vectors.Vector ) return String is
		-- join all parts of the Vector into a String of value
		-- element1[SEP]element2[SEP]ele...
	begin
		return To_String( implode( Sep, Vect ) );
	end implode;


	function implode(	Sep: Character; 
				Vect: KOW_Lib.UString_Vectors.Vector ) return Unbounded_String is
		-- join all parts of the Vector into a Unbounded_String
		-- of value element1[SEP]element2[SEP]ele...

		use KOW_Lib.UString_Vectors;

		function implode_int ( c: Cursor ) return Unbounded_String is
			-- recursive function to implode string
		begin
			return Element( c ) & Sep & implode_int( Next( c ) );
		exception
			when CONSTRAINT_ERROR =>
				-- we've reached the end of the vector
				return Element( c );
		end;

	begin
		return implode_int( First( Vect ) );
	exception
		when CONSTRAINT_ERROR =>
			--there is nothing on this vector! :O
			return To_Unbounded_String( "" );
	end implode;


	function explode( Sep: Character; Str: String )
		return KOW_Lib.UString_Vectors.Vector is
		-- split the string Str by Sep and return
		-- a vector containing it.
	begin
		return explode( Sep, To_Unbounded_String( Str ) );
	end explode;


	function explode(	Sep: Character; 
				Str: Unbounded_String ) return KOW_Lib.UString_Vectors.Vector is
		-- split the string Str by Sep and return a vector containing it.
		use KOW_Lib.UString_Vectors;
		Vect: Vector;
		ini, fim: Natural := 1;
	
	begin
		if Str = Null_Unbounded_String then
			return Vect;
		end if;

		while fim <= Length( Str )
		loop

			if Element( Str, fim ) = Sep  then
				if ini = fim then
					Append( Vect, To_Unbounded_String( "" ) );
				else
					Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
				end if;
				ini := fim + 1;
			end if;
			fim := fim + 1;

		end loop;

		-- now we add the last element, it doesn't matter if it's null or not:
		if ini = fim then
			-- it's a null element
			Append( Vect, To_Unbounded_String( "" ) );
		else
			Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
		end if;

		return Vect;

	end explode;


	procedure Str_Replace( From, To: in Character; Str: in out String ) is
	-- replace all the ocurences of the character From by To.
	begin
		for i in Str'Range loop
			if Str(i) = From then
			Str(i) := To;
	end if;
	end loop;
	end Str_Replace;

	function Str_Replace( From, To: in Character; Str: in String ) return String is
	 -- replace all the ocurences of the character From by To returning the new Value.
		R: String := Str;
	begin
		Str_Replace( From, To, R );
		return R;
	end Str_Replace;



	procedure Str_Replace(	From, To, Str: in Unbounded_String;
				Result: in out Unbounded_String;
				Case_Sensitive: Boolean := True	) is
	begin
		Result := Str_Replace( From, To, Str, Case_Sensitive );
	end Str_Replace;

	function Str_Replace(	From, To, Str: in Unbounded_String;
				Case_Sensitive: Boolean := True	) return Unbounded_String is
	begin
		return Str_Replace(	To_String( From ),
					To_String( To ),
					To_String( Str ),
					Case_Sensitive	);
	end Str_Replace;



	procedure Str_Replace(	From, To, Str: in String;
				Result: in out Unbounded_String;
				Case_Sensitive: Boolean := True	) is
	begin
		Result := Str_Replace( From, To, Str, Case_Sensitive );
	end Str_Replace;


	function Str_Replace(	From, To: in String;
				Str: in String; Case_Sensitive: Boolean := True )
		return Unbounded_String is

		use Positions_Vectors;

		Occurances  : Positions_Vectors.Vector := 
			Find_Occurances( From, Str, Case_Sensitive );
		Size_Dif	: Integer := To'Length - From'Length;
		Result		: Unbounded_String;
		Index_str	: Positive := Str'First;
		Index_Occu	: Positions_Vectors.Extended_Index := Occurances.First_Index;
		Element_Occu	: Positions_Vectors.Cursor;
		Actual		: Natural;
		
		subtype i is Integer range To'Range;

	begin

		if Occurances.Length = 0 then
			return To_Unbounded_String( Str );
		end if;

		while Index_str <= Str'Last loop

			if Index_Occu /= Occurances.Last_Index + 1 then
				Actual := Positions_Vectors.Element(
					To_Cursor( Occurances, Index_Occu ) );
			else
				Actual := 0;
			end if;

			if Index_str = Actual then
				-- replace 

				for i in To'Range loop
					Append( Result, To( i ) );
				end loop;

				Index_Str := Index_Str + From'Length;
				Index_Occu := Index_Occu + 1;
				-- Index_Occu := Occurances.Next;
			else
				Append( Result, Str( Index_Str ) );
				Index_Str := Index_Str + 1;
			end if;

		end loop;

		return Result;

	end Str_Replace;


	function Find_Occurances(
					Find, Context	: in String;
					Case_Sensitive	: in Boolean
				) return Positions_Vectors.Vector is
		Last_Index	: Natural := 0;
		Vector		: Positions_Vectors.Vector;
	begin
		loop
			Last_Index := Ada.Strings.Fixed.Index(
					Source	=> Context,
					Pattern	=> Find,
					From	=> Positive( Last_Index + 1 )
				);
			exit when Last_Index = 0;

			Positions_Vectors.Append( Vector, Last_Index );
		end loop;

		return Vector;
	end Find_Occurances;


	function KMP_Find_Occurances(	Find, Context : in String; 
					Case_Sensitive: Boolean	)
		return Positions_Vectors.Vector is
		-- this implementation is broken as it expects every string to go from 1 to str'length..

		type List is array ( Natural range <> ) of Integer;

		function Pre_Compute( Str : in String; Case_Sensitive: Boolean ) return List is
		Pragma Inline ( Pre_Compute );

			-- computes the failure_function table of the KMP algorithm
			pos : Integer;
			T : List( 0 .. Str'Length ) := ( 0 .. Str'Length => -1 );
			Str2 : String := Str;

			use Ada.Text_IO;
		begin

			if Case_Sensitive then
				Str2 := To_Lower( Str );
			end if;

			for i in 1 .. Str'Length loop
				pos := T( i - 1 );

				while pos /= -1 and then Str( pos + 1 ) /= Str( i ) loop
					pos := T( pos );
				end loop;

				T( i ) := pos + 1;
			end loop;

			return T;

		end;

		-- declarations
		Matches		: Positions_Vectors.Vector;
		Context_Idx	: Positive := 1;
		Table		: List := Pre_Compute( Find, Case_Sensitive );
		Find_Idx	: Integer := 0;

		Find2		: String := Find;
		Context2	: String := Context;

		use Ada.Text_IO;
	begin
		-- Knuth-Morris-Pratt algorithm

		if Case_Sensitive then
			Find2 := To_Lower( Find );
			Context2 := To_Lower( Context );
		end if;

		while Context_Idx <= Context2'Length loop

			while Find_Idx /= -1 and then ( Find_Idx = Find2'Length or 
				else Find2( Find_Idx + 1 ) /= Context2( Context_Idx ) )
			loop
				Find_Idx := Table( Find_Idx );
			end loop;

			Find_Idx := Find_Idx + 1;
			Context_Idx := Context_Idx + 1;

			if Find_Idx = Find2'Length then
				Matches.Append( Context_Idx - Find2'Length );
			end if;
		end loop;

		return Matches;

	end KMP_Find_Occurances;


	-- for compatibility with older version
	procedure Str_Replace( From, To: in Unbounded_String; Str: in out Unbounded_String ) is
	begin
		Str := Str_Replace( To_String( From ), To_String( To ), To_String( Str ) );
	end Str_Replace;


	procedure Str_Replace( From, To: in String; Str: in out Unbounded_String ) is
	begin
		Str := Str_Replace( From, To, To_String( Str ) );
	end Str_Replace;
 
	function Str_Replace( From, To: in String; Str: in Unbounded_String )
		return Unbounded_String is
	begin
		return Str_Replace( From, To, To_String( Str ) );
	end Str_Replace;



	function Scriptify( Str : in String ) return String is
		-- process any string setting it safe to directly send to script environments such as bash
		-- replace \ by \\, " by \" and ' by \' (and so on) for sending to external resources as a single string

		The_Str : Unbounded_String := To_Unbounded_String( Str );

		Escape	: String := (
					 1	=> '\',
					 2	=> ''',
					 3	=> '"',
					 4	=> '$',
					 5	=> '{',  6	=> '}',
					 7	=> '[',  8	=> ']',
					 9	=> '&'
				);

	begin
		for i in Escape'Range loop
			Str_Replace( From => Escape( i .. i ), To => '\' & Escape( i ), Str => The_Str );
		end loop;

		return To_String( The_Str );
	end Scriptify;



	function JSon_Scriptify( Str : in String ) return String is
		-- process any string setting it safe to directly send to JSon script environments such as bash
		-- replace \ by \\, " by \" and ' by \' (and so on) for sending to external resources as a single string
		The_Str : Unbounded_String := To_Unbounded_String( Str );

		Escape	: String := (
					 1	=> '\',
					 2	=> ''',
					 3	=> '"',
					 4	=> Ada.Characters.Latin_1.LF
				);

	begin
		for i in Escape'Range loop
			Str_Replace( From => Escape( i .. i ), To => '\' & Escape( i ), Str => The_Str );
		end loop;
		Str_Replace( From => "" & Ada.Characters.Latin_1.CR, To => "" , Str => The_Str );

		return To_String( The_Str );
	end JSon_Scriptify;

	function Json_Unescriptify( Str : in String ) return String is
		-- inverse of json_scriptfy
	begin
		return To_String( Json_Unescriptify( To_Unbounded_String( Str ) ) );
	end Json_Unescriptify;

	function Json_Unescriptify( Str : in Unbounded_String ) return Unbounded_String is
		-- inverse of json_scriptfy
		The_Str : Unbounded_String := Str;

		Escape	: String := (
					 1	=> '\',
					 2	=> ''',
					 3	=> '"',
					 4	=> Ada.Characters.Latin_1.LF
				);

	begin
		for i in Escape'Range loop
			Str_Replace( From => '\' & Escape( i ), To => Escape( i .. i ), Str => The_Str );
		end loop;

		return The_Str;
	end Json_Unescriptify;


	function Texify( Str : in String; Fix_Underscore : Boolean := False ) return String is
		-- process the string so it's safe to run with TeX :: doesn't take into account the encoding!
		-- if Fix_Underscore then replace _ to \_

		-- process the string so it's safe to run with TeX :: doesn't take into account the encoding!

		-- process a string so it is safe to send to LaTeX.
		-- notice all commands are removed when calling this

		use KOW_Lib.String_Util;

		The_Str : Unbounded_String := To_Unbounded_String( Str );

		Escape	: String := (
					1	=> '\',
					2	=> '$',
					3	=> '{',  4	=> '}',
					5	=> '&',
					6	=> '#',
					7	=> '%',
					8	=> '_'
				);
		function Last return integer is
		begin
			if Fix_Underscore then
				return Escape'Last;
			else
				return Escape'Last - 1;
			end if;
		end Last;
	begin
		for i in Escape'First .. Last loop
			Str_Replace( From => Escape( i .. i ), To => '\' & Escape( i ), Str => The_Str );
		end loop;


		Str_Replace( From => Character'Val( 16#c2# ) &  Character'Val( 16#a0# ), To => " ", Str => The_Str );
		-- LaTeX doesn't like the NO-BREAK SPACE unicode character..

		return To_String( The_Str );
	end Texify;


	function Cfgify( Str : in String ) return String is
		-- process the string so it's safe to put as a value in our own config file syntax..
		The_Str : Unbounded_String := To_Unbounded_String( Str );
	begin
		Str_Replace( From => """", To => """""", Str => The_Str );
		Str_Replace( From => "" & Ada.Characters.Latin_1.CR, To => "" , Str => The_Str );

		return To_String( The_Str );
	end Cfgify;

	procedure Copy( To : in out String; From : in String ) is
	begin
		To( To'First .. To'First + From'Length - 1 )	:= From;
		To( To'First + From'Length .. To'Last )		:= ( others => ' ' );
	end Copy;





	function Expand( Value : in String ) return String is
		use Ada.Strings;
		From : Natural := Fixed.Index( Value, "${", Forward );
		To   : Natural;
	begin
		while From /= 0 loop

			if From = Value'First or else Value( From - 1 ) /= '$' then
				To := Fixed.Index( Value, "}", From, Forward );
				if To = 0 or else From + 2 > To - 1 then
					From := Fixed.Index( Value, "${", From + 1, Forward );
				else
					declare
						Before	: constant String := Value( Value'First .. From - 1 );
						Key	: constant String := Value( From + 2 .. To - 1 );
						After	: constant String := Value( To + 1 .. Value'Last );
					begin
						return Expand( Before & Value_Of( Key ) & After );
					end;
				end if;
			else
				From := Fixed.Index( Value, "${", From + 1, Forward );
			end if;
		end loop;

		return Value;
	end Expand;


end KOW_Lib.String_Util;

