-----------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
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
-- This is the Aw_Lib.Wide_String_util package                              --
--                                                                          --
-- Library to perform actions over Wide_Strings                             --
------------------------------------------------------------------------------


with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;
with Ada.Containers;			use Ada.Containers;

with Aw_Lib.UWide_String_Vectors;
with Ada.Text_IO;
with Ada.Characters.Handling;		use Ada.Characters.Handling;

-- Used for Wide_String replacement
-- with GNAT.Spitbol.Patterns;


package body Aw_Lib.Wide_String_Util is

	function implode(	Sep: Wide_Character;
				Vect: Aw_Lib.UWide_String_Vectors.Vector )
		return Wide_String is
	-- join all parts of the Vector into a Wide_String 
	-- of value element1[SEP]element2[SEP]ele...
	begin
		return To_Wide_String( implode( Sep, Vect ) );
	end implode;


	function implode(	Sep: Wide_Character; 
				Vect: Aw_Lib.UWide_String_Vectors.Vector )
		return Unbounded_Wide_String is
	-- join all parts of the Vector into a Unbounded_Wide_String
	-- of value element1[SEP]element2[SEP]ele...

		use Aw_Lib.UWide_String_Vectors;

		function implode_int ( c: Cursor ) return Unbounded_Wide_String is
			-- recursive function to implode Wide_String
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
			return To_Unbounded_Wide_String( "" );
	end implode;


	function explode( Sep: Wide_Character; Str: Wide_String )
		return Aw_Lib.UWide_String_Vectors.Vector is
		-- split the Wide_String Str by Sep and return a vector containing it.
	begin
		return explode( Sep, To_Unbounded_Wide_String( Str ) );
	end explode;


	function explode( Sep: Wide_Character; Str: Unbounded_Wide_String ) 
		return Aw_Lib.UWide_String_Vectors.Vector is
		-- split the Wide_String Str by Sep and return a vector containing it.
		
		use Aw_Lib.UWide_String_Vectors;
		
		Vect: Vector;
		ini, fim: Natural := 1;
	
	begin

		while fim <= Length( Str )
		loop
			if Element( Str, fim ) = Sep  then
				if ini = fim then
					Append( Vect, To_Unbounded_Wide_String( "" ) );
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
			Append( Vect, To_Unbounded_Wide_String( "" ) );
		else
			Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
		end if;

		return Vect;
	end explode;


	procedure Str_Replace( From, To: in Wide_Character; Str: in out Wide_String ) is
	-- replace all the ocurences of the Wide_Character From by To.
	begin
		for i in Str'Range loop
			if Str(i) = From then
				Str(i) := To;
	  		end if;
		end loop;
	end Str_Replace;

	function Str_Replace(	From, To: in Wide_Character; 
				Str: in Wide_String ) return Wide_String is
	 -- replace all the ocurences of the Wide_Character
	 -- from by To returning the new Value.
	
		R: Wide_String := Str;
	begin
		Str_Replace( From, To, R );
		
		return R;
	end Str_Replace;



	procedure Str_Replace( From, To, Str: in Unbounded_Wide_String;
		Result: in out Unbounded_Wide_String) is
	begin
		Result := Str_Replace( From, To, Str);
	end Str_Replace;

	function Str_Replace( From, To, Str: in Unbounded_Wide_String)
		return Unbounded_Wide_String is
	begin
		return Str_Replace( To_Wide_String( From ), 
			To_Wide_String( To ), To_Wide_String( Str ));
	end Str_Replace;



	procedure Str_Replace( From, To, Str: in Wide_String;
		Result: in out Unbounded_Wide_String) is
	begin
		Result := Str_Replace( From, To, Str);
	end Str_Replace;


	function Str_Replace( From, To: in Wide_String; Str: in Wide_String)
		return Unbounded_Wide_String is

		use Positions_Vectors;

		Occurances	: Positions_Vectors.Vector := Find_Occurances(From, Str);
		Size_Dif	: Integer := To'Length - From'Length;
		Result		: Unbounded_Wide_String;
		Index_str	: Positive := Str'First;
		Index_Occu	: Positions_Vectors.Extended_Index := Occurances.First_Index;
		Element_Occu	: Positions_Vectors.Cursor;
		Actual		: Natural;
	
		subtype i is Integer range To'Range;

	begin

		if Occurances.Length = 0 then
			return To_Unbounded_Wide_String( Str );
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


	function Find_Occurances( Find, Context : in Wide_String) 
		return Positions_Vectors.Vector is

		type List is array ( Natural range <> ) of Integer;

		function Pre_Compute( Str : in Wide_String) return List is
		Pragma Inline ( Pre_Compute );

			-- computes the failure_function table of the KMP algorithm
			pos : Integer;
			T : List( 0 .. Str'Length ) := ( 0 .. Str'Length => -1 );
			Str2 : Wide_String := Str;

			use Ada.Text_IO;
		begin

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
		Table		: List := Pre_Compute(Find);
		Find_Idx	: Integer := 0;

		Find2		: Wide_String := Find;
		Context2	: Wide_String := Context;

		use Ada.Text_IO;
	begin
		-- Knuth-Morris-Pratt algorithm

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

	end Find_Occurances;


	-- for compatibility with older version
	procedure Str_Replace( From, To: in Unbounded_Wide_String; 
		Str: in out Unbounded_Wide_String ) is
	begin
		Str := Str_Replace( To_Wide_String( From ), 
			To_Wide_String( To ), To_Wide_String( Str ) );
	end Str_Replace;


	procedure Str_Replace( From, To: in Wide_String; 
		Str: in out Unbounded_Wide_String ) is
	begin
		Str := Str_Replace( From, To, To_Wide_String( Str ) );
	end Str_Replace;
 
	function Str_Replace( From, To: in Wide_String;
		Str: in Unbounded_Wide_String ) return Unbounded_Wide_String is
	begin
		return Str_Replace( From, To, To_Wide_String( Str ) );
	end Str_Replace;



end Aw_Lib.Wide_String_Util;

