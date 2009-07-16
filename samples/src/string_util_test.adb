------------------------------------------------------------------------------
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
-- This is the String_Util_Test                                             --
--                                                                          --
-- test procedure for String utilities in KOW_Lib                            --
------------------------------------------------------------------------------

with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with KOW_Lib.String_Util;		use KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;		use KOW_Lib.UString_Vectors;
with KOW_Lib.UString_Ordered_Maps;	use KOW_Lib.UString_Ordered_Maps;


procedure String_Util_Test is


	procedure Vector_Iterator( c: in KOW_Lib.UString_Vectors.Cursor ) is
		-- print the element at position c
	begin
		Put_Line( " " & To_String( Element( c ) ) );
	end Vector_Iterator;


	procedure Map_Iterator( c: in KOW_Lib.UString_Ordered_Maps.Cursor ) is
	begin
		Put_Line( " " & To_String( Key( C ) ) & " => " & To_String( Element( C ) ) );
	end Map_Iterator;


	-- now our own variables:
	Str: String := ";this;;is;a;string;for;testing;explode;and;implode;functions;;;";
	Str2: String := "This is a stringinging for testinginging replace ace function";
	Vect: Vector;
	UStr, UStr2: Unbounded_String;

	Ma: Map;

begin
	New_Line;

	Put_Line( "The original String is: " );
	Put_Line ( """" & Str & """" );

	New_Line;

	Put_Line( "The exploded vector has the data: " );
	Vect := explode( ';', Str );
	Iterate( Vect, Vector_Iterator'Access );

	New_Line;

	Put_Line( "And after imploding we've got:" );
	Put_Line( """" & Implode( ';', Vect ) & """" );

	New_Line;

	Put_Line( "Replacing '';'' by ''!'' on the sample string" );
	Str_Replace( ';', '!', Str );
	Put_Line( Str );

	New_Line;

	Put_Line( "Replacing 'ing' by 'REPLACED' on the sample string" );
	Str_Replace(	From => "ing",
			To => "REPLACED",
			Str => Str2,
			Result => UStr ); 
	Put_Line ("- Original string is: """ & Str2 & """" );
	Put_Line( "- Replaced string is: """ & To_String( UStr ) & """" );

	New_Line;

	Put_Line( "Replacing 'ACED' by '' on the replaced string" );
	Str_Replace(	From	=> "ACED",
			To	=> "",
			Str	=> To_String( UStr ),
			Result	=> UStr2 ); 
	Put_Line ("- Original string is: """ & To_String( UStr ) & """" );
	Put_Line( "- Replaced string is: """ & To_String( UStr2 ) & """" );

	New_Line;

	Put_Line( "Adding some data do the Map" );
	Insert( Ma, To_Unbounded_String( "test2" ) , To_Unbounded_String( "doioi" ) );
	Insert( Ma, To_Unbounded_String( "test3" ) , To_Unbounded_String( "doioi" ) );
	Insert( Ma, To_Unbounded_String( "test4" ) , To_Unbounded_String( "doioi" ) );
	Insert( Ma, To_Unbounded_String( "test12" ) , To_Unbounded_String( "doioi" ) );
	Insert( Ma, To_Unbounded_String( "test1" ) , To_Unbounded_String( "doioi" ) );

	Put_Line( "Iterating... " );
	Iterate( Ma, Map_Iterator'Access );


	New_Line;

end String_Util_Test;
