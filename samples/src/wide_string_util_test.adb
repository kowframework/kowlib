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
-- This is the Wide_String_Util_Test                                        --
--                                                                          --
-- test procedure for Wide_String utilities in Aw_Lib                       --
------------------------------------------------------------------------------


with Ada.Wide_Text_IO;			use Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;

with Aw_Lib.Wide_String_Util;		use Aw_Lib.Wide_String_Util;
with Aw_Lib.UWide_String_Vectors;	use Aw_Lib.UWide_String_Vectors;
with Aw_Lib.UWide_String_Ordered_Maps;	use Aw_Lib.UWide_String_Ordered_Maps;


procedure Wide_String_Util_Test is


	procedure Vector_Iterator( c: in Aw_Lib.UWide_String_Vectors.Cursor ) is
		-- print the element at position c
	begin
		Put_Line( " " & To_Wide_String( Element( c ) ) );
	end Vector_Iterator;


	procedure Map_Iterator( c: in Aw_Lib.UWide_String_Ordered_Maps.Cursor ) is
	begin
	 Put_Line( " " & To_Wide_String( Key( C ) ) & " => " & To_Wide_String( Element( C ) ) );
	end Map_Iterator;



	-- now our own variables:
	Str: Wide_String := " 学校;へ;行きます。" & 
		";this;;is;a;Wide_String;for;testing;explode;and;implode;functions;;;";
	
	Str2: Wide_String := "学校へ行きます。" & 
		"This is a Wide_Stringinging for testinginging replace ace function";
	
	Vect: Vector;
	UStr, UStr2: Unbounded_Wide_String;

	Ma: Map;


begin
	New_Line;

	Put_Line( "The original Wide_String is: " );
	Put_Line ("""" & Str & """" );

	New_Line;

	Put_Line( "The exploded vector has the data: " );
	Vect := explode( ';', Str );
	Iterate( Vect, Vector_Iterator'Access );

	New_Line;

	Put_Line( "And after imploding we've got:" );
	Put_Line( """" & Implode( ';', Vect ) & """" );

	New_Line;

	Put_Line( "Replacing '';'' by ''!'' on the sample Wide_String" );
	Str_Replace( ';', '!', Str );
	Put_Line( Str );

	New_Line;

	Put_Line( "Replacing 'ing' by 'REPLACED' on the sample Wide_String" );
	Str_Replace(	From => "ing",
			To => "REPLACED",
			Str => Str2,
			Result => UStr ); 
	Put_Line ("- Original Wide_String is: """ & Str2 & """" );
	Put_Line( "- Replaced Wide_String is: """ & To_Wide_String( UStr ) & """" );

	New_Line;

	Put_Line( "Replacing 'ACED' by '' on the replaced Wide_String" );
	Str_Replace(	From => "ACED",
			To => "",
			Str => To_Wide_String( UStr ),
			Result => UStr2 ); 
	Put_Line ("- Original Wide_String is: """ & To_Wide_String( UStr ) & """" );
	Put_Line( "- Replaced Wide_String is: """ & To_Wide_String( UStr2 ) & """" );

	New_Line;

	Put_Line( "Adding some data do the Map" );
	Insert( Ma, To_Unbounded_Wide_String( "test2" ) , To_Unbounded_Wide_String( "doioi" ) );
	Insert( Ma, To_Unbounded_Wide_String( "test3" ) , To_Unbounded_Wide_String( "doioi" ) );
	Insert( Ma, To_Unbounded_Wide_String( "test4" ) , To_Unbounded_Wide_String( "doioi" ) );
	Insert( Ma, To_Unbounded_Wide_String( "test12" ) , To_Unbounded_Wide_String( "doioi" ) );
	Insert( Ma, To_Unbounded_Wide_String( "test1" ) , To_Unbounded_Wide_String( "doioi" ) );

	Put_Line( "Iterating... " );
	Iterate( Ma, Map_Iterator'Access );


	New_Line;


end Wide_String_Util_Test;
