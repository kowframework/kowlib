-- test procedure for String utilities in Aw_Lib
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$



with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Aw_Lib.String_Util;		use Aw_Lib.String_Util;
with Aw_Lib.UString_Vectors;		use Aw_Lib.UString_Vectors;
with Aw_Lib.UString_Ordered_Maps;	use Aw_Lib.UString_Ordered_Maps;


procedure String_Util_Test is





   procedure Vector_Iterator( c: in Aw_Lib.UString_Vectors.Cursor ) is
      -- print the element at position c
   begin
      Put_Line( "   " & To_String( Element( c ) ) );
   end Vector_Iterator;


   procedure Map_Iterator( c: in Aw_Lib.UString_Ordered_Maps.Cursor ) is
   begin
	   Put_Line( "   " & To_String( Key( C ) ) & " => " & To_String( Element( C ) ) );
   end Map_Iterator;



   -- now our own variables:
   Str: String := ";this;;is;a;string;for;testing;explode;and;implode;functions;;;";
   Vect: Vector;

   Ma: Map;

begin
   New_Line;

   Put_Line( "The original String is: " );
   Put_Line ("""" & Str & """" );

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
