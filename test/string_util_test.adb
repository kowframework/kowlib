-- test procedure for String utilities in ALOS
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26


with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with ALOS.String_Util;		use ALOS.String_Util;
with ALOS.UString_Vectors;	use ALOS.UString_Vectors;


procedure String_Util_Test is





   procedure Vector_Iterator( c: in Cursor ) is
      -- print the element at position c
   begin
      Put_Line( "   " & To_String( Element( c ) ) );
   end Vector_Iterator;



   -- now our own variables:
   Str: String := ";this;;is;a;string;for;testing;explode;and;implode;functions;;;";
   Vect: Vector;


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



end String_Util_Test;
