-- Library to perform actions over Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26




with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


with ALOS.UString_Vectors;



package ALOS.String_Util is


   function implode( Sep: Character; Vect: ALOS.UString_Vectors.Vector ) return String;
   -- join all parts of the Vector into a String of value element1[SEP]element2[SEP]ele...

   function implode( Sep: Character; Vect: ALOS.UString_Vectors.Vector ) return Unbounded_String;
   -- join all parts of the Vector into a Unbounded_String of value element1[SEP]element2[SEP]ele...

   function explode( Sep: Character; Str: String ) return ALOS.UString_Vectors.Vector;
   -- split the string Str by Sep and return a vector containing it.

   function explode( Sep: Character; Str: Unbounded_String ) return ALOS.UString_Vectors.Vector;
   -- split the string Str by Sep and return a vector containing it.



   procedure Str_Replace( From, To: in Character; Str: in out String );
   -- replace all the ocurences of the character From by To.

   function Str_Replace( From, To: in Character; Str: in String ) return String;
   -- replace all the ocurences of the character From by To returning the new Value.

end ALOS.String_Util;
