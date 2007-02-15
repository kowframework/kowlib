-- Library to perform actions over Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26




with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;


with Aw_Lib.UWide_String_Vectors;



package Aw_Lib.Wide_String_Util is


   function implode( Sep: Wide_Character; Vect: Aw_Lib.UWide_String_Vectors.Vector ) return Wide_String;
   -- join all parts of the Vector into a String of value element1[SEP]element2[SEP]ele...

   function implode( Sep: Wide_Character; Vect: Aw_Lib.UWide_String_Vectors.Vector ) return Unbounded_Wide_String;
   -- join all parts of the Vector into a Unbounded_String of value element1[SEP]element2[SEP]ele...

   function explode( Sep: Wide_Character; Str: Wide_String ) return Aw_Lib.UWide_String_Vectors.Vector;
   -- split the string Str by Sep and return a vector containing it.

   function explode( Sep: Wide_Character; Str: Unbounded_Wide_String ) return Aw_Lib.UWide_String_Vectors.Vector;
   -- split the string Str by Sep and return a vector containing it.



   procedure Str_Replace( From, To: in Wide_Character; Str: in out Wide_String );
   -- replace all the ocurences of the character From by To.

   function Str_Replace( From, To: in Wide_Character; Str: in Wide_String ) return Wide_String;
   -- replace all the ocurences of the character From by To returning the new Value.

end Aw_Lib.Wide_String_Util;
