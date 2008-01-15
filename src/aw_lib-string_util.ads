-- Library to perform actions over Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$



with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


with Aw_Lib.UString_Vectors;



package Aw_Lib.String_Util is


   function implode( Sep: Character; Vect: Aw_Lib.UString_Vectors.Vector ) return String;
   -- join all parts of the Vector into a String of value element1[SEP]element2[SEP]ele...

   function implode( Sep: Character; Vect: Aw_Lib.UString_Vectors.Vector ) return Unbounded_String;
   -- join all parts of the Vector into a Unbounded_String of value element1[SEP]element2[SEP]ele...

   function explode( Sep: Character; Str: String ) return Aw_Lib.UString_Vectors.Vector;
   -- split the string Str by Sep and return a vector containing it.

   function explode( Sep: Character; Str: Unbounded_String ) return Aw_Lib.UString_Vectors.Vector;
   -- split the string Str by Sep and return a vector containing it.



   procedure Str_Replace( From, To: in Character; Str: in out String );
   -- replace all the ocurences of the character From by To.

   function Str_Replace( From, To: in Character; Str: in String ) return String;
   -- replace all the ocurences of the character From by To returning the new Value.


   procedure Str_Replace( From, To: in Unbounded_String; Str: in out Unbounded_String );
   -- replace all the ocurences of the character From by To.

   function Str_Replace( From, To: in Unbounded_String; Str: in Unbounded_String ) return Unbounded_String;
   -- replace all the ocurences of the character From by To returning the new Value.

   procedure Str_Replace( From, To: in String; Str: in out Unbounded_String );
   -- replace all the ocurences of the character From by To.

   function Str_Replace( From, To: in String; Str: in Unbounded_String ) return Unbounded_String;
   -- replace all the ocurences of the character From by To returning the new Value.

end Aw_Lib.String_Util;

