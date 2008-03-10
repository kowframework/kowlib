-- Library to perform actions over Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$



with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Ada.Containers.Vectors;
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


   package Positions_Vectors is new Ada.Containers.Vectors( Element_Type => Natural,
                                                            Index_Type   => Natural );

   function Str_Replace( From, To, Str: in String; Case_Sensitive: Boolean := True ) return Unbounded_String;
   procedure Str_Replace( From, To, Str: in String; Result: in out Unbounded_String; Case_Sensitive: Boolean := True );


   function Str_Replace( From, To, Str: in Unbounded_String; Case_Sensitive: Boolean := True ) return Unbounded_String;
   procedure Str_Replace( From, To, Str: in Unbounded_String; Result: in out Unbounded_String; Case_Sensitive: Boolean := True );

   -- for compatibility
   procedure Str_Replace( From, To: in Unbounded_String; Str: in out Unbounded_String ); 
   --function Str_Replace( From, To: in Unbounded_String; Str: in Unbounded_String ) return Unbounded_String; 

   procedure Str_Replace( From, To: in String; Str: in out Unbounded_String ); 
   function Str_Replace( From, To: in String; Str: in Unbounded_String ) return Unbounded_String; 



private

   function Find_Occurances( Find, Context : in String; Case_Sensitive: Boolean ) return Positions_Vectors.Vector;
   -- uses the Knuth-Morris_Pratt string searching algorithm to find all occurances of Find in Context
   -- and returns an vector with the starting positions of each occurance

end Aw_Lib.String_Util;

