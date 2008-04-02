-- Library to perform actions over Wide_Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date: 2008-03-10 17:26:13 -0300 (Seg, 10 Mar 2008) $
-- $Revision: 261 $
-- $Author: kali $



with Ada.Strings.Wide_Unbounded;   	use Ada.Strings.Wide_Unbounded;
with Ada.Containers.Vectors;
with Aw_Lib.UWide_String_Vectors;



package Aw_Lib.Wide_String_Util is


   function implode( Sep: Wide_Character; Vect: Aw_Lib.UWide_String_Vectors.Vector ) return Wide_String;
   -- join all parts of the Vector into a Wide_String of value element1[SEP]element2[SEP]ele...

   function implode( Sep: Wide_Character; Vect: Aw_Lib.UWide_String_Vectors.Vector ) return Unbounded_Wide_String;
   -- join all parts of the Vector into a Unbounded_Wide_String of value element1[SEP]element2[SEP]ele...

   function explode( Sep: Wide_Character; Str: Wide_String )
   	return Aw_Lib.UWide_String_Vectors.Vector;
   -- split the Wide_String Str by Sep and return a vector containing it.

   function explode( Sep: Wide_Character; Str: Unbounded_Wide_String )
   	return Aw_Lib.UWide_String_Vectors.Vector;
   -- split the Wide_String Str by Sep and return a vector containing it.


   procedure Str_Replace( From, To: in Wide_Character;
   	Str: in out Wide_String );
   -- replace all the ocurences of the Wide_Character From by To.

   function Str_Replace( From, To: in Wide_Character; Str: in Wide_String )
   	return Wide_String;
   -- replace all the ocurences of the Wide_Character From by To returning the new Value.


   package Positions_Vectors is new Ada.Containers.Vectors( Element_Type => Natural,
                                                            Index_Type   => Natural );

   function Str_Replace( From, To, Str: in Wide_String) return Unbounded_Wide_String;
   procedure Str_Replace( From, To, Str: in Wide_String; Result: in out Unbounded_Wide_String);


   function Str_Replace( From, To, Str: in Unbounded_Wide_String) return Unbounded_Wide_String;
   procedure Str_Replace( From, To, Str: in Unbounded_Wide_String;
   	Result: in out Unbounded_Wide_String);

   -- for compatibility
   procedure Str_Replace( From, To: in Unbounded_Wide_String; Str: in out Unbounded_Wide_String ); 
   --function Str_Replace( From, To: in Unbounded_Wide_String; Str: in Unbounded_Wide_String ) return Unbounded_Wide_String; 

   procedure Str_Replace( From, To: in Wide_String; Str: in out Unbounded_Wide_String ); 
   function Str_Replace( From, To: in Wide_String; Str: in Unbounded_Wide_String ) return Unbounded_Wide_String; 



private

   function Find_Occurances( Find, Context : in Wide_String) return Positions_Vectors.Vector;
   -- uses the Knuth-Morris_Pratt Wide_String searching algorithm to find all occurances of Find in Context
   -- and returns an vector with the starting positions of each occurance

end Aw_Lib.Wide_String_Util;

