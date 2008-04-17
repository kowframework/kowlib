------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
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
-- This is the Aw_Lib.Wide_String_Util package                              --
--                                                                          --
-- Library to perform actions over Wide_Strings                             --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Unbounded;		use Ada.Strings.Wide_Unbounded;
with Ada.Containers.Vectors;
with Aw_Lib.UWide_String_Vectors;



package Aw_Lib.Wide_String_Util is


	function implode(	Sep: Wide_Character; 
				Vect: Aw_Lib.UWide_String_Vectors.Vector )
		return Wide_String;
	-- join all parts of the Vector into a Wide_String
	-- of value element1[SEP]element2[SEP]ele...

	function implode(	Sep: Wide_Character;
				Vect: Aw_Lib.UWide_String_Vectors.Vector )
		return Unbounded_Wide_String;
	-- join all parts of the Vector into a Unbounded_Wide_String
	-- of value element1[SEP]element2[SEP]ele...

	function explode( Sep: Wide_Character; Str: Wide_String )
		return Aw_Lib.UWide_String_Vectors.Vector;
	-- split the Wide_String Str by Sep and return a vector containing it.

	function explode( Sep: Wide_Character; Str: Unbounded_Wide_String )
		return Aw_Lib.UWide_String_Vectors.Vector;
	-- split the Wide_String Str by Sep and return a vector containing it.


	procedure Str_Replace(	From, To: in Wide_Character;
				Str: in out Wide_String	);
	-- replace all the ocurences of the Wide_Character From by To.

	function Str_Replace(	From, To: in Wide_Character;
				Str: in Wide_String )	return Wide_String;
	-- replace all the ocurences of the Wide_Character
	-- From by To returning the new Value.


	package Positions_Vectors is new 
		Ada.Containers.Vectors(	Element_Type	=> Natural,
					Index_Type	=> Natural );

	function Str_Replace( From, To, Str: in Wide_String) return Unbounded_Wide_String;

	procedure Str_Replace(	From, To, Str: in Wide_String;
				Result: in out Unbounded_Wide_String);


	function Str_Replace( From, To, Str: in Unbounded_Wide_String) 
		return Unbounded_Wide_String;
	
	procedure Str_Replace(	From, To, Str: in Unbounded_Wide_String;
				Result: in out Unbounded_Wide_String);

	-- for compatibility
	procedure Str_Replace(	From, To: in Unbounded_Wide_String;
				Str: in out Unbounded_Wide_String ); 
	--function Str_Replace( From, To: in Unbounded_Wide_String;
	--			Str: in Unbounded_Wide_String ) return Unbounded_Wide_String; 

	procedure Str_Replace(	From, To: in Wide_String;
				Str: in out Unbounded_Wide_String ); 
	function Str_Replace(	From, To: in Wide_String;
				Str: in Unbounded_Wide_String ) return Unbounded_Wide_String; 


private

	function Find_Occurances( Find, Context : in Wide_String)
		return Positions_Vectors.Vector;
	-- uses the Knuth-Morris_Pratt Wide_String searching
	-- algorithm to find all occurances of Find in Context
	-- and returns an vector with the starting positions of each occurance

end Aw_Lib.Wide_String_Util;

