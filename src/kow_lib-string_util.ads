------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
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
-- This is the KOW_Lib.String_Util package                                   --
--                                                                          --
-- Library to perform actions over Strings                                  --
------------------------------------------------------------------------------


with Ada.Strings;		use Ada.Strings;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Containers.Vectors;
with KOW_Lib.UString_Vectors;



package KOW_Lib.String_Util is

	type UString_Array is Array( Integer range <> ) of Unbounded_String;

	function implode(	Sep: Character;
				Vect: KOW_Lib.UString_Vectors.Vector	) return String;
	-- join all parts of the Vector into a String of 
	-- value element1[SEP]element2[SEP]ele...

	function implode(	Sep: Character;
				Vect: KOW_Lib.UString_Vectors.Vector	) return Unbounded_String;
	-- join all parts of the Vector into a Unbounded_String
	-- of value element1[SEP]element2[SEP]ele...

	function explode( Sep: Character; Str: String )	return KOW_Lib.UString_Vectors.Vector;
	-- split the string Str by Sep and
	-- return a vector containing it.

	function explode(	Sep: Character;
				Str: Unbounded_String ) return KOW_Lib.UString_Vectors.Vector;
	-- split the string Str by Sep and return a vector containing it.


	procedure Str_Replace( From, To: in Character; Str: in out String );
	-- replace all the ocurences of the character From by To.

	function Str_Replace( From, To: in Character; Str: in String ) return String;
	-- replace all the ocurences of the character From by To returning the new Value.


	package Positions_Vectors is new
		Ada.Containers.Vectors(	Element_Type 	=> Natural,
					Index_Type	=> Natural );



	-- TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO --
	-- TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO --
	-- TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO --
	-- -------------------BIG AND FAT WARNING ---------------  --
	-- TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO --
	-- TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO --
	-- TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO --
	
	-- The str_replace implementation assumes the string is from 1 .. 'length...
	-- which is a big and fat lie! no one can assure that and it's also undocumented in here....
	--
	-- change the implementation so it really works... DAMNIT!
	function Str_Replace(	From, To, Str: in String;
				Case_Sensitive: Boolean := True ) return Unbounded_String;
	
	procedure Str_Replace(	From, To, Str: in String;
				Result: in out Unbounded_String;
				Case_Sensitive: Boolean := True );


	function Str_Replace(	From, To, Str: in Unbounded_String; 
				Case_Sensitive: Boolean := True	) return Unbounded_String;
	
	procedure Str_Replace(	From, To, Str: in Unbounded_String;
				Result: in out Unbounded_String;
				Case_Sensitive: Boolean := True );

	-- for compatibility
	procedure Str_Replace(	From, To : in Unbounded_String;
				Str	 : in out Unbounded_String ); 
	--function Str_Replace( From, To: in Unbounded_String;
	--			Str: in Unbounded_String ) return Unbounded_String; 

	procedure Str_Replace(	From, To: in String;
				Str: in out Unbounded_String ); 
	
	function Str_Replace(	From, To: in String;
				Str: in Unbounded_String ) return Unbounded_String; 



	function Scriptify( Str : in String ) return String;
	-- replace \ by \\, " by \" and ' by \' for sending to external resources as a single string
private

	function Find_Occurances(	Find, Context : in String;
					Case_Sensitive: Boolean ) return Positions_Vectors.Vector;
	-- uses the Knuth-Morris_Pratt string searching algorithm to
	-- find all occurances of Find in Context and returns an vector
	-- with the starting positions of each occurance

end KOW_Lib.String_Util;

