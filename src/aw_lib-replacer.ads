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
-- This is the Aw_Lib.Replacer package                                      --
--                                                                          --
-- Provides functions that help to create strings using curly brackets      --
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;	 use Ada.Strings.Unbounded;

package Aw_Lib.Replacer is

	-- Source Example: 
	-- "New York, $month day, $year"
	-- "Good morning, $name. "i'd like you to return $value
	-- you borrowed since ${m}/${d}/${y}"

	type Content is new Integer; -- TODO To define

	type Replace_Ammount is (Replace_All, Replace_First, Replace_Last);
	-- Replace all, the first or the last part;

	type Replace_Type is (Is_Word, Is_Anything);
	-- Full Word or Anything

	type Replace_Case is (Case_Sensitive, Case_Unsensitive);
	-- Case_Sensitive distinguishes words based on case,
	-- while Case_Unsensitive does not


	function Replace(	From, To, What	: Unbounded_String; 
				R_Ammount	: Replace_Ammount := Replace_All;
				R_Type		: Replace_Type := Is_Word;
				R_Case		: Replace_Case := Case_Sensitive	) 
		return Unbounded_String;
	-- Exchanges simple strings.



	procedure Get_Variable(	Source		: in	 Unbounded_String; 
				Beginning	: in	 Integer; 
				Position	: out	 Integer; 
				Size		: out	 Integer; 
				Value		: out	 Unbounded_String	);
	-- Finds the defined variables starting from the
	-- 'Beginning' position  (e.g. $var or ${var})



	function Replace_Variables (	Table	: in Content; 
					Source	: in Unbounded_String	) 
		return Unbounded_String;
	-- Exchanges the Source's variables for Table's variabels and
	-- returns the modified source as Unbounded_String;

	procedure Replace_Variables (	Table	: in Content;
					Text	: in out Unbounded_String	);
	-- Exchanges the Text's variables for the Tables's variables;

end Aw_Lib.Replacer;


