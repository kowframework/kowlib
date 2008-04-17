------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                 	--
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
-- This is the Aw_Lib.Replacer package                                      --
--                                                                          --
-- Provides functions that help to create strings using variables with      --
-- curly brackets                                                           --
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

package body Aw_Lib.Replacer is

	-- Source Example: 
	-- "New York, $month day, $year"
	-- "Good morning, $name. "i'd like you to return $value
	-- you borrowed since ${m}/${d}/${y}"


	function Replace(	From, To, What	: Unbounded_String; 
						R_Ammount		: Replace_Ammount := Replace_All;
						R_Type			: Replace_Type := Is_Word;
						R_Case			: Replace_Case := Case_Sensitive	)
		return Unbounded_String is
	-- Exchanges simple strings.
		
		Str: Unbounded_String := To_Unbounded_String("ola");
	
	begin
		-- TODO
		return Str;
	end Replace;



	procedure Get_Variable(		Source		: in	Unbounded_String; 
								Beginning	: in	Integer; 
								Position	: out 	Integer; 
								Size		: out 	Integer; 
								Value		: out	Unbounded_String	) is
	-- Finds the defined variables starting from the
	-- 'Beginning' position  (e.g. $var or ${var})
	
	begin
		Position := 20;
		Size 	 := 7;
		Value	 := To_Unbounded_String("d");
	end Get_Variable;



	function Replace_Variables (	Table	: in Content;
									Source	: in Unbounded_String	)
		return Unbounded_String is
	-- Exchanges the Source's variables for Table's variabels and
	-- returns the modified source as Unbounded_String;
		
		Str: Unbounded_String := Source;
	
	begin
		Replace_Variables( Table, Str );

		return Str;
	end Replace_Variables;

	procedure Replace_Variables (	Table	: in Content; 
									Text	: in out Unbounded_String	) is
	--Exchanges the Text's variables for the Tables's variables;
	
	begin
		Text := Text & Text;
	end Replace_Variables;


end Aw_Lib.Replacer;

