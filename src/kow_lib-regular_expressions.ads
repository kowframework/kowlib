------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Library                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWLib is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWLib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWLib; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Package with usefull utilities when handling Regular Expressions         --
------------------------------------------------------------------------------


with GNAT.Regpat;		use GNAT.Regpat;



with KOW_Lib.UString_Vectors;

package KOW_Lib.Regular_Expressions is



	type Replace_Method_Type is ( Group_Only, Entire_String );
	-- should the replace procedures return the entire string with
	-- the group replaced or only the replaced group?
	--
	-- if entire_string, replaces only if has found match; otherwise does not change 
	-- the string
	--
	-- if group_only, return the replace_subject with some atempt to fill in some info


	function Get_Match(
			Subject		: in String;
			Location	: in Match_Location
		) return String;
	-- get the value matched (if any). 
	-- if location = no_match return ""

	function Replace(
			Subject		: in String;
			Pattern		: in Pattern_Matcher;
			Replace_Subject	: in String;
			Replace_Method	: in Replace_Method_Type := Group_Only;
			Max_Groups	: in Match_Count := 5
		) return String;
	-- Replace using agiven matcher pattern and
	-- a subject to the replace procedure
	--
	-- The replace_subject is a string where \# are
	-- replaced by the #th group found (0 = the entire string) OR "" in case
	-- the groups is empty.


	function Replace( 
			Subject		: in String;
			Pattern		: in String;
			Replace_Subject	: in String;
			Replace_Method	: in Replace_Method_Type := Group_Only;
			Max_Groups	: in Match_Count := 5
		) return String;
	-- same as above, but pattern isn't compilled when it's comming in


	function Split(
			Subject		: in String;
			Pattern		: in Pattern_Matcher;
			Append_Null	: in Boolean := True
		) return KOW_Lib.UString_Vectors.Vector;
	-- splits the Subject in each occurrence of Pattern returning a
	-- vector of ustrings.
	--
	-- if pattern is not found, the first element of the
	-- vector  will be the entire subject string.
	--
	-- if append_null =  false then ignore null substrings
	-- 	as a result if the subject is empty the vector has _NO_ elements
	
	
	function Split(
			Subject		: in String;
			Pattern		: in String;
			Append_Null	: in Boolean := True
		) return KOW_Lib.UString_Vectors.Vector;
	-- same as above but with pattern isn't pre compiled


end KOW_Lib.Regular_Expressions;
