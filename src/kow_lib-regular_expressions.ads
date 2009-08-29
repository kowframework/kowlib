


with GNAT.Regpat;		use GNAT.Regpat;



with KOW_Lib.UString_Vectors;

package KOW_Lib.Regular_Expressions is


	Replace_Pattern	: constant Pattern_Matcher := Compile( "(\\\d+)" );




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
