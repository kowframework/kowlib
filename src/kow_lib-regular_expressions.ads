


with GNAT.Regpat;		use GNAT.Regpat;


package KOW_Lib.Regular_Expressions is


	Replace_Pattern	: constant Pattern_Matcher := Compile( "(\\\d+)" );


	function Get_Match(
			Subject		: in String;
			Location	: in Match_Location
		) return String;

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

end KOW_Lib.Regular_Expressions;
