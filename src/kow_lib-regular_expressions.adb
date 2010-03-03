


with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;



package body KOW_Lib.Regular_Expressions is



	function Get_Match(
			Subject		: in String;
			Location	: in Match_Location
		) return String is
		-- get the value matched (if any). 
		-- if location = no_match return ""
		pragma Inline( Get_Match );
	begin
		if Location = No_Match then
			return "";
		else
			return Subject( Location.First .. Location.Last );
		end if;
	end Get_Match;

	function Replace(
			Subject		: in String;
			Pattern		: in Pattern_Matcher;
			Replace_Subject	: in String;
			Replace_Method	: in Replace_Method_Type := Group_Only;
			Max_Groups	: in Match_Count := 5
		) return String is
		-- Replace using agiven matcher pattern and
		-- a subject to the replace procedure
		--
		-- The replace_subject is a string where \# are
		-- replaced by the #th group found (0 = the entire string) OR "" in case
		-- the groups is empty.

		Matches		: Match_Array( 0 .. Max_Groups );

		Replace_Matches	: Match_Array( 0 .. Max_Groups );


		function Build_Response( Current_Index	: in Integer ) return String is
			The_Match	: Match_Location;
		begin


			if Current_Index > Replace_Subject'Last then
				return "";
			end if;

			Match(
					Self		=> Replace_Pattern,
					Data		=> Replace_Subject,
					Matches		=> Replace_Matches,
					Data_First	=> Current_Index
				);
			The_Match := Replace_Matches( 1 );

			if The_Match = No_Match then
				-- quando cai em No_Match é por que acabou o padrão
				-- de substituição..
				return Replace_Subject( Current_Index .. Replace_Subject'Last );
			else
				declare
					Subj		: String := Replace_Subject( Current_index .. ( The_Match.First - 1 ) );
					Index_Str	: String := Replace_Subject( The_Match.First + 1 .. The_Match.Last );
					Index		: Match_Count := Match_Count'Value( Index_Str );
					Local_Response	: String := Subj & Get_Match( Subject, Matches( Index ) );
				begin
					return Local_Response & Build_Response( The_Match.Last + 1 );
				end;
			end if;
		end Build_Response;
	begin
		Match(
				Self	=> Pattern,
				Data	=> Subject,
				Matches	=> Matches
			);
		if Replace_Method = Group_Only then
			return Build_Response( Current_Index => Replace_Subject'First );
		else
			if Matches(0) = No_Match then
				return Subject;
			else
				return  Subject( Subject'First .. Matches(0).First - 1 ) &
						Build_Response( Current_Index => Replace_Subject'First ) &
						Subject( Matches(0).Last + 1 .. Subject'Last );
			end if;
		end if;
	end Replace;


	function Replace( 
			Subject		: in String;
			Pattern		: in String;
			Replace_Subject	: in String;
			Replace_Method	: in Replace_Method_Type := Group_Only;
			Max_Groups	: in Match_Count := 5
		) return String is
		-- same as above, but pattern isn't compilled when it's comming in

		P_Matcher : Pattern_Matcher := Compile( Pattern );
	begin
		return Replace( Subject, P_Matcher, Replace_Subject, Replace_Method, Max_Groups );
	end Replace;




	function Split(
			Subject		: in String;
			Pattern		: in Pattern_Matcher;
			Append_Null	: in Boolean := True
		) return KOW_Lib.UString_Vectors.Vector is
		-- splits the Subject in each occurrence of Pattern returning a
		-- vector of ustrings.
		--
		-- if pattern is not found, the first element of the
		-- vector  will be the entire subject string.
		-- if append_null =  false then ignore null substrings
		-- 	as a result if the subject is empty the vector has _NO_ elements

		Result		: KOW_Lib.UString_Vectors.Vector;
		Current_Index	: Integer := Subject'First;
		Matches		: Match_Array( 0 .. 1 );
		The_Match	: Match_Location;
	begin
		loop
			Match(
					Self		=> Pattern,
					Data		=> Subject,
					Matches		=> Matches,
					Data_First	=> Current_Index
				);
			The_Match := Matches( 1 );
			-- we ignore the group #0 because when there is a match it's
			-- the entire string.. we do not want that at all!
			exit when The_Match = No_Match;

			if Append_Null OR ELSE Current_Index < The_Match.First then
				-- make sure we don't append null values if append_null isn't true
				KOW_Lib.UString_Vectors.Append(
							Result,
							Ada.Strings.Unbounded.To_Unbounded_String(
								Subject( Current_Index .. The_Match.First - 1 )
							)
						);
			end if;
			Current_Index := The_Match.Last + 1;
		end loop;

		if Append_Null OR ELSE Current_Index <= Subject'Last then
				-- make sure we don't append null values if append_null isn't true
			KOW_Lib.UString_Vectors.Append(
						Result,
						Ada.Strings.Unbounded.To_Unbounded_String(
							Subject( Current_Index .. Subject'Last )
						)
					);
		end if;


		return Result;
	exception
		when others =>
			raise CONSTRAINT_ERROR with "You can have only one group while using split function";
	end Split;




	function Split(
			Subject		: in String;
			Pattern		: in String;
			Append_Null	: in Boolean := True
		) return KOW_Lib.UString_Vectors.Vector is
		-- same as above but with pattern isn't pre compiled
	begin
		return Split( Subject, Compile( Pattern ), Append_Null );
	end Split;


end KOW_Lib.Regular_Expressions;
