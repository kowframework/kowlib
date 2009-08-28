




package body KOW_Lib.Regular_Expressions is



	function Get_Match(
			Subject		: in String;
			Location	: in Match_Location
		) return String is
		-- retorna "" se Location = No_Match
		-- retorna o valor do grupo encontrado caso contrário
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
			Max_Groups	: in Match_Count := 5
		) return String is
		-- função de substituição
		-- retorna Replace_patern sunstituindo
		-- 	\n
		-- pelo n'o grupo enconrado em subject usando
		-- pattern.
		--
		-- se não existir o n'o grupo, substitui por
		-- vazio

		Matches		: Match_Array( 0 .. Max_Groups );
		-- possibilitamos ter 10 grupos por regexp..
		-- 	0 => expressão inteira
		-- 	1 em diante => grupos encontrados

		Replace_Matches	: Match_Array( 0 .. Max_Groups );
		The_Match	: Match_Location;


		function Build_Response( Current_Index	: in Integer ) return String is
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
		return Build_Response( Current_Index	=> Replace_Subject'First );
	end Replace;


	function Replace( 
			Subject		: in String;
			Pattern		: in String;
			Replace_Subject	: in String;
			Max_Groups	: in Match_Count := 5
		) return String is
		-- função clássica de substituição 

		P_Matcher : Pattern_Matcher := Compile( Pattern );
	begin
		return Replace( Subject, P_Matcher, Replace_Subject, Max_Groups );
	end Replace;

end KOW_Lib.Regular_Expressions;
