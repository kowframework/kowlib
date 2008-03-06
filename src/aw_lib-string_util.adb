-- Library to perform actions over Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$





with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


with Aw_Lib.UString_Vectors;


-- Used for string replacement
with GNAT.Spitbol.Patterns;



package body Aw_Lib.String_Util is

	function implode( Sep: Character; Vect: Aw_Lib.UString_Vectors.Vector ) return String is
		-- join all parts of the Vector into a String of value element1[SEP]element2[SEP]ele...
	begin
		return To_String( implode( Sep, Vect ) );
	end implode;


	function implode( Sep: Character; Vect: Aw_Lib.UString_Vectors.Vector ) return Unbounded_String is
		-- join all parts of the Vector into a Unbounded_String of value element1[SEP]element2[SEP]ele...

		use Aw_Lib.UString_Vectors;

		function implode_int ( c: Cursor ) return Unbounded_String is
			-- recursive function to implode string
		begin
			return Element( c ) & Sep & implode_int( Next( c ) );
		exception
			when CONSTRAINT_ERROR =>
				-- we've reached the end of the vector
				return Element( c );
		end implode_int;

	begin
		return implode_int( First( Vect ) );
	exception
		when CONSTRAINT_ERROR =>
			--there is nothing on this vector! :O
			return To_Unbounded_String( "" );
	end implode;


	function explode( Sep: Character; Str: String ) return Aw_Lib.UString_Vectors.Vector is
		-- split the string Str by Sep and return a vector containing it.
	begin
		return explode( Sep, To_Unbounded_String( Str ) );
	end explode;


	function explode( Sep: Character; Str: Unbounded_String ) return Aw_Lib.UString_Vectors.Vector is
		-- split the string Str by Sep and return a vector containing it.
		use Aw_Lib.UString_Vectors;
		Vect: Vector;
		ini, fim: Natural := 1;
	begin


		while fim <= Length( Str )
		loop
			if Element( Str, fim ) = Sep  then
				if ini = fim then
					Append( Vect, To_Unbounded_String( "" ) );
					else
						Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
				end if;
				ini := fim + 1;
			end if;
			fim := fim + 1;
		end loop;

		-- now we add the last element, it doesn't matter if it's null or not:
		if ini = fim then
			-- it's a null element
			Append( Vect, To_Unbounded_String( "" ) );
		else
			Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
		end if;

		return Vect;

	end explode;


	procedure Str_Replace( From, To: in Character; Str: in out String ) is
		-- replace all the ocurences of the character From by To.
	begin
		for i in Str'Range loop
			if Str(i) = From then
				Str(i) := To;
			end if;
		end loop;
	end Str_Replace;

	function Str_Replace( From, To: in Character; Str: in String ) return String is
		-- replace all the ocurences of the character From by To returning the new Value.
		R: String := Str;
	begin
		Str_Replace( From, To, R );
		return R;
	end Str_Replace;


	procedure Str_Replace( From, To: in Unbounded_String; Str: in out Unbounded_String ) is
		use GNAT.Spitbol.Patterns;
		From2 : constant PString := To_String( From );
	begin
		Match( Str, From2, To );
	end Str_Replace;
	
	procedure Str_Replace( From, To: in String; Str: in out Unbounded_String ) is
	begin
		Str_Replace( To_Unbounded_String( From ), To_Unbounded_String( To ), Str );
	end Str_Replace;

	function Str_Replace( From, To: in Unbounded_String; Str: in Unbounded_String ) return Unbounded_String is
		use GNAT.Spitbol.Patterns;
		Replaced : Unbounded_String;
		From2 : constant PString := To_String( From );
	begin
		Replaced := Str;
		Match( Replaced, From2, To );
		return Replaced;
	end Str_Replace;

	
	function Str_Replace( From, To: in String; Str: in Unbounded_String ) return Unbounded_String is
	begin
		return Str_Replace( To_Unbounded_String( From ), To_Unbounded_String( To ), Str );
	end Str_Replace;



end Aw_Lib.String_Util;

