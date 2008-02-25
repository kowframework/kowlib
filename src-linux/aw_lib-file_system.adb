-- this is the linux implementation of system-dependent functions for Aw_Lib
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$



with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Ada.Environment_Variables;	use Ada.Environment_Variables;
with Aw_Lib.String_Util;	
with Aw_Lib.UString_Vectors;

package body Aw_Lib.File_System is


	function Separator return Character is
		Pragma Inline( Separator );
	begin
		return '/';
	end Separator;

	function Get_Home return String is
	-- return the location of user's home dir/my documents folder
	begin
		return Value( "HOME" );
	end Get_Home;

	function Get_Config_Dir( App: in String ) return String is
		-- return a hidden folder where the user can store settings for the
		-- application called Application.

		use Aw_Lib.String_Util;
	begin
		return  Get_Home & "/." & Str_Replace( '/', '-', App );
	end Get_Config_Dir;

	function Get_Global_Config_Dir( App: in String := "" ) return String is
		-- return a directory where all the configuration global to all users
		-- of the application App (when set) should be stored.
		-- if App is not set, return the global configuration folder of the system
		use Aw_Lib.String_Util;
	begin
		return "/etc/" & Str_Replace( '/', '-', App );
	end Get_Global_Config_Dir;




	function Get_Working_Dir return String is
		-- return the local working directory
	begin
		return Value( "PWD" );
	end Get_Working_Dir;


	function Get_Absolute_Path (Original : in String) return String is
		-- return the absolute path of the URL.
		-- system dependent implementation
		i : Integer := Original'First;

		function Process_Home (O : in String) return String is
			-- replace ~/ by it's home
			i : Integer := O'First;
		begin
			if O'Length < 1 or O (i) /= '~' then
				return O;
			else
				if O'Length = 1 then
					return Get_Home;
				elsif O (i + 1) = '/' then
					return Get_Home & O (i + 1 .. O'Last);
				end if;
			end if;
			return O;
		end Process_Home;

		function Process_Working_Dir( O: in String ) return String is
			-- replace ./ by local working directory
		begin
			if O'Length < 1 or O (i) /= '.' then
				return O;
			else
				if O'Length = 1 then
					return Get_Working_dir;
				elsif O (i + 1) = '/' then
					return Get_Working_Dir & O (i + 1 .. O'Last);
				end if;
			end if;
			return O;
		end Process_Working_Dir;


		function Process_Dots (O : in String) return String is
			-- process '..' signs removing one level from the directory hierarchy

			use Aw_Lib.UString_Vectors;
			use Aw_Lib.String_Util;
			use Ada.Strings.Unbounded;

			Vect: Vector := explode( '/', O );

			i: Natural := 1;

			function "=" (L: Unbounded_String; R: String ) return Boolean is
				-- compare an unbonded string with a bounded string
				-- returns true only if they are identical

				L_Index: Natural := 1;
			begin
				if Length( L ) /= R'Length then
					return false;
				end if;

				for R_Index in R'Range loop
					if Element( L, L_Index ) /= R(R_Index) then
						return false;
					end if;
					L_Index := L_Index + 1;
				end loop;

				return true;

			end "=";


		begin

			while i < Last_Index( Vect ) loop
				if Element( Vect, i ) = ".." then
					i := i - 1;
					Delete( Vect, i );
					Delete( Vect, i );
				else
					i := i + 1;
				end if;
			end loop;

			return implode( '/', Vect );
		end Process_Dots;

		Str: String := Process_Dots (Process_Working_Dir( Process_Home( Original ) ) );
	begin
		if Str'Length = 0  OR Str(Str'First) = '/' then
			return Str;
		end if;

		return Get_Working_Dir & '/' & Str;
	end Get_Absolute_Path;


	function Get_File_Name( Path: in String ) return String is
		-- given the path, return the file name without the directory
		
		use Aw_Lib.String_Util;
		use Aw_Lib.UString_Vectors;

		Expl: Aw_Lib.UString_Vectors.Vector := explode('/', Path);
	begin
		return To_String(Last_Element( Expl ));
	end Get_File_Name;

	function Get_Dir_Name( Path: in String ) return String is
		-- given the complete path, return the path for the directory
		
		use Aw_Lib.String_Util;
		use Aw_Lib.UString_Vectors;

		Expl: Aw_Lib.UString_Vectors.Vector := explode('/', Path);
	begin
		Delete_Last(Expl);
		return Get_Absolute_Path(implode('/', Expl));
	end Get_Dir_Name;




	function To_Vector (SPath : in String) return Aw_Lib.UString_Vectors.Vector is

		-- explode the String SPath using ":"
		use Aw_Lib.UString_Vectors;
		Vect : Vector := Aw_Lib.String_Util.explode (':', SPath);
	begin
		Append (Vect, To_Unbounded_String(Value ("PWD")));

		return Vect;
	end To_Vector;


end Aw_Lib.File_System;


