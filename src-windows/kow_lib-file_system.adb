------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Library                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
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


with System; 			use System;
with Ada.unchecked_conversion;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO; 		use Ada.Text_IO;
with Ada.Environment_Variables;	use Ada.Environment_Variables;
with KOW_Lib.String_Util;	
with KOW_Lib.UString_Vectors;
with Interfaces.C;		use Interfaces.C;
with Interfaces.C.Strings;	use Interfaces.C.Strings;

package body KOW_Lib.File_System is

	-----------
	-- TYPES --
	-----------
	
	type CharPtr is access constant Interfaces.C.Char;
	
	--------------
	-- BINDINGS --
	--------------
	
	function GetCurrentDirectory(	nBufferLength	: Interfaces.C.Unsigned_Long;
					lpBuffer 	: Interfaces.C.Strings.Chars_Ptr	)
		return Interfaces.C.Unsigned_Long;
	pragma Import(Stdcall, GetCurrentDirectory, "GetCurrentDirectoryA");
	
	---------------------
	-- IMPLEMENTATIONS --
	---------------------
	
	function Separator return Character is
		Pragma Inline( Separator );
	begin
		return '\';
	end Separator;
	
	
	function Get_Home return String is
		-- return the location of user's home dir/my documents folder
	begin
		return Value("USERPROFILE");
	end Get_Home;

	
	function Get_Config_Dir( App: in String ) return String is
		-- return a hidden folder where the user can store settings for the
		-- application called Application.

		use KOW_Lib.String_Util;
	begin
		return Get_Home & Str_Replace( '/', '-', App);
	end Get_Config_Dir;

	function Get_Global_Config_Dir( App: in String := "" ) return String is
		-- return a directory where all the configuration global to all users
		-- of the application App (when set) should be stored.
		-- if App is not set, return the global configuration folder of the system
		use KOW_Lib.String_Util;
	begin
		return Value("APPDATA") & "\" & Str_Replace( '/', '-', App );
	end Get_Global_Config_Dir;


	function Get_Working_Dir return String is
		-- return the local working directory
		
		Ada_Buffer: String(1 .. 1000 ) := ( Others => ' ' );
		Buffer : Interfaces.C.Strings.Chars_Ptr := new_string(ada_buffer);		
			
		Result : Interfaces.C.Unsigned_Long;
	begin
		Result := GetCurrentDirectory(1000, Buffer);

		return Interfaces.C.Strings.Value(Buffer);
	end Get_Working_Dir;


	function Get_Absolute_Path (Original : in String) return String is
		-- return the absolute path of the URL.
		-- system dependent implementation
		i : Integer := Original'First;
		
		use KOW_Lib.String_Util;
		
		function Process_Home (O : in String) return String is
			-- replace ~/ by it's home
			i : Integer := O'First;
		begin
			if O'Length < 1 or O (i) /= '~' then
				return O;
			else
				if O'Length = 1 then
					return Get_Home;
				elsif O (i + 1) = '\' then
					return Get_Home & O (i + 1 .. O'Last);
				end if;
			end if;
			return O;
		end Process_Home;	
		

		function Process_Working_Dir( O: in String ) return String is
			-- replace .\ by local working directory
		begin
			if O'Length < 1 or O (i) /= '.' then
				return O;
			else
				if O'Length = 1 then
					return Get_Working_Dir;
				elsif O (i + 1) = '\' then
					return Get_Working_Dir &
						O (i + 1 .. O'Last);
				end if;
			end if;
			return O;
		end Process_Working_Dir;


		function Process_Dots (O : in String) return String is
			-- process '..' signs removing one level from the directory hierarchy

			use KOW_Lib.UString_Vectors;
			use KOW_Lib.String_Util;
			use Ada.Strings.Unbounded;

			Vect: Vector := explode( '\', O );

			i: Natural := 1;

			function "=" (L: Unbounded_String; R: String ) 
				return Boolean is
				-- compare an unbonded string with a bounded string
				-- returns true only if they are identical
				L_Index: Natural := 1;
			begin
				if Length( L ) /= R'Length then
					return false;
				end if;

				for R_Index in R'Range loop
					if Element( L, L_Index ) /= R(R_Index)
						then
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

			return implode( '\', Vect );
		end Process_Dots;

	begin
		return Process_Dots (Process_Working_Dir(Process_Home( 
			Str_Replace( '/', '\', Original ) )));
	end Get_Absolute_Path;


	function Get_File_Name( Path: in String ) return String is
		-- given the path, return the file name without the directory
		
		use KOW_Lib.String_Util;
		use KOW_Lib.UString_Vectors;

		Expl: KOW_Lib.UString_Vectors.Vector := 
			explode('\', Str_Replace( '/', '\', Path ));
	begin
		return To_String(Last_Element( Expl ));
	end Get_File_Name;

	function Get_Dir_Name( Path: in String ) return String is
		-- given the complete path, return the path for the directory
				
		use KOW_Lib.String_Util;
		use KOW_Lib.UString_Vectors;

		Expl: KOW_Lib.UString_Vectors.Vector :=
			explode('\', Str_Replace( '/', '\', Path ));
	begin
		Delete_Last(Expl);
		return Get_Absolute_Path(implode('\', Expl));
	end Get_Dir_Name;


	function To_Vector (SPath : in String)
		return KOW_Lib.UString_Vectors.Vector is

		-- explode the String SPath using ":"
		use KOW_Lib.UString_Vectors;
		Vect : Vector := KOW_Lib.String_Util.explode (';', SPath);
	begin
		--	Append (Vect, To_Unbounded_String(Get_Working_Dir));

		return Vect;
	end To_Vector;

	function To_Unix_Path( Path: in String ) return String is
		-- Convert the path using the system separator to the unix path
		Result: String := Path;
	begin
		KOW_Lib.String_Util.Str_Replace(
				From	=> '\',
				To	=> '/',
				Str	=> Result
				);
		return Result;
	end To_Unix_Path;

	function To_System_Path( Path: in String ) return String is
		-- Convert the unix path to the one using the system separator
		Result: String := Path;
	begin
		KOW_Lib.String_Util.Str_Replace(
				From	=> '/',
				To	=> '\',
				Str	=> Result
				);
		return Result;
	end To_System_Path;


	function "/"( L,R : in String ) return String is
		-- return L & Separator & R
	begin
		return L & '\' & R;
	end "/";

	procedure Copy_Directory(
				Source_Name	: in String;
				Target_Name	: in String;
				Form		: in String := ""
			) is
		use Ada.Directories;
		-- copy the directory and all it's files from Source_Name to Target_Name
		-- ignore special files

		Childs : UString_Vectors.Vector;

		procedure Process_All( D : in Directory_Entry_Type ) is
		begin
			case Kind( D ) is
				when Directory =>
					UString_Vectors.Append( Childs, To_Unbounded_String( Simple_Name( D  ) ) );
				when Ordinary_File =>
						Copy_File(
								Source_name	=> Full_Name( D ),
								Target_Name	=> Target_Name / Simple_Name( D ),
								Form		=> Form
							);
				when Special_File => null;
			end case;
		end Process_All;

		procedure Copy_Childs( C : in UString_Vectors.Cursor ) is
			SName : constant String := To_String( UString_Vectors.Element( C ) );
		begin
			if SName = "." or else SName = ".." then
				return;
			end if;
			Copy_Directory(
					Source_Name	=> Source_Name / SName,
					Target_Name	=> Target_Name / SName,
					Form		=> Form
				);
		end Copy_Childs;

	begin
		Create_Path(
				New_Directory	=> Target_Name,
				Form		=> Form
			);
		Search(
				Directory	=> Source_Name,
				Pattern		=> "",
				Process		=> Process_All'Access
			);

		UString_Vectors.Iterate( Childs, Copy_Childs'Access );
	end Copy_Directory;

end KOW_Lib.File_System;

