------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
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
-- This is the Fs_Tests                                                     --
--                                                                          --
-- test of the Aw_Lib.File_System package.                                  --     
-- it's ment to be running on a unix environment!                           --
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;

with Ada.Integer_Text_IO;	use Ada.Integer_text_IO;

with Ada.Environment_Variables;	use Ada.Environment_Variables;

with Aw_Lib.File_System;	use Aw_Lib.File_System;
with Aw_Lib.UString_Vectors;	use Aw_Lib.UString_Vectors;
with Interfaces.C;		use Interfaces.C;
with Interfaces.C.Strings;	use Interfaces.C.Strings;

procedure fs_tests is
	
	procedure Vector_Iterator( C: in Cursor ) is
	      -- iterator to test the To_Vector from File_System.
	begin
	      Put_Line( "     " & To_String( Element( c ) ) );
	end Vector_Iterator;

	Path_String : Array(Natural range <>) of Unbounded_String := (To_Unbounded_String("~"),
                                                                 To_Unbounded_String("~/tmp/../../usr"),
                                                                 To_Unbounded_String("./../"),
                                                                 To_Unbounded_String("/usr/../etc/"),
								 To_Unbounded_String("data")
                                                                );

	Path: String := Ada.Environment_Variables.Value( "PATH" );
	
	
	begin
		
	   New_Line;

	   Put_Line( "MY home directory is " & Get_Home );
	
	   New_Line;

	   Put_Line( "My Path currently is");
	   Put_Line( Path );
	   Put_Line( "Which after been split becomes" );
	   Iterate( To_Vector( Path ), Vector_Iterator'Access );

	   New_Line;

		for i in Path_String'Range loop
			Put( "   " & To_String( Path_String( i ) ) & "  =>  " );
			Put_Line( Get_Absolute_Path( To_String( Path_String( i ) ) ) );
		end loop;

	   New_Line;

	   Put_Line( "A local configuration file for FS_Tests: " & Get_Config_Dir( "FS_Tests" ) );
	   Put_Line( "A global configuration file for FS_Tests: " & Get_Global_Config_Dir( "FS_Tests" ) );

	   New_Line;

	   Put( "Does the file ''~/oopzy.lol'' exist? " );
	--   if Is_File( "~/oopzy.lol" ) then
	--	   Put_Line( "YES!" );
	--  else
	--	   Put_Line( "NO!" );
	--   end if;

	   New_Line;

	   Put("The name of the /lol/file.ads is: ");
	   Put_Line(Get_File_Name("/lol/file.ads"));

	   Put("The complete path for the dir of the file ~/oopzy.lol is ");
	   Put_Line(Get_Dir_Name("~/oopzy.lol"));

end fs_tests;
