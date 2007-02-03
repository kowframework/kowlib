-- test of the ALOS.File_System package.
-- it's ment to be running on a unix environment!
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;


with ALOS.Env_Vars;
with ALOS.File_System;		use ALOS.File_System;
with ALOS.UString_Vectors;	use ALOS.UString_Vectors;




procedure FS_Tests is
   Path_String : Array(Natural range <>) of Unbounded_String := (To_Unbounded_String("~"),
                                                                 To_Unbounded_String("~/tmp/../../usr"),
                                                                 To_Unbounded_String("./../"),
                                                                 To_Unbounded_String("/usr/../etc/")
                                                                );


   Path: String := ALOS.Env_Vars.Value( "PATH" );




   procedure Vector_Iterator( C: in Cursor ) is
      -- iterator to test the To_Vector from File_System.
   begin
      Put_Line( "     " & To_String( Element( c ) ) );
   end Vector_Iterator;



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

   Put( "Does the file ''oopsy'' exist? " );
   if Is_File( "oopsy" ) then
	   Put_Line( "YES!" );
   else
	   Put_Line( "NO!" );
   end if;

   New_Line;


end FS_Tests;
