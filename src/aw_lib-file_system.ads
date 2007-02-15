-- Header for system-dependent filesystem utilities
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate



with Aw_Lib.UString_Vectors;


package Aw_Lib.File_System is

   function Get_Home return Wide_String;
   -- return the location of user's home dir/my documents folder

   function Get_Config_Dir( App: in Wide_String ) return Wide_String;
   -- return a hidden folder where the user can store settings for the
   -- application called Application.


   function Get_Global_Config_Dir( App: in Wide_String := "" ) return Wide_String;
   -- return a directory where all the configuration global to all users
   -- of the application App (when set) should be stored.
   -- if App is not set, return the global configuration folder of the system


   function Get_Working_Dir return Wide_String;
   -- return the local working directory

   function Get_Absolute_Path( Original: in String ) return Wide_String;
   -- return the absolute path of the URL.
   -- system dependent implementation

   function To_Vector( SPath: in Wide_String ) return Aw_Lib.UWide_String_Vectors.Vector;
   -- explode the String SPath using ":" or ";" or the specific system separator
   -- returning an array with the absolute path of each directory.
   -- also add the current directory to the path


   function Is_File( File_Name: in Wide_String ) return Boolean;
   -- return true if file called File_Name exists

end Aw_Lib.File_System;
