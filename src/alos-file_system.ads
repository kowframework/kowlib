-- Header for system-dependent filesystem utilities
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate



with ALOS.UString_Vectors;


package ALOS.File_System is

   function Get_Home return String;
   -- return the location of user's home dir/my documents folder

   function Get_Working_Dir return String;
   -- return the local working directory

   function Get_Absolute_Path( Original: in String ) return String;
   -- return the absolute path of the URL.
   -- system dependent implementation

   function To_Vector( SPath: in String ) return ALOS.UString_Vectors.Vector;
   -- explode the String SPath using ":" or ";" or the specific system separator
   -- returning an array with the absolute path of each directory.
   -- also add the current directory to the path

end ALOS.File_System;
