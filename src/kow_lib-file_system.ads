------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- KOW_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Lib; see file COPYING. If not, write --
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
-- This is the KOW_Lib.File_System package                                   --
--                                                                          --
-- Header for system-dependent filesystem utilities                         --
------------------------------------------------------------------------------

with KOW_Lib.UString_Vectors;


package KOW_Lib.File_System is


	function Separator return Character;
	-- return the directory separator used in this system
	
	function Get_Home return String;
	-- return the location of user's home dir/my documents folder

	function Get_Config_Dir( App: in String ) return String;
	-- return a hidden folder where the user can store settings for the
	-- application called Application.


	function Get_Global_Config_Dir( App: in String := "" ) return String;
	-- return a directory where all the configuration global to all users
	-- of the application App (when set) should be stored.
	-- if App is not set, return the global configuration folder of the system


	function Get_Working_Dir return String;
	-- return the local working directory

	function Get_Absolute_Path( Original: in String ) return String;
	-- return the absolute path of the URL.
	-- system dependent implementation
	
	function Get_File_Name( Path: in String ) return String;
	-- given the path, return the file name without the directory

	function Get_Dir_Name( Path: in String ) return String;
	-- given the complete path, return the path for the directory

	function To_Vector( SPath: in String ) return KOW_Lib.UString_Vectors.Vector;
	-- explode the String SPath using ":" or ";" or the specific system separator
	-- returning an array with the absolute path of each directory.
	-- also add the current directory to the path


	function To_Unix_Path( Path: in String ) return String;
	-- Convert the path using the system separator to the unix path

	function To_System_Path( Path: in String ) return String;
	-- Convert the unix path to the one using the system separator

end KOW_Lib.File_System;
