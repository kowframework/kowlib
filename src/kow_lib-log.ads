------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Library                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
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


------------------------------------------------------------------------------
-- This is the KOW_Lib.Log package                                          --
--                                                                          --
--  It has been conceived to avoid circular dependencies problems.          --
--  It's a simple Logging implementation meant to be used by the KOW Framwrk--
-- components (including KOW_Log) for debugging.                            --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Text_IO;


package KOW_Lib.Log is

	type Log_Level is (
			Level_Nul,		-- use the default level
			Level_Off,		-- disable the logging for this component
			Level_Error,		-- show error messages only
			Level_Warning,		-- show error and warning messages only
			Level_Info,		-- show error, warning and info messages only
			Level_Debug		-- show all kind of messages!
		);	

	for Log_Level use (
			Level_Nul	=> -1,
			Level_Off	=> 0,
			Level_Error	=> 1,
			Level_Warning	=> 7,
			Level_Info	=> 14,
			Level_Debug	=> 20
		);


	type Log_File_Type is not null access Ada.Text_IO.File_Type;
	-- set those output files to manage where the log messages are stored ::
	Info_Output	: Log_File_Type	:= new Ada.Text_IO.File_Type'( Ada.Text_IO.Standard_Output );
	Error_Output	: Log_File_Type := new Ada.Text_IO.File_Type'( Ada.Text_IO.Standard_Error );


	Default_Level: Log_Level := Level_Error;
	-- the default is to disable all the logging.


	type Logger_Type is private;
	-- a record type representing logger
	--
	-- the loggers are used to identify the component sending the message
	-- this way the developer can change logging level for his module only.


	function Get_Logger( Component: in String )
		return Logger_Type;
	-- return a logger for the desired component, using the default log level

	function Get_Logger( Component: in String; Level: in Log_Level )
		return Logger_Type;
	-- return a logger for the desired component, using the
	-- given log level

	procedure Log(	Logger	: in Logger_Type;
			Level	: in Log_Level;
			Message	: in String);
	-- logs a message in an active logger in the UTC

private
	type Logger_Type is record
		Component	: Unbounded_String;
		Level		: Log_Level := Level_nul;
		-- TODO :: provide means for the user to set the level for each logger
		-- this is gonna be usefull when debugging a given package only
	end record;
end KOW_Lib.Log;

