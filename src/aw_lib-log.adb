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



with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;


package body Aw_Lib.Log is


	function Get_Logger( Component: in String )
		return Logger_Type is
		-- return a logger for the desired component, using the default log level
		Logger: Logger_Type;
	begin
		Logger.Component := To_Unbounded_String( Component );
		return Logger;
	end Get_Logger;


	function Get_Logger( Component: in String; Level: in Log_Level )
		return Logger_Type is
		-- return a logger for the desired component, using the
		-- given log level
		Logger: Logger_Type := Get_Logger( Component );
	begin
		if Level /= Level_Nul then
			Logger.Level := Level;
		end if;
		return Logger;
	end Get_Logger;


	procedure Log(	Logger	: in Logger_Type;
			Level	: in Log_Level;
			Message	: in String) is
		-- logs a message in an active logger


		procedure Inner_Log( Output: in File_Type ) is
			-- this procedure is called only when the message
			-- has to be logged.
			Log_String: String :=
				"[" & Log_Level'Image( Level ) &
				"@" & To_String( Logger.Component ) & "] " &
				Message;
		begin
			Put( Output, Log_String );
			New_Line;
		end Inner_Log;
	begin
		if Level > Logger.Level then
			-- checks if the message has to be logged
			return;
		end if;

		case Level is
			when Level_Info | Level_Debug =>
				Inner_Log( Info_Output );
			when Level_Error | Level_Warning =>
				Inner_Log( Error_Output );
			when Others =>
				null; -- does nothing if the user tries to log NUl or off
		end case;
	end Log;

end Aw_Lib.Log;

