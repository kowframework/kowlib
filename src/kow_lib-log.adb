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



--------------
-- Ada 2005 --
--------------
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;

package body KOW_Lib.Log is


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


			function Now_Str return String is
				pragma Inline( Now_Str );
			begin
				return '[' &
						Ada.Calendar.Formatting.Image(
								Date			=> Ada.Calendar.Clock,
								Include_Time_Fraction	=> True,
								Time_Zone		=> 0	-- the log is in the UTC
							) &
						']';
			end Now_Str;

			Log_String: String := Now_Str &
						" [" & Log_Level'Image( Level ) &
						" @ " & To_String( Logger.Component ) & "] " &
						Message;
		begin
			Put( Output, Log_String );
			New_Line( Output );
			Flush( Output );
		end Inner_Log;


		Use_Level : Log_Level := Logger.Level;
	begin
		if Use_Level = Level_Nul then
			Use_level := Default_Level;
		end if;
		if Level > Use_Level then
			-- checks if the message has to be logged
			return;
		end if;

		case Level is
			when Level_Info | Level_Debug =>
				Inner_Log( Info_Output.all );
			when Level_Error | Level_Warning =>
				Inner_Log( Error_Output.all );
			when Others =>
				null; -- does nothing if the user tries to log NUl or off
		end case;
	end Log;

end KOW_Lib.Log;

