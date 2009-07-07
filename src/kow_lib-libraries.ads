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
-- This is the KOW_Lib.Libraries package                                     --
--                                                                          --
-- Header for system-dependent libraries (dll/so) utilities                 --
-- It doesn't initialize any compílation unit. It's just a simple           --
-- binding to system libraries such as dlopen or the win32 api.             --
------------------------------------------------------------------------------


with KOW_Lib.UString_Vectors;


with Ada.Unchecked_Conversion;
with System;

package KOW_Lib.Libraries is


	Library_Exception: Exception;
	
	type Handler is private;
	-- used to handle the library in the memory
	--
	-- this type is different in every system
	-- must be declared in the package body


	function Load(Path: in string) return Handler;
	-- load a library, returning a handler for it.
	-- the library is always loaded in RTLD_NOW mode if it's available.
	--
	-- this mode means (from man dlopen in gentoo linux)
	-- 	"If  this  value  is  specified,  or  the  environment
	-- 	variable LD_BIND_NOW is set to a non-empty string, all 
	-- 	undefined symbols in the library are resolved before 
	-- 	dlopen() returns.  If this cannot be done, an error is 
	-- 	returned."
	--
	-- this is, AFAIK, the only mode Microsoft Windows(TM) loads dll's.
	--
	-- Even though other systems support lazy bindings, we do not provide
	-- mechanisms to handle it.
	--
	-- One wanting to do that will have to extend this package (hence the
	-- opensource glory!)

	generic 
		type Symbol_Type is private;
		with function Convert( Addr: System.Address ) return Symbol_Type;
	function Get_Symbol(H: in Handler; Symbol: in String) return Symbol_Type;
	-- get a symbol that shall be returned.

	procedure Call(H: in out Handler; Symbol: in String);
	-- call a procedure with no parameters in the library.

	procedure Unload(H: in out Handler);
	-- unload the library.
	
	function Is_Loaded(H: in Handler) return Boolean;
	-- Check if this handler is in use

	----------------------------------------------------
	-- NOW SOME FUNCTIONS FOR HANDLING DEFAULT VALUES --
	----------------------------------------------------

	function Library_Prefix return String;
	-- Returns the default library preffix
	-- Example:
	-- 	in Linux => "lib"
	-- 	in Windows => "" (an empty string)


	function Library_Suffix return String;
	-- Returns the default library suffix
	-- Example:
	-- 	in Linux => ".so"
	-- 	in Windows => ".dll"


	function Library_Path return KOW_Lib.UString_Vectors.Vector;
	-- Return the vector representation for the default library
	-- search path for this system.
	--
	-- It should include the ADA_OBJECTS_PATH and the default 
	-- libraty path in the system, such as:
	-- 	in Linux: /usr/lib
	-- 	in Windows: c:\Windows


	private

	type Handler is record
		Os_Handler : System.Address := System.Null_Address;
	end record;
	-- used to handle the library in the memory
	--
	-- this type is different in every system
	-- must be declared in the package body

end KOW_Lib.Libraries;
