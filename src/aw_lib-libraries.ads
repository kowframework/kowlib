-- Header for system-dependent libraries (dll/so) utilities
-- It doesn't initialize any compílation unit. It's just a simple
-- binding to system libraries such as dlopen or the win32 api.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2008-02-13
--
-- Repository information:
-- $Date: $
-- $Revision: $
-- $Author: $



with Aw_Lib.UString_Vectors;


with Ada.Unchecked_Conversion;
with System;

package Aw_Lib.Libraries is


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


	function Library_Path return Aw_Lib.UString_Vectors.Vector;
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

end Aw_Lib.Libraries;
