-- Implementation for library handling in linux and other unixes.
--
-- It's based in the Util.Dll package from Michael Erdmann.
--
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2008-02-13
--
-- Repository information:
-- $Date: $
-- $Revision: $
-- $Author: $



with Aw_Lib.String_Util;	use Aw_Lib.String_Util;
with Aw_Lib.UString_Vectors;	use Aw_Lib.UString_Vectors;


with Ada.Environment_Variables;
with Ada.Exceptions;		use Ada.Exceptions;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Interfaces.C;		use Interfaces.C;
with Interfaces.C.Strings;	use Interfaces.C.Strings;

with System;			use System;

package body Aw_Lib.Libraries is


	



	function Dlopen (	Lib_Name : Interfaces.C.Strings.Chars_Ptr;
		 		Mode     : Interfaces.C.int) return System.Address;
	pragma Import(C, Dlopen, "dlopen");
	
	function Dlsym (Handle   : System.Address;
		Sym_name : Interfaces.C.Strings.Chars_Ptr) return System.Address;
	pragma Import(C, Dlsym, "dlsym");
	
	function Dlclose (Handle : System.Address) return Interfaces.C.int;
	pragma Import(C, Dlclose, "dlclose");

	function Dlerror return Interfaces.C.Strings.Chars_Ptr;
	pragma Import (C, Dlerror, "dlerror");


	function Error return String is
		C_Str: Chars_Ptr := Dlerror;
	begin
		if C_Str = Interfaces.C.Strings.Null_Ptr then
			return "";
		end if;

		return Interfaces.C.Strings.Value(C_Str);
	end Error;



	function Load(Path: in string) return Handler is
		-- load a library, returning it.
		C_Str		: Chars_Ptr := Interfaces.C.Strings.New_String(Path);
		H: Handler;
	begin
		H.Os_Handler := Dlopen(
			Lib_Name => C_Str,
			Mode     => Interfaces.C.Int(0));
		Interfaces.C.Strings.Free(C_Str);
		
		if H.OS_Handler = System.Null_Address then
			Raise_Exception(Library_Exception'Identity, "Can't Load Library " & Path & " [ " & Error & " ] ");
		end if;
		return H;
	end Load;
			
	procedure Call(H: in out Handler; Symbol: in String) is
		-- call a symbol in the library.i
		C_Str: Chars_Ptr := Interfaces.C.Strings.New_String(Symbol);
		Addr: System.Address := Dlsym(H.OS_Handler, C_Str);


		type Proc_Type is access procedure;
		function Conv is new Ada.Unchecked_Conversion(Source => System.Address, Target => Proc_Type);

		Proc: Proc_Type;
	begin
		if Addr = System.Null_Address then
			Raise_Exception(Library_Exception'Identity, "Can't call symbol " & symbol & " [ " & Error & " ] ");
		end if;


		Proc := Conv(Addr);

		Proc.all;
	end Call;

	procedure Unload(H: in out Handler) is
		-- unload the library.
		i: Interfaces.C.int := Dlclose(H.OS_Handler);
	begin
		if i /= 0 then
			Raise_Exception(Library_Exception'Identity, "Unable to unload the library [ " & Error & " ] ");
		end if;
	end Unload;

	----------------------------------------------------
	-- NOW SOME FUNCTIONS FOR HANDLING DEFAULT VALUES --
	----------------------------------------------------

	function Library_Prefix return String is
	begin
		return "lib";
	end Library_Prefix;
	
	function Library_Suffix return String is
	begin
		return ".so";
	end Library_Suffix;


	function Library_Path return Aw_Lib.UString_Vectors.Vector is
		use Ada.Environment_Variables;
		aos: constant String := "ADA_OBJECTS_PATH";
		P: Vector;
	begin
		if(Exists(aos)) then
			P := Explode(':', Value(aos));
		end if;

		Append(P, To_Unbounded_String("/usr/lib"));
		Append(P, To_Unbounded_String("/usr/local/lib"));

		return P;
	end Library_Path;

end Aw_Lib.Libraries;
