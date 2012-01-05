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



with KOW_Lib.File_System;
with KOW_Lib.String_Util;	use KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;	use KOW_Lib.UString_Vectors;


with Ada.Environment_Variables;
with Ada.Exceptions;		use Ada.Exceptions;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Interfaces.C;		use Interfaces.C;
with Interfaces.C.Strings;	use Interfaces.C.Strings;

with System;			use System;

package body KOW_Lib.Libraries is


	function Dlopen (	Lib_Name : Interfaces.C.Strings.Chars_Ptr;
		 		Mode     : Interfaces.C.int) return System.Address;
	pragma Import(C, Dlopen, "dlopen");


	function Dlsym (	Handle   : System.Address;
				Sym_name : Interfaces.C.Strings.Chars_Ptr)
		return System.Address;
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
		C_Str	: Chars_Ptr := Interfaces.C.Strings.New_String(Path);
		H	: Handler;
	begin
		H.Os_Handler := Dlopen(
			Lib_Name => C_Str,
			Mode     => Interfaces.C.Int(2));
		-- RTDL_NOW mode for loading library
		
		Interfaces.C.Strings.Free(C_Str);
		
		if H.OS_Handler = System.Null_Address then
			Raise_Exception(Library_Exception'Identity,
					"Can't Load Library " &
					Path &	" [ " & Error & " ] ");
		end if;
		
		return H;
	end Load;
			
	function Get_Symbol(H: in Handler; Symbol: in String) return Symbol_Type is
		C_Str	: Chars_Ptr	 := Interfaces.C.Strings.New_String(Symbol);
		Addr	: System.Address := Dlsym(H.OS_Handler, C_Str);
		S	: Symbol_Type;
	begin
		if Addr = System.Null_Address then
			Raise_Exception(Library_Exception'Identity,
			"Can't get symbol " & symbol & " [ " & Error & " ] ");
		end if;

		S := Convert( Addr );
		
		return S;
	end Get_Symbol;


	procedure Call(H: in out Handler; Symbol: in String) is

		type Proc_Type is access procedure;

		function Convert is 
			new Ada.Unchecked_Conversion(	Source	=> System.Address,
							Target	=> Proc_Type	);
		
		function Get_Proc is 
			new Get_Symbol(	Symbol_Type	 => Proc_Type,
					Convert		 => Convert );

		Proc: Proc_Type := Get_Proc(H, Symbol);
	begin
		Proc.all;
	end Call;

	procedure Unload(H: in out Handler) is
		-- unload the library.
		i: Interfaces.C.int := Dlclose(H.OS_Handler);
	begin
		if i /= 0 then
			Raise_Exception(Library_Exception'Identity,
			"Unable to unload the library [ " & Error & " ] ");
		end if;

		H.OS_Handler := System.Null_Address;

	end Unload;


	function Is_Loaded(H: in Handler) return Boolean is
	begin
		return H.OS_Handler /= System.Null_Address;
	end Is_Loaded;



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


	function Library_Path return KOW_Lib.UString_Vectors.Vector is
		
		use Ada.Environment_Variables;
		
		aos: constant String := "ADA_OBJECTS_PATH";
		P: Vector;
	begin
		if(Exists(aos)) then
			P := Explode(':', Value(aos));
		end if;

		Append( P, To_Unbounded_String( "/usr/lib" ) );
		Append( P, To_Unbounded_String( "/usr/local/lib" ) );
		Append( P, To_Unbounded_String( KOW_Lib.File_System.Get_Working_Dir ) ); 

		return P;
	end Library_Path;

end KOW_Lib.Libraries;
