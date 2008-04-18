------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
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

------------------------------------------------------------------------------
-- This is the Aw_Lib.Libraries package                                     --
--                                                                          --
-- Implementation for library handling in windows.                          --
------------------------------------------------------------------------------


with Aw_Lib.File_System;	use Aw_Lib.File_System;
with Aw_Lib.String_Util;	use Aw_Lib.String_Util;
with Aw_Lib.UString_Vectors;	use Aw_Lib.UString_Vectors;

with Ada.Environment_Variables;
with Ada.Exceptions;		use Ada.Exceptions;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Interfaces.C;		use Interfaces.C;
with Interfaces.C.Strings;	use Interfaces.C.Strings;

with Ada.Text_IO;		use Ada.Text_IO;


with System;			use System;

package body Aw_Lib.Libraries is

	Not_Found : exception;
	
	--type IntFPtr is access function return Interfaces.C.Int;
	
	type CharPtr is access constant Interfaces.C.Char;

	function To_CharPtr is
	new Ada.Unchecked_Conversion (Source => System.Address,
		Target => CharPtr);
		
	--------------
	-- BINDINGS --
	--------------
	function GetProcAddress(	hModule   : System.Address;
					lpProcName: CharPtr	) return System.Address;
	pragma Import(Stdcall, GetProcAddress, "GetProcAddress"); 
	
	
	function LoadLibraryA(	lpLibFileName: CharPtr	) 
             return System.Address;
	pragma Import(Stdcall, LoadLibraryA, "LoadLibraryA");
	
	
	function FreeLibrary( hLibModule: System.Address ) return Interfaces.C.Int;
	pragma Import(Stdcall, FreeLibrary, "FreeLibrary");
	
	--------------------
	-- IMPLEMENTATIONS--
	--------------------

	function Is_Loaded(H: in Handler) return Boolean is
	begin
		return H.OS_Handler /= System.Null_Address;
	end Is_Loaded;
	
	
	function Load(Path: in string) return Handler is
		
	Name		: aliased String := Path & ASCII.Nul;
	Instance	: System.Address;
	H		: Handler;
	
	begin
		Instance := LoadLibraryA(To_CharPtr(Name'Address));

		if Instance = System.Null_Address then
			Raise_Exception (Not_Found'Identity, Path);
		end if;
		
		H.Os_Handler := Instance;
		
		return H;
	end Load;
	
	
	function Get_Symbol(	H: in Handler;
				Symbol: in String	) return Symbol_Type is
	
		Name	: aliased String := '_' & Symbol;
		Addr	: System.Address;
		S	: Symbol_Type;
	
	begin	
		Name (Symbol'Range) := Symbol;
		Name (Name'Last) := ASCII.Nul;
		
		Addr := GetProcAddress(H.OS_Handler, To_CharPtr(Name'Address));

		if Addr = System.Null_Address then
			Raise_Exception(	Library_Exception'Identity,
						"Can't get symbol " & Symbol	);
		end if;
		
		S := Convert( Addr );
		
		return S;
	end Get_Symbol;

	
	procedure Call(H: in out Handler; Symbol: in String) is		
					
		type Proc_Type is access procedure;
		
		function Convert is 
			new Ada.Unchecked_Conversion(	Source => System.Address,
							Target => Proc_Type);
		function Get_Proc is
			new Get_Symbol(	Symbol_Type	=> Proc_Type,
					Convert		=> Convert	);
		
		Proc: Proc_Type := Get_Proc(H, Symbol);
	begin
		Proc.all;
	end Call;
	
	
	procedure Unload(H: in out Handler) is
		-- unload the library.
		Status : Interfaces.C.Int;
	begin
		Status := FreeLibrary(H.Os_Handler);
		H.Os_Handler := System.Null_Address;
	exception 
		when Constraint_Error => 
			Raise_Exception (	Not_Found'Identity,
						"Error on Unload Library"	);
	end Unload;
	

	----------------------------------------------------
	-- NOW SOME FUNCTIONS FOR HANDLING DEFAULT VALUES --
	----------------------------------------------------
	
	function Library_Prefix return String is
	begin
		return "";
	end Library_Prefix;
	
	function Library_Suffix return String is
	begin
		return ".dll";
	end Library_Suffix;


	function Library_Path return Aw_Lib.UString_Vectors.Vector is
		use Ada.Environment_Variables;
		aos: constant String := "ADA_OBJECTS_PATH";
		P: Vector;
	begin
		if(Exists(aos)) then
			P := Explode(';', Value(aos));
		end if;
		
		Append(P, To_Unbounded_String(Value("SYSTEMROOT") & "\system32"));
		Append(P, To_Unbounded_String( Get_Working_Dir ) );

		return P;
	end Library_Path;

end Aw_Lib.Libraries;
