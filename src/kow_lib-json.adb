

with ada.text_io;		use ada.text_io;

--------------
-- Ada 2005 --
--------------
with Ada.Characters.Latin_1;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.String_Util;

package body KOW_Lib.Json is

	--------------------
	-- Helper Methods --
	--------------------

	procedure Inc( Map : in out Json_Data_Maps.Map; Key : in String; Value : in Json_Data_Type ) is
	begin
		Json_Data_Maps.Include( Map, To_Unbounded_String( Key ), Value );
	end Inc;

	
	procedure Check( Expected : Json_Object_Type; Received : Json_Object_Type ) is
	begin
		if Expected /= Json_String then
			raise CONSTRAINT_ERROR with
				"expected " & Json_Object_Type'Image( Expected ) & 
				" but received " & Json_object_Type'Image( Received );
		end if;
	end Check;


	function is_space( ch: in character ) return boolean is
	begin
		return 	Ch = ' ' 
			OR Ch = Ada.Characters.Latin_1.HT	-- tab
			OR Ch = Ada.Characters.Latin_1.LF	-- line feed
			OR Ch = Ada.Characters.Latin_1.CR;	-- return
	end is_space;



	procedure Jump_spaces( Str : in String; Char_index : in out Positive ) is
	begin
		while is_space( Str( Char_Index ) ) loop
			Char_index := Char_index + 1;
		end loop;
	end Jump_Spaces;


	---------------------
	-- The Object Type --
	---------------------


	-- 
	-- Setters
	--

	procedure Set( Object : in out Object_Type; Key : in String; Value : in String ) is
	begin
		Set( Object, To_Unbounded_String( Key ), To_Unbounded_String( Value ) );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in String; Value : in Unbounded_String ) is
	begin
		Set( Object, To_Unbounded_String( Key ), Value );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in Unbounded_String; Value : in Unbounded_String ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_String;
		Data.Str := Value;
		Json_Data_Maps.Include( Object.Data, Key, Data );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in String; Value : in Object_Type ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_Object;
		Data.Object.all := Value;
		Inc( Object.Data, Key, Data );
	end Set;

	--
	-- Getters
	--
	function Get( Object : in Object_Type; Key : in String ) return String is
	begin
		return To_String( Get( Object, To_Unbounded_String( Key ) ) );
	end Get;

	function Get( Object : in Object_Type; Key : in String ) return Unbounded_String is
	begin
		return Get( Object, To_Unbounded_String( Key ) );
	end Get;

	function Get( Object : in Object_Type; Key : in Unbounded_String ) return Unbounded_String is
		Data : Json_Data_Type := Json_Data_Maps.Element( Object.Data, Key );
	begin
		Check( Data.The_Type, Json_String );
		return Data.Str;
	end Get;

	function Get( Object : in Object_Type; Key : in String ) return Object_Type is
		Data : Json_Data_Type := Json_Data_Maps.Element( Object.Data, To_Unbounded_String( Key ) );
	begin
		Check( Data.The_Type, Json_object );
		return Data.Object.all;
	end Get;

	function Get_Type( Object : in Object_Type; Key : in String ) return Json_Object_Type is
		Data : Json_Data_type := Json_Data_maps.Element( Object.Data, To_Unbounded_String( Key ) );
	begin
		return Data.The_Type;
	end Get_Type;


	function From_Json( Str : in String ) return Object_Type is
		Object		: Object_Type;
		Char_Index	: Positive := 1;
	begin
		From_Json( Str, Char_Index, Object );
		return Object;
	end From_Json;



	procedure read_key( str : in string; char_index : in out positive; key : in out unbounded_String ) is
		use_quotation	: boolean := false;
		quotation	: character;
		from		: positive;
		to		: positive := char_index;
	begin
		if str( to ) = ''' OR str( to ) = '"' then
			use_quotation := true;
			quotation := str( to );
			to := to + 1;
			from := to;
		else
			-- look for the string start..
			jump_spaces( str, to );
			from := to;
		end if;

		loop
			exit when use_quotation and then str( to ) = quotation;
			exit when not use_quotation and then is_space( str( to ) );
			exit when not use_quotation and then str( to ) = ':';
			to := to + 1;
		end loop;

		Key := To_Unbounded_String( Str( From .. To - 1 ) );
		if str(to) = quotation then
			Char_Index := To + 1;
		else
			Char_Index := To;
		end if;
	exception
		when CONSTRAINT_ERROR =>
			raise SYNTAX_ERROR with "json string prematurelly ended";
	end read_key;

	procedure From_Json(
				Str		: in     String;
				Char_Index	: in out Positive;
				Object		:    out object_type
			) is
		O 		: Object_Type;
		Last_Key	: Unbounded_String;

		type Read_Type is ( Read_Key, Read_Value );
		Should_Read : Read_Type := Read_Key;
	begin
		loop
			Jump_Spaces( Str, Char_Index );
			case Str( Char_Index ) is
				when '}' =>
					Char_Index	:= Char_Index + 1;
					Object		:= O;
					return;

				when '{' | ',' =>
					if Should_Read /= Read_Key then
						raise SYNTAX_ERROR with "should be ready to read a value and I found a key at " & Positive'Image( char_index );
					end if;
					Char_index := Char_Index + 1;
					Read_Key( Str, Char_Index, Last_Key );
					Should_Read := Read_Value;
				when ':' =>
					if Should_Read /= Read_Value then
						raise SYNTAX_ERROR with "should be ready to read a key and I found a value at " & positive'Image( char_index );
					end if;
					if Str( Char_Index ) = ':' then
						Char_Index := Char_Index + 1;
					end if;
					Jump_Spaces( Str, Char_Index );
					declare
						Data : Json_Data_Type;
					begin
						From_Json( Str, Char_Index, Data );
						Json_Data_Maps.Include( O.Data, Last_Key, Data );
					end;
					Should_Read := Read_Key;
				when others =>
					raise SYNTAX_ERROR with "(object_type) unexpected character '" & str(Char_Index) & "' at " & positive'image( char_index );
			end case;
		end loop;
	end From_Json;


	function To_Json( Object : in Object_Type ) return String is
		Buf		: Unbounded_String;
		is_first	: boolean	:= true;

		procedure Iterator( C : Json_Data_Maps.Cursor ) is
		begin
			if is_first then
				is_first := false;
			else
				Append( Buf, ',' );
			end if;
			Append( Buf, String( ''' & KOW_Lib.String_Util.Json_Scriptify( To_String( Json_Data_Maps.Key( C ) ) ) & "':" ) );
			Append( Buf, To_Json( Json_Data_Maps.Element( C ) ) );
		end Iterator;
	begin
		Append( Buf, '{' );
		Json_Data_Maps.Iterate( Object.Data, Iterator'Access );
		Append( Buf, '}' );

		return To_String( Buf );
	end To_Json;



	--------------------
	-- The Array Type --
	--------------------


	--
	-- Replacers
	--

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in String ) is
	begin
		Replace( A, Index, To_Unbounded_String( Value ) );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Unbounded_String ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_String;
		Data.Str := Value;
		Json_Data_Vectors.Replace_Element( A.Data, Index, Data );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Object_Type ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_Object;
		Data.Object.all := Value;
		Json_Data_Vectors.Replace_Element( A.Data, Index, Data );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Array_Type ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_Array;
		Data.Vector.all := Value;
		Json_Data_Vectors.Replace_Element( A.Data, Index, Data );
	end Replace;

	-- 
	-- Appenders
	--

	procedure Append( A : in out Array_Type; Value : in String ) is
	begin
		Append( A, To_Unbounded_String( Value ) );
	end Append;

	procedure Append( A : in out Array_Type; Value : in Unbounded_String ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_String;
		Data.Str := Value;
		Json_Data_Vectors.Append( A.Data, Data );
	end Append;

	procedure Append( A : in out Array_Type; Value : in Object_Type ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_Object;
		Data.Object.all := Value;
		Json_Data_Vectors.Append( A.Data, Data );
	end Append;

	procedure Append( A : in out Array_Type; Value : in Array_Type ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_Array;
		Data.Vector.all := Value;
		Json_Data_Vectors.Append( A.Data, Data );
	end Append;

	
	function Get( A : in Array_Type; Index : in Natural ) return String is
	begin
		return To_String( Get( A, Index ) );
	end Get;

	function Get( A : in Array_Type; Index : in Natural ) return Unbounded_String is
		Data : Json_Data_Type := Json_Data_Vectors.Element( A.Data, Index );
	begin
		return Data.Str;
	end Get;
		
	function Get( A : in Array_Type; Index : in Natural ) return Object_Type is
		Data : Json_Data_Type := Json_Data_Vectors.Element( A.Data, Index );
	begin
		Check( Data.The_Type, Json_Object );
		return Data.Object.all;
	end Get;

	function Get( A : in Array_Type; Index : in Natural ) return Array_Type is
		Data : Json_Data_Type := Json_Data_Vectors.Element( A.Data, Index );
	begin
		Check( Data.The_Type, Json_Array );
		return Data.Vector.all;
	end Get;



	function Get_Type( A : in Array_Type; Index: in Natural ) return Json_Object_Type is
		Data : Json_Data_Type := Json_Data_Vectors.Element( A.Data, Index );
	begin
		return Data.The_Type;
	end Get_Type;




	function From_Json( Str : in String ) return Array_Type is
		Char_index : positive := 1;
		A : Array_Type;
	begin
		From_Json( Str, Char_Index, A );
		return A;
	end From_Json;

	procedure From_Json(
				Str		: in     String;
				Char_Index	: in out Positive;
				A		:    out Array_Type
			) is
		The_A : Array_Type;
	begin
		loop
			Jump_Spaces( Str, Char_Index );
			case Str( Char_Index ) is
				when ']' =>
					Char_index	:= Char_Index + 1;
					A		:= The_A;
					return;
				when '[' | ',' =>
					Char_Index := Char_Index + 1;
					Jump_Spaces( Str, Char_Index );
					declare
						Data : Json_Data_Type;
					begin
						From_Json( Str, Char_Index, Data );
						Json_Data_Vectors.Append( The_A.Data, Data );
					end;
				when others =>
					raise SYNTAX_ERROR with "(array_type) unexpected character '" & str(Char_Index) & "' at " & positive'image( char_index );
			end case;
		end loop;
	end From_Json;


	function To_Json( A : in Array_type ) return String is
		Is_First	: Boolean	:= True;
		Buf		: Unbounded_String;

		procedure Iterator( C : Json_Data_Vectors.Cursor ) is
		begin
			if Is_First then
				Is_First := False;
			else
				Append( Buf, ',' );
			end if;
			Append( Buf, To_Json( Json_Data_Vectors.Element( C ) ) );
		end Iterator;
	begin
		Append( Buf, '[' );
		Json_Data_Vectors.Iterate( A.Data, Iterator'Access );
		Append( Buf, ']' );

		return To_String( Buf );
	end To_Json;




	-- Missing methods for object_Type
	procedure Set( Object : in out Object_Type; Key : in String; Value : in Array_Type ) is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_Array;
		Data.Vector.all := Value;
		Inc( Object.Data, Key, Data );
	end Set;

	function Get( Object : in Object_Type; Key : in String ) return Array_Type is
		Data : Json_Data_Type := Json_Data_Maps.Element( Object.Data, To_Unbounded_String( Key ) );
	begin
		Check( Data.The_Type, Json_Array );
		return Data.Vector.all;
	end Get;




-- private


	--------------------
	-- Json Data Type --
	--------------------
	
	overriding
	procedure Initialize( Object : in out Json_Data_Type ) is
	begin
		Object.Object := new Object_Type;
		Object.Vector := new Array_Type;
	end Initialize;

	overriding
	procedure Adjust( Object : in out Json_Data_Type ) is
		new_object	: object_ptr := new object_type'( object.object.all );
		new_vector	: array_ptr := new array_type'( object.vector.all );
	begin
		Object.Object	:= New_Object;
		Object.Vector	:= New_Vector;
	end Adjust;
		

	overriding
	procedure Finalize( Object : in out Json_Data_Type ) is
	begin
		Free( Object.Object );
		Free( Object.Vector );
	end Finalize;


	function From_Json( Str : in String ) return Json_Data_Type is
		Data		: Json_Data_Type;
		Char_Index	: Positive := 1;
	begin
		From_Json(
				Str		=> Str,
				Char_Index	=> Char_Index,
				Data		=> Data
			);
		return Data;
	end From_Json;


	procedure From_Json(
				Str		: in     String;
				Char_Index	: in out Positive;
				Data		:    out Json_Data_Type
			) is
		D	: Json_Data_Type;
	begin
		loop
			Jump_Spaces( Str, Char_Index );
			case Str( Char_Index ) is
				when ''' | '"' =>
					declare
						Last_Quotation	: constant Character := Str( Char_Index );
						From		: Positive := Char_Index + 1;
						To		: Positive := Char_Index + 1;
					begin
						loop
							if To > Str'Last then
								raise SYNTAX_ERROR with "string ended prematurely";
							end if;
							if Str( to ) = Last_Quotation  then
								D.The_Type := Json_String;
								if To - 1 >= From then
									D.Str := To_Unbounded_String( Str( From .. To - 1 ) );
								else
									D.Str := Null_Unbounded_String;
								end if;
								Char_Index := To + 1;
								Data := D;
								return;
							elsif Str( to ) = '\' then
								To := To + 2;
							else
								To := To + 1;
							end if;
						end loop;
					end;
				when '[' =>
					declare
						A : Array_Type;
					begin
						From_Json( Str, Char_Index, A );
						D.The_Type := Json_Array;
						D.Vector.all := A;
						Data := D;
						return;
					end;
				when '{' =>
					declare
						O : Object_Type;
					begin
						From_Json( Str, Char_Index, O );
						D.The_Type := Json_Object;
						D.Object.all := O;
						Data := D;
						return;
					end;
				when others =>
					raise SYNTAX_ERROR with "(json_data_type) unexpected character '" & str(Char_Index) & "' at " & positive'image( char_index );
			end case;
		end loop;
	end From_Json;
	
	

	function To_Json( Data : in Json_Data_Type ) return String is
	begin
		case Data.The_Type is
			when Json_String =>
				return ''' & KOW_Lib.String_Util.Json_Scriptify( To_String( Data.Str ) ) & ''';
			when Json_Array =>
				return To_Json( Data.Vector.All );
			when Json_Object =>
				return To_Json( Data.Object.All );
		end case;
	end To_Json;


end KOW_Lib.Json;
