

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

	procedure Check( Expected : Json_Object_Type; Received : Json_Object_Type ) is
	begin
		if Expected /= Received then
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


	--------------------
	-- Json Data Type --
	--------------------
	
	-- int
	function To_Data( Value : in Integer ) return Json_Data_Type is
		D : Json_Data_Type;
	begin
		D.The_Type := Json_Integer;
		D.Int := Value;
		return D;
	end To_Data;
	
	function From_Data( Data : in Json_Data_Type ) return Integer is
	begin
		Check( Json_Integer, Data.The_Type );
		return Data.Int;
	end From_Data;

	-- string	
	function To_Data( Value : in String ) return Json_Data_Type is
	begin
		return To_Data( To_Unbounded_String( Value ) );
	end To_Data;

	function From_Data( Data : in Json_Data_Type ) return String is
	begin
		return To_String( From_Data( Data ) );
	end From_Data;

	-- unbounded string
	function To_Data( Value : in Unbounded_String ) return Json_Data_Type is
		D : Json_Data_Type;
	begin
		D.The_Type := Json_String;
		D.Str := Value;
		return D;
	end To_Data;

	function From_Data( Data : in Json_Data_Type ) return Unbounded_String is
	begin
		Check( Data.The_Type, Json_String );
		return Data.Str;
	end From_Data;


	---------------------
	-- The Object Type --
	---------------------


	function To_Data( Value : in Object_Type ) return Json_Data_Type is
		D : Json_Data_Type;
	begin
		D.The_Type := Json_Object;
		D.Object.all := Value;
		return D;
	end To_Data;

	function From_Data( Data : in Json_Data_Type ) return Object_Type is
	begin
		Check( Json_Object, Data.The_type );
		return Data.Object.all;
	end From_Data;

	-- 
	-- Setters
	--

	procedure Set( Object : in out Object_Type; Key : in String; Value : in Json_Data_Type ) is
	begin
		Set( Object, To_Unbounded_String( Key ), Value );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in Unbounded_String; Value : in Json_Data_Type ) is
	begin
		Json_Data_Maps.Include( Object.Data, Key, Value );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in String; Value : in Integer ) is
	begin
		Set( Object, Key, To_Data( Value ) );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in String; Value : in String ) is
	begin
		Set( Object, Key, To_Data( Value ) );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in String; Value : in Unbounded_String ) is
	begin
		Set( Object, Key, To_Data( Value ) );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in Unbounded_String; Value : in Unbounded_String ) is
	begin
		Set( Object, Key, To_Data( Value ) );
	end Set;

	procedure Set( Object : in out Object_Type; Key : in String; Value : in Object_Type ) is
	begin
		Set( Object, Key, To_Data( Value ) );
	end Set;

	--
	-- Getters
	--


	function Get( Object : in Object_Type; Key : in String ) return Json_Data_Type is
	begin
		return Get( Object, To_Unbounded_String( key ) );
	end Get;

	function Get( Object : in Object_Type; Key : in Unbounded_String ) return Json_Data_Type is
	begin
		return Json_Data_Maps.Element( Object.Data, Key );
	end Get;

	function Get( Object : in Object_Type; Key : in String ) return integer is
	begin
		return From_Data( Get( Object, Key ) );
	end Get;

	function Get( Object : in Object_Type; Key : in String ) return String is
	begin
		return From_Data( Get( Object, Key ) );
	end Get;

	function Get( Object : in Object_Type; Key : in String ) return Unbounded_String is
	begin
		return From_Data( Get( Object, Key ) );
	end Get;

	function Get( Object : in Object_Type; Key : in Unbounded_String ) return Unbounded_String is
	begin
		return From_Data( Get( Object, Key ) );
	end Get;

	function Get( Object : in Object_Type; Key : in String ) return Object_Type is
	begin
		return From_Data( Get( Object, Key ) );
	end Get;


	function Contains( Object : in Object_Type; Key : in String ) return Boolean is
	begin
		return Contains( Object, To_Unbounded_String( Key ) );
	end Contains;

	function Contains( Object : in Object_Type; Key : in Unbounded_String ) return Boolean is
	begin
		return Json_Data_maps.Contains( Object.Data, Key );
	end Contains;

	function Get_Type( Object : in Object_Type; Key : in String ) return Json_Object_Type is
		Data : Json_Data_type := Get( Object, key );
	begin
		return Data.The_Type;
	end Get_Type;



	procedure Iterate( Object : in Object_Type; Iterator : access procedure( Key : in String ) ) is
		procedure It( C : Json_Data_Maps.Cursor ) is
		begin
			Iterator.all( To_String( Json_Data_Maps.Key( C ) ) );
		end It;
	begin
		Json_Data_Maps.Iterate( Object.Data, It'Access );
	end Iterate;

	procedure Iterate( Object : in Object_Type; Iterator : access procedure( Key : in Unbounded_String ) ) is
		procedure It( C : Json_Data_Maps.Cursor ) is
		begin
			Iterator.all( Json_Data_Maps.Key( C ) );
		end It;
	begin
		Json_Data_Maps.Iterate( Object.Data, It'Access );
	end Iterate;

	procedure Iterate( Object : in Object_Type; Iterator : access procedure( Key : in String; Value : in Json_Data_Type ) ) is
		procedure It( C : Json_Data_maps.Cursor ) is
		begin
			Iterator.all( To_String( Json_Data_Maps.Key( C ) ), Json_Data_Maps.Element( C ) );
		end It;
	begin
		Json_Data_Maps.Iterate( Object.Data, It'Access );
	end Iterate;

	procedure Iterate( Object : in Object_Type; Iterator : access procedure( Key : in Unbounded_String; Value : in Json_Data_Type ) ) is
		procedure It( C : Json_Data_maps.Cursor ) is
		begin
			Iterator.all( Json_Data_Maps.Key( C ), Json_Data_Maps.Element( C ) );
		end It;
	begin
		Json_Data_Maps.Iterate( Object.Data, It'Access );
	end Iterate;


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

				when '{' =>
					if Should_Read /= Read_Key then
						raise SYNTAX_ERROR with "should be ready to read a value and I found a key at " & Positive'Image( char_index );
					end if;
					Char_index := Char_Index + 1;
					Jump_Spaces( Str, Char_Index );
					if Str( Char_Index ) = '}' then
						-- empty object
						Char_Index := Char_Index + 1;
						return;
					end if;
					Read_Key( Str, Char_Index, Last_Key );
					Should_Read := Read_Value;

				when ',' =>
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


	function To_Data( Value : in Array_Type ) return Json_Data_Type is
		Data : Json_Data_Type;
	begin
		Data.The_Type := Json_Array;
		Data.Vector.all := Value;
		return Data;
	end To_Data;

	function From_Data( Data : in Json_Data_Type ) return Array_Type is
	begin
		Check( Json_Array, Data.The_Type );
		return Data.Vector.all;
	end From_Data;
	--
	-- Replacers
	--

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Json_Data_type ) is
	begin
		Json_Data_Vectors.Replace_Element( A.Data, Index, Value );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Integer ) is
	begin
		Replace( A, Index, To_Data( Value ) );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in String ) is
	begin
		Replace( A, Index, To_Data( Value ) );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Unbounded_String ) is
	begin
		Replace( A, Index, To_Data( Value ) );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Object_Type ) is
	begin
		Replace( A, Index, To_Data( Value ) );
	end Replace;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Array_Type ) is
	begin
		Replace( A, Index, To_Data( Value ) );
	end Replace;

	-- 
	-- Appenders
	--

	procedure Append( A : in out Array_Type; Value : in Json_Data_Type ) is
	begin
		Json_Data_Vectors.Append( A.Data, Value );
	end Append;

	procedure Append( A : in out Array_Type; Value : in Integer ) is
	begin
		Append( A, To_Data( Value ) );
	end Append;

	procedure Append( A : in out Array_Type; Value : in String ) is
	begin
		Append( A, To_Data( Value ) );
	end Append;

	procedure Append( A : in out Array_Type; Value : in Unbounded_String ) is
	begin
		Append( A, To_Data( Value ) );
	end Append;

	procedure Append( A : in out Array_Type; Value : in Object_Type ) is
	begin
		Append( A, To_Data( Value ) );
	end Append;

	procedure Append( A : in out Array_Type; Value : in Array_Type ) is
	begin
		Append( A, To_Data( Value ) );
	end Append;

	
	--
	-- Getters
	--

	function Get( A : in Array_Type; Index : in Natural ) return Json_Data_Type is
	begin
		return Json_Data_Vectors.ELement( A.Data, Index );
	end Get;


	function Get( A : in Array_Type; Index : in Natural ) return Integer is
	begin
		return From_Data( Get( A, Index ) );
	end Get;

	function Get( A : in Array_Type; Index : in Natural ) return String is
	begin
		return From_Data( Get( A, Index ) );
	end Get;

	function Get( A : in Array_Type; Index : in Natural ) return Unbounded_String is
	begin
		return From_Data( Get( A, Index ) );
	end Get;
		
	function Get( A : in Array_Type; Index : in Natural ) return Object_Type is
	begin
		return From_Data( Get( A, Index ) );
	end Get;

	function Get( A : in Array_Type; Index : in Natural ) return Array_Type is
	begin
		return From_Data( Get( A, Index ) );
	end Get;



	function Get_Type( A : in Array_Type; Index: in Natural ) return Json_Object_Type is
		Data : Json_Data_Type := Get( A, Index );
	begin
		return Data.The_Type;
	end Get_Type;



	procedure Iterate( A : in Array_Type; Iterator : access procedure( Index : in Natural ) ) is
		procedure It( C : in Json_Data_Vectors.Cursor ) is
		begin
			Iterator.all( Json_Data_Vectors.To_Index( C ) );
		end It;
	begin
		Json_Data_Vectors.Iterate( A.Data, It'Access );
	end Iterate;


	procedure Iterate( A : in Array_Type; Iterator : access procedure( Index : in Natural; Value : in Json_Data_Type ) ) is
		procedure It( C : in Json_Data_Vectors.Cursor ) is
		begin
			Iterator.All( Json_Data_Vectors.To_Index( C ), Json_Data_Vectors.Element( C ) );
		end It;
	begin
		Json_Data_Vectors.Iterate( A.Data, It'Access );
	end Iterate;

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
				when '[' =>
					Char_Index := Char_Index + 1;
					Jump_Spaces( Str, Char_Index );
					if Str( Char_Index ) = ']' then
						-- empty array
						Char_Index := Char_Index + 1;
						return;
					end if;

					declare
						Data : Json_Data_Type;
					begin
						From_Json( Str, Char_Index, Data );
						Json_Data_Vectors.Append( The_A.Data, Data );
					end;
				when ',' =>
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
	begin
		Json_Data_Maps.Include( Object.Data, To_Unbounded_String( Key ), To_Data( Value ) );
	end Set;

	function Get( Object : in Object_Type; Key : in String ) return Array_Type is
	begin
		return From_Data( Get( Object, Key ) );
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
	begin
		loop
			Jump_Spaces( Str, Char_Index );
			case Str( Char_Index ) is
				when '0' .. '9' =>
					declare
						From	: Positive := Char_Index;
						To	: Positive := Char_Index;
					begin
						while Str( To ) in '0' .. '9' loop
							To := To + 1;
						end loop;
						Data := To_Data( Integer'Value( Str ( From .. To - 1 ) ) );
						Char_Index := To;
						return;
					end;
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
								if To - 1 >= From then
									Data := To_Data( Str( From .. To - 1 ) );
								else
									Data := To_Data( "" );
								end if;
								Char_Index := To + 1;
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
						Data := To_Data( A );
						return;
					end;
				when '{' =>
					declare
						O : Object_Type;
					begin
						From_Json( Str, Char_Index, O );
						Data := To_Data( O ); 
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
			when Json_Integer =>
				return Integer'Image( Data.Int );
			when Json_String =>
				return ''' & KOW_Lib.String_Util.Json_Scriptify( To_String( Data.Str ) ) & ''';
			when Json_Array =>
				return To_Json( Data.Vector.All );
			when Json_Object =>
				return To_Json( Data.Object.All );
		end case;
	end To_Json;


end KOW_Lib.Json;
