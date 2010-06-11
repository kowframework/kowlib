

--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


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

end KOW_Lib.Json;
