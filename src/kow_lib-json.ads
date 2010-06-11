

--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Finalization;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-- TODO :: implement support for numeric values

package KOW_Lib.Json is

	SYNTAX_ERROR : Exception;
	-- the syntax error when parsin a json string

	type Json_Object_Type is ( Json_String, Json_Array, Json_Object );
	-- the possible json types.
	-- an array can have string, arrays (in the ada side they are vectors) and objects
	-- an object can have all of them also, but hold them as a map

	-- object => map of json_data
	-- array => vector of  json_data
	--
	--
	-- Array can have:
	-- 	. object
	-- 	. array
	-- 	. string
	-- Object can have:
	-- 	. array
	-- 	. to object
	-- 	. string





	---------------------
	-- The Object Type --
	---------------------
	type Object_Type is private;
	type Object_Ptr is access Object_Type;

	--
	-- setters
	--

	procedure Set( Object : in out Object_Type; Key : in String; Value : in String );
	procedure Set( Object : in out Object_Type; Key : in String; Value : in Unbounded_String );
	procedure Set( Object : in out Object_Type; Key : in Unbounded_String; Value : in Unbounded_String );
	procedure Set( Object : in out Object_Type; Key : in String; Value : in Object_Type );

	--
	-- getters
	--

	function Get( Object : in Object_Type; Key : in String ) return String;
	function Get( Object : in Object_Type; Key : in String ) return Unbounded_String;
	function Get( Object : in Object_Type; Key : in Unbounded_String ) return Unbounded_String;
	function Get( Object : in Object_Type; Key : in String ) return Object_Type;

	function Get_Type( Object : in Object_Type; Key : in String ) return Json_Object_Type;


	procedure Iterate( Object : in Object_Type; Iterator : access procedure( Key : in String ) );
	procedure Iterate( Object : in Object_Type; Iterator : access procedure( Key : in Unbounded_String ) );

	function From_Json( Str : in String ) return Object_Type;
	procedure From_Json(
				Str		: in     String;
				Char_Index	: in out Positive;
				Object		:    out object_type
			);


	function To_Json( Object : in Object_Type ) return String;

	--------------------
	-- The Array Type --
	--------------------

	type Array_Type is private;
	type Array_Ptr is access Array_Type;

	--
	-- Replacers
	--

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in String );
	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Unbounded_String );
	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Object_Type );
	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Array_Type );

	-- 
	-- Appenders
	--

	procedure Append( A : in out Array_Type; Value : in String );
	procedure Append( A : in out Array_Type; Value : in Unbounded_String );
	procedure Append( A : in out Array_Type; Value : in Object_Type );
	procedure Append( A : in out Array_Type; Value : in Array_Type );

	
	--
	-- Getters
	--

	function Get( A : in Array_Type; Index : in Natural ) return String;
	function Get( A : in Array_Type; Index : in Natural ) return Unbounded_String;
	function Get( A : in Array_Type; Index : in Natural ) return Object_Type;
	function Get( A : in Array_Type; Index : in Natural ) return Array_Type;

	function Get_Type( A : in Array_Type; Index: in Natural ) return Json_Object_Type;

	procedure Iterate( A : in Array_Type; Iterator : access procedure( Index : in Natural ) );


	function From_Json( Str : in String ) return Array_Type;

	procedure From_Json(
				Str		: in     String;
				Char_Index	: in out Positive;
				A		:    out Array_Type
			);


	function To_Json( A : in Array_type ) return String;


	-- Missing methods for object_Type
	procedure Set( Object : in out Object_Type; Key : in String; Value : in Array_Type );
	function Get( Object : in Object_Type; Key : in String ) return Array_Type;



private
	type Json_Data_Type is new Ada.Finalization.Controlled with record
		the_type : json_object_type;

		str	: unbounded_string;
		object	: object_ptr;
		vector	: array_ptr;
	end record;

	
	overriding
	procedure Initialize( Object : in out Json_Data_Type );

	overriding
	procedure Adjust( Object : in out Json_Data_Type );
		
	overriding
	procedure Finalize( Object : in out Json_Data_Type );


	function From_Json( Str : in String ) return Json_Data_Type;
	procedure From_Json(
				Str		: in     String;
				Char_Index	: in out Positive;
				Data		:    out Json_Data_Type
			);
	function To_Json( Data : in Json_Data_Type ) return String;



	package Json_Data_Maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Unbounded_String,
					Element_Type	=> Json_Data_Type
				);
	package Json_Data_Vectors is new Ada.Containers.Vectors(
					Index_Type	=> Natural,
					Element_Type	=> Json_Data_Type
				);


	type Object_Type is record
		Data	: Json_Data_Maps.Map;
	end record;
	procedure Free is new Ada.Unchecked_Deallocation(
						Object	=> Object_Type,
						Name	=> Object_ptr
					);


	type Array_Type is record
		Data	: Json_Data_Vectors.Vector;
	end record;
	procedure Free is new Ada.Unchecked_Deallocation(
						Object	=> Array_Type,
						Name	=> Array_Ptr
					);


end KOW_Lib.Json;