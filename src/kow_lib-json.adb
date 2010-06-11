

--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;
with Ada.Finalization.Controlled;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-- TODO :: make the object_type into ta controlled type

package KOW_Lib.Json is


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
	-- Controlled Type Methods
	--

	overriding
	procedure Adjust( Object : in out Object_Type );

	overriding
	procedure Finalize( Object : in out Object_Type );


	--
	-- New primitives
	--

	procedure Set( Object : in out Object_Type; Key : in String; Value : in String );
	procedure Set( Object : in out Object_Type; Key : in String; Value : in Unbounded_String );
	procedure Set( Object : in out Object_Type; Key : in Unbounded_String; Value : in Unbounded_String );
	procedure Set( Object : in out Object_Type; Key : in String; Value : in Object_Type );
--	procedure Set( Object : in out Object_Type; Key : in String; Value : in Array_Type );
--	Note: the set() and get() for array types are declared after the definition..

	function Get( Object : in Object_Type; Key : in String ) return String;
	function Get( Object : in Object_Type; Key : in String ) return Unbounded_String;
	function Get( Object : in Object_Type; Key : in Unbounded_String ) return Unbounded_String;
	function Get( Object : in Object_Type; Key : in String ) return Object_Type;

	function Get_Type( Object : in Object_Type; Key : in String ) return Json_Object_Type;

--	function Get( Object : in Object_Type; Key : in String ) return Array_Type;
--	Note: the set() and get() for array types are declared after the definition..

	--------------------
	-- The Array Type --
	--------------------

	type Array_Type is private;
	type Array_Ptr is access Array_Type;

	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in String );
	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Unbounded_String );
	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Object_Type );
	procedure Replace( A : in out Array_Type; Index : in Natural; Value : in Array_Type );

	procedure Append( A : in out Array_Type; Value : in String );
	procedure Append( A : in out Array_Type; Value : in Unbounded_String );
	procedure Append( A : in out Array_Type; Value : in Object_Type );
	procedure Append( A : in out Array_Type; Value : in Object_Type );

	
	function Get( A : in out Array_Type; Index : in Natural ) return String;
	function Get( A : in out Array_Type; Index : in Natural ) return Unbounded_String;
	function Get( A : in out Array_Type; Index : in Natural ) return Object_Type;
	function Get( A : in out Array_Type; Index : in Natural ) return Array_Type;



	procedure Set( Object : in out Object_Type; Key : in String; Value : in Array_Type );
	function Get( Object : in Object_Type; Key : in String ) return Array_Type;



private
	type Json_Data is new Ada.Finalization.Controlled with record
		the_type : json_object_type;

		str	: unbounded_string;
		object	: object_ptr;
		vector	: array_ptr;
	end record;
	overriding
	procedure Adjust( Object : in out Json_Data );

	overriding
	procedure Finalize( Object : in out Json_Data );



	package Json_Data_Maps is new Ada.Container.Ordered_Maps(
					Key_Type	=> Unbounded_String,
					Element_Type	=> Json_Data
				);
	package Json_Data_Vectors is new Ada.Container.Vectors(
					Index_Type	=> Natural,
					Element_Type	=> Json_Data
				);


	type Object_Type is new Ada.Finalization.Controlled with record
		Data	: Json_Data_Maps.Map;
	end record;


	type Array_Type is new Ada.Finalization.Controlled with record
		Data	: Json_Data_Vectors.Vector;
	end record;


end KOW_Lib.Json;
