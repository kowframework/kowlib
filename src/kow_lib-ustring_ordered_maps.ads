with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

package KOW_Lib.UString_Ordered_Maps is new 
	Ada.Containers.Ordered_Maps(	Key_Type	=> Unbounded_String,
					Element_Type	=> Unbounded_String );
