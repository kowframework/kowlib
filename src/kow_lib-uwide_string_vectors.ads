with Ada.Containers.Vectors;
with Ada.Strings.Wide_Unbounded;   	use Ada.Strings.Wide_Unbounded;

package KOW_Lib.UWide_String_Vectors is new 
	Ada.Containers.Vectors(	Element_Type	=> Unbounded_Wide_String,
        			Index_Type	=> Natural	);

