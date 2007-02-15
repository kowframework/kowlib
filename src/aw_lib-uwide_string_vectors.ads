-- Vector of Natural index and String values
--
-- autor Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26



with Ada.Containers.Vectors;
with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;

package Aw_Lib.UWide_String_Vectors is new Ada.Containers.Vectors(	Element_Type	=> Unbounded_Wide_String,
                                                           	Index_Type	=> Natural );
