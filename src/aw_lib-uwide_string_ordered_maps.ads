-- Vector of Natural index and String values
--
-- autor Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26



with Ada.Containers.Ordered_Maps;
with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;

package Aw_Lib.UWide_String_Ordered_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type	=> Unbounded_Wide_String,
			Element_Type	=> Unbounded_Wide_String
			);
