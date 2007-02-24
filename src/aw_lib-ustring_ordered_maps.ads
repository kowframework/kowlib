-- Vector of Natural index and String values
--
-- autor Marcelo C. de Freitas <marcelo.batera@gmail.com>
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$



with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

package Aw_Lib.UString_Ordered_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type	=> Unbounded_String,
			Element_Type	=> Unbounded_String
			);
