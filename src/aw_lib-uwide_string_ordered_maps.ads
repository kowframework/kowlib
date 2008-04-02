-- Vector of Natural index and String values
--
-- autor Marcelo C. de Freitas <marcelo.batera@gmail.com>
--
-- Repository information:
-- $Date: 2007-02-24 00:12:54 -0200 (Sáb, 24 Fev 2007) $
-- $Revision: 147 $
-- $Author: ogro $



with Ada.Containers.Ordered_Maps;
with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;

package Aw_Lib.UWide_String_Ordered_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type	=> Unbounded_Wide_String,
			Element_Type	=> Unbounded_Wide_String
);
