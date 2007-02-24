-- Vector of Natural index and String values
--
-- autor Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$



with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

package Aw_Lib.UString_Vectors is new Ada.Containers.Vectors(	Element_Type	=> Unbounded_String,
                                                           	Index_Type	=> Natural );
