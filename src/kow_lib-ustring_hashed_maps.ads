with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package KOW_Lib.UString_Hashed_Maps is new Ada.Containers.Hashed_Maps(
						Key_Type	=> Unbounded_String,
						Element_Type	=> Unbounded_String,
						Hash		=> Ada.Strings.Unbounded.Hash,
						Equivalent_Keys	=> Ada.Strings.Unbounded."="
					);
