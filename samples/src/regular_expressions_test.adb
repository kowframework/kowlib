-- Testa regexp do GNAT



with Ada.Text_IO;			use Ada.Text_IO;



with KOW_Lib.Regular_Expressions;	use KOW_Lib.Regular_Expressions;
with KOW_Lib.UString_Vectors;

procedure Regular_Expressions_Test is

begin

	Put_Line( 
			KOW_Lib.Regular_Expressions.Replace(
				"não-coisa não-coisas",
				"^não-(.*)$",
				"\0 => não \1"
			)
		);
end Regular_Expressions_Test;
