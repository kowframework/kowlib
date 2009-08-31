-- Testa regexp do GNAT



with Ada.Strings.Unbounded;
with Ada.Text_IO;			use Ada.Text_IO;



with KOW_Lib.Regular_Expressions;	use KOW_Lib.Regular_Expressions;
with KOW_Lib.UString_Vectors;

procedure Regular_Expressions_Test is

	procedure Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
		use Ada.Strings.Unbounded;
		use KOW_Lib.UString_Vectors;
	begin
		Put_Line( "    => " & To_String( Element( C ) ) );
	end Iterator;


	procedure Split_It( Subject : in String; Append_Null : Boolean := True ) is
		My_Vector : KOW_Lib.UString_Vectors.Vector;
	begin
		My_Vector := KOW_Lib.Regular_Expressions.Split(
				subject,
				"(\d+)|(\ )",
				append_null
			);

		Put_Line( "Running split on """ & Subject & """ ( Append_Null :: " & Boolean'Image( Append_Null ) & ")" );
		KOW_Lib.UString_Vectors.Iterate( My_Vector, Iterator'Access );
	end Split_It;


	procedure Split_It2( Subject : in String; Append_Null : Boolean := True ) is
		My_Vector : KOW_Lib.UString_Vectors.Vector;
	begin
		My_Vector := KOW_Lib.Regular_Expressions.Split(
				subject,
				"(\ |\.|\,|\(|\)|\[|\]|\{|\}|\!|\?|\;|\:)",
				append_null
			);

		Put_Line( "Running split on """ & Subject & """ ( Append_Null :: " & Boolean'Image( Append_Null ) & ")" );
		KOW_Lib.UString_Vectors.Iterate( My_Vector, Iterator'Access );
	end Split_It2;


begin

	Put_Line( 
			KOW_Lib.Regular_Expressions.Replace(
				"não-coisa não-coisas",
				"^não-(.*)$",
				"\0 => não \1"
			)
		);
	
	Put_Line( 
			KOW_Lib.Regular_Expressions.Replace(
				"não-coisa não-coisas",
				"^não-(.*)$",
				"\0 => não \1",
				KOW_Lib.Regular_Expressions.Entire_String
			)
		);
	

	Put_Line( 
			KOW_Lib.Regular_Expressions.Replace(
				"idéia de girico",
				"éia(s|)(\W|$)",
				"eia\1\2"
			)
		);
	
	Put_Line( 
			KOW_Lib.Regular_Expressions.Replace(
				"idéia de girico",
				"éia(s|)(\W|$)",
				"eia\1\2",
				KOW_Lib.Regular_Expressions.Entire_String
			)
		);


	New_Line( 2 );


	
	Split_It( "uma1string2separada3por4números" );
	New_Line;
	Split_It( "uma1string2separada3por4" );
	New_Line;
	Split_It2( "uma string não separada por números" );


	New_Line( 2 );

	Split_It( "uma1string2separada3por4números", false );
	New_Line;
	Split_It( "uma1string2separada3por4", false );
	New_Line;
	Split_It( "uma string não separada por números", false );


end Regular_Expressions_Test;
