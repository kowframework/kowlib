
with Ada.Text_IO;		use Ada.Text_IO;

with KOW_Lib.Json;		use KOW_Lib.Json;



procedure Json_Test is

	A_Object : Object_Type;


	A_Array : Array_Type;

begin

	Put_line( "Parsing object" );
	declare
		B_Object : Object_Type := From_Json( "{chave:'valor',chave:['um','vetor']}" );
	begin
		Put_Line( "Copying object" );
		A_Object := B_Object;
	end;
	Put_Line( "Printing object" );
	Put_Line( To_Json( A_Object ) );


	Put_line( "==========" );

	Put_Line( "Parsing array" );
	declare
		B_Array : Array_Type := From_Json( "['a','b',{chave:'valor'}]" );
	begin
		Put_Line( "Copying array" );
		A_Array := B_Array;
	end;
	Put_Line( "Printing array" );
	Put_Line( To_Json( A_Array ) );


	Put_Line( "Exiting" );
end;




