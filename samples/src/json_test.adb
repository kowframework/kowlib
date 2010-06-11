
with Ada.Text_IO;		use Ada.Text_IO;

with KOW_Lib.Json;		use KOW_Lib.Json;



procedure Json_Test is

	A_Object : Object_Type;


	A_Array : Array_Type;

begin

	Put_line( "Parsing object" );
	declare
		B_Object : Object_Type := From_Json( "{chave:'valor','outra_chave':['um','vetor']}" );
	begin
		Put_Line( "Copying object" );
		A_Object := B_Object;
	end;
	Put_Line( "Printing object" );
	Put_Line( To_Json( A_Object ) );


	Put_line( "==========" );

	Put_Line( "Parsing array" );
	declare
		B_Array : Array_Type := From_Json( "['ab','b',{chave:'valor'}]" );
	begin
		Put_Line( "Copying array" );
		A_Array := B_Array;
	end;
	Put_Line( "Printing array" );
	Put_Line( To_Json( A_Array ) );


	Put_Line( "PArsing a biger json" );
	A_object := From_Json(
"{status:'success',text_contents:[{original_tag:'TECA.ENTITIES.TEXT_CONTENT_ENTITY',id:'1',filter_tags:'',title:'Teste',owner_identity:'teste_aluno1',creation_time:'2010-06-09 03:04:22',update_time:'2010-06-09 03:04:22',content:'<p>Texto de <strong>teste</strong>!</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>Aqui vai mais texto pra testar :)</p>',owner_identity:'teste_aluno1'}]}"
);
	Put_Line("Printing.." );
	Put_Line( TO_json( A_object ) );
	Put_Line("And Reparsing and reprinting" );
	A_Object := From_Json( To_Json( A_Object ) );
	Put_Line( To_Json( A_Object ) );


	Put_Line( "Exiting" );
end;




