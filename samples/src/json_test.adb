
with Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;

with KOW_Lib.Json;		use KOW_Lib.Json;



procedure Json_Test is

	A_Object : Object_Type;


	A_Array : Array_Type;


	procedure Array_Iterator( Index : in Natural ) is
		the_type : json_object_type := get_type( a_array, index );
	begin
		Put("        " & Natural'Image( Index ) );
		Put("[" & json_object_type'image( the_type ) & "] =>" );

		case the_type is
			when json_integer=>
				Put_Line( Integer'Image( Get( A_Array, Index ) ) );
			when json_float =>
				Put_Line( Float'Image( Get( A_Array, index ) ) );
			when Json_boolean =>
				Put_Line( Boolean'Image( Get( A_Array, index ) ) );
			when json_string =>
				Put_Line( Ada.Strings.Unbounded.To_String( Get( A_Array, Index ) ) );
			when json_array =>
				declare
					a: array_type := get( A_Array, Index );
				begin
					Put_Line( To_Json( a ) );
				end;
			when json_object =>
				declare
					o: object_Type := get( a_array, index );
				begin
					Put_Line( To_Json( o ) );
				end;
		end case;
	end Array_Iterator;

	procedure Object_Iterator( Key : in String ) is
		the_type : json_object_Type := get_type( a_object, key );
	begin
		Put("   " & Key );
		Put('[' & json_object_type'image( the_type ) & "] =>" );

		case the_type is
			when json_integer=>
				Put_Line( Integer'Image( Get( A_Object, Key ) ) );
			when json_float =>
				Put_Line( Float'Image( Get( A_Object, Key ) ) );
			when json_boolean =>
				Put_line( Boolean'Image( Get( A_Object, Key ) ) );
			when json_string =>
				Put_Line( Ada.Strings.Unbounded.To_String( Get( A_Object, Key ) ) );
			when json_array =>
				Ada.Text_IO.New_Line;
				A_Array := Get( A_Object, Key );
				Iterate( A_Array, Array_Iterator'Access );
			when json_object =>
				declare
					O : Object_Type := Get( A_Object, Key );
				begin
					Put_Line( To_Json( O ) );
				end;
		end case;
	end Object_iterator;

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


	Put_Line( "Parsing a biger json" );
	A_object := From_Json(
"{status:'success',text_contents:[{original_tag:'TECA.ENTITIES.TEXT_CONTENT_ENTITY',id:'1',filter_tags:'',title:'Teste',owner_identity:'teste_aluno1',creation_time:'2010-06-09 03:04:22',update_time:'2010-06-09 03:04:22',content:'<p>Texto de <strong>teste</strong>!</p> <p>&nbsp;</p> <p>&nbsp;</p> <p>Aqui vai mais texto pra testar :)</p>',owner_identity:'teste_aluno1'}],some_integer:1 ,some_other_integer:10,empty_object:{},empty_array:[]}"
);

	Put_Line("Printing.." );
	Put_Line( TO_json( A_object ) );
	Put_Line("And Reparsing and reprinting" );
	A_Object := From_Json( To_Json( A_Object ) );
	Put_Line( To_Json( A_Object ) );


	declare
		Str		: String := Get( A_Object, "status" );
		Contents	: Array_Type := Get( A_object, "text_contents" );
		First_Tag	: String := Get( Get( Contents, 0 ), "original_tag" );
	begin
		Put_Line( First_Tag );
		Put_Line( Str );
	end;



	Put_Line( "Iterating..." );
	Iterate( A_Object, Object_Iterator'Access );



	A_Array := From_Json(
"[{""type"":""user"",""user"":{""username"":""teste_admin"",""first_name"":""Administrador"",""last_name"":""do Colégio de Testes""}},{""type"":""user"",""user"":{""username"":""teste_professor1"",""first_name"":""Marcelo"",""last_name"":""Coraça de Freitas""}},{""type"":""user"",""user"":{""username"":""teste_professor2"",""first_name"":""Jorge"",""last_name"":""Sallum""}},{""type"":""user"",""user"":{""username"":""teste_aluno1"",""first_name"":""Bruno"",""last_name"":""Costa""}},{""type"":""user"",""user"":{""username"":""teste_aluno2"",""first_name"":""Iuri"",""last_name"":""Pereira""}},{""type"":""user"",""user"":{""username"":""teste_aluno3"",""first_name"":""Adélia"",""last_name"":""Barbosa""}},{""type"":""user"",""user"":{""username"":""teste_aluno4"",""first_name"":""Fabio"",""last_name"":""Mantegari""}},{""type"":""textContent"",""textContent"":{""id"":""1"",""title"":""Teste"",""owner_identity"":""teste_aluno1""}},{type:'trash'}]"
);


	Put_line( "Iterating users.." );
	declare
		procedure The_Iterator( index : in natural; value : in json_data_type ) is
			Object		: Object_Type := From_Data( Value );
			The_Type	: String := Get( Object, "type" );
			Contained	: Object_Type;
		begin
			if The_Type = "user" then
				Contained := Get( Object, "user" );
				Put_Line( "  * [user] " & Get( Contained, "username" )  );
			elsif The_Type = "textContent" then
				Contained := Get( Object, "textContent" );
				Put_Line( "  * [textContent] " & Get( Contained, "title" ) );
			else
				Put_line( "  * unknonw type :: '" & The_Type & ''' );
			end if;
		end The_Iterator;
	begin
		Iterate( A_Array, The_Iterator'Access );
	end;


	Put_Line( "Exiting" );
end;




