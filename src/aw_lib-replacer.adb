-- Bibliteca que auxilia criacao de string usando chaves.
--
-- author Willian Gigliotti <wgigliotti@gmail.com>
-- created at 2007-02-20

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Aw_Lib.Replacer is

	-- Exemplo de Source: 
	--      "Rancharia, $dia de $mes de $ano"
	--      "Bom Dia, $nome. Gostaria que vc me pagasse os $valor que vc esta me devendo desde $d/$m/$a"

	function Replace(From, To, What: Unbounded_String; 
		R_Ammount: Replace_Ammount := Replace_All;
		R_Type: Replace_Type := Is_Word;
		R_Case: Replace_Case := Case_Sensitive) return Unbounded_String is
		-- Troca strings simples!
		Str: Unbounded_String := To_Unbounded_String("ola");
	begin
		-- TODO
		return Str;
	end Replace;



	procedure Get_Variavel(
		Source: in Unbounded_String; 
		Inicio: in Integer; 
		Posicao: out Integer; 
		Tamanho: out Integer; 
		Valor: out Unbounded_String) is
		-- Procura no Source variaveis que agente definiu ($var ou ${var})
		-- a partir da Posicao inicio da mesma
	begin
		Posicao := 20;
		Tamanho := 7;
		Valor := To_Unbounded_String("d");
	end Get_Variavel;


	function ReplaceVars(Tabela: in Conteudo; Source: in Unbounded_String) return Unbounded_String is
	-- Troca as variaveis encontradas em Source pelas da Tabela e retorna a Unbounded_String;
		Str: Unbounded_String := Source;
	begin
		ReplaceVars(Tabela, Str);
		return Str;
	end ReplaceVars;

	procedure ReplaceVars(Tabela: in Conteudo; Texto: in out Unbounded_String) is
	-- Troca as variaveis do Texto pelas da Tabela;
	begin
		Texto := Texto & Texto;
	end ReplaceVars;

end Aw_Lib.Replacer;

