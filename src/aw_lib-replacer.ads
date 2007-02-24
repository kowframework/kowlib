-- Bibliteca que auxilia criacao de string usando chaves.
--
-- author Willian Gigliotti <wgigliotti@gmail.com>
-- created at 2007-02-20
--
-- Repository information
-- $Date$
-- $Revision$
-- $Author$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Aw_Lib.Replacer is

	-- Exemplo de Source: 
	--      "Rancharia, $dia de $mes de $ano"
	--      "Bom Dia, $nome. Gostaria que vc me pagasse os $valor que vc esta me devendo desde ${d}/${m}/${a}"

	type Conteudo is new Integer; -- TODO definir;

	type Replace_Ammount is (Replace_All, Replace_First, Replace_Last);
	-- Todas, primeira ou ultima;

	type Replace_Type is (Is_Word, Is_Anything);
	-- palavra inteira ou no meio

	type Replace_Case is (Case_Sensitive, Case_Unsensitive);
	-- diferencia ou nao de maiuscula de minuscula;

	function Replace(From, To, What: Unbounded_String; 
		R_Ammount: Replace_Ammount := Replace_All;
		R_Type: Replace_Type := Is_Word;
		R_Case: Replace_Case := Case_Sensitive) return Unbounded_String;
	-- Troca strings simples!



	procedure Get_Variavel(
		Source: in Unbounded_String; 
		Inicio: in Integer; 
		Posicao: out Integer; 
		Tamanho: out Integer; 
		Valor: out Unbounded_String);
	-- Procura no Source variaveis que agente definiu ($var ou ${var})
	-- a partir da Posicao inicio da mesma

	-- Retorna a variavel encontrada, dando a posicao do comeco dela "$"
	-- e seu tamanho, incluindo $ e {} se existir;

	function ReplaceVars(Tabela: in Conteudo; Source: in Unbounded_String) return Unbounded_String;
	-- Troca as variaveis encontradas em Source pelas da Tabela e retorna a Unbounded_String;

	procedure ReplaceVars(Tabela: in Conteudo; Texto: in out Unbounded_String);
	-- Troca as variaveis do Texto pelas da Tabela;

	


end Aw_Lib.Replacer;


