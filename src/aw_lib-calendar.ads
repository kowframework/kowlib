-- Biblioteca de Calendario, com timestamp
--
-- author Willian Gigliotti <wgigliotti@gmail.com>
-- created at 2007-02-21
--
-- Repository information
-- $Date$
-- $Revision$
-- $Author$

with Ada.Calendar; use Ada.Calendar; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Aw_Lib.Replacer; use Aw_Lib.Replacer;



package Aw_Lib.Calendar is


	type Timestamp is new Integer;
	
	function Get(This: in Timestamp; Key: in Character) return Unbounded_String;
	-- Funcao que recebauma chave e retorna o valor;
	-- Essas chaves sao usadas na funcao Date tambem!
	--
	-- Chave    Retorno                 Exemplos

	--                   Hora

	-- a        am ou pm minisculo      am e pm
	-- A        AM ou PM maiusculo      AM e PM

	--                   Dia

	-- d        Dia do mes com 0s       01 ate 31
	-- D        Dia da Semana 3 letras  TODO (Ingles, portugues, etc)
	-- F        Mes escrito             TODO (Ingles, portugues, etc)
	-- M        Mes escrito, 3 letras   TODO (,....)
	-- m        Numero do Mes, com 0s   01 ate 12
	-- TODO continuar documentacao

	function To_Timestamp( Hora: in Ada.Calendar.Time) return Timestamp;
	-- Retorna o timestamp dado um Time;

	function To_Time( Hora: in Timestamp) return Time;
	-- Retorna o Time dado um Timestamp;

        function Date( Formato: in Unbounded_String; Hora: in Timestamp) return Unbounded_String;
	-- Funcao que Coloca horarios e Datas num formato dado pela string

	procedure Date( Formato: in out Unbounded_String; Hora: in Timestamp);
	-- Coloca os horarios e dadas na propria string;

end Aw_Lib.Calendar;






