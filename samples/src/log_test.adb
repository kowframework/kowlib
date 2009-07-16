with Ada.Text_IO;	use Ada.Text_IO;


with KOW_Lib.Log;	use KOW_Lib.Log;



procedure log_test is

	procedure Run_Tests( Logger: in Logger_Type ) is
	begin
		Log( Logger, Level_Error, "One error" );
		Log( Logger, Level_Warning, "One warning" );
		Log( Logger, Level_Info, "One info" );
		Log( Logger, Level_Debug, "One debug" );
	end Run_Tests;
begin

	Run_Tests( Get_Logger( "debug_logger", Level_Debug ) );
	New_Line(2);
	Run_Tests( Get_Logger( "info_logger", Level_Info ) );
	New_Line(2);
	Run_Tests( Get_Logger( "warning_logger", Level_Warning ) );
	New_Line(2);
	Run_Tests( Get_Logger( "error_logger", Level_Error ) );
	New_Line(2);
	Run_Tests( Get_Logger( "default_logger", Level_Nul ) );
	New_Line(2);
	Run_Tests( Get_Logger( "off_logger", Level_Off ) );
end log_test;
