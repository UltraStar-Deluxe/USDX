unit zlportio;

{$I switches.inc}

interface

uses GlueWindows;

    procedure zlioportwrite( const Port,DataType,Data:dword );
    procedure portwriteb( const Port:Dword;const Data:byte );
    function  GetTime : Real;

implementation

uses SysUtils;

procedure zlioportwrite( const Port,DataType,Data:dword );
begin
end;

procedure portwriteb( const Port:Dword;const Data:byte );
begin
end;

function  GetTime : Real;
begin
    Result := Now;
end;

end.
 
