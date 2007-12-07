unit UCommandLine;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


type
  //-----------
  // TCMDParams - Class Reaads Infos from ParamStr and set some easy Interface Variables
  //-----------
  TCMDParams = class
    private
      sLanguage:   String;
      sResolution: String;
    public
      //Some Boolean Variables Set when Reading Infos
      Debug:      Boolean;
      Benchmark:  Boolean;
      NoLog:      Boolean;
      FullScreen: Boolean;
      Joypad:     Boolean;

      //Some Value Variables Set when Reading Infos {-1: Not Set, others: Value}
      Depth:      Integer;
      Screens:    Integer;

      //Some Strings Set when Reading Infos {Length=0 Not Set}
      SongPath:   String;
      ConfigFile: String;
      ScoreFile:  String;

      procedure showhelp();

      //Pseudo Integer Values
      Function GetLanguage:   Integer;
      Property Language:      Integer read GetLanguage;

      Function GetResolution: Integer;
      Property Resolution:    Integer read GetResolution;

      //Some Procedures for Reading Infos
      Constructor Create;

      Procedure ResetVariables;
      Procedure ReadParamInfo;
  end;

var
  Params:    TCMDParams;
  
const
  cHelp            = 'help';
  cMediaInterfaces = 'showinterfaces';


implementation

uses SysUtils;
//      uINI   -- Nasty requirement... ( removed with permission of blindy )


//-------------
// Constructor - Create class, Reset Variables and Read Infos
//-------------
Constructor TCMDParams.Create;
begin

  if FindCmdLineSwitch( cHelp ) then
    showhelp();

  ResetVariables;
  ReadParamInfo;
end;

procedure TCMDParams.showhelp();

  function s( aString : String ) : string;
  begin
    result := aString + StringofChar( ' ', 15 - length( aString ) );
  end;
  
begin

  writeln( '' );
  writeln( '**************************************************************' );
  writeln( '  UltraStar Deluxe - Command line switches                    ' );
  writeln( '**************************************************************' );
  writeln( '' );
  writeln( '  '+s( 'Switch' ) +' : Purpose' );
  writeln( '  ----------------------------------------------------------' );
  writeln( '  '+s( cMediaInterfaces ) + ' : Show in-use media interfaces' );
  writeln( '' );

  halt();
end;

//-------------
// ResetVariables - Reset Class Variables
//-------------
Procedure TCMDParams.ResetVariables;
begin
  Debug       := False;
  Benchmark   := False;
  NoLog       := False;
  FullScreen  := False;
  Joypad      := False;

  //Some Value Variables Set when Reading Infos {-1: Not Set, others: Value}
  sResolution := '';
  sLanguage   := '';
  Depth       := -1;
  Screens     := -1;

  //Some Strings Set when Reading Infos {Length=0 Not Set}
  SongPath    := '';
  ConfigFile  := '';
  ScoreFile   := '';
end;

//-------------
// ReadParamInfo - Read Infos from Parameters
//-------------
Procedure TCMDParams.ReadParamInfo;
var
  I:        Integer;
  PCount:   Integer;
  Command:  String;
begin
  PCount := ParamCount;
  //Log.LogError('ParamCount: ' + Inttostr(PCount));
  

  //Check all Parameters
  For I := 1 to PCount do
  begin
    Command := Paramstr(I);
    //Log.LogError('Start parsing Command: ' + Command);
    //Is String Parameter ?
    if (Length(Command) > 1) AND (Command[1] = '-') then
    begin
      //Remove - from Command
      Command := Lowercase(Trim(Copy(Command, 2, Length(Command) - 1)));
      //Log.LogError('Command prepared: ' + Command);

      //Check Command

      // Boolean Triggers:
      if (Command = 'debug') then
        Debug       := True
      else if (Command = 'benchmark') then
        Benchmark   := True
      else if (Command = 'nolog') then
        NoLog       := True
      else if (Command = 'fullscreen') then
        Fullscreen  := True
      else if (Command = 'joypad') then
        Joypad    := True

      //Integer Variables
      else if (Command = 'depth') then
      begin
        //Check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          Command := ParamStr(I + 1);

          //Check for valid Value
          If (Command = '16') then
            Depth := 0
          Else If (Command = '32') then
            Depth := 1;
        end;
      end

      else if (Command = 'screens') then
      begin
        //Check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          Command := ParamStr(I + 1);

          //Check for valid Value
          If (Command = '1') then
            Screens := 0
          Else If (Command = '2') then
            Screens := 1;
        end;
      end

      //Pseudo Integer Values
      else if (Command = 'language') then
      begin
        //Check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          //Write Value to String
          sLanguage := Lowercase(ParamStr(I + 1));
        end;
      end

      else if (Command = 'resolution') then
      begin
        //Check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          //Write Value to String
          sResolution := Lowercase(ParamStr(I + 1));
        end;
      end

      //String Values
      else if (Command = 'songpath') then
      begin
        //Check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          //Write Value to String
          SongPath := ParamStr(I + 1);
        end;
      end

      else if (Command = 'configfile') then
      begin
        //Check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          //Write Value to String
          ConfigFile := ParamStr(I + 1);

          //is this a relative PAth -> then add Gamepath
          if Not ((Length(ConfigFile) > 2) AND (ConfigFile[2] = ':')) then
            ConfigFile := ExtractFilePath(ParamStr(0)) + Configfile;
        end;
      end

      else if (Command = 'scorefile') then
      begin
        //Check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          //Write Value to String
          ScoreFile := ParamStr(I + 1);
        end;
      end;

    end;

  end;

{  Log.LogError('Values: ');

  if Debug then
    Log.LogError('Debug');

  if Benchmark then
    Log.LogError('Benchmark');

  if NoLog then
    Log.LogError('NoLog');

  if Fullscreen then
    Log.LogError('FullScreen');

  if JoyStick then
    Log.LogError('Joystick');


  Log.LogError('Screens: ' + Inttostr(Screens));
  Log.LogError('Depth: ' + Inttostr(Depth));

  Log.LogError('Resolution: ' + Inttostr(Resolution));
  Log.LogError('Resolution: ' + Inttostr(Language));

  Log.LogError('sResolution: ' + sResolution);
  Log.LogError('sLanguage: ' + sLanguage);

  Log.LogError('ConfigFile: ' + ConfigFile);
  Log.LogError('SongPath: ' + SongPath);
  Log.LogError('ScoreFile: ' + ScoreFile);  }

end;

//-------------
// GetLanguage - Get Language ID from saved String Information
//-------------
Function TCMDParams.GetLanguage:   Integer;
var
  I: integer;
begin
  Result := -1;
{*  JB - 12sep07 to remove uINI dependency

  //Search for Language
  For I := 0 to high(ILanguage) do
    if (LowerCase(ILanguage[I]) = sLanguage) then
    begin
      Result := I;
      Break;
    end;
*}
end;

//-------------
// GetResolution - Get Resolution ID from saved String Information
//-------------
Function TCMDParams.GetResolution: Integer;
var
  I: integer;
begin
  Result := -1;
{*  JB - 12sep07 to remove uINI dependency

  //Search for Resolution
  For I := 0 to high(IResolution) do
    if (LowerCase(IResolution[I]) = sResolution) then
    begin
      Result := I;
      Break;
    end;
*}
end;

end.
