{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UCommandLine;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  TScreenMode = (scmDefault, scmFullscreen, scmWindowed);

  {**
   * Reads infos from ParamStr and set some easy interface variables
   *}
  TCMDParams = class
    private
      fLanguage:   string;
      fResolution: string;

      procedure ShowHelp();

      procedure ReadParamInfo;
      procedure ResetVariables;

      function GetLanguage:   integer;
      function GetResolution: integer;
    public
      // some boolean variables set when reading infos
      Debug:      boolean;
      Benchmark:  boolean;
      NoLog:      boolean;
      ScreenMode: TScreenMode;
      Joypad:     boolean;

      // some value variables set when reading infos {-1: Not Set, others: Value}
      Depth:      integer;
      Screens:    integer;

      // some strings set when reading infos {Length=0: Not Set}
      SongPath:   string;
      ConfigFile: string;
      ScoreFile:  string;

      // pseudo integer values
      property Language:      integer read GetLanguage;
      property Resolution:    integer read GetResolution;

      // some procedures for reading infos
      constructor Create;
  end;

var
  Params:    TCMDParams;
  
const
  cHelp            = 'help';
  cDebug           = 'debug';
  cMediaInterfaces = 'showinterfaces';


implementation

uses SysUtils,
     UPlatform;

{**
 * Resets variables and reads info
 *}
constructor TCMDParams.Create;
begin
  inherited;
  
  if FindCmdLineSwitch( cHelp ) or FindCmdLineSwitch( 'h' ) then
    ShowHelp();

  ResetVariables;
  ReadParamInfo;
end;

procedure TCMDParams.ShowHelp();

  function Fmt(aString : string) : string;
  begin
    Result := Format('%-15s', [aString]);
  end;

begin
  writeln;
  writeln('**************************************************************');
  writeln('  UltraStar Deluxe - Command line switches                    ');
  writeln('**************************************************************');
  writeln;
  writeln('  '+ Fmt('Switch') +' : Purpose');
  writeln('  ----------------------------------------------------------');
  writeln('  '+ Fmt(cMediaInterfaces) +' : Show in-use media interfaces');
  writeln('  '+ Fmt(cDebug) +' : Display Debugging info');
  writeln;

  platform.halt;
end;

{**
 * Reset Class Variables
 *}
procedure TCMDParams.ResetVariables;
begin
  Debug       := False;
  Benchmark   := False;
  NoLog       := False;
  ScreenMode  := scmDefault;
  Joypad      := False;

  // some value variables set when reading infos {-1: Not Set, others: Value}
  fResolution := '';
  fLanguage   := '';
  Depth       := -1;
  Screens     := -1;

  // some strings set when reading infos {Length=0 Not Set}
  SongPath    := '';
  ConfigFile  := '';
  ScoreFile   := '';
end;

{**
 * Read command-line parameters
 *}
procedure TCMDParams.ReadParamInfo;
var
  I:        integer;
  PCount:   integer;
  Command:  string;
begin
  PCount := ParamCount;
  //Log.LogError('ParamCount: ' + Inttostr(PCount));
  
  // check all parameters
  for I := 1 to PCount do
  begin
    Command := ParamStr(I);
    // check if the string is a parameter
    if (Length(Command) > 1) and (Command[1] = '-') then
    begin
      // remove '-' from command
      Command := LowerCase(Trim(Copy(Command, 2, Length(Command) - 1)));
      //Log.LogError('Command prepared: ' + Command);

      // check command

      // boolean triggers
      if (Command = 'debug') then
        Debug       := True
      else if (Command = 'benchmark') then
        Benchmark   := True
      else if (Command = 'nolog') then
        NoLog       := True
      else if (Command = 'fullscreen') then
        ScreenMode  := scmFullscreen
      else if (Command = 'window') then
        ScreenMode  := scmWindowed
      else if (Command = 'joypad') then
        Joypad    := True

      // integer variables
      else if (Command = 'depth') then
      begin
        // check if there is another Parameter to get the Value from
        if (PCount > I) then
        begin
          Command := ParamStr(I + 1);

          // check for valid value
          // FIXME: guessing an array-index of depth is very error prone.  
          If (Command = '16') then
            Depth := 0
          Else If (Command = '32') then
            Depth := 1;
        end;
      end

      else if (Command = 'screens') then
      begin
        // check if there is another parameter to get the value from
        if (PCount > I) then
        begin
          Command := ParamStr(I + 1);

          // check for valid value
          If (Command = '1') then
            Screens := 0
          Else If (Command = '2') then
            Screens := 1;
        end;
      end

      // pseudo integer values
      else if (Command = 'language') then
      begin
        // check if there is another parameter to get the value from
        if (PCount > I) then
        begin
          // write value to string
          fLanguage := Lowercase(ParamStr(I + 1));
        end;
      end

      else if (Command = 'resolution') then
      begin
        // check if there is another parameter to get the value from
        if (PCount > I) then
        begin
          // write value to string
          fResolution := Lowercase(ParamStr(I + 1));
        end;
      end

      // string values
      else if (Command = 'songpath') then
      begin
        // check if there is another parameter to get the value from
        if (PCount > I) then
        begin
          // write value to string
          SongPath := ParamStr(I + 1);
        end;
      end

      else if (Command = 'configfile') then
      begin
        // check if there is another parameter to get the value from
        if (PCount > I) then
        begin
          // write value to string
          ConfigFile := ParamStr(I + 1);

          // is this a relative path -> then add gamepath
          if Not ((Length(ConfigFile) > 2) AND (ConfigFile[2] = ':')) then
            ConfigFile := ExtractFilePath(ParamStr(0)) + Configfile;
        end;
      end

      else if (Command = 'scorefile') then
      begin
        // check if there is another parameter to get the value from
        if (PCount > I) then
        begin
          // write value to string
          ScoreFile := ParamStr(I + 1);
        end;
      end;

    end;

  end;

{
  Log.LogInfo('Screens: ' + Inttostr(Screens));
  Log.LogInfo('Depth: ' + Inttostr(Depth));

  Log.LogInfo('Resolution: ' + Inttostr(Resolution));
  Log.LogInfo('Resolution: ' + Inttostr(Language));

  Log.LogInfo('sResolution: ' + sResolution);
  Log.LogInfo('sLanguage: ' + sLanguage);

  Log.LogInfo('ConfigFile: ' + ConfigFile);
  Log.LogInfo('SongPath: ' + SongPath);
  Log.LogInfo('ScoreFile: ' + ScoreFile);
}

end;

//-------------
// GetLanguage - Get Language ID from saved String Information
//-------------
function TCMDParams.GetLanguage: integer;
{var
  I: integer;
}
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
function TCMDParams.GetResolution: integer;
{var
  I: integer;
}
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
