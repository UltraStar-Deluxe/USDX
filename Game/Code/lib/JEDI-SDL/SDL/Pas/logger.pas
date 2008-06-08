unit logger;
{
  $Id: logger.pas,v 1.2 2006/11/26 16:58:04 savage Exp $

}
{******************************************************************************}
{                                                                              }
{                Error Logging Unit                                            }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominique Louis.                                   }
{                                                                              }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   Logging functions...                                                       }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{               2001 - DL : Initial creation                                   }
{         25/10/2001 - DRE : Added $M+ directive to allow published            }
{                                in classes. Added a compile directive         }
{                                around fmShareExclusive as this does not      }
{                                exist in Free Pascal                          }
{                                                                              }
{******************************************************************************}
{
  $Log: logger.pas,v $
  Revision 1.2  2006/11/26 16:58:04  savage
  Modifed to create separate log files. Therefore each instance running from the same directory will have their own individual log file, prepended with a number.

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release

  
}

{$I jedi-sdl.inc}

{$WEAKPACKAGEUNIT OFF}

interface

uses
  Classes,
  SysUtils;

type
  TLogger = class
  private
    FFileHandle : TextFile;
    FApplicationName : string;
    FApplicationPath : string;
  protected

  public
    constructor Create;
    destructor Destroy; override;
    function GetApplicationName: string;
    function GetApplicationPath: string;
    procedure LogError( ErrorMessage : string; Location : string );
    procedure LogWarning( WarningMessage : string; Location : string );
    procedure LogStatus( StatusMessage : string; Location : string );
  published
    property ApplicationName : string read GetApplicationName;
    property ApplicationPath : string read GetApplicationPath;
  end;

var
  Log : TLogger;

implementation

{ TLogger }
constructor TLogger.Create;
var
  FileName : string;
  FileNo : integer;
begin
  FApplicationName := ExtractFileName( ParamStr(0) );
  FApplicationPath := ExtractFilePath( ParamStr(0) );
  FileName := FApplicationPath + ChangeFileExt( FApplicationName, '.log' );
  FileNo := 0;
  while FileExists( FileName ) do
  begin
    inc( FileNo );
    FileName := FApplicationPath + IntToStr( FileNo ) + ChangeFileExt( FApplicationName, '.log' )
  end;
  AssignFile( FFileHandle, FileName );
  ReWrite( FFileHandle );
  (*inherited Create( FApplicationPath + ChangeFileExt( FApplicationName, '.log' ),
                    fmCreate {$IFNDEF FPC}or fmShareExclusive{$ENDIF} );*)
end;

destructor TLogger.Destroy;
begin
  CloseFile( FFileHandle );
  inherited;
end;

function TLogger.GetApplicationName: string;
begin
  result := FApplicationName;
end;

function TLogger.GetApplicationPath: string;
begin
  result := FApplicationPath;
end;

procedure TLogger.LogError(ErrorMessage, Location: string);
var
  S : string;
begin
  S := '*** ERROR *** : @ ' + TimeToStr(Time) + ' MSG : ' + ErrorMessage + ' IN : ' + Location + #13#10;
  WriteLn( FFileHandle,  S );
  Flush( FFileHandle );
end;

procedure TLogger.LogStatus(StatusMessage, Location: string);
var
  S : string;
begin
  S := 'STATUS INFO : @ ' + TimeToStr(Time) + ' MSG : ' + StatusMessage + ' IN : ' + Location + #13#10;
  WriteLn( FFileHandle,  S );
  Flush( FFileHandle );
end;

procedure TLogger.LogWarning(WarningMessage, Location: string);
var
  S : string;
begin
  S := '=== WARNING === : @ ' + TimeToStr(Time) + ' MSG : ' + WarningMessage + ' IN : ' + Location + #13#10;
  WriteLn( FFileHandle,  S );
  Flush( FFileHandle );
end;

initialization
begin
  Log := TLogger.Create;
  Log.LogStatus( 'Starting Application', 'Initialization' );
end;

finalization
begin
  Log.LogStatus( 'Terminating Application', 'Finalization' );
  Log.Free;
  Log := nil;
end;

end.
 