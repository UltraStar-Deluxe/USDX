unit registryuserpreferences;
{
  $Id: registryuserpreferences.pas,v 1.1 2004/09/30 22:35:47 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{             Wrapper class for Windows Register and INI Files                 }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominqiue Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominqiue Louis.                                   }
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
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   September   23 2004 - DL : Initial Creation                                }
{
  $Log: registryuserpreferences.pas,v $
  Revision 1.1  2004/09/30 22:35:47  savage
  Changes, enhancements and additions as required to get SoAoS working.


}
{******************************************************************************}

interface

uses
  {$IFDEF REG}
  Registry,
  {$ELSE}
  IniFiles,
  {$ENDIF}
  Classes,
  userpreferences;

type
  TRegistryUserPreferences = class( TUserPreferences )
  private

  protected
    function GetSection( const Index : Integer ) : string; virtual; abstract;
    function GetIdentifier( const Index : Integer ) : string; virtual; abstract;
    function GetDefaultBoolean( const Index : Integer ) : Boolean; override;
    function GetBoolean( const Index : Integer ) : Boolean; override;
    procedure SetBoolean( const Index : Integer; const Value : Boolean ); override;
    function GetDefaultDateTime( const Index : Integer ) : TDateTime; override;
    function GetDateTime( const Index : Integer ) : TDateTime; override;
    procedure SetDateTime( const Index : Integer; const Value : TDateTime ); override;
    function GetDefaultInteger( const Index : Integer ) : Integer; override;
    function GetInteger( const Index : Integer ) : Integer; override;
    procedure SetInteger( const Index : Integer; const Value : Integer ); override;
    function GetDefaultFloat( const Index : Integer ) : single; override;
    function GetFloat( const Index : Integer ) : single; override;
    procedure SetFloat( const Index : Integer; const Value : single ); override;
    function GetDefaultString( const Index : Integer ) : string; override;
    function GetString( const Index : Integer ) : string; override;
    procedure SetString( const Index : Integer; const Value : string ); override;
  public
    Registry : {$IFDEF REG}TRegIniFile{$ELSE}TIniFile{$ENDIF};
    constructor Create( const FileName : string = '' ); reintroduce;
    destructor Destroy; override;
    procedure Update; override;
  end;

implementation

uses
  SysUtils;

{ TRegistryUserPreferences }
constructor TRegistryUserPreferences.Create( const FileName : string );
var
  defFileName : string;
begin
  inherited Create;

  if FileName <> '' then
    defFileName := FileName
  else
    defFileName := ChangeFileExt( ParamStr( 0 ), '.ini' );

  Registry := {$IFDEF REG}TRegIniFile{$ELSE}TIniFile{$ENDIF}.Create( defFileName );
end;

destructor TRegistryUserPreferences.Destroy;
begin
  Update;
  Registry.Free;
  Registry := nil;
  inherited;
end;

function TRegistryUserPreferences.GetBoolean( const Index : Integer ) : Boolean;
begin
  Result := Registry.ReadBool( GetSection( Index ), GetIdentifier( Index ), GetDefaultBoolean( Index ) );
end;

function TRegistryUserPreferences.GetDateTime( const Index : Integer ): TDateTime;
begin
  Result := Registry.ReadDateTime( GetSection( Index ){$IFNDEF REG}, GetIdentifier( Index ), GetDefaultDateTime( Index ){$ENDIF} );
end;

function TRegistryUserPreferences.GetDefaultBoolean( const Index : Integer ) : Boolean;
begin
  result := false;
end;

function TRegistryUserPreferences.GetDefaultDateTime( const Index: Integer ) : TDateTime;
begin
  result := Now;
end;

function TRegistryUserPreferences.GetDefaultFloat( const Index: Integer ) : single;
begin
  result := 0.0;
end;

function TRegistryUserPreferences.GetDefaultInteger(const Index : Integer ) : Integer;
begin
  result := 0;
end;

function TRegistryUserPreferences.GetDefaultString( const Index : Integer ) : string;
begin
  result := '';
end;

function TRegistryUserPreferences.GetFloat( const Index : Integer ): single;
begin
  Result := Registry.ReadFloat( GetSection( Index ){$IFNDEF REG}, GetIdentifier( Index ), GetDefaultFloat( Index ){$ENDIF} );
end;

function TRegistryUserPreferences.GetInteger( const Index : Integer ) : Integer;
begin
  Result := Registry.ReadInteger( GetSection( Index ), GetIdentifier( Index ), GetDefaultInteger( Index )  );
end;

function TRegistryUserPreferences.GetString( const Index : Integer ): string;
begin
  Result := Registry.ReadString( GetSection( Index ), GetIdentifier( Index ), GetDefaultString( Index ) );
end;

procedure TRegistryUserPreferences.SetBoolean( const Index : Integer; const Value : Boolean );
begin
  Registry.WriteBool( GetSection( Index ), GetIdentifier( Index ), Value );
  inherited;
end;

procedure TRegistryUserPreferences.SetDateTime( const Index: Integer; const Value: TDateTime );
begin
  Registry.WriteDateTime( GetSection( Index ){$IFNDEF REG}, GetIdentifier( Index ){$ENDIF}, Value );
  inherited;
end;

procedure TRegistryUserPreferences.SetFloat(const Index: Integer; const Value: single);
begin
  Registry.WriteFloat( GetSection( Index ){$IFNDEF REG}, GetIdentifier( Index ){$ENDIF}, Value );
  inherited;
end;

procedure TRegistryUserPreferences.SetInteger( const Index, Value : Integer );
begin
  Registry.WriteInteger( GetSection( Index ), GetIdentifier( Index ), Value );
  inherited;
end;

procedure TRegistryUserPreferences.SetString( const Index : Integer; const Value : string );
begin
  Registry.WriteString( GetSection( Index ), GetIdentifier( Index ), Value );
  inherited;
end;

procedure TRegistryUserPreferences.Update;
begin
  {$IFDEF REG}
  Registry.CloseKey;
  {$ELSE}
  Registry.UpdateFile;
  {$ENDIF}
end;

end.
