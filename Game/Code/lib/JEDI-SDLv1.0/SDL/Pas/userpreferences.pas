unit userpreferences;
{
  $Id: userpreferences.pas,v 1.1 2004/09/30 22:35:47 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{                 Base Class for User Preferences                              }
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
  $Log: userpreferences.pas,v $
  Revision 1.1  2004/09/30 22:35:47  savage
  Changes, enhancements and additions as required to get SoAoS working.


}
{******************************************************************************}

interface

uses
  Classes;

type
  TUserPreferences = class
  private
    FAutoSave: Boolean;
    procedure CheckAutoSave;
  protected
    function GetDefaultBoolean( const Index : Integer ) : Boolean; virtual; abstract;
    function GetBoolean( const Index : Integer ) : Boolean; virtual; abstract;
    procedure SetBoolean( const Index : Integer; const Value : Boolean ); virtual;
    function GetDefaultDateTime( const Index : Integer ) : TDateTime; virtual; abstract;
    function GetDateTime( const Index : Integer ) : TDateTime; virtual; abstract;
    procedure SetDateTime( const Index : Integer; const Value : TDateTime ); virtual;
    function GetDefaultInteger( const Index : Integer ) : Integer; virtual; abstract;
    function GetInteger( const Index : Integer ) : Integer; virtual; abstract;
    procedure SetInteger( const Index : Integer; const Value : Integer ); virtual;
    function GetDefaultFloat( const Index : Integer ) : single; virtual; abstract;
    function GetFloat( const Index : Integer ) : single; virtual; abstract;
    procedure SetFloat( const Index : Integer; const Value : single ); virtual;
    function GetDefaultString( const Index : Integer ) : string; virtual; abstract;
    function GetString( const Index : Integer ) : string; virtual; abstract;
    procedure SetString( const Index : Integer; const Value : string ); virtual;
    function GetDefaultBinaryStream( const Index : Integer ) : TStream; virtual; abstract;
    function GetBinaryStream( const Index : Integer ) : TStream; virtual; abstract;
    procedure SetBinaryStream( const Index : Integer; const Value : TStream ); virtual;
  public
    procedure Update; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    property AutoSave : Boolean read FAutoSave write FAutoSave;
  end;

implementation

{ TUserPreferences }
procedure TUserPreferences.CheckAutoSave;
begin
  if FAutoSave then
    Update;
end;

constructor TUserPreferences.Create;
begin
  inherited;
  FAutoSave := false;
end;

destructor TUserPreferences.Destroy;
begin

  inherited;
end;

procedure TUserPreferences.SetBinaryStream( const Index : Integer; const Value : TStream );
begin
  CheckAutoSave;
end;

procedure TUserPreferences.SetBoolean(const Index: Integer; const Value: Boolean);
begin
  CheckAutoSave;
end;

procedure TUserPreferences.SetDateTime(const Index: Integer; const Value: TDateTime);
begin
  CheckAutoSave;
end;

procedure TUserPreferences.SetFloat(const Index: Integer; const Value: single);
begin
  CheckAutoSave;
end;

procedure TUserPreferences.SetInteger(const Index, Value: Integer);
begin
  CheckAutoSave;
end;

procedure TUserPreferences.SetString(const Index: Integer; const Value: string);
begin
  CheckAutoSave;
end;

end.
