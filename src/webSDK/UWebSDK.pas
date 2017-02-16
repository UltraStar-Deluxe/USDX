unit UWebSDK;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

type
  // Website
  TWebsiteInfo = record
    Name:  array [0..30] of char;
    ID: integer;
  end;

  TSendInfo = record
      Username:   UTF8String;   // Username & name of the player
      Password:   UTF8String;   // Password
      ScoreInt:       integer;  // Player's Score Int
      ScoreLineInt:   integer;  // Player's Score Line
      ScoreGoldenInt: integer;  // Player's Score Golden
      MD5Song:        string;   // Song Hash
      Level:      byte;         // Level (0- Easy, 1- Medium, 2- Hard)
  end;

  TLoginInfo = record
      Username:   UTF8String;   // Username
      Password:   UTF8String;   // Password
  end;

  pModi_WebsiteInfo = procedure (var Info: TWebsiteInfo);
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  fModi_SendScore = function (SendInfo: TSendInfo): integer;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  fModi_EncryptScore = function (SendInfo: TSendInfo): widestring;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  fModi_Login = function (LoginInfo: TLoginInfo): byte;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  fModi_EncryptPassword = function (LoginInfo: TLoginInfo): widestring;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  fModi_DownloadScore = function (ListMD5Song: widestring; Level: byte): widestring;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  fModi_VerifySong = function (MD5Song: widestring): widestring;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

implementation

end.
