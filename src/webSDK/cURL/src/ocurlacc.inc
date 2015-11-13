(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFDEF CURLOBJ_INTERFACE}
  function FileIsReadable(const FN:string):boolean;   // File exists and is readable.
  function FileIsWriteable(const FN:string):boolean;  // File can be written and/or created.
  function GetFileSize(const FN:string):LongInt;      // File size in bytes or -1 on error.
{$ENDIF CURLOBJ_INTERFACE}

{$IFDEF CURLOBJ_IMPLEMENTATION}

{$IFDEF LINUX}

{$IFDEF FPC}
  // workaround for FPC "uninitialized variable" complaint (use "out" instead of "var" )
  function stat(fn:pChar; out rec:_stat):longint; cdecl; external 'c' name 'stat';
{$ENDIF}


function FileIsReadable(const FN:string):Boolean;
var
  R:tStatBuf;
begin
  Result:= ( stat( pChar(FN), R ) = 0 ) and S_ISREG(R.st_mode ) and ( access(pChar(FN), R_OK) = 0 );
end;


function GetFileSize(const FN:string):LongInt;
var 
  R:tStatBuf;
begin
  if ( stat( pChar(FN), R ) = 0 ) then GetFileSize:=R.st_size else GetFileSize:=-1;
end;


function FileIsWriteable(const FN:string):boolean;
var 
  R:tStatBuf;
  DN:string;
begin
  if ( FN <> '' ) then begin
    if ( stat( pChar(FN), R ) = 0 ) 
    then Result:= S_ISREG(R.st_mode ) and ( access(pChar(FN), W_OK) = 0 )
    else begin
      DN:=dirname(pChar(FN));
      if DN='' then DN:='.';
      Result:=( stat( pChar(DN), R ) = 0 ) and S_ISDIR(R.st_mode ) and ( access(pChar(DN), W_OK) = 0 );
    end;
  end else Result:=False;
end;

{$ELSE} 
(* **********    WIN32   ********** *)

{$HINTS OFF} // Yes, Delphi - I know they are platform specific - didn't you read my ifdef ???
const
  faReadOnly=SysUtils.faReadOnly;
  faVolumeID=SysUtils.faVolumeID;
{$HINTS ON}

function FileIsWriteable(const FN:string):boolean;
var
  R:tSearchRec;
  FH:text;
begin
  if FileExists(FN) then begin
    R.Size:=0;
    FileIsWriteable:= FindFirst(FN, faAnyFile, R) = 0;
    if Result  then Result:=( ( R.Attr and faReadOnly ) <= 0 );
    SysUtils.FindClose(R);
  end else begin
    { Hell, I don't know - just try it and find out ... }
    ioresult;
    {$I-}
    AssignFile(FH, FN);
    Rewrite(FH);
    CloseFile(FH);
    Erase(FH);
    Result:=ioResult=0;
    {$I+}
  end;
end;

function FileIsReadable(const FN:string):boolean;
var
  R:TSearchRec;
begin
  R.Size:=0;
  Result:= ( FindFirst(FN, faAnyFile, R) = 0 )
  and ( ( R.Attr and faDirectory ) = 0 ) and ( ( R.Attr and faVolumeID ) = 0 );
  SysUtils.FindClose(R);
end;

function GetFileSize(const FN:string):LongInt;
var
  R:TSearchRec;
begin
  R.Size:=0;
  if ( FindFirst(FN, faAnyFile, R) = 0 )
  then Result:=R.size else Result:=-1;
  SysUtils.FindClose(R);
end;

{$ENDIF LINUX}

{$ENDIF CURLOBJ_IMPLEMENTATION}
