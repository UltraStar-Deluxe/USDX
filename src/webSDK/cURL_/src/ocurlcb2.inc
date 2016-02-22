(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{ ErrorBuffer, and callbacks for DEBUG, PROGRESS, SSLCTX, and IOCTL }



{******  PROGRESS FUNCTION  *******}
function DoProgress( pClient:tCurl; dltotal, dlnow, ultotal, ulnow:double ): LongInt; cdecl;
var
  bContinue:Boolean;
begin
  tCurl(pClient).MutexLock();
  bContinue:=True;
  with pClient do
  if ( @fOnProgress <> nil ) then  begin
    if (ulTotal+ulNow) = 0
    then fOnProgress(pClient, LongInt(round(dlTotal)), LongInt(round(dlNow)), bContinue)
    else fOnProgress(pClient, LongInt(round(ulTotal)), LongInt(round(ulNow)), bContinue);
  end;
  if bContinue then DoProgress:=0 else DoProgress:=1;
  tCurl(pClient).MutexUnlock();
end;

procedure tCurl.SetOnProgress(aProc:tCurlProgressEvent);
begin
  fOnProgress:=aProc;
  if ( FuncPtr(@aProc) <> nil ) then begin
    SetResultCode(curl_easy_setopt(fCurl, CURLOPT_PROGRESSFUNCTION, @DoProgress), CURLOPT_PROGRESSFUNCTION);
    SetResultCode(curl_easy_setopt(fCurl, CURLOPT_PROGRESSDATA, pointer(Self)), CURLOPT_PROGRESSDATA);
    SetResultCode(curl_easy_setopt(fCurl, CURLOPT_NOPROGRESS, LongInt(False)), CURLOPT_NOPROGRESS);
  end else begin
    SetResultCode(curl_easy_setopt(fCurl, CURLOPT_PROGRESSFUNCTION, nil), CURLOPT_PROGRESSFUNCTION);
    SetResultCode(curl_easy_setopt(fCurl, CURLOPT_NOPROGRESS,  LongInt(True)), CURLOPT_NOPROGRESS);
  end;
end;

procedure tCurl.SetProgressFunction( const aFunc:curl_progress_callback);
begin
  fProgressFunction:=aFunc;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_PROGRESSFUNCTION, FuncPtr(@aFunc)), CURLOPT_PROGRESSFUNCTION);
end;

procedure tCurl.SetProgressData(aData:pointer);
begin
  fProgressData:=aData;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_PROGRESSDATA, fProgressData), CURLOPT_PROGRESSDATA);
end;


{****** INFORMATION ******}
function DoDebug(handle: pCurl; infotype: curl_infotype;
                   data:pChar; size:LongInt; pClient:pointer):longint; cdecl;
var
  bContinue:boolean;
  i:LongInt;
  info_str:pChar;
  info_size:LongInt;
begin
  if  ( Handle <> nil ) then {do nothing - compiler shut up!};
  bContinue:=True;
  with tCurl(pClient) do
  if ( @fOnDebug <> nil ) then begin
    {$IFDEF CURL_COMPONENT}
    if ( csDestroying in ComponentState ) then begin
      Result:=0;
      EXIT;
    end;
    {$ENDIF}
    tCurl(pClient).MutexLock();
    if ( InfoType = CURLINFO_TEXT ) then begin
      info_size:=size + 1;
      info_str:=StrAlloc(info_size);
      FillChar(info_str[0], info_size, #0);
      for i:=0 to size-1 do if ( data[i] in [ #13, #10 ] ) then begin
          info_str[i]:=#0;
          dec(info_size);
      end else info_str[i]:=data[i];
      if ( StrLen(info_str) > 0 ) then fOnDebug(tCurl(pClient), infotype, info_str, info_size, bContinue);
      StrDispose(info_str);
    end else fOnDebug(tCurl(pClient), infotype, data, size, bContinue);
    tCurl(pClient).MutexUnlock();
  end;
  if bContinue then Result:=0 else Result:=-1;
end;


procedure tCurl.SetOnDebug(aProc:tCurlDebugEvent);
begin
  fOnDebug:=aProc;
  if ( FuncPtr(@aProc) <> nil ) then begin
   SetResultCode(curl_easy_setopt(fCurl, CURLOPT_DEBUGFUNCTION, @DoDebug), CURLOPT_DEBUGFUNCTION);
   SetResultCode(curl_easy_setopt(fCurl, CURLOPT_DEBUGDATA, pointer(Self)), CURLOPT_DEBUGDATA);
   SetResultCode(curl_easy_setopt(fCurl, CURLOPT_VERBOSE, LongInt(True)), CURLOPT_VERBOSE);
  end else begin
    SetResultCode(curl_easy_setopt(fCurl, CURLOPT_DEBUGFUNCTION, nil), CURLOPT_DEBUGFUNCTION);
    SetResultCode(curl_easy_setopt(fCurl, CURLOPT_VERBOSE, LongInt(False)), CURLOPT_VERBOSE);
  end;
end;

procedure tCurl.SetDebugFunction(aFunc:curl_debug_callback);
begin
  fDebugFunction:=aFunc;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_DEBUGFUNCTION, FuncPtr(@aFunc)), CURLOPT_DEBUGFUNCTION);
end;

procedure tCurl.SetDebugData(aData:pointer);
begin
  fDebugData:=aData;
  if ( FuncPtr(@fOnDebug) <> nil ) then SetOnDebug(nil);
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_DEBUGDATA, fDebugData), CURLOPT_DEBUGDATA);
end;

{****** SSLCTX ******}

procedure tCurl.SetSslCtxFunction(value: curl_ssl_ctx_callback);
var
  dummy:pointer;
begin
  fSslCtxFunction:=value;
  SetPtrOption(CURLOPT_SSL_CTX_FUNCTION, dummy, FuncPtr(@value));
end;


procedure tCurl.SetSslCtxData(value: pointer);
begin
  SetPtrOption(CURLOPT_SSL_CTX_DATA, fSslCtxData, value);
end;

{****** IOCTL ******}

procedure tCurl.SetIoCtlCallback(value: curl_ioctl_callback);
var
  dummy:pointer;
begin
  fIoCtlCallback:=value;
  SetPtrOption(CURLOPT_IOCTLFUNCTION, dummy, FuncPtr(@value));
end;


procedure tCurl.SetIoCtlData(value: pointer);
begin
  SetPtrOption(CURLOPT_IOCTLDATA, fIoCtlData, value);
end;


{****** ERROR *******}

procedure tCurl.SetResultCode(aCode:CurlCode; aOption: CURLoption);
var
  strOpt:string;
begin
  strOpt:='';
  if ( fCurlResult = CURLE_OK ) and ( aCode <> CURLE_OK ) then begin
    fCurlResult:=aCode;
    str(LongInt(aOption), strOpt);
    SetError(aCode, 'Error: ' + curl_easy_strerror(aCode) + ' setting option #' + strOpt);
  end;
end;

procedure tCurl.SetError(code:CurlCode; const msg:string);
begin
  fCurlResult:=code;
  StrLCopy(fErrorBuffer, pChar(msg), CURL_ERROR_SIZE-1);
end;


{****** COOKIE LIST ******}

function DoListCookies(Sender:tObject; const Cookie:ansistring; UserData:pointer):boolean;
begin
  Result:=True;
  if ( {$IFNDEF FPC}@{$ENDIF}tCurl(UserData).fOnListCookies <> nil ) 
  then tCurl(UserData).fOnListCookies(TObject(UserData), Cookie, Result) else Result:=False;
end;

procedure tCurl.ListCookies;
begin
  if ( fCookieList <> nil ) and ( {$IFNDEF FPC}@{$ENDIF}fOnListCookies <> nil )  
  then fCookieList.ForEachCookie({$IFDEF FPC}@{$ENDIF}DoListCookies, self);
end;

