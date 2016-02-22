(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{ THREADED TCURL IMPLEMENTATION }


constructor TCurlBase.Create(aOwner:tComponent); 
begin
  inherited Create(aOwner);
  fBusy:=False;
  fThreaded:=False;
  fWaitInterval:=1000;
  fWaitCallback:=nil;
  fOnWait:=nil;
  fWaitData:=nil;
end;


procedure TCurlBase.SetOnWait(aEvent:tNotifyEvent);
begin
  fWaitCallback:=nil;
  fOnWait:=aEvent;
end;

procedure TCurlBase.SetWaitCallback(aCallback:tCurlWaitCallback);
begin
  fOnWait:=nil;
  fWaitCallback:=aCallback;
end;

procedure TCurlBase.SetWaitInterval(ms:Longint);
begin
  if ( ms >= 0 ) then fWaitInterval:=ms else fWaitInterval:=1000;
end;




{$IFDEF WIN32}
  procedure CurlThreadExecute(o:pointer); cdecl;
  begin
    with TCurl(o) do begin
      fThdResult:=curl_easy_perform(fCurl);
      fBusy:=False;
    end;
    _endthread();
  end;
{$ELSE WIN32}
  function CurlThreadExecute(o:pointer):LongInt; cdecl;
  begin
    with TCurl(o) do begin 
      fThdResult:=curl_easy_perform(fCurl);
      fBusy:=False;
    end;
    Result:=0;
  end;
{$ENDIF WIN32}

function tCurl.DoPerform(aCurl:pCurl):CurlCode;
const
  ERR_MSG='Transfer already in progress, cannot re-enter thread.';
begin
  if fThreaded then begin
    if ( fBusy ) then begin
      Result:=CURLE_COULDNT_CONNECT;
      SetError(Result, ERR_MSG);
      {$IFDEF CURL_COMPONENT}
      RAISE Exception.Create(ERR_MSG);
      {$ENDIF}
      EXIT;
    end;
    fThdResult:=fCurlResult;
    fBusy:=True;
    {$IFDEF WIN32}
      fThread:=_beginthread( @CurlThreadExecute,0, self );
      while (fBusy) do begin
        Windows.Sleep(fWaitInterval);
        if Assigned(fOnWait) and ( {$IFNDEF FPC}@{$ENDIF}fOnWait <> nil ) then fOnWait(self)
        else if Assigned(fWaitCallback) and ( {$IFNDEF FPC}@{$ENDIF}fWaitCallback <> nil ) then fWaitCallback(fWaitData);
      end;
      CloseHandle(fThread);
    {$ELSE WIN32}
      pthread_mutex_init({$IFDEF FPC}@{$ENDIF}fMutex, nil);
      pthread_create({$IFDEF FPC}@{$ENDIF}fThread, nil, {$IFDEF FPC}@{$ENDIF}CurlThreadExecute, self);
      while (fBusy) do begin 
        usleep(fWaitInterval * 1000);
        if Assigned(fOnWait) and ( {$IFNDEF FPC}@{$ENDIF}fOnWait <> nil ) then begin 
          pthread_mutex_lock(fMutex);
          fOnWait(self);
          pthread_mutex_unlock(fMutex);
        end else if Assigned(fWaitCallback) and ( {$IFNDEF FPC}@{$ENDIF}fWaitCallback <> nil ) then begin
          pthread_mutex_lock(fMutex);
          fWaitCallback(fWaitData);
          pthread_mutex_unlock(fMutex);
        end;
      end;
      pthread_mutex_destroy({$IFDEF FPC}@{$ENDIF}fMutex);
    {$ENDIF}
    Result:=fThdResult;
  end else begin 
    fBusy:=True;
    Result:=curl_easy_perform(aCurl);
    fBusy:=False;
  end;
end;



procedure tCurl.MutexLock;
begin
  {$IFNDEF WIN32}
    if fThreaded then pthread_mutex_lock(fMutex);
  {$ENDIF}
end;

procedure tCurl.MutexUnlock;
begin
  {$IFNDEF WIN32}
    if fThreaded then pthread_mutex_unlock(fMutex);
  {$ENDIF}
end;



function tCurl.InitTransfer:CurlCode;
begin
  SetListOption(CURLOPT_HTTPHEADER, fHttpHdr);
  SetListOption(CURLOPT_QUOTE, fQuote);
  SetListOption(CURLOPT_POSTQUOTE, fPostQuote);
  SetListOption(CURLOPT_PREQUOTE, fPreQuote);
  SetListOption(CURLOPT_TELNETOPTIONS, fTelnetOptions);
  SetListOption(CURLOPT_HTTP200ALIASES, fHttp200Aliases);
  SetListOption(CURLOPT_SOURCE_QUOTE, fSourceQuote);
  SetListOption(CURLOPT_SOURCE_POSTQUOTE, fSourcePostQuote);
  SetListOption(CURLOPT_SOURCE_PREQUOTE, fSourcePreQuote);
  SetCookieListOption;
  if ( fFormData <> nil ) and ( fFormData.PostPtr <> nil ) then begin
    case fFormData.Result of
     CURLE_OK: fHttpPost:=fFormData.PostPtr;
     CURLE_READ_ERROR: SetError(CURLE_READ_ERROR, 'Unable to open POST input file');
     else fCurlResult:= fFormData.Result;
    end;
  end;
  SetHttpPostOption;
  if ( fUpload or fPut ) then InitInputFile else InitOutputFile;
  InitHeaderFile;
  if ( fUrl = nil ) or ( fUrl^ = #0 ) then SetError(CURLE_URL_MALFORMAT, 'NULL or empty URL.');
  Result:=fCurlResult;
end;

procedure tCurl.DoneTransfer(aCode:CurlCode);
var
  tmpCode:CurlCode;
begin
  tmpCode:=CURLE_OK;

  with fReceiveScheme do if ( fs_type = FST_FILENAME ) and ( stream <> nil ) then begin
    if ( fclose(stream) <> 0 ) then tmpCode:=CURLE_WRITE_ERROR;
    stream:=nil;
  end;

  with fHeaderScheme do begin
    if ( ( fs_type = FST_FILENAME ) and ( stream <> nil ) )
    or ( ( fs_type = FST_INTERNAL ) and ( not ( fNoBody or fHeader )  ) and ( stream <> nil ) )
    then begin
      if ( fclose(stream) <> 0 ) then tmpCode:=CURLE_WRITE_ERROR;
      stream:=nil;
    end;
  end;

  with fTransmitScheme do if ( fs_type = FST_FILENAME ) and ( stream <> nil ) then begin
    if ( fclose(stream) <> 0 ) then tmpCode:=CURLE_READ_ERROR;
    stream:=nil;
  end;

  if ( aCode = CURLE_OK ) and ( tmpCode <> CURLE_OK )
  then fCurlResult:=tmpCode
  else if ( fCurlResult <> aCode ) then fCurlResult:=aCode;

  UpdateCookieList;
end;
