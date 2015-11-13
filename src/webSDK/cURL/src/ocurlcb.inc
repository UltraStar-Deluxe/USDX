(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{ CALLBACKS AND DATA POINTERS FOR FILE I/O }

const
  stdin  = 0;
  stdout = 1;


function FuncPtr( const aPtr:pPointer):pointer; {$IFDEF FPC} inline; {$ENDIF}
begin
 // Function pointers in FreePascal's OBJFPC mode are different than Borland's - 
// This function cleans up all the {$IFDEF FPC}@{$ENDIF} stuff...
  {$IFDEF FPC}
    Result:=aPtr^;
  {$ELSE}
    Result:=aPtr;
  {$ENDIF}
end;

{******** RESPONSE HEADERS ********}
function DoHeader(ptr: pchar; size, nmemb:size_t; pClient:tCurl):size_t; cdecl;
var
  bContinue:Boolean;
  I:LongWord;
  Hdr:string;
begin
  pClient.MutexLock();
  bContinue:=True;
  Result:= ( size * nmemb );
  with pClient,fHeaderScheme do begin
    if ( @hdr_event <> nil ) then begin
       SetLength(Hdr, Result);
       Hdr[Result]:=#0;
       for I:=0 to Result-1 do if ( ptr[I] in [#10, #13] ) then Hdr[I+1]:=#0 else Hdr[I+1]:=ptr[I];
       SetLength(Hdr, StrLen(@Hdr[1]));
       hdr_event(tCurl(pClient), Hdr, bContinue);
       if bContinue and ( fs_type = FST_FILENAME ) then Result:=size * fwrite(ptr, size, nmemb, stream);
    end;
  end;
  if not bContinue then Result:=0;
  pClient.MutexUnlock();
end;


procedure tCurl.SetOnHeader(aProc:tCurlHeaderEvent);
begin
  fHeaderScheme.hdr_event:=aProc;
  with fHeaderScheme do if ( FuncPtr(@aProc) <> nil ) 
  then cb_type:=CBT_EVENT
  else if ( cb_type = CBT_EVENT ) then cb_type:=CBT_INTERNAL;
end;

procedure tCurl.SetHeaderFunction(aHeaderFunc: curl_write_callback);
begin
  fHeaderScheme.callback:=aHeaderFunc;
  with fHeaderScheme do if ( FuncPtr(@aHeaderFunc) <> nil ) 
  then cb_type:= CBT_CALLBACK
  else if ( cb_type = CBT_CALLBACK ) then cb_type:=CBT_INTERNAL;
end;

procedure tCurl.SetHeaderStream(value: pointer);
begin
  fHeaderScheme.stream:=value;
  if ( value <> nil ) 
  then fHeaderScheme.fs_type:= FST_STREAM
  else if ( fHeaderScheme.fs_type = FST_STREAM )
       then fHeaderScheme.fs_type:= FST_INTERNAL;
end;


procedure tCurl.SetHeaderFile(value:string);
begin
  SetStringProp(fHeaderScheme.filename, value);
  if ( value <> '' ) 
  then fHeaderScheme.fs_type:= FST_FILENAME
  else if ( fHeaderScheme.fs_type = FST_FILENAME )
       then fHeaderScheme.fs_type:= FST_INTERNAL;
end;


procedure tCurl.InitHeaderFile;
var
  TmpFunc, TmpStrm:pointer;
begin
  TmpFunc:=nil;
  TmpStrm:=nil;
  with fHeaderScheme do case cb_type of
    CBT_CALLBACK: begin
      TmpFunc:=FuncPtr(@callback);
      TmpStrm:=stream;
    end;
    CBT_EVENT: begin
      TmpFunc:=@DoHeader;
      TmpStrm:=Self;
      if ( fs_type = FST_FILENAME ) then begin 
        stream:=fopen( filename, 'w'#0 );
        if ( stream = nil ) then SetError(CURLE_WRITE_ERROR, 'Error writing headers to local file');
      end;
    end;
    CBT_INTERNAL: begin
      TmpFunc:=@fwrite;
      case fs_type of 
        FST_STREAM: begin
          TmpStrm:=stream;
          if ( TmpStrm = nil ) then SetError(CURLE_WRITE_ERROR, 'Invalid header stream for internal callback');
        end;
        FST_FILENAME: begin
          TmpStrm:=fopen( filename, 'w'#0 );
          stream:=TmpStrm;
          if ( TmpStrm = nil ) then SetError(CURLE_WRITE_ERROR, 'Error writing headers to local file');
        end;
        FST_INTERNAL: begin
          if ( fNoBody or fHeader ) then begin 
            TmpStrm:=fdopen(stdout, 'w'#0);
            if ( TmpStrm = nil ) then SetError(CURLE_WRITE_ERROR, 'Error writing headers to standard output');
          end else begin 
              //  TmpStrm:=fopen(pChar(CURL_NULL_FILE), 'w'#0);
              //  stream:=TmpStrm;
              {     <<== Revised 2005-09-29: 
                         Don't waste a file descriptor here, set HeaderFunction to nil instead. ==>> }
              TmpFunc:=nil;
              TmpStrm:=nil;
              stream:=nil;
          end;
        end;
      end 
    end;
  end;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_HEADERFUNCTION, TmpFunc), CURLOPT_HEADERFUNCTION);
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_WRITEHEADER, TmpStrm), CURLOPT_WRITEHEADER);
end;


{******  INCOMING DATA  *******}
function DoReceive (ptr: pointer; size, nmemb:LongWord; pClient:tCurl):LongWord; cdecl;
var
  bContinue:Boolean;
begin
  pClient.MutexLock();
  bContinue:=True;
  Result:= ( size * nmemb );
  with pClient, fReceiveScheme do begin
   if ( @rx_event <> nil ) then rx_event(pClient, ptr, Result, bContinue);
   if bContinue and ( fs_type =FST_FILENAME ) then Result:=size * fwrite(ptr, size, nmemb, stream);
  end;
  if not bContinue then Result:=0;
  pClient.MutexUnlock();
end;

procedure tCurl.SetOnReceive(aProc:tCurlReceiveEvent);
begin
  fReceiveScheme.rx_event:=aProc;
  with fReceiveScheme do if ( FuncPtr(@aProc) <> nil ) 
  then cb_type:= CBT_EVENT
  else if ( cb_type = CBT_EVENT ) then cb_type:= CBT_INTERNAL;
end;

procedure tCurl.SetWriteFunction( aWriteFunc: curl_write_callback);
begin
  fReceiveScheme.callback:=aWriteFunc;
  with fReceiveScheme do if ( FuncPtr(@aWriteFunc) <> nil ) then begin 
    cb_type:=CBT_CALLBACK;
  end else if ( cb_type = CBT_CALLBACK ) then cb_type:=CBT_INTERNAL;
end;


procedure tCurl.SetOutputStream(value: pointer);
begin
  fReceiveScheme.stream:=value;
  with fReceiveScheme do if ( value <> nil ) then begin 
    fs_type:=FST_STREAM;
  end else if ( fs_type = FST_STREAM ) then fs_type:=FST_INTERNAL;
end;

procedure tCurl.SetOutputFile(value:string);
begin
  SetStringProp(fReceiveScheme.filename, value);
  with fReceiveScheme do if ( value <> '' ) 
  then fs_type:=FST_FILENAME
  else if ( fs_type = FST_FILENAME ) then fs_type:=FST_INTERNAL;
end;

procedure tCurl.InitOutputFile;
var
  TmpFunc, TmpStrm:pointer;
const
  ModeWrite: pChar = {$IFDEF WIN32}'wb'{$ELSE}'w'{$ENDIF}; { ... Write binary for win32 ! }
  ModeAppend: pChar = {$IFDEF WIN32}'ab'{$ELSE}'a'{$ENDIF};
begin
  TmpFunc:=nil;
  TmpStrm:=nil;
  with fReceiveScheme do case cb_type of 
    CBT_CALLBACK: begin 
      TmpFunc:=FuncPtr(@callback);
      TmpStrm:=stream;
    end;
    CBT_EVENT: begin 
      TmpFunc:=@DoReceive;
      TmpStrm:=Self;
      if ( fs_type = FST_FILENAME ) then begin 
        if ( fResumeFrom <= 0 ) 
        then stream:=fopen( filename, ModeWrite )
        else stream:=fopen( filename, ModeAppend ); {<- 'append' if resuming a download}
        if ( stream = nil ) then SetError(CURLE_WRITE_ERROR, 'Error writing to file');
      end;
    end;
    CBT_INTERNAL: begin
      TmpFunc:=@fwrite;
      case fs_type of 
        FST_STREAM: begin
          TmpStrm:=stream;
          if ( TmpStrm = nil ) then SetError(CURLE_WRITE_ERROR, 'Invalid receive stream for internal callback');
        end;
        FST_FILENAME: begin
          if ( fResumeFrom <= 0 )
          then TmpStrm:=fopen( filename, ModeWrite )
          else TmpStrm:=fopen( filename, ModeAppend ); {<- 'append' if resuming a download}
          stream:=TmpStrm;
          if ( TmpStrm = nil ) then SetError(CURLE_WRITE_ERROR, 'Error writing to local file');
        end;
        FST_INTERNAL: begin 
          TmpStrm:=fdopen(stdout, 'w'#0);
          if (TmpStrm = nil ) then SetError(CURLE_WRITE_ERROR, 'Error writing to standard output');
        end;
      end;
    end;
  end;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_WRITEFUNCTION, TmpFunc), CURLOPT_WRITEFUNCTION);
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_FILE, TmpStrm), CURLOPT_FILE);
end;


{******* OUTGOING DATA *******}
function DoTransmit (ptr: pointer; size, nmemb:LongWord; pClient:tCurl):LongWord; cdecl;
begin
  pClient.MutexLock();
  Result:=( size * nmemb );
  with pClient, fTransmitScheme do begin
    if ( @tx_event <> nil ) then tx_event(pClient, ptr, Result);
    if ( Result > 0 ) and ( fs_type = FST_FILENAME ) then Result:= fread(ptr, 1, Result, stream);
  end;
  pClient.MutexUnlock();
end;

procedure tCurl.SetOnTransmit(aProc:tCurlTransmitEvent);
begin
  fTransmitScheme.tx_event:=aProc;
  with fTransmitScheme do if ( FuncPtr(@aProc) <> nil ) 
  then cb_type:=CBT_EVENT
  else if ( cb_type = CBT_EVENT ) then cb_type:=CBT_INTERNAL;
end;

procedure tCurl.SetReadFunction( aReadFunc: curl_read_callback);
begin
  fTransmitScheme.callback:=aReadFunc;
  with fTransmitScheme do if ( FuncPtr(@aReadFunc) <> nil ) then begin 
    cb_type:=CBT_CALLBACK;
  end else if ( cb_type = CBT_CALLBACK) then cb_type:=CBT_INTERNAL;
end;

procedure tCurl.SetInputStream(value: pointer);
begin
  fTransmitScheme.stream:=value;
  with fTransmitScheme do if ( value <> nil ) then begin 
    fs_type:=FST_STREAM;
  end else if ( fs_type = FST_STREAM ) then fs_type:=FST_INTERNAL;
end;

procedure tCurl.SetInputFile(value:string);
begin
  SetStringProp(fTransmitScheme.filename, value);
  with fTransmitScheme do if ( value <> '' ) 
  then fs_type:=FST_FILENAME
  else if ( fs_type = FST_FILENAME ) then fs_type:=FST_INTERNAL;
end;

procedure tCurl.InitInputFile;
var
  TmpFunc, TmpStrm:pointer;

const
  ModeRead: pChar = {$IFDEF WIN32}'rb'{$ELSE}'r'{$ENDIF}; { ... Binary input for win32 ! }

begin
  TmpFunc:=nil;
  TmpStrm:=nil;
  with fTransmitScheme do case cb_type of
    CBT_CALLBACK: begin 
      TmpFunc:=FuncPtr(@callback);
      TmpStrm:=stream;
    end;
    CBT_EVENT: begin 
      TmpFunc:=@DoTransmit;
      TmpStrm:=Self;
      if ( fs_type = FST_FILENAME ) then begin 
        if ( fInFilesize < 0 ) then SetResultCode(curl_easy_setopt(fCurl, CURLOPT_INFILESIZE, GetFileSize(filename)), CURLOPT_INFILESIZE);
        stream:=fopen( filename, ModeRead );
        if (stream = nil ) then SetError(CURLE_READ_ERROR, 'Error reading file');
      end;
    end;
    CBT_INTERNAL: begin
      TmpFunc:=@fread;
      case fs_type of
        FST_STREAM: begin
          TmpStrm:=stream;
          if ( TmpStrm = nil ) then SetError(CURLE_READ_ERROR, 'Invalid transmit stream for internal callback');
        end;
        FST_FILENAME: begin
          if ( fInFilesize < 0 ) then SetResultCode(curl_easy_setopt(fCurl, CURLOPT_INFILESIZE, GetFileSize(filename)), CURLOPT_INFILESIZE);
          TmpStrm:=fopen( filename, ModeRead );
          stream:=TmpStrm;
          if ( TmpStrm = nil ) then SetError(CURLE_READ_ERROR, 'Error reading local file');
        end;
        FST_INTERNAL: begin
          TmpStrm:=fdopen(stdin, ModeRead);
          if ( TmpStrm = nil ) then SetError(CURLE_READ_ERROR, 'Error reading standard input');
        end;
      end;
    end;
  end;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_READFUNCTION, TmpFunc), CURLOPT_READFUNCTION);
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_INFILE, TmpStrm), CURLOPT_INFILE);
end;
