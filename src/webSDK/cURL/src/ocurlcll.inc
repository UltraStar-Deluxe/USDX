(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)


{ WRAPPERS FOR LINKED LISTS, LIKE CURL_SLIST AND CURL_HTTPPOST }


{=========================================================}
{=================   tCurlROList  ========================}
{=========================================================}

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}
{$H+}


constructor tCurlROList.Create;
begin
  inherited Create;
  fList:=nil;
  fCount:=0;
  fDirty:=False;
end;


procedure tCurlROList.Add(const aStr:string);
var
  tmp, p1, p2:pChar;
begin
  if ( pos(#10, aStr) = 0 ) then begin
    fList:=curl_slist_append(fList, pChar(aStr));
    inc(fCount);
    fDirty:=True;
  end else begin
    tmp:=StrNew(pChar(aStr));
    p1:=tmp;
    repeat
      p1:=StrScan(p1, #13);
      if ( p1 <> nil ) then StrCopy(p1, @p1[1]);
    until ( p1 = nil );
    p1:=tmp;
    repeat
      p2:=StrScan(p1, #10);
      if ( p2 <> nil ) then p2^:=#0;
      fList:=curl_slist_append(fList, p1);
      inc(fCount);
      if ( p2 = nil ) then BREAK;
      p1:=@p2[1];
    until False;
    StrDispose(tmp);
  end;
end;


destructor tCurlROList.Destroy;
begin
  Clear;
  inherited Destroy;
end;


function tCurlROList.GetText:string;
var
  p:pcurl_slist;
  L:LongInt;
begin
  if ( fList <> nil ) then begin
    L:=0;
    p:=fList;
    while ( p <> nil ) do begin
      inc(L, 1);
      if ( p^.data <> nil ) then inc(L, StrLen(p^.data) );
      p:=p^.next;
    end;
    SetLength(Result, L);
    Result:='';
    p:=fList;
    while ( p <> nil ) do begin
      Result:=Result + p^.data + #10;
      p:=p^.next;
    end;
  end else Result:='';
end;


procedure tCurlROList.SetText(const aStr:string);
begin
  Clear;
  if ( aStr <> '' ) then Add(aStr);
end;


procedure tCurlROList.Clear;
begin
  curl_slist_free_all(fList);
  fList:=nil;
  fCount:=0;
  fDirty:=True;
end;

function tCurlROList.GetItemPtr(n:LongInt):pcurl_slist;
var
  i:LongInt;
begin
  i:=0;
  Result:=fList;
  while ( Result <> nil ) do begin
    if ( i = n ) then EXIT;
    Result:=Result^.next;
    inc(i);
  end;
  Result:=nil;
end;


function tCurlROList.GetItems(n:LongInt):string;
var
  p:pcurl_slist;
begin
  p:=GetItemPtr(n);
  if ( p <> nil ) then Result:=p^.data else Result:='';
end;


procedure tCurlROList.SetItems(n:LongInt; const s:string);
var
  p:pcurl_slist;
  L:LongInt;
begin
  p:=GetItemPtr(n);
  if ( p <> nil ) then begin
    if ( p^.data <> nil ) then _free(p^.data);
    L:=length(s);
    p^.data:=Getmem(L+1);
    FillChar(p^.data^, L+1, #0);
    StrLCopy(p^.data,pChar(s), L);
    fDirty:=True;
  end;
end;

procedure tCurlROList.Delete(n:LongInt);
var
  prev, me:pcurl_slist;
begin
  me:=GetItemPtr(n);
  if ( me <> nil ) then begin
    if ( fList = me ) then begin
      fList:=me^.next;
    end else begin
      prev:=GetItemPtr(n-1);
      prev^.next:=me^.next;
    end;
    me^.next:=nil;
    curl_slist_free_all(me);
    fDirty:=True;
  end;
end;


{=========================================================}
{=================   tCurlRWList  ========================}
{=========================================================}


procedure tCurlRWList.Add(const aStr:string);
begin
  inherited;
end;

procedure tCurlRWList.Delete(n:LongInt);
begin
  inherited;
end;

procedure tCurlRWList.Clear;
begin
  inherited;
end;

{=========================================================}
{==============   tMultiPartFormData   ===================}
{=========================================================}

constructor tMultiPartFormData.Create;
begin
  inherited Create;
  fErrCode:=CURLE_OK;
  fHttpPost:=nil;
end;


destructor tMultiPartFormData.Destroy;
begin
  DoClear(fHttpPost);
  inherited Destroy;
end;


procedure tMultiPartFormData.Append(const PostInfo:Curl_HttpPost);
begin
  DoAppend(fHttpPost, PostInfo);
end;


procedure CopyList(const Source:pcurl_slist; var Target: pcurl_slist);
var
  vList:pcurl_slist;
  vStr:string;
begin
  if ( Source <> nil ) then begin
    vList:=Source;
    while ( vList <> nil ) do begin
    if ( vList^.Data <> nil ) then begin
      vStr:=StrPas(vList^.Data)+#0;
      curl_slist_append(Target, @vStr[1]);
    end;
    vList:=vList^.Next;
    end;
  end else Target:=nil;
end;


procedure tMultiPartFormData.DoAppend(var aPost:pCurl_HttpPost; const PostInfo:curl_HttpPost);
var
  NewPost, LastPost: pcurl_HttpPost;
begin
  new(NewPost);
  with NewPost^ do begin

    namelength:=(StrLen(PostInfo.Name));
    name:=StrAlloc(namelength+1);
    StrCopy(name,PostInfo.name);

    contentslength:=PostInfo.ContentsLength;
    contents:=stralloc(contentslength+1);
    StrCopy(contents,postinfo.contents);

    contenttype:=StrAlloc(StrLen(postinfo.contenttype)+1);
    StrCopy(contenttype, PostInfo.contenttype);

    flags:=PostInfo.flags;

    if ( PostInfo.ContentHeader <> nil ) then CopyList( PostInfo.contentheader, ContentHeader);
    if ( PostInfo.more <> nil ) then DoAppend(more, PostInfo.more^);

    next:=nil;

  end;
  if ( aPost = nil ) then aPost:=NewPost else begin
    LastPost:=aPost;
    while ( LastPost <> nil ) and ( LastPost^.Next <> nil ) do LastPost:=LastPost^.Next;
    LastPost^.Next:=NewPost;
  end;

end;


procedure tMultiPartFormData.Add(
  const aName:string; const aContents:string; const aContentType:string; aPostType: tCurlPostType);
var
  NewPost: pCurl_HttpPost;
begin
  new(NewPost);
  FillChar( NewPost^, sizeof(NewPost^), 0);//3:37pm 22 december 2009 Eric de Lange added to prevent access violation in libcurl
  with NewPost^ do begin
    namelength:=Length(aName);
    name:=StrAlloc(namelength + 1);
    StrPCopy(name, aName);
    contentslength:=Length(aContents);
    contents:=StrAlloc(contentslength + 1);
    StrPCopy(contents, aContents);
    contenttype:=StrAlloc(Length(aContentType) + 1);
    StrPCopy(contenttype, aContentType);
    flags:=HTTPPOST_PTRNAME or HTTPPOST_PTRCONTENTS;
    case aPostType of
      POST_TYPE_ATTACHMENT: flags:=flags or HTTPPOST_FILENAME;
      POST_TYPE_FILEDATA: flags:= flags or HTTPPOST_READFILE;
    end;
    if ( aPostType in [ POST_TYPE_ATTACHMENT, POST_TYPE_FILEDATA ] )
     and ( not FileIsReadable(aContents) )
       then fErrCode:=CURLE_READ_ERROR;
    contentheader:=nil;
    more:=nil;
    next:=fHttpPost;
  end;
  fHttpPost:=NewPost;
end;


procedure tMultiPartFormData.Clear;
begin
  DoClear(fHttpPost);
end;


procedure tMultiPartFormData.DoClear(var aPost:pCurl_HttpPost);
var ThisPost, NextPost: pCurl_HttpPost;
begin
  ThisPost:=aPost;
  while ( ThisPost <> nil ) do begin
    NextPost:=ThisPost^.Next;
    with ThisPost^ do begin
      StrDispose(name);
      StrDispose(contents);
      StrDispose(contenttype);
      if ( more <> nil ) then DoClear(more); { ... Recursive }
      if ( contentheader <> nil ) then curl_slist_free_all(contentheader);
    end;
    FreeMem(ThisPost);
    ThisPost:=NextPost;
  end;
  aPost:=nil;
end;


{=========================================================}
{===============  tCurlCookieList   ======================}
{=========================================================}

constructor tCurlCookieList.Create(aOwner:tObject);
begin
  inherited Create;
  fOwner:=aOwner;
  fHandle:=tCurl(aOwner).Handle;
  fList:=nil;
  fState:=[];
end;


destructor tCurlCookieList.Destroy;
begin
  if ( COOKIES_OBJ_STALE in fState ) then SendListToLib;
  curl_slist_free_all(fList);
  inherited Destroy;
end;


function tCurlCookieList.Add(const cookie:ansistring):boolean;
var
  n:LongInt;
  p:pChar;
begin
  n:=0;
  p:=pChar(cookie);
  while ( p^ <> #0 ) do begin
    if p^=#9 then inc(n);
    inc(p);
  end;
  p:=pChar(cookie);
  Result:= ( n = 6 )  or ( StrLIComp('SET-COOKIE:', p, 11) = 0 );
  if Result then begin
    fList:=curl_slist_append(fList, p);
    if ( not ( COOKIES_LIB_STALE in fState ) ) then fState:=fState+[COOKIES_LIB_STALE];
  end;
end;


function tCurlCookieList.AddFromFile(const filename:ansistring):CurlCode;
var
  f:system.text;
  s:ansistring;
begin
  s:='';
  if FileIsReadable(filename) then begin
    {$I-}
    assign(f, filename);
    reset(f);
    if ( ioresult = 0 ) then begin
      while not eof(f) do begin
        ReadLn(f, s);
        Add(s);
      end;
      Result:=CURLE_OK;
    end else Result:=CURLE_READ_ERROR;
    {$I+}
    close(f);
  end else Result:=CURLE_READ_ERROR;
end;


procedure tCurlCookieList.ForEachCookie(callback:tCurlEnumCookiesFunction; user_data:pointer);
var
  p1:pcurl_slist;
begin
  if ( COOKIES_LIB_STALE in fState ) then GetListFromLib;
  p1:=fList;
  while ( p1 <> nil ) do begin
    if not callback(self, p1^.data, user_data) then BREAK;
    p1:=p1^.next;
  end;
end;


procedure tCurlCookieList.RemoveDuplicates;
var
  curr, prev: pcurl_slist;
  needle:pcurl_slist;
  H:LongInt;
  L:LongInt;
begin
  needle:=fList;
  while ( needle <> nil ) do begin
    if ( needle^.data <> nil ) then begin
      prev:=nil;
      curr:=needle;
      L:=StrLen(needle^.data);
      H:=pLongInt(needle^.data)^;
      while ( curr <> nil  ) do begin
        if ( curr <> needle )
         and ( curr^.data <> nil )
          and ( H = pLongInt(curr^.data)^ )
           and ( curr^.data[L] = #0 )
            and ( StrLComp(curr^.data, needle^.data, L) = 0 )
        then begin
          if ( curr^.data = needle^.data ) then curr^.data:=nil;
          prev^.next:=curr^.next;
          curr^.next:=nil;
          curl_slist_free_all(curr);
          curr:=prev^.next;
        end else begin
          prev:=curr;
          curr:=curr^.next;
        end;
      end;
    end;
    needle:=needle^.next;
  end;
end;


procedure tCurlCookieList.Clear;
begin
  curl_slist_free_all(fList);
  fList:=nil;
  if ( fOwner <> nil ) and ( fHandle <> nil ) then curl_easy_setopt(fHandle, CURLOPT_COOKIELIST, 'ALL');
  fState:=[];
end;


function tCurlCookieList.GetListFromLib:CurlCode;
var
  tmp, p1:pcurl_slist;
begin
  Result:=curl_easy_getinfo(fHandle, CURLINFO_COOKIELIST, tmp);
  if ( Result = CURLE_OK ) then begin
    if ( fList <> nil ) then begin
       p1:=fList;
       while ( p1 <> nil ) do begin
         if ( p1^.next = nil ) then begin
           p1^.next:=tmp;
           p1:=nil;
         end else p1:=p1^.next;
       end;
    end else fList:=tmp;
    RemoveDuplicates;
    if ( COOKIES_OBJ_STALE in fState ) then fState:= fState - [COOKIES_OBJ_STALE];
  end;
end;


function tCurlCookieList.SendListToLib:CurlCode;
var
  p1:pcurl_slist;
begin
  Result:=CURLE_OK;
  p1:=fList;
  while ( p1 <> nil ) do begin
    if ( p1^.data <> nil ) then begin
      Result:=curl_easy_setopt(fHandle, CURLOPT_COOKIELIST, p1^.data);
      if (Result <> CURLE_OK) then BREAK;
    end;
    p1:=p1^.next;
  end;
  if ( Result = CURLE_OK ) then begin
     if ( fList <> nil ) then begin
       curl_slist_free_all(fList);
       fList:=nil;
     end;
    if ( COOKIES_LIB_STALE in fState ) then fState:= fState - [COOKIES_LIB_STALE] ;
  end;
end;

type
  pTextFile = ^text;


function FileSaveCB(Sender:tObject; const cookie:ansistring; user_data:pointer):boolean;
begin
  WriteLn(pTextFile(user_data)^, cookie);
  Result:=(ioresult=0);
end;


function tCurlCookieList.SaveToFile(const filename:ansistring):CurlCode;
var
  f:text;
begin
  if FileIsWriteable(filename) then begin
    {$I-}
    assign(f, filename);
    rewrite(f);
    if ( ioresult = 0 ) then begin
      ForEachCookie({$IFDEF FPC}@{$ENDIF}FileSaveCB, @f);
      close(f);
      if ( ioresult = 0 ) then Result:=CURLE_OK else Result:=CURLE_WRITE_ERROR;
    end else Result:=CURLE_WRITE_ERROR;
    {$I+}
  end else Result:=CURLE_WRITE_ERROR;
end;




