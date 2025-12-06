unit UHelp;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface
uses
  ULog,
  UPathUtils,
  UFilesystem,
  IniFiles,
  SysUtils,
  Classes,
  UKeyBindings;

const
  KEY_BINDING_BREAK = '__ALTERNATE_BINDING_BREAK__';

type
  TKeymap = record
    Text:   string;   //description text
    Id:     string;
  end;

  TSection = record
    name:   string;
    Keys:   array of TKeymap;
    Id:     string;
  end;

  TSub = record
    title:  string;
    text:   array of AnsiString;
  end;

  TKeyNames = record
    Key:    array of string;
  end;

  TTextResultSection = record
    name:           string;
    Keys:           array of TKeyNames;
    KeyDescription: array of AnsiString;
    BindingHandles: array of TKeyBindingHandle;
    HelpTokens:     array of AnsiString;
  end;

  TTextResult = record
    Title:          String;
    Description:    AnsiString;
    Subs:           array of TSub;
    Sections:       array of TTextResultSection;
  end;

  TEntry = record
    ID:           string;
    Title:        string;
    Description:  string;
    Subs:         array of TSub;
    Sections:     array of TSection;
  end;

  TLanguageList = record
    Name:   string;
  end;

  THelp = class
    private
      actualID:     string;
      ScrollPos:    double;
      Entry:        array of TEntry;
      SEntry:       array of TEntry;
      AEntry:       TEntry;
      List:         array of TLanguageList;
      Implode_Glue1, Implode_Glue2: String;
      KeyTranslations: TStringList;
      function TranslateKeyToken(const Token: string): UTF8String;
      function GetSectionNameById(const SectionId: string): UTF8String;
      function GetKeyDescriptionById(const HelpToken: string): UTF8String;
      function ResolveSectionName(const SectionId: string): UTF8String;
      function ResolveKeyDescription(const HelpToken: string): UTF8String;
    public
      MaxLines: integer;
      constructor Create;
      destructor Destroy; override;
      procedure LoadList;

      procedure ChangeLanguage(Language: String);
      //procedure LoadKeys;
      function SetHelpID(ID: String):boolean;
      function GetHelpID(): string;
      function GetHelpStr(): TTextResult;
      function TranslateKeyTokenPublic(const Token: string): UTF8String;
      procedure SetScrollPos(pos: double);
      function GetScrollPos(): double;
  end;


var
  Help:   THelp;


implementation
uses
  UFiles,
  ULanguage,
  sdl2,
  UUnicodeUtils;

//----------
//Create - Construct Class then LoadList + Standard Language + Set Standard Implode Glues
//----------
constructor THelp.Create;
var
  I, J, K: Integer;
begin
  LoadList;
  MaxLines := 0;
  //LoadKeys;
  //Set Implode Glues for Backward Compatibility
  Implode_Glue1 := ', ';
  Implode_Glue2 := ' and ';

  KeyTranslations := TStringList.Create;
  KeyTranslations.CaseSensitive := false;
  KeyTranslations.NameValueSeparator := '=';
  KeyTranslations.Duplicates := dupIgnore;

  if (Length(List) = 0) then //No Language Files Loaded -> Abort Loading
    Log.CriticalError('Could not load any Language File (Help System)');

  //Standard Language (If a Language File is Incomplete)
  //Then use English Language
  for I := 0 to high(List) do //Search for English Language
  begin
    //English Language Found -> Load
    if Uppercase(List[I].Name) = 'ENGLISH' then
    begin
      ChangeLanguage('English');

      SetLength(SEntry, Length(Entry));

      for J := low(Entry) to high(Entry) do
      begin
        SetLength(SEntry[J].Sections, Length(Entry[J].Sections));
        SEntry[J].ID := Entry[J].ID;
        SEntry[J].Title := Entry[J].Title;
        SEntry[J].Description := Entry[J].Description;
        for K := low(Entry[J].Sections) to high(Entry[J].Sections) do
          SEntry[J].Sections[K] := Entry[J].Sections[K];
      end;

      SetLength(List, 0);

      Break;
    end;

    if (I = high(List)) then
      Log.LogError('English language file missing! No standard Translation loaded (Help System)');
  end;

  actualID := '';
  //Standard Language END

end;

destructor THelp.Destroy;
begin
  FreeAndNil(KeyTranslations);
  inherited Destroy;
end;

//----------
//LoadList - Parse the Help Dir searching Translations
//----------
procedure THelp.LoadList;
var
  SR:     TSearchRec;   // for parsing directory
begin
  SetLength(List, 0);

  if FindFirst(LanguagesPath.ToNative() + '*.ini', 0, SR) = 0 then
  begin
    repeat
      SetLength(List, Length(List)+1);
      SR.Name := ChangeFileExt(SR.Name, '');
      List[High(List)].Name := SR.Name;
    until FindNext(SR) <> 0;
    SysUtils.FindClose(SR);
  end; // if FindFirst
end;

//----------
//ChangeLanguage - Load the specified LanguageFile
//----------
procedure THelp.ChangeLanguage(Language: String);
var
  IniFile:    TIniFile;
  E:          integer; // entry
  S:          TStringList;
  SL :        TStringList;
  num:        Integer;
  I, J:       Integer;
  tempStr:    AnsiString;
  ActSection: integer;
  ActSub:     integer;
  ActKey:     integer;
  ActSubEntry:    integer;

  function GetIDStr(ID: integer):string;
  var
    S: string;
  begin
    S := IntToStr(ID);
    while (Length(S)<3) do S := '0' + S;
    Result := 'ID_' + S;
  end;

  function GetKeyTranslation(Key: string):string;
  var
    Upper: UTF8String;
  begin
    Upper := UTF8UpperCase(Key);
    if (KeyTranslations <> nil) and (KeyTranslations.IndexOfName(Upper) <> -1) then
      Result := KeyTranslations.Values[Upper]
    else
      Result := Key;
  end;

begin
  IniFile := TIniFile.Create(LanguagesPath.toNative() + Language + '.ini');

  //read keys
  S := TStringList.Create;
  IniFile.ReadSectionValues('Keymap', S);

  if KeyTranslations <> nil then
    KeyTranslations.Clear;

  for E := 0 to S.Count - 1 do
  begin
    if S.Names[E] = 'IMPLODE_GLUE1' then
      Implode_Glue1 := S.ValueFromIndex[E]+ ' '
    else if S.Names[E] = 'IMPLODE_GLUE2' then
      Implode_Glue2 := ' ' + S.ValueFromIndex[E] + ' ';

    if (KeyTranslations <> nil) and (Pos('IMPLODE', UpperCase(S.Names[E])) = 0) then
      KeyTranslations.Values[UTF8UpperCase(S.Names[E])] := S.ValueFromIndex[E];
  end;

  SetLength(Entry, 0);
  num := IniFile.ReadInteger('config', 'NumIDs', 0);

  if num>0 then
  begin
    SetLength(Entry, num);
    for I:=1 to num do
    begin

      Entry[I-1].ID:= GetIDStr(I);
      Entry[I-1].Title := IniFile.ReadString(GetIDStr(I), 'Title', 'error title: ' + GetIDStr(I));
      Entry[I-1].Description := IniFile.ReadString(GetIDStr(I), 'Description', 'error description: ' + GetIDStr(I));

      //read subs, sections and keymapping
      S := TStringList.Create;
      IniFile.ReadSectionValues(GetIDStr(I), S);
      ActSub := -1;
      ActSubEntry := -1;
      ActSection := -1;
      ActKey := -1;

      //SetLength(Entry[I-1].Keys, S.Count-2);
      for E := 0 to S.Count-1 do
      begin
        if S.Names[E] = 'IMPLODE_GLUE1' then
          Implode_Glue1 := S.ValueFromIndex[E]+ ' '
        else if S.Names[E] = 'IMPLODE_GLUE2' then
          Implode_Glue2 := ' ' + S.ValueFromIndex[E] + ' ';

        if (S.Names[E] <> 'Title') and (S.Names[E] <> 'Description') then
        begin
          tempStr := S.Names[E];

          SL:=TStringList.Create;
          try
            ExtractStrings(['_'], [], PChar(tempStr), SL);
            //check new Sub
            if SL[0]='SUB' then
            begin
              inc(ActSub);
              ActSubEntry := -1;
              SetLength(Entry[I-1].Subs, ActSub+1);
              Entry[I-1].Subs[ActSub].title := S.ValueFromIndex[E];

            end

            //check sub entry
            else if SL[0]='ENT' then
            begin
              inc(ActSubEntry);
              SetLength(Entry[I-1].Subs[ActSub].text, ActSubEntry+1);
              Entry[I-1].Subs[ActSub].text[ActSubEntry] := S.ValueFromIndex[E];
            end

            //check new section
            else if SL[0]='SEC' then
            begin
              inc(ActSection);
              ActKey := -1;
              SetLength(Entry[I-1].Sections, ActSection+1);
              Entry[I-1].Sections[ActSection].Id := tempStr;
              Entry[I-1].Sections[ActSection].name := S.ValueFromIndex[E];
            end

            //read keymap
            else if ActSection=-1 then
            begin
              Log.LogError('No section in keymapping of ' + GetIDStr(I) + ' in ' + language + '.ini');
            end else
            begin
              inc(ActKey);
              SetLength(Entry[I-1].Sections[ActSection].Keys, ActKey +1);
              Entry[I-1].Sections[ActSection].Keys[ActKey].Text := S.ValueFromIndex[E];
              Entry[I-1].Sections[ActSection].Keys[ActKey].Id := tempStr;

            end;

          Finally
            SL.Free;
          end;

        end;
      end;

      S.Free;
    end;
  end;

  IniFile.Free;
end;

function THelp.SetHelpID(ID: String):boolean;
var
  E:   integer; // entry
begin
  Result := false;
  ScrollPos := 0.0;

  for E := 0 to high(Entry) do
  begin
    if ID = Entry[E].ID then
    begin
      Result := true;
      AEntry := Entry[E];
      actualID := ID;
      exit;
    end;
  end;

  //Standard Language (If a Language File is Incomplete)
  //Then use Standard Language
  for E := low(SEntry) to high(SEntry) do
  begin
  if ID = SEntry[E].ID then
    begin
      Result := true;
      AEntry := SEntry[E];
      actualID := ID;
      exit;
    end;
  end;
  //Standard Language END
end;

function THelp.GetHelpID(): string;
begin
  Result := actualID;
end;

function THelp.GetHelpStr(): TTextResult;
var
  K, I: Integer;
  Bindings: TKeyBindingEntryViewArray;
  SectionMap: TStringList;
  SectionIdx, EntryIdx: integer;
  MapIdx: integer;
  KeyNames: TKeyNames;
  TextResult: TTextResult;

  function BuildKeyNames(const Binding: TKeyBindingEntryView): TKeyNames;
  var
    BaseToken: UTF8String;
    UnboundText: UTF8String;
    ModMask: word;

    procedure AppendToken(const Token: UTF8String);
    var
      Len: integer;
      Display: UTF8String;
    begin
      if Token = '' then
        Exit;
      Len := Length(Result.Key);
      SetLength(Result.Key, Len + 1);
      Display := TranslateKeyToken(Token);
      Result.Key[Len] := '[' + Display + ']';
    end;

  begin
    SetLength(Result.Key, 0);
    if Binding.CurrentInput = IGNORE_KEY then
    begin
      UnboundText := Language.Translate('HELP_KEY_UNBOUND');
      if UnboundText = 'HELP_KEY_UNBOUND' then
        UnboundText := 'Not Bound';
      SetLength(Result.Key, 1);
      Result.Key[0] := '[' + UnboundText + ']';
      Exit;
    end;

    ModMask := CombinedKeyToModifierMask(Binding.CurrentInput);
    if (ModMask and KMOD_CTRL) <> 0 then
      AppendToken('CTRL');
    if (ModMask and KMOD_ALT) <> 0 then
      AppendToken('ALT');
    if (ModMask and KMOD_SHIFT) <> 0 then
      AppendToken('SHIFT');

    BaseToken := KeyCodeToToken(CombinedKeyToKeyCode(Binding.CurrentInput));
    AppendToken(BaseToken);
  end;

  function EnsureSection(const CategoryId: UTF8String): integer;
  var
    UpperId: UTF8String;
  begin
    UpperId := UTF8UpperCase(CategoryId);
    MapIdx := SectionMap.IndexOf(UpperId);
    if MapIdx = -1 then
    begin
      MapIdx := SectionMap.AddObject(UpperId, TObject(PtrInt(Length(TextResult.Sections))));
      SetLength(TextResult.Sections, Length(TextResult.Sections) + 1);
      TextResult.Sections[High(TextResult.Sections)].name := ResolveSectionName(CategoryId);
      SetLength(TextResult.Sections[High(TextResult.Sections)].Keys, 0);
      SetLength(TextResult.Sections[High(TextResult.Sections)].KeyDescription, 0);
      SetLength(TextResult.Sections[High(TextResult.Sections)].BindingHandles, 0);
      SetLength(TextResult.Sections[High(TextResult.Sections)].HelpTokens, 0);
    end;
    Result := PtrInt(SectionMap.Objects[MapIdx]);
  end;

begin
  FillChar(TextResult, SizeOf(TextResult), 0);
  if (KeyBindings <> nil) and KeyBindings.HasContext(actualID) then
    Bindings := KeyBindings.EnumerateContext(actualID)
  else
    SetLength(Bindings, 0);

  SetLength(TextResult.Sections, 0);
  if Length(Bindings) > 0 then
  begin
    SectionMap := TStringList.Create;
    try
      SectionMap.CaseSensitive := false;
      SectionMap.Duplicates := dupIgnore;

      for K := 0 to High(Bindings) do
      begin
        SectionIdx := EnsureSection(Bindings[K].CategoryId);
        KeyNames := BuildKeyNames(Bindings[K]);
        EntryIdx := Length(TextResult.Sections[SectionIdx].Keys);
        SetLength(TextResult.Sections[SectionIdx].Keys, EntryIdx + 1);
        SetLength(TextResult.Sections[SectionIdx].KeyDescription, EntryIdx + 1);
        SetLength(TextResult.Sections[SectionIdx].BindingHandles, EntryIdx + 1);
        SetLength(TextResult.Sections[SectionIdx].HelpTokens, EntryIdx + 1);
        TextResult.Sections[SectionIdx].Keys[EntryIdx] := KeyNames;
        TextResult.Sections[SectionIdx].KeyDescription[EntryIdx] := ResolveKeyDescription(Bindings[K].HelpToken);
        TextResult.Sections[SectionIdx].BindingHandles[EntryIdx] := Bindings[K].Handle;
        TextResult.Sections[SectionIdx].HelpTokens[EntryIdx] := Bindings[K].HelpToken;
      end;
    finally
      SectionMap.Free;
    end;
  end;

  SetLength(TextResult.Subs, Length(AEntry.Subs));
  for K := 0 to Length(AEntry.Subs) - 1 do
  begin
    TextResult.Subs[K].title := AEntry.Subs[K].title;
    SetLength(TextResult.Subs[K].text, Length(AEntry.Subs[K].text));
    for I := 0 to Length(AEntry.Subs[K].text) - 1 do
      TextResult.Subs[K].text[I] := AEntry.Subs[K].text[I];
  end;

  TextResult.Title := AEntry.Title;
  TextResult.Description := AEntry.Description;
  Result := TextResult;
end;

function THelp.TranslateKeyToken(const Token: string): UTF8String;
var
  Upper: UTF8String;
begin
  Upper := UTF8UpperCase(Token);
  if (KeyTranslations <> nil) and (KeyTranslations.IndexOfName(Upper) <> -1) then
    Result := KeyTranslations.Values[Upper]
  else
    Result := Token;
end;

function THelp.GetSectionNameById(const SectionId: string): UTF8String;
var
  I: integer;
begin
  for I := 0 to High(AEntry.Sections) do
    if SameText(AEntry.Sections[I].Id, SectionId) then
      Exit(AEntry.Sections[I].name);
  Result := SectionId;
end;

function THelp.GetKeyDescriptionById(const HelpToken: string): UTF8String;
var
  S, K: integer;
begin
  for S := 0 to High(AEntry.Sections) do
    for K := 0 to High(AEntry.Sections[S].Keys) do
      if SameText(AEntry.Sections[S].Keys[K].Id, HelpToken) then
        Exit(AEntry.Sections[S].Keys[K].Text);
  Result := HelpToken;
end;

function THelp.ResolveSectionName(const SectionId: string): UTF8String;
begin
  Result := GetSectionNameById(SectionId);
end;

function THelp.ResolveKeyDescription(const HelpToken: string): UTF8String;
begin
  Result := GetKeyDescriptionById(HelpToken);
end;

function THelp.TranslateKeyTokenPublic(const Token: string): UTF8String;
begin
  Result := TranslateKeyToken(Token);
end;

procedure THelp.SetScrollPos(pos: double);
begin
  ScrollPos := pos;
end;

function THelp.GetScrollPos(): double;
begin
  Result := ScrollPos;
end;

end.
