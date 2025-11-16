unit UKeyBindings;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  sdl2;

type
  TKeyBindingHandle = type integer;

  TKeyBindingEntryView = record
    Handle: TKeyBindingHandle;
    ContextId: UTF8String;
    CategoryId: UTF8String;
    CategoryOrder: integer;
    HelpToken: UTF8String;
    ModifierMask: word;
    DefaultInput: cardinal;
    CurrentInput: cardinal;
    OutputKey: cardinal;
    EntryOrder: integer;
    IsModified: boolean;
  end;
  TKeyBindingEntryViewArray = array of TKeyBindingEntryView;

  TKeyBindingRegistry = class
  private type
    TCategoryInfo = record
      Id: UTF8String;
      Order: integer;
    end;

    TContextInfo = record
      Id: UTF8String;
      RegistrationCount: integer;
      Categories: array of TCategoryInfo;
    end;

    TEntry = record
      Handle: TKeyBindingHandle;
      ContextIdx: integer;
      CategoryId: UTF8String;
      CategoryOrder: integer;
      HelpToken: UTF8String;
      DuplicateIndex: integer;
      ModifierMask: word;
      DefaultInput: cardinal;
      CurrentInput: cardinal;
      OutputKey: cardinal;
      EntryOrder: integer;
    end;
  private
  FContexts: array of TContextInfo;
  FEntries: array of TEntry;
  FOverrides: TStringList;
  FHelpTokenCounts: TStringList;
  FNextHandle: integer;
    function GetContextIndex(const ContextId: UTF8String): integer;
    function EnsureContext(const ContextId: UTF8String): integer;
    function EnsureCategory(ContextIdx: integer; const CategoryId: UTF8String): integer;
    function NextDuplicateIndex(ContextIdx: integer; const HelpToken: UTF8String): integer;
    function BuildOverrideKey(const ContextId, HelpToken: UTF8String;
      ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer): UTF8String;
    function LookupOverride(const ContextId, HelpToken: UTF8String;
      ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer;
      out KeyCode: cardinal): boolean;
    function KeyNameToCode(const KeyName: UTF8String): cardinal;
    function KeyCodeToName(KeyCode: cardinal): UTF8String;
  function NormalizeModifiers(ModState: word): word;
    procedure SetCurrentInput(var Entry: TEntry; NewInput: cardinal);
    procedure RemoveOverride(const ContextId, HelpToken: UTF8String;
      ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer);
    procedure StoreOverride(const ContextId, HelpToken: UTF8String;
      ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer; KeyCode: cardinal);
    procedure ResolveConflicts(const Entry: TEntry; NewInput: cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function RegisterBinding(const ContextId, CategoryId, HelpToken: UTF8String;
      ModifierMask: word; DefaultInput, OutputKey: cardinal): TKeyBindingHandle;

    function TranslateKey(const ContextId: UTF8String; ModState: word;
      PressedKey: cardinal): cardinal;

    procedure UpdateBinding(Handle: TKeyBindingHandle; NewInput: cardinal);
    procedure ResetBinding(Handle: TKeyBindingHandle);

    function EnumerateContext(const ContextId: UTF8String): TKeyBindingEntryViewArray;
    function HasContext(const ContextId: UTF8String): boolean;

    procedure LoadOverrides(IniFile: TCustomIniFile);
    procedure SaveOverrides(IniFile: TCustomIniFile);
  end;

var
  KeyBindings: TKeyBindingRegistry;

function NormalizeModifierState(ModState: word): word;
function KeyCodeToToken(KeyCode: cardinal): UTF8String;

implementation

uses
  UUnicodeUtils;

const
  IGNORE_KEYCODE = 0;
  UNBOUND_OVERRIDE_VALUE = 'UNBOUND'; // persisted marker meaning the binding was explicitly cleared

{ Helper functions }

function NormalizeModifierState(ModState: word): word;
begin
  Result := 0;
  if (ModState and (KMOD_LSHIFT or KMOD_RSHIFT)) <> 0 then
    Result := Result or KMOD_SHIFT;
  if (ModState and (KMOD_LCTRL or KMOD_RCTRL)) <> 0 then
    Result := Result or KMOD_CTRL;
  if (ModState and (KMOD_LALT or KMOD_RALT)) <> 0 then
    Result := Result or KMOD_ALT;
end;

function KeyCodeToToken(KeyCode: cardinal): UTF8String;
begin
  Result := '';

  if KeyCode = SDLK_SLASH then
    Result := 'SLASH'
  else if KeyCode = SDLK_BACKSLASH then
    Result := 'BACKSLASH'
  else if KeyCode = SDLK_BACKQUOTE then
    Result := 'BACKQUOTE'
  else if KeyCode = SDLK_EQUALS then
    Result := 'EQUALS'
  else if KeyCode = SDLK_MINUS then
    Result := 'MINUS'
  else if KeyCode = SDLK_PERIOD then
    Result := 'PERIOD'
  else if KeyCode = SDLK_COMMA then
    Result := 'COMMA'
  else if KeyCode = SDLK_SEMICOLON then
    Result := 'SEMICOLON'
  else if KeyCode = SDLK_QUOTE then
    Result := 'QUOTE'
  else if KeyCode = SDLK_LEFTBRACKET then
    Result := 'LEFTBRACKET'
  else if KeyCode = SDLK_RIGHTBRACKET then
    Result := 'RIGHTBRACKET'
  else if KeyCode = SDLK_PAGEUP then
    Result := 'PAGEUP'
  else if KeyCode = SDLK_PAGEDOWN then
    Result := 'PAGEDOWN'
  else if KeyCode = SDLK_DELETE then
    Result := 'DELETE'
  else if KeyCode = SDLK_BACKSPACE then
    Result := 'BACKSPACE'
  else if KeyCode = SDLK_ESCAPE then
    Result := 'ESC'
  else if KeyCode = SDLK_RETURN then
    Result := 'RETURN'
  else if KeyCode = SDLK_TAB then
    Result := 'TAB'
  else if KeyCode = SDLK_UP then
    Result := 'UP'
  else if KeyCode = SDLK_DOWN then
    Result := 'DOWN'
  else if KeyCode = SDLK_LEFT then
    Result := 'LEFT'
  else if KeyCode = SDLK_RIGHT then
    Result := 'RIGHT'
  else if KeyCode = SDLK_SPACE then
    Result := 'SPACE'
  else if KeyCode = SDLK_KP_PLUS then
    Result := 'KPPLUS'
  else if KeyCode = SDLK_KP_MINUS then
    Result := 'KPMINUS'
  else if KeyCode = SDLK_KP_ENTER then
    Result := 'KPENTER'
  else if KeyCode = SDLK_KP_MULTIPLY then
    Result := 'KPMULTIPLY'
  else if KeyCode = SDLK_KP_DIVIDE then
    Result := 'KPDIVIDE';

  if Result = '' then
  begin
    if (KeyCode >= SDLK_F1) and (KeyCode <= SDLK_F12) then
      Result := 'F' + IntToStr(KeyCode - SDLK_F1 + 1)
    else if (KeyCode >= SDLK_0) and (KeyCode <= SDLK_9) then
      Result := IntToStr(KeyCode - SDLK_0)
    else if (KeyCode >= SDLK_a) and (KeyCode <= SDLK_z) then
      Result := UTF8UpperCase(Chr(KeyCode))
    else if (KeyCode >= SDLK_KP_0) and (KeyCode <= SDLK_KP_9) then
      Result := 'KP' + IntToStr(KeyCode - SDLK_KP_0)
    else
      Result := UTF8UpperCase(UTF8String(SDL_GetKeyName(KeyCode)));
  end;

  if Result = '' then
    Result := IntToStr(KeyCode);
end;

{ TKeyBindingRegistry }

constructor TKeyBindingRegistry.Create;
begin
  inherited Create;
  FOverrides := TStringList.Create;
  FOverrides.CaseSensitive := false;
  FOverrides.NameValueSeparator := '=';
  FOverrides.Duplicates := dupIgnore;
  FHelpTokenCounts := TStringList.Create;
  FHelpTokenCounts.CaseSensitive := false;
  FHelpTokenCounts.NameValueSeparator := '=';
  FNextHandle := 1;
end;

destructor TKeyBindingRegistry.Destroy;
begin
  FOverrides.Free;
  FHelpTokenCounts.Free;
  inherited Destroy;
end;

procedure TKeyBindingRegistry.Clear;
begin
  SetLength(FEntries, 0);
  SetLength(FContexts, 0);
  FOverrides.Clear;
  FHelpTokenCounts.Clear;
  FNextHandle := 1;
end;

function TKeyBindingRegistry.NormalizeModifiers(ModState: word): word;
begin
  Result := NormalizeModifierState(ModState);
end;

function TKeyBindingRegistry.GetContextIndex(const ContextId: UTF8String): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to High(FContexts) do
    if SameText(FContexts[I].Id, ContextId) then
      Exit(I);
end;

function TKeyBindingRegistry.EnsureContext(const ContextId: UTF8String): integer;
var
  Idx: integer;
begin
  Idx := GetContextIndex(ContextId);
  if Idx <> -1 then
    Exit(Idx);

  Idx := Length(FContexts);
  SetLength(FContexts, Idx + 1);
  FContexts[Idx].Id := ContextId;
  FContexts[Idx].RegistrationCount := 0;
  SetLength(FContexts[Idx].Categories, 0);
  Result := Idx;
end;

function TKeyBindingRegistry.EnsureCategory(ContextIdx: integer; const CategoryId: UTF8String): integer;
var
  I: integer;
  Len: integer;
begin
  for I := 0 to High(FContexts[ContextIdx].Categories) do
    if SameText(FContexts[ContextIdx].Categories[I].Id, CategoryId) then
      Exit(FContexts[ContextIdx].Categories[I].Order);

  Len := Length(FContexts[ContextIdx].Categories);
  SetLength(FContexts[ContextIdx].Categories, Len + 1);
  FContexts[ContextIdx].Categories[Len].Id := CategoryId;
  FContexts[ContextIdx].Categories[Len].Order := Len;
  Result := Len;
end;

function TKeyBindingRegistry.NextDuplicateIndex(ContextIdx: integer; const HelpToken: UTF8String): integer;
var
  Key: UTF8String;
  Count: integer;
begin
  if (ContextIdx < 0) or (ContextIdx > High(FContexts)) then
    Exit(1);

  Key := UTF8UpperCase(FContexts[ContextIdx].Id + '.' + HelpToken);
  Count := StrToIntDef(FHelpTokenCounts.Values[Key], 0) + 1;
  FHelpTokenCounts.Values[Key] := IntToStr(Count);
  Result := Count;
end;

function TKeyBindingRegistry.BuildOverrideKey(const ContextId, HelpToken: UTF8String;
  ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer): UTF8String;
var
  TokenWithIndex: UTF8String;
begin
  TokenWithIndex := HelpToken;
  if DuplicateIndex > 1 then
    TokenWithIndex := TokenWithIndex + IntToStr(DuplicateIndex);

  Result := UTF8UpperCase(Format('%s.%s.%d.%d',
    [ContextId, TokenWithIndex, ModifierMask, DefaultInput]));
end;

function TKeyBindingRegistry.LookupOverride(const ContextId, HelpToken: UTF8String;
  ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer;
  out KeyCode: cardinal): boolean;
var
  Key: UTF8String;
  Name: UTF8String;
  Code: integer;
begin
  Result := false;
  KeyCode := IGNORE_KEYCODE;
  Key := BuildOverrideKey(ContextId, HelpToken, ModifierMask, DefaultInput, DuplicateIndex);
  Name := UTF8String(FOverrides.Values[Key]);
  if Name = '' then
    Exit;

  if SameText(Name, UNBOUND_OVERRIDE_VALUE) then
    Exit(true);

  Code := KeyNameToCode(Name);
  if Code = SDLK_UNKNOWN then
    Exit(false);

  KeyCode := Code;
  Result := true;
end;

function TKeyBindingRegistry.KeyNameToCode(const KeyName: UTF8String): cardinal;
var
  Code: integer;
  Upper: UTF8String;
begin
  Upper := UTF8UpperCase(KeyName);
  Code := SDL_GetKeyFromName(PAnsiChar(AnsiString(Upper)));
  if Code = SDLK_UNKNOWN then
    Code := SDL_GetKeyFromName(PAnsiChar(AnsiString(KeyName)));
  Result := Code;
end;

function TKeyBindingRegistry.KeyCodeToName(KeyCode: cardinal): UTF8String;
begin
  if KeyCode = IGNORE_KEYCODE then
    Exit('');
  Result := UTF8UpperCase(UTF8String(SDL_GetKeyName(KeyCode)));
  if Result = '' then
    Result := IntToStr(KeyCode);
end;

procedure TKeyBindingRegistry.SetCurrentInput(var Entry: TEntry; NewInput: cardinal);
begin
  Entry.CurrentInput := NewInput;
end;

procedure TKeyBindingRegistry.RemoveOverride(const ContextId, HelpToken: UTF8String;
  ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer);
var
  Key: UTF8String;
  Idx: integer;
begin
  Key := BuildOverrideKey(ContextId, HelpToken, ModifierMask, DefaultInput, DuplicateIndex);
  Idx := FOverrides.IndexOfName(Key);
  if Idx >= 0 then
    FOverrides.Delete(Idx);
end;

procedure TKeyBindingRegistry.StoreOverride(const ContextId, HelpToken: UTF8String;
  ModifierMask: word; DefaultInput: cardinal; DuplicateIndex: integer; KeyCode: cardinal);
var
  Key: UTF8String;
  Value: UTF8String;
begin
  Key := BuildOverrideKey(ContextId, HelpToken, ModifierMask, DefaultInput, DuplicateIndex);
  if KeyCode = IGNORE_KEYCODE then
    Value := UNBOUND_OVERRIDE_VALUE
  else
    Value := KeyCodeToName(KeyCode);

  if Value = '' then
    RemoveOverride(ContextId, HelpToken, ModifierMask, DefaultInput, DuplicateIndex)
  else
    FOverrides.Values[Key] := UTF8UpperCase(Value);
end;

procedure TKeyBindingRegistry.ResolveConflicts(const Entry: TEntry; NewInput: cardinal);
var
  I: integer;
begin
  for I := 0 to High(FEntries) do
  begin
    if (FEntries[I].Handle = Entry.Handle) then
      Continue;
    if FEntries[I].ContextIdx <> Entry.ContextIdx then
      Continue;
    if FEntries[I].ModifierMask <> Entry.ModifierMask then
      Continue;
    if FEntries[I].CurrentInput <> NewInput then
      Continue;

    if FEntries[I].DefaultInput <> NewInput then
    begin
      FEntries[I].CurrentInput := FEntries[I].DefaultInput;
    end
    else
    begin
      FEntries[I].CurrentInput := IGNORE_KEYCODE;
    end;

    if FEntries[I].CurrentInput = FEntries[I].DefaultInput then
      RemoveOverride(FContexts[FEntries[I].ContextIdx].Id, FEntries[I].HelpToken,
        FEntries[I].ModifierMask, FEntries[I].DefaultInput, FEntries[I].DuplicateIndex)
    else
      StoreOverride(FContexts[FEntries[I].ContextIdx].Id, FEntries[I].HelpToken,
        FEntries[I].ModifierMask, FEntries[I].DefaultInput, FEntries[I].DuplicateIndex,
        FEntries[I].CurrentInput);
  end;
end;

function TKeyBindingRegistry.RegisterBinding(const ContextId, CategoryId, HelpToken: UTF8String;
  ModifierMask: word; DefaultInput, OutputKey: cardinal): TKeyBindingHandle;
var
  ContextIdx: integer;
  EntryIdx: integer;
  OverrideKey: cardinal;
  Entry: TEntry;
begin
  ContextIdx := EnsureContext(ContextId);
  EntryIdx := Length(FEntries);
  SetLength(FEntries, EntryIdx + 1);

  Entry.Handle := FNextHandle;
  Inc(FNextHandle);
  Entry.ContextIdx := ContextIdx;
  Entry.CategoryId := CategoryId;
  Entry.CategoryOrder := EnsureCategory(ContextIdx, CategoryId);
  Entry.HelpToken := HelpToken;
  Entry.DuplicateIndex := NextDuplicateIndex(ContextIdx, HelpToken);
  Entry.ModifierMask := NormalizeModifiers(ModifierMask);
  Entry.DefaultInput := DefaultInput;
  Entry.CurrentInput := DefaultInput;
  Entry.OutputKey := OutputKey;
  Entry.EntryOrder := FContexts[ContextIdx].RegistrationCount;
  Inc(FContexts[ContextIdx].RegistrationCount);

  if LookupOverride(ContextId, HelpToken, Entry.ModifierMask,
    Entry.DefaultInput, Entry.DuplicateIndex, OverrideKey) then
    Entry.CurrentInput := OverrideKey;

  FEntries[EntryIdx] := Entry;
  Result := Entry.Handle;
end;

function TKeyBindingRegistry.TranslateKey(const ContextId: UTF8String; ModState: word;
  PressedKey: cardinal): cardinal;
var
  ContextIdx: integer;
  I: integer;
  NormMods: word;
  Suppress: boolean;
begin
  Result := PressedKey;
  ContextIdx := GetContextIndex(ContextId);
  if ContextIdx = -1 then
    Exit;
  NormMods := NormalizeModifiers(ModState);
  Suppress := false;
  for I := 0 to High(FEntries) do
  begin
    if FEntries[I].ContextIdx <> ContextIdx then
      Continue;
    if FEntries[I].ModifierMask <> NormMods then
      Continue;

    if (FEntries[I].CurrentInput <> IGNORE_KEYCODE) and
       (FEntries[I].CurrentInput = PressedKey) then
      Exit(FEntries[I].OutputKey);

    if (FEntries[I].DefaultInput = PressedKey) and
       (FEntries[I].CurrentInput <> FEntries[I].DefaultInput) then
      Suppress := true;
  end;

  if Suppress then
    Result := IGNORE_KEYCODE;
end;

procedure TKeyBindingRegistry.UpdateBinding(Handle: TKeyBindingHandle; NewInput: cardinal);
var
  I: integer;
  ContextId: UTF8String;
begin
  for I := 0 to High(FEntries) do
  begin
    if FEntries[I].Handle <> Handle then
      Continue;

    ResolveConflicts(FEntries[I], NewInput);
    FEntries[I].CurrentInput := NewInput;

    ContextId := FContexts[FEntries[I].ContextIdx].Id;
    if FEntries[I].CurrentInput = FEntries[I].DefaultInput then
      RemoveOverride(ContextId, FEntries[I].HelpToken,
        FEntries[I].ModifierMask, FEntries[I].DefaultInput, FEntries[I].DuplicateIndex)
    else
      StoreOverride(ContextId, FEntries[I].HelpToken,
        FEntries[I].ModifierMask, FEntries[I].DefaultInput, FEntries[I].DuplicateIndex,
        FEntries[I].CurrentInput);
    Exit;
  end;
end;

procedure TKeyBindingRegistry.ResetBinding(Handle: TKeyBindingHandle);
var
  I: integer;
  ContextId: UTF8String;
begin
  for I := 0 to High(FEntries) do
  begin
    if FEntries[I].Handle <> Handle then
      Continue;
    FEntries[I].CurrentInput := FEntries[I].DefaultInput;
    ContextId := FContexts[FEntries[I].ContextIdx].Id;
    RemoveOverride(ContextId, FEntries[I].HelpToken,
      FEntries[I].ModifierMask, FEntries[I].DefaultInput, FEntries[I].DuplicateIndex);
    Exit;
  end;
end;

function TKeyBindingRegistry.EnumerateContext(const ContextId: UTF8String): TKeyBindingEntryViewArray;
var
  ContextIdx: integer;
  Count: integer;
  I, InsertIdx, J: integer;
  View: TKeyBindingEntryView;
begin
  SetLength(Result, 0);
  ContextIdx := GetContextIndex(ContextId);
  if ContextIdx = -1 then
    Exit;

  Count := 0;
  for I := 0 to High(FEntries) do
    if FEntries[I].ContextIdx = ContextIdx then
    begin
      SetLength(Result, Count + 1);
      Result[Count].Handle := FEntries[I].Handle;
      Result[Count].ContextId := ContextId;
      Result[Count].CategoryId := FEntries[I].CategoryId;
      Result[Count].CategoryOrder := FEntries[I].CategoryOrder;
      Result[Count].HelpToken := FEntries[I].HelpToken;
      Result[Count].ModifierMask := FEntries[I].ModifierMask;
      Result[Count].DefaultInput := FEntries[I].DefaultInput;
      Result[Count].CurrentInput := FEntries[I].CurrentInput;
      Result[Count].OutputKey := FEntries[I].OutputKey;
      Result[Count].EntryOrder := FEntries[I].EntryOrder;
      Result[Count].IsModified := FEntries[I].CurrentInput <> FEntries[I].DefaultInput;
      Inc(Count);
    end;

  // simple insertion sort by category + entry order
  for I := 1 to High(Result) do
  begin
    View := Result[I];
    InsertIdx := I;
    while (InsertIdx > 0) and
          ((View.CategoryOrder < Result[InsertIdx-1].CategoryOrder) or
          ((View.CategoryOrder = Result[InsertIdx-1].CategoryOrder) and
           (View.EntryOrder < Result[InsertIdx-1].EntryOrder))) do
    begin
      Result[InsertIdx] := Result[InsertIdx-1];
      Dec(InsertIdx);
    end;
    Result[InsertIdx] := View;
  end;
end;

function TKeyBindingRegistry.HasContext(const ContextId: UTF8String): boolean;
begin
  Result := GetContextIndex(ContextId) <> -1;
end;

procedure TKeyBindingRegistry.LoadOverrides(IniFile: TCustomIniFile);
var
  SectionValues: TStringList;
  I: integer;
  Name, Value: UTF8String;
begin
  FOverrides.Clear;
  SectionValues := TStringList.Create;
  try
    IniFile.ReadSectionValues('KeyBindings', SectionValues);
    for I := 0 to SectionValues.Count - 1 do
    begin
      Name := UTF8String(SectionValues.Names[I]);
      if SameText(Name, 'PianoKeysLow') or SameText(Name, 'PianoKeysHigh') then
        Continue;
      Value := UTF8String(SectionValues.ValueFromIndex[I]);
      if Pos('.', Name) > 0 then
        FOverrides.Values[UTF8UpperCase(Name)] := UTF8UpperCase(Value);
    end;
  finally
    SectionValues.Free;
  end;
end;

procedure TKeyBindingRegistry.SaveOverrides(IniFile: TCustomIniFile);
var
  I: integer;
  Name, Value: UTF8String;
begin
  IniFile.EraseSection('KeyBindings');
  for I := 0 to FOverrides.Count - 1 do
  begin
    Name := UTF8String(FOverrides.Names[I]);
    Value := UTF8String(FOverrides.ValueFromIndex[I]);
    if (Name <> '') and (Value <> '') then
      IniFile.WriteString('KeyBindings', Name, Value);
  end;
end;

initialization
  KeyBindings := TKeyBindingRegistry.Create;

finalization
  FreeAndNil(KeyBindings);

end.
