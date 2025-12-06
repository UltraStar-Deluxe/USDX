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
  TCombinedKey = QWord;

// High-bit modifier constants for packed key
const
  MOD_LSHIFT  = QWord(KMOD_LSHIFT) shl 32;
  MOD_RSHIFT  = QWord(KMOD_RSHIFT) shl 32;
  MOD_LCTRL   = QWord(KMOD_LCTRL)  shl 32;
  MOD_RCTRL   = QWord(KMOD_RCTRL)  shl 32;
  MOD_LALT    = QWord(KMOD_LALT)   shl 32;
  MOD_RALT    = QWord(KMOD_RALT)   shl 32;
  IGNORE_KEY: TCombinedKey = 0;

type
  TKeyBindingHandle = type integer;

  TKeyBindingEntryView = record
    Handle: TKeyBindingHandle;
    ContextId: UTF8String;
    CategoryId: UTF8String;
    CategoryOrder: integer;
    HelpToken: UTF8String;
    DefaultInput: TCombinedKey;
    CurrentInput: TCombinedKey;
    OutputKey: TCombinedKey;
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
      DefaultInput: TCombinedKey;
      CurrentInput: TCombinedKey;
      OutputKey: TCombinedKey;
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
      DefaultInput: TCombinedKey; DuplicateIndex: integer): UTF8String;
    function LookupOverride(const ContextId, HelpToken: UTF8String;
      DefaultInput: TCombinedKey; DuplicateIndex: integer;
      out Key: TCombinedKey): boolean;
    function KeyNameToCode(const KeyName: UTF8String): cardinal;
    function KeyCodeToName(KeyCode: cardinal): UTF8String;
    function NormalizeModifiers(ModState: word): word;
    procedure SetCurrentInput(var Entry: TEntry; NewInput: TCombinedKey);
    procedure RemoveOverride(const ContextId, HelpToken: UTF8String;
      DefaultInput: TCombinedKey; DuplicateIndex: integer);
    procedure StoreOverride(const ContextId, HelpToken: UTF8String;
      DefaultInput: TCombinedKey; DuplicateIndex: integer; KeyValue: TCombinedKey);
    procedure ResolveConflicts(const Entry: TEntry; NewInput: TCombinedKey);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function RegisterBinding(const ContextId, CategoryId, HelpToken: UTF8String;
      DefaultInput, OutputKey: TCombinedKey): TKeyBindingHandle;

    function TranslateKey(const ContextId: UTF8String;
      PressedKey: TCombinedKey): TCombinedKey;

    procedure UpdateBinding(Handle: TKeyBindingHandle; NewInput: TCombinedKey);
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
function CombinedKeyToModifierMask(Key: TCombinedKey): word;
function CombinedKeyToKeyCode(Key: TCombinedKey): cardinal;
function NormalizeCombinedKey(Key: TCombinedKey): TCombinedKey;
function MakeCombinedKey(ModState: word; KeyCode: cardinal): TCombinedKey;
function CombinedKeyToTokenString(Key: TCombinedKey): UTF8String;
function ParseCombinedKeyToken(const Value: UTF8String; out Key: TCombinedKey): boolean;

implementation

uses
  UUnicodeUtils;

const
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

function CombinedKeyToModifierMask(Key: TCombinedKey): word;
begin
  Result := NormalizeModifierState(word(Key shr 32));
end;

function CombinedKeyToKeyCode(Key: TCombinedKey): cardinal;
begin
  Result := cardinal(Key and $FFFFFFFF);
end;

function NormalizeCombinedKey(Key: TCombinedKey): TCombinedKey;
var
  Code: cardinal;
  Mods: word;
begin
  if Key = IGNORE_KEY then
    Exit(IGNORE_KEY);
  Code := CombinedKeyToKeyCode(Key);
  Mods := CombinedKeyToModifierMask(Key);
  Result := (QWord(Mods) shl 32) or (QWord(Code) and $FFFFFFFF);
end;

function MakeCombinedKey(ModState: word; KeyCode: cardinal): TCombinedKey;
begin
  if (KeyCode = SDLK_UNKNOWN) or (KeyCode = 0) then
    Exit(IGNORE_KEY);
  Result := (QWord(NormalizeModifierState(ModState)) shl 32) or QWord(KeyCode);
end;

function CombinedKeyToTokenString(Key: TCombinedKey): UTF8String;
var
  Parts: UTF8String;
  Mods: word;
  KeyName: UTF8String;
begin
  if Key = IGNORE_KEY then
    Exit('');

  Mods := CombinedKeyToModifierMask(Key);
  KeyName := KeyCodeToToken(CombinedKeyToKeyCode(Key));
  Parts := '';
  if (Mods and KMOD_CTRL) <> 0 then
  begin
    if Parts <> '' then
      Parts := Parts + '+';
    Parts := Parts + 'CTRL';
  end;
  if (Mods and KMOD_ALT) <> 0 then
  begin
    if Parts <> '' then
      Parts := Parts + '+';
    Parts := Parts + 'ALT';
  end;
  if (Mods and KMOD_SHIFT) <> 0 then
  begin
    if Parts <> '' then
      Parts := Parts + '+';
    Parts := Parts + 'SHIFT';
  end;
  if KeyName <> '' then
  begin
    if Parts <> '' then
      Parts := Parts + '+';
    Parts := Parts + KeyName;
  end;
  Result := Parts;
end;

function ParseCombinedKeyToken(const Value: UTF8String; out Key: TCombinedKey): boolean;
var
  Remaining: UTF8String;
  Segment: UTF8String;
  Sep: SizeInt;
  Mods: word;
  KeyCode: cardinal;
begin
  Key := IGNORE_KEY;
  if Value = '' then
    Exit(false);

  Remaining := UTF8UpperCase(Trim(Value));
  Mods := 0;
  KeyCode := SDLK_UNKNOWN;

  while Remaining <> '' do
  begin
    Sep := Pos('+', Remaining);
    if Sep > 0 then
    begin
      Segment := Trim(Copy(Remaining, 1, Sep - 1));
      Delete(Remaining, 1, Sep);
    end
    else
    begin
      Segment := Trim(Remaining);
      Remaining := '';
    end;

    if Segment = '' then
      Continue;

    if (Segment = 'CTRL') or (Segment = 'CONTROL') then
      Mods := Mods or KMOD_CTRL
    else if Segment = 'ALT' then
      Mods := Mods or KMOD_ALT
    else if Segment = 'SHIFT' then
      Mods := Mods or KMOD_SHIFT
    else
    begin
      if Assigned(KeyBindings) then
        KeyCode := KeyBindings.KeyNameToCode(Segment)
      else
        KeyCode := SDL_GetKeyFromName(PAnsiChar(AnsiString(Segment)));

      if KeyCode = SDLK_UNKNOWN then
        Exit(false);
    end;
  end;

  if KeyCode = SDLK_UNKNOWN then
    Exit(false);

  Key := MakeCombinedKey(Mods, KeyCode);
  Result := true;
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
  DefaultInput: TCombinedKey; DuplicateIndex: integer): UTF8String;
var
  TokenWithIndex: UTF8String;
  DefaultToken: UTF8String;
begin
  TokenWithIndex := HelpToken;
  if DuplicateIndex > 1 then
    TokenWithIndex := TokenWithIndex + IntToStr(DuplicateIndex);
  DefaultToken := CombinedKeyToTokenString(DefaultInput);
  if DefaultToken = '' then
    DefaultToken := 'NONE';
  Result := UTF8UpperCase(Format('%s.%s.%s',
    [ContextId, TokenWithIndex, DefaultToken]));
end;

function TKeyBindingRegistry.LookupOverride(const ContextId, HelpToken: UTF8String;
  DefaultInput: TCombinedKey; DuplicateIndex: integer;
  out Key: TCombinedKey): boolean;
var
  OverrideKey: UTF8String;
  Name: UTF8String;
begin
  Result := false;
  Key := IGNORE_KEY;
  OverrideKey := BuildOverrideKey(ContextId, HelpToken, DefaultInput, DuplicateIndex);
  Name := UTF8String(FOverrides.Values[OverrideKey]);
  if Name = '' then
    Exit;

  if SameText(Name, UNBOUND_OVERRIDE_VALUE) then
  begin
    Key := IGNORE_KEY;
    Exit(true);
  end;

  if ParseCombinedKeyToken(Name, Key) then
  begin
    Result := true;
    Exit;
  end;
end;

function TKeyBindingRegistry.KeyNameToCode(const KeyName: UTF8String): cardinal;
var
  Code: integer;
  Upper: UTF8String;
begin
  Upper := UTF8UpperCase(KeyName);
  if Upper = 'SLASH' then
    Exit(SDLK_SLASH)
  else if Upper = 'BACKSLASH' then
    Exit(SDLK_BACKSLASH)
  else if Upper = 'BACKQUOTE' then
    Exit(SDLK_BACKQUOTE)
  else if Upper = 'EQUALS' then
    Exit(SDLK_EQUALS)
  else if Upper = 'MINUS' then
    Exit(SDLK_MINUS)
  else if Upper = 'PERIOD' then
    Exit(SDLK_PERIOD)
  else if Upper = 'COMMA' then
    Exit(SDLK_COMMA)
  else if Upper = 'SEMICOLON' then
    Exit(SDLK_SEMICOLON)
  else if Upper = 'QUOTE' then
    Exit(SDLK_QUOTE)
  else if Upper = 'LEFTBRACKET' then
    Exit(SDLK_LEFTBRACKET)
  else if Upper = 'RIGHTBRACKET' then
    Exit(SDLK_RIGHTBRACKET)
  else if Upper = 'PAGEUP' then
    Exit(SDLK_PAGEUP)
  else if Upper = 'PAGEDOWN' then
    Exit(SDLK_PAGEDOWN)
  else if Upper = 'DELETE' then
    Exit(SDLK_DELETE)
  else if Upper = 'BACKSPACE' then
    Exit(SDLK_BACKSPACE)
  else if Upper = 'ESC' then
    Exit(SDLK_ESCAPE)
  else if Upper = 'RETURN' then
    Exit(SDLK_RETURN)
  else if Upper = 'TAB' then
    Exit(SDLK_TAB)
  else if Upper = 'UP' then
    Exit(SDLK_UP)
  else if Upper = 'DOWN' then
    Exit(SDLK_DOWN)
  else if Upper = 'LEFT' then
    Exit(SDLK_LEFT)
  else if Upper = 'RIGHT' then
    Exit(SDLK_RIGHT)
  else if Upper = 'SPACE' then
    Exit(SDLK_SPACE)
  else if Upper = 'KPPLUS' then
    Exit(SDLK_KP_PLUS)
  else if Upper = 'KPMINUS' then
    Exit(SDLK_KP_MINUS)
  else if Upper = 'KPENTER' then
    Exit(SDLK_KP_ENTER)
  else if Upper = 'KPMULTIPLY' then
    Exit(SDLK_KP_MULTIPLY)
  else if Upper = 'KPDIVIDE' then
    Exit(SDLK_KP_DIVIDE)
  else if (Copy(Upper, 1, 1) = 'F') and TryStrToInt(Copy(Upper, 2, Length(Upper) - 1), Code) and
          (Code >= 1) and (Code <= 24) then
    Exit(SDLK_F1 + (Code - 1))
  else if (Length(Upper) = 1) and (Upper[1] in ['0'..'9']) then
    Exit(SDLK_0 + Ord(Upper[1]) - Ord('0'))
  else if (Length(Upper) = 1) and (Upper[1] in ['A'..'Z']) then
    Exit(SDLK_a + Ord(Upper[1]) - Ord('A'))
  else if (Copy(Upper, 1, 2) = 'KP') and (Length(Upper) = 3) and (Upper[3] in ['0'..'9']) then
    Exit(SDLK_KP_0 + Ord(Upper[3]) - Ord('0'));

  Code := SDL_GetKeyFromName(PAnsiChar(AnsiString(Upper)));
  if Code = SDLK_UNKNOWN then
    Code := SDL_GetKeyFromName(PAnsiChar(AnsiString(KeyName)));
  Result := Code;
end;

function TKeyBindingRegistry.KeyCodeToName(KeyCode: cardinal): UTF8String;
begin
  if KeyCode = IGNORE_KEY then
    Exit('');
  Result := UTF8UpperCase(UTF8String(SDL_GetKeyName(KeyCode)));
  if Result = '' then
    Result := IntToStr(KeyCode);
end;

procedure TKeyBindingRegistry.SetCurrentInput(var Entry: TEntry; NewInput: TCombinedKey);
begin
  Entry.CurrentInput := NormalizeCombinedKey(NewInput);
end;

procedure TKeyBindingRegistry.RemoveOverride(const ContextId, HelpToken: UTF8String;
  DefaultInput: TCombinedKey; DuplicateIndex: integer);
var
  Key: UTF8String;
  Idx: integer;
begin
  Key := BuildOverrideKey(ContextId, HelpToken, DefaultInput, DuplicateIndex);
  Idx := FOverrides.IndexOfName(Key);
  if Idx >= 0 then
    FOverrides.Delete(Idx);
end;

procedure TKeyBindingRegistry.StoreOverride(const ContextId, HelpToken: UTF8String;
  DefaultInput: TCombinedKey; DuplicateIndex: integer; KeyValue: TCombinedKey);
var
  Key: UTF8String;
  Value: UTF8String;
begin
  Key := BuildOverrideKey(ContextId, HelpToken, DefaultInput, DuplicateIndex);
  if KeyValue = IGNORE_KEY then
    Value := UNBOUND_OVERRIDE_VALUE
  else
    Value := CombinedKeyToTokenString(KeyValue);

  if Value = '' then
    RemoveOverride(ContextId, HelpToken, DefaultInput, DuplicateIndex)
  else
    FOverrides.Values[Key] := UTF8UpperCase(Value);
end;

procedure TKeyBindingRegistry.ResolveConflicts(const Entry: TEntry; NewInput: TCombinedKey);
var
  I: integer;
begin
  for I := 0 to High(FEntries) do
  begin
    if (FEntries[I].Handle = Entry.Handle) then
      Continue;
    if FEntries[I].ContextIdx <> Entry.ContextIdx then
      Continue;
    if FEntries[I].CurrentInput <> NewInput then
      Continue;

    if FEntries[I].DefaultInput <> NewInput then
    begin
      FEntries[I].CurrentInput := FEntries[I].DefaultInput;
    end
    else
    begin
      FEntries[I].CurrentInput := IGNORE_KEY;
    end;

    if FEntries[I].CurrentInput = FEntries[I].DefaultInput then
      RemoveOverride(FContexts[FEntries[I].ContextIdx].Id, FEntries[I].HelpToken,
        FEntries[I].DefaultInput, FEntries[I].DuplicateIndex)
    else
      StoreOverride(FContexts[FEntries[I].ContextIdx].Id, FEntries[I].HelpToken,
        FEntries[I].DefaultInput, FEntries[I].DuplicateIndex,
        FEntries[I].CurrentInput);
  end;
end;

function TKeyBindingRegistry.RegisterBinding(const ContextId, CategoryId, HelpToken: UTF8String;
  DefaultInput, OutputKey: TCombinedKey): TKeyBindingHandle;
var
  ContextIdx: integer;
  EntryIdx: integer;
  OverrideKey: TCombinedKey;
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
  Entry.DefaultInput := NormalizeCombinedKey(DefaultInput);
  Entry.CurrentInput := Entry.DefaultInput;
  Entry.OutputKey := NormalizeCombinedKey(OutputKey);
  Entry.EntryOrder := FContexts[ContextIdx].RegistrationCount;
  Inc(FContexts[ContextIdx].RegistrationCount);

  if LookupOverride(ContextId, HelpToken,
    Entry.DefaultInput, Entry.DuplicateIndex, OverrideKey) then
    Entry.CurrentInput := OverrideKey;

  FEntries[EntryIdx] := Entry;
  Result := Entry.Handle;
end;

function TKeyBindingRegistry.TranslateKey(const ContextId: UTF8String;
  PressedKey: TCombinedKey): TCombinedKey;
var
  ContextIdx: integer;
  I: integer;
  Suppress: boolean;
  Key: TCombinedKey;
begin
  Key := NormalizeCombinedKey(PressedKey);
  Result := Key;
  ContextIdx := GetContextIndex(ContextId);
  if ContextIdx = -1 then
    Exit;

  Suppress := false;
  for I := 0 to High(FEntries) do
  begin
    if FEntries[I].ContextIdx <> ContextIdx then
      Continue;

    if (FEntries[I].CurrentInput <> IGNORE_KEY) and
       (FEntries[I].CurrentInput = Key) then
      Exit(FEntries[I].OutputKey);

    if (FEntries[I].DefaultInput = Key) and
       (FEntries[I].CurrentInput <> FEntries[I].DefaultInput) then
      Suppress := true;
  end;

  if Suppress then
    Result := IGNORE_KEY;
end;

procedure TKeyBindingRegistry.UpdateBinding(Handle: TKeyBindingHandle; NewInput: TCombinedKey);
var
  I: integer;
  ContextId: UTF8String;
  Value: TCombinedKey;
begin
  Value := NormalizeCombinedKey(NewInput);
  for I := 0 to High(FEntries) do
  begin
    if FEntries[I].Handle <> Handle then
      Continue;

    ResolveConflicts(FEntries[I], Value);
    FEntries[I].CurrentInput := Value;

    ContextId := FContexts[FEntries[I].ContextIdx].Id;
    if FEntries[I].CurrentInput = FEntries[I].DefaultInput then
      RemoveOverride(ContextId, FEntries[I].HelpToken,
        FEntries[I].DefaultInput, FEntries[I].DuplicateIndex)
    else
      StoreOverride(ContextId, FEntries[I].HelpToken,
        FEntries[I].DefaultInput, FEntries[I].DuplicateIndex,
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
      FEntries[I].DefaultInput, FEntries[I].DuplicateIndex);
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
  I, J: integer;
  DotCount: integer;
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
      DotCount := 0;
      for J := 1 to Length(Name) do
        if Name[J] = '.' then
          Inc(DotCount);
      if (Length(Name) > 0) and (DotCount = 2) then
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
