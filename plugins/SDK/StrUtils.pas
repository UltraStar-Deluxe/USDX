unit StrUtils;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses ModiSDK;

//function StrToAChar(Str: String): AChar;
function CreateStr(Str: PChar): PChar;
procedure FreeStr(Str: PChar);

implementation

{$IFDEF FPC}
  {$ASMMODE Intel}
{$ENDIF}

{function StrToAChar(Str: String): AChar;
var
  L, I: Integer;
begin
  L := Length(Str);
  For I := 0 to L-1 do
    AChar[I] := Str[I+1];

  For I := L to 254 do
    AChar[I] := #0;
end; }

function StrCopy(Dest, Source: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        POP     ESI
        POP     EDI
end;

function StrLen(Str: PChar): Cardinal; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function CreateStr(Str: PChar): PChar;
begin
  GetMem(Result, StrLen(Str) + 1);
  StrCopy(Result, Str);
end;

procedure FreeStr(Str: PChar);
begin
  FreeMem(Str);
end;

end.
