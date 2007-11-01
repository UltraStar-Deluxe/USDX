unit Messages;

{$I switches.inc}

interface

uses Windows;

type

  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0: (
        WParam: Longint;
        LParam: Longint;
        Result: Longint);
      1: (
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);
  end;
  PMessage = ^TMessage;

implementation

end.