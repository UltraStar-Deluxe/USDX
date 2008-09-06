unit UMenuButtonCollection;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses UMenuButton;

type
  //----------------
  //TButtonCollection
  //No Extra Attributes or Functions ATM
  //----------------
  AButton = Array of TButton;
  PAButton = ^AButton;
  TButtonCollection = class(TButton)
    //num of the First Button, that can be Selected
    FirstChild: Byte;
    CountChilds: Byte;
    
    ScreenButton: PAButton;

    procedure SetSelect(Value : Boolean); override;
    procedure Draw; override;
  end;

implementation

procedure TButtonCollection.SetSelect(Value : Boolean);
var I: Integer;
begin
  inherited;

  //Set Visible for Every Button that is a Child of this ButtonCollection
  if (Not Fade) then
    For I := 0 to High(ScreenButton^) do
      if (ScreenButton^[I].Parent = Parent) then
        ScreenButton^[I].Visible := Value;
end;

procedure TButtonCollection.Draw;
var I, J: Integer;
begin
  inherited;
  //If fading is activated, Fade Child Buttons
  if (Fade) then
  begin
    For I := 0 to High(ScreenButton^) do
      if (ScreenButton^[I].Parent = Parent) then
      begin
        if (FadeProgress < 0.5) then
        begin
          ScreenButton^[I].Visible := SelectBool;

          For J := 0 to High(ScreenButton^[I].Text) do
            ScreenButton^[I].Text[J].Visible := SelectBool;
        end
        else
        begin
          ScreenButton^[I].Texture.Alpha := (FadeProgress-0.666)*3;
          
          For J := 0 to High(ScreenButton^[I].Text) do
            ScreenButton^[I].Text[J].Alpha := (FadeProgress-0.666)*3;
        end;
      end;
  end;
end;



end.
