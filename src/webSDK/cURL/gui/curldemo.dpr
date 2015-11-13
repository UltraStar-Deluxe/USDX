program curldemo;


{$IFDEF FPC}
   {$MODE OBJFPC}{$H+}
   uses Interfaces, Forms, curlform, curlpas;
{$ELSE}
   uses {$IFDEF LINUX}QForms,{$ELSE}Forms,{$ENDIF}
   curlform in 'curlform.pas' {Form1};
   {$R *.res}
{$ENDIF}



begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
