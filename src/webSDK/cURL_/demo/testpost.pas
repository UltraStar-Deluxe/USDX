program testpost;

(* Program to test HTTP POST variables *)

{$INCLUDE curltest.inc}

uses curlobj;

var 
  MyCurl:tCurl;
  Name, Color, Food:string;

begin
  Write('Enter your nickname: ');
  ReadLn(Name);
  Write('Your favorite color: ');
  ReadLn(Color);
  Write('Your favorite food: ');
  ReadLn(Food);
  MyCurl:=tCurl.Create(nil);
  MyCurl.URL:='http://curlpas.sourceforge.net/tests/testpost.php';
  MyCurl.PostFields:= 'NAME=' + Name + '&COLOR=' + Color + '&FOOD=' + Food;
  if not MyCurl.Perform then WriteLn(MyCurl.ErrorString);
  MyCurl.Free;
end.
