program USDXResCompiler;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  LResources;

var
  lResourceFile : TMemoryStream;
  
procedure AddFile( aResName, aResType, aFile : string );
var
  lTmpStream : TmemoryStream;
begin
  if aFile[1] = '"' then
  begin
    aFile := copy( aFile, 2, length( aFile ) - 2 );
  end;

  if not fileexists( aFile ) then
  begin
    writeln( 'SKIPED' + ' ( '+aFile+' ) File not found' );
    exit;
  end;
    
  lTmpStream := TmemoryStream.create();
  try
    lTmpStream.loadfromfile( aFile );
    lTmpStream.position := 0;
    
    BinaryToLazarusResourceCode(lTmpStream, lResourceFile, aResName, aResType);
    writeln( 'ADDED - ' + aResType + ' : ' + aResName + ' ( '+aFile+' )' );
  finally
    freeandnil( lTmpStream );
  end;
end;

procedure addresourceline( aRCLine : String );
var
  lName : String;
  lType : String;
  lFile : String;
  lTmp  ,
  lTmp2 : Integer;
begin
  if trim( aRCLine ) = '' then
    exit;
    
  if aRCLine[1] = '#' then
    exit;

  if ( aRCLine[1] = '/' ) AND
     ( aRCLine[2] = '/' ) THEN
    exit;

  // find 2nd column... and put it in lTmp
  lTmp  := pos( ' ', aRcLine );
  lTmp2 := pos( #9, aRcLine  );
  if lTmp2 < lTmp then
    lTmp := lTmp2;
  
  // Name = Start to lTmp
  lName := trim( copy( aRcLine, 1, lTmp ) );

  // Type = lTmp to first "
  lTmp2 := pos( '"', aRcLine );
  lType := trim( copy( aRcLine, lTmp, lTmp2-lTmp ) );

  // File = " to end of line
  lFile := trim( copy( aRcLine, lTmp2, maxint ) );

  lFile := StringReplace( lFile, '\', PathDelim ,[rfReplaceAll] );

(*
  writeln( aRcLine );
  writeln( lName );
  writeln( lType );
  writeln( lFile );
  writeln( '' );
*)
  AddFile( lName , lType , lFile );

end;


var
  lRCFile : TStringList;
  iCount  : Integer;
begin
  if not fileexists( paramstr(1) ) then
  begin
    writeln( 'MUST Specify Delphi format RC File' );
    exit;
  end;

  lRCFile := TStringList.create();
  lResourceFile := TMemoryStream.create();
  try
     lRCFile.loadfromfile( paramstr(1) );
     
     if trim( lRCFile.text ) = '' then
       exit;
     
     for iCount := 0 to lRCFile.count -1 do
     begin
       addresourceline( lRCFile[ iCount ] );
     end;
  
(*
     AddFile( 'Font', 'TEX', '..\Fonts\Normal\eurostar_regular.png' );
     AddFile( 'Font', 'FNT', '..\Fonts\Normal\eurostar_regular.dat' );

     AddFile( 'FontB', 'TEX', '..\Fonts\Bold\eurostar_regular_bold.png' );
     AddFile( 'FontB', 'FNT', '..\Fonts\Bold\eurostar_regular_bold.dat' );

     AddFile( 'FontO', 'TEX', '..\Fonts\Outline 1\Outline 1.PNG' );
     AddFile( 'FontO', 'FNT', '..\Fonts\Outline 1\Outline 1.dat' );
     
     AddFile( 'FontO2', 'TEX', '..\Fonts\Outline 2\Outline 2.PNG' );
     AddFile( 'FontO2', 'FNT', '..\Fonts\Outline 2\Outline 2.dat' );
     
     AddFile( 'MAINICON', 'ICON', '..\Graphics\ustar-icon_v01.ico' );


     AddFile( 'CRDTS_BG', 'TEX', '..\Graphics\credits_v5_bg.png' );
     AddFile( 'CRDTS_OVL', 'TEX', '..\Graphics\credits_v5_overlay.png' );
     AddFile( 'CRDTS_blindguard', 'TEX', '..\Graphics\names_blindguard.png' );
     AddFile( 'CRDTS_blindy', 'TEX', '..\Graphics\names_blindy.png' );
     AddFile( 'CRDTS_canni', 'TEX', '..\Graphics\names_canni.png' );
     AddFile( 'CRDTS_commandio', 'TEX', '..\Graphics\names_commandio.png' );
     AddFile( 'CRDTS_lazyjoker', 'TEX', '..\Graphics\names_lazyjoker.png' );
     AddFile( 'CRDTS_mog', 'TEX', '..\Graphics\names_mog.png' );
     AddFile( 'CRDTS_mota', 'TEX', '..\Graphics\names_mota.png' );
     AddFile( 'CRDTS_skillmaste', 'TEX', '..\Graphics\names_skillmaster.png' );
     AddFile( 'CRDTS_whiteshark', 'TEX', '..\Graphics\names_whiteshark.png' );
     AddFile( 'INTRO_L01', 'TEX', '..\Graphics\intro-l-01.png' );
     AddFile( 'INTRO_L02', 'TEX', '..\Graphics\intro-l-02.png' );
     AddFile( 'INTRO_L03', 'TEX', '..\Graphics\intro-l-03.png' );
     AddFile( 'INTRO_L04', 'TEX', '..\Graphics\intro-l-04.png' );
     AddFile( 'INTRO_L05', 'TEX', '..\Graphics\intro-l-05.png' );
     AddFile( 'INTRO_L06', 'TEX', '..\Graphics\intro-l-06.png' );
     AddFile( 'INTRO_L07', 'TEX', '..\Graphics\intro-l-07.png' );
     AddFile( 'INTRO_L08', 'TEX', '..\Graphics\intro-l-08.png' );
     AddFile( 'INTRO_L09', 'TEX', '..\Graphics\intro-l-09.png' );
     AddFile( 'OUTRO_BG', 'TEX', '..\Graphics\outro-bg.png' );
     AddFile( 'OUTRO_ESC', 'TEX', '..\Graphics\outro-esc.png' );
     AddFile( 'OUTRO_EXD', 'TEX', '..\Graphics\outro-exit-dark.png' );
*)

  finally
    lResourceFile.SaveToFile( 'UltraStar.lrs' );
    freeandnil( lResourceFile );
    
    freeandnil( lRCFile );
  end;
end.

