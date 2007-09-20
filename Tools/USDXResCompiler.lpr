program USDXResCompiler;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  LResources;

var
  lResourceFile : TMemoryStream;
  
procedure AddFile( aResName, aResTye, aFile : string );
var
  lTmpStream : TmemoryStream;
begin
  writeln();
  writeln( aFile );
  if not fileexists( aFile ) then
  begin
    writeln( 'NOT FOUND' );
    exit;
  end;
    
  lTmpStream := TmemoryStream.create();
  try
    lTmpStream.loadfromfile( aFile );
    lTmpStream.position := 0;
    
    BinaryToLazarusResourceCode(lTmpStream, lResourceFile, aResName, aResTye);
    writeln( 'Added' );
  finally
    freeandnil( lTmpStream );
  end;
end;
  
  
begin

  lResourceFile := TMemoryStream.create();
  try
     AddFile( 'Font', 'PNG', '..\Fonts\Normal\eurostar_regular.png' );
     AddFile( 'Font', 'FNT', '.\Fonts\Normal\eurostar_regular.dat' );

     AddFile( 'FontB', 'PNG', '..\Fonts\Bold\eurostar_regular_bold.png' );
     AddFile( 'FontB', 'FNT', '..\Fonts\Bold\eurostar_regular_bold.dat' );

     AddFile( 'FontO', 'PNG', '..\Fonts\Outline 1\Outline 1.PNG' );
     AddFile( 'FontO', 'FNT', '..\Fonts\Outline 1\Outline 1.dat' );
     
     AddFile( 'FontO2', 'PNG', '..\Fonts\Outline 2\Outline 2.PNG' );
     AddFile( 'FontO2', 'FNT', '..\Fonts\Outline 2\Outline 2.dat' );
     
     AddFile( 'MAINICON', 'ICON', '..\Graphics\ustar-icon_v01.ico' );

     AddFile( 'MAINICON', 'ICON', '..\Graphics\ustar-icon_v01.ico' );

     AddFile( 'CRDTS_BG', 'PNG', '..\Graphics\credits_v5_bg.png' );
     AddFile( 'CRDTS_OVL', 'PNG', '..\Graphics\credits_v5_overlay.png"' );
     AddFile( 'CRDTS_blindguard', 'PNG', '..\Graphics\names_blindguard.png' );
     AddFile( 'CRDTS_blindy', 'PNG', '..\Graphics\names_blindy.png' );
     AddFile( 'CRDTS_canni', 'PNG', '..\Graphics\names_canni.png' );
     AddFile( 'CRDTS_commandio', 'PNG', '..\Graphics\names_commandio.png' );
     AddFile( 'CRDTS_lazyjoker', 'PNG', '..\Graphics\names_lazyjoker.png' );
     AddFile( 'CRDTS_mog', 'PNG', '..\Graphics\names_mog.png' );
     AddFile( 'CRDTS_mota', 'PNG', '..\Graphics\names_mota.png' );
     AddFile( 'CRDTS_skillmaste', 'PNG', '..\Graphics\names_skillmaster.png' );
     AddFile( 'CRDTS_whiteshark', 'PNG', '..\Graphics\names_whiteshark.png' );
     AddFile( 'INTRO_L01', 'PNG', '..\Graphics\intro-l-01.png' );
     AddFile( 'INTRO_L02', 'PNG', '..\Graphics\intro-l-02.png' );
     AddFile( 'INTRO_L03', 'PNG', '..\Graphics\intro-l-03.png' );
     AddFile( 'INTRO_L04', 'PNG', '..\Graphics\intro-l-04.png' );
     AddFile( 'INTRO_L05', 'PNG', '..\Graphics\intro-l-05.png' );
     AddFile( 'INTRO_L06', 'PNG', '..\Graphics\intro-l-06.png' );
     AddFile( 'INTRO_L07', 'PNG', '..\Graphics\intro-l-07.png' );
     AddFile( 'INTRO_L08', 'PNG', '..\Graphics\intro-l-08.png' );
     AddFile( 'INTRO_L09', 'PNG', '..\Graphics\intro-l-09.png' );
     AddFile( 'OUTRO_BG', 'PNG', '..\Graphics\outro-bg.png' );
     AddFile( 'OUTRO_ESC', 'PNG', '..\Graphics\outro-esc.png' );
     AddFile( 'OUTRO_EXD', 'PNG', '..\Graphics\outro-exit-dark.png' );


  finally
    lResourceFile.SaveToFile( 'UltraStar.lrs' );
    freeandnil( lResourceFile );
  end;
end.

