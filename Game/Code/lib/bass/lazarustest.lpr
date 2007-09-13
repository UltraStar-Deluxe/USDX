program lazarustest;

{$mode objfpc}{$H+}

uses
  sysutils,
  bass in 'delphi\bass.pas'
  { add your units here };

var
  chan : HSTREAM;

begin
  // check the correct BASS was loaded
  if (hi(BASS_GetVersion) <> BASSVERSION) then
  begin
    writeln('An incorrect version of BASS.DLL was loaded');
    Halt;
  end;
  
  //init BASS
  if not BASS_Init(0,44100,0,0,nil) then
    writeln('Can''t initialize device');
    
  //creating stream
  if fileexists( 'music.mp3 ') then
  begin
    chan := BASS_StreamCreateFile(FALSE,pchar('music.mp3'),0,0,0);
    if chan = 0 then
    begin
      chan := BASS_MusicLoad(False, pchar('music.mp3'), 0, 0, BASS_MUSIC_RAMPS or BASS_MUSIC_POSRESET or BASS_MUSIC_PRESCAN, 0);
      if (chan = 0) then
      begin
        writeln('Can''t play file');
        Exit;
      end;
    end;

    BASS_ChannelPlay(chan,FALSE); // start playing
  end
  else
  begin
    writeln( 'music.mp3 not found.. could not test mp3 playback');
  end;
    
  BASS_Free();
  writeln( 'Seems BASS is compatible with lazarus :) ');


end.

