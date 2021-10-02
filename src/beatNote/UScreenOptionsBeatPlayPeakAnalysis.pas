{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UScreenOptionsBeatPlayPeakAnalysis;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UFiles,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  URecord,
  sdl2,
  UBeatNote;




type

TBeatPeakInfo = record
    BufferIndex: integer; // Detected onset
    BufferIndexLast: integer; // Last point considered to be part of the peak
    Volume:                 single; // Fraction of to full scale
    RiseRateMillisecond:         single; // Rise rate in units of fraction of full scale, per ms
    MinPeakMillisecond:                 single; // Estimation of the peak duration in milliseconds
    DropAfterPeak:                 single; // Drop after the peak in full scale units
    PeakValid:                            boolean;
 end;

// Class definition for the options screen for the tapping (accessible through
// Tools -> Options -> Beat Tapping in the english version
  TScreenOptionsBeatPlayPeakAnalysis = class(TMenu)
    private
      // current input device
      CurrentDeviceIndex: integer;
      PreviewDeviceIndex: integer;
      CurrentChannel: array of integer;

      // string arrays for select-slide options
      InputSourceNames: array of UTF8String;
      InputDeviceNames: array of UTF8String;
      SelectChannelOptions: array of UTF8String;

      // indices for widget-updates
      SelectInputSourceID: integer;
      SelectChannelID: integer;
      SelectIntensityID: integer;
      SelectMicBoostID: integer;
      //SelectRiseRateID: integer; not used
      //SelectMinPeakID: integer; not used
      //SelectFallRateID: integer; not used

      PreviewChannel: TCaptureBuffer; // For showing the current sound trace

      peakDetected: Boolean; // True when we saw a peak
      peakDetectionTime : cardinal;
      peakInfo: TBeatPeakInfo;

      AnalysisBufferAtPeakDetection:  array of smallint;

       // interaction IDs
      ExitButtonIID: integer;

    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure onHide; override;
      function  Draw: boolean; override;
      procedure UpdateInputDevice;

      procedure StartPreview;
      procedure StopPreview;

      procedure doBeatDetection; // With the current parameters, try beat detection on running recording

      procedure BeatDrawOscilloscope(X, Y, W, H: real);

  end;

function AnalyzeBufferBeatDetails(AnalysisBuffer: array of smallint; AudioFormat: TAudioFormatInfo;
         timeBack: real; Threshold: integer; RiseRate: integer; MinPeakDuration: integer;
                               DropAfterPeak: integer; TestTimeAfterPeak: integer): TBeatPeakInfo;


implementation

uses
  UGraphic,
  UHelp,
  ULog,
  UUnicodeUtils,
  SysUtils,
  dglOpenGL,
  UCommon,
  TextGL;

type
TGetTextFunc = function(var Param: integer; Offset: integer; Modify: boolean; OptText: PUtf8String): boolean;
UTF8StringArray = array of UTF8String;

function TScreenOptionsBeatPlayPeakAnalysis.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptionsBeatPlay);
        end;
      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;
      SDLK_RETURN:
        begin
          if SelInteraction = 7 then
          begin
            //Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptionsBeatPlay);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < 7) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
          UpdateInputDevice;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < 7) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          UpdateInputDevice;
    end;
  end;

end;



end;

constructor TScreenOptionsBeatPlayPeakAnalysis.Create;
var
  DeviceIndex:  integer;
  SourceIndex:  integer;
  ChannelIndex: integer;

  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
  WidgetYPos: integer;

begin
  inherited Create;

  LoadFromTheme(Theme.OptionsBeatDetect);

  // set CurrentDeviceIndex to a valid device
  if (Length(AudioInputProcessor.DeviceList) > 0) then
    CurrentDeviceIndex := 0
  else
    CurrentDeviceIndex := -1;

  PreviewDeviceIndex := -1;

  peakDetected:=false; // For the visualization by the oscilloscope
  peakDetectionTime:=0;



  // init sliders if at least one device was detected
  if (Length(AudioInputProcessor.DeviceList) > 0) then
  begin
    SetLength(CurrentChannel, High(AudioInputProcessor.DeviceList)+1);
    for DeviceIndex:=0 to High(AudioInputProcessor.DeviceList) do
        CurrentChannel[DeviceIndex]:=0;

    InputDevice := AudioInputProcessor.DeviceList[CurrentDeviceIndex];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];

    // init device-selection slider
    SetLength(InputDeviceNames, Length(AudioInputProcessor.DeviceList));
    for DeviceIndex := 0 to High(AudioInputProcessor.DeviceList) do
    begin
      InputDeviceNames[DeviceIndex] := AudioInputProcessor.DeviceList[DeviceIndex].Name;
    end;
    // add device-selection slider (InteractionID: 0)
    Theme.OptionsBeatDetect.SelectSlideCard.showArrows := true;
    Theme.OptionsBeatDetect.SelectSlideCard.oneItemOnly := true;
    AddSelectSlide(Theme.OptionsBeatDetect.SelectSlideCard, CurrentDeviceIndex, InputDeviceNames);

    // init source-selection slider
    SetLength(InputSourceNames, Length(InputDevice.Source));
    for SourceIndex := 0 to High(InputDevice.Source) do
    begin
      InputSourceNames[SourceIndex] := InputDevice.Source[SourceIndex].Name;
    end;

    Theme.OptionsBeatDetect.SelectSlideInput.showArrows := true;
    Theme.OptionsBeatDetect.SelectSlideInput.oneItemOnly := true;
    // add source-selection slider (InteractionID: 1)
    SelectInputSourceID := AddSelectSlide(Theme.OptionsBeatDetect.SelectSlideInput,
        InputDeviceCfg.Input, InputSourceNames);

    // compute list of selectable channels
    SetLength(SelectChannelOptions, InputDevice.AudioFormat.Channels);
    for ChannelIndex := 0 to InputDevice.AudioFormat.Channels-1 do
    begin
      SelectChannelOptions[ChannelIndex] := IntToStr(ChannelIndex + 1);
    end;


    Theme.OptionsBeatDetect.SelectChannel.showArrows := true;
    Theme.OptionsBeatDetect.SelectChannel.oneItemOnly := true;
    SelectChannelID := AddSelectSlide(Theme.OptionsBeatDetect.SelectChannel,
        CurrentChannel[CurrentDeviceIndex], SelectChannelOptions);



    Theme.OptionsBeatDetect.SelectIntensityThreshold.showArrows := true;
    Theme.OptionsBeatDetect.SelectIntensityThreshold.oneItemOnly := true;
    SelectIntensityID := AddSelectSlide(Theme.OptionsBeatDetect.SelectIntensityThreshold,
        Ini.InputDeviceBeatDetectionConfig[0].ChannelBeatDectectionSettings[0].IntensityThreshold, IBeatDetectIntensityThreshold);


    Theme.OptionsBeatDetect.SelectMicBoost.showArrows := true;
    Theme.OptionsBeatDetect.SelectMicBoost.oneItemOnly := true;

    SelectMicBoostID:= AddSelectSlide(Theme.OptionsBeatDetect.SelectMicBoost, Ini.MicBoost, IMicBoostTranslated);



  end;






  ExitButtonIID:=AddButton(Theme.OptionsBeatDetect.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);



  Interaction := 0;



end;


procedure TScreenOptionsBeatPlayPeakAnalysis.OnShow;
begin
  inherited;

  Interaction := 0;

  // create preview sound-buffer
  PreviewChannel := TCaptureBuffer.Create();

  UpdateInputDevice();
end;

procedure TScreenOptionsBeatPlayPeakAnalysis.OnHide;
begin
  StopPreview();

  // free preview buffer
  PreviewChannel.Free;
end;



procedure TScreenOptionsBeatPlayPeakAnalysis.UpdateInputDevice;
var
  SourceIndex: integer;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
  ChannelIndex: integer;
begin
  //Log.LogStatus('Update input-device', 'TScreenOptionsRecord.UpdateCard') ;

  StopPreview();

  // set CurrentDeviceIndex to a valid device
  if (CurrentDeviceIndex > High(AudioInputProcessor.DeviceList)) then
    CurrentDeviceIndex := 0;



  // update sliders if at least one device was detected
  if (Length(AudioInputProcessor.DeviceList) > 0) then
  begin
    InputDevice := AudioInputProcessor.DeviceList[CurrentDeviceIndex];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];

    // update source-selection slider
    SetLength(InputSourceNames, Length(InputDevice.Source));
    for SourceIndex := 0 to High(InputDevice.Source) do
    begin
      InputSourceNames[SourceIndex] := InputDevice.Source[SourceIndex].Name;
    end;
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectInputSourceID,
        InputSourceNames, InputDeviceCfg.Input);
    // update channel
    SetLength(SelectChannelOptions, InputDevice.AudioFormat.Channels);
    for ChannelIndex := 0 to InputDevice.AudioFormat.Channels-1 do
    begin
      SelectChannelOptions[ChannelIndex] := IntToStr(ChannelIndex+1);
    end;
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectChannel,
      SelectChannelID, SelectChannelOptions,
      CurrentChannel[CurrentDeviceIndex]);

    ChannelIndex := CurrentChannel[CurrentDeviceIndex];



    // update the intensity threshold field
    UpdateSelectSlideOptions(Theme.OptionsBeatDetect.SelectIntensityThreshold,
        SelectIntensityID, IBeatDetectIntensityThreshold, Ini.InputDeviceBeatDetectionConfig[InputDeviceCfg.Input].
             ChannelBeatDectectionSettings[ChannelIndex].IntensityThreshold);

    // update the mic boost field
    UpdateSelectSlideOptions(Theme.OptionsBeatDetect.SelectMicBoost,
        SelectMicBoostID, IMicBoostTranslated, Ini.MicBoost);



  end;

  StartPreview();
end;

procedure TScreenOptionsBeatPlayPeakAnalysis.StartPreview;
var
  Device: TAudioInputDevice;
begin
  if ((CurrentDeviceIndex >= 0) and
      (CurrentDeviceIndex <= High(AudioInputProcessor.DeviceList))) then
  begin
    Device := AudioInputProcessor.DeviceList[CurrentDeviceIndex];

    // set preview channel as active capture channel
    PreviewChannel.Clear();
    Device.LinkCaptureBuffer(CurrentChannel[CurrentDeviceIndex], PreviewChannel);
    Device.Start();
    PreviewDeviceIndex := CurrentDeviceIndex;



  end;
end;


procedure TScreenOptionsBeatPlayPeakAnalysis.doBeatDetection;
var
  BeatDetectionParams: TBeatDetectionParameters;
  SampleIndex: integer;
begin
  if peakDetected then
  begin
     if SDL_GetTicks()>peakDetectionTime+2000 then
     begin
       peakDetected := false;
       peakDetectionTime:=0;
       for SampleIndex:=0 to High(AnalysisBufferAtPeakDetection) do
           AnalysisBufferAtPeakDetection[SampleIndex]:=0;
     end;
  end
  else
  begin

  BeatDetectionParams:=BeatDetectionParametersForDeviceAndChannel(CurrentDeviceIndex,CurrentChannel[CurrentDeviceIndex]);

  PreviewChannel.AnalyzeBufferBeatOnly(0.1,BeatDetectionParams.Threshold,
        BeatDetectionParams.RiseRate, BeatDetectionParams.MinPeakDuration, BeatDetectionParams.DropAfterPeak,
        BeatDetectionParams.TestTimeAfterPeak);

  if PreviewChannel.ToneValid then
    begin
       peakDetectionTime:=SDL_GetTicks();
       peakDetected:=true;
       setLength(AnalysisBufferAtPeakDetection,High(PreviewChannel.AnalysisBuffer)+1);
       for SampleIndex:=0 to High(AnalysisBufferAtPeakDetection) do
       begin
           AnalysisBufferAtPeakDetection[SampleIndex]:=PreviewChannel.AnalysisBuffer[SampleIndex];
       end;

       peakInfo:=AnalyzeBufferBeatDetails(AnalysisBufferAtPeakDetection, PreviewChannel.AudioFormat,
         0.1,BeatDetectionParams.Threshold,
        BeatDetectionParams.RiseRate, BeatDetectionParams.MinPeakDuration, BeatDetectionParams.DropAfterPeak,
        BeatDetectionParams.TestTimeAfterPeak);




    end;

  end; // End detection of new peak
end;

function TScreenOptionsBeatPlayPeakAnalysis.Draw: boolean;
begin
  DrawBG;
  DrawFG;

  doBeatDetection;

  BeatDrawOscilloscope(Theme.OptionsBeatDetect.ButtonExit.X+Theme.OptionsBeatDetect.ButtonExit.W+20,
       Theme.OptionsBeatDetect.ButtonExit.Y-50,
       Theme.OptionsBeatDetect.ButtonExit.W, 100);

  Result := true;
end;

procedure TScreenOptionsBeatPlayPeakAnalysis.BeatDrawOscilloscope(X, Y, W, H: real);
var
  SampleIndex: integer;

  MaxX, MaxY:  real;
  Col: TRGB;

begin

   Col.R:=255;
   Col.G:=255;
   Col.B:=255;

   MaxX := W-1;
   MaxY := (H-1) / 2;


   glColor3f(0, 0, 0.2);
   glBegin(GL_QUADS);
      glVertex2f(X , Y);
      glVertex2f(X , Y+2*MaxY);
      glVertex2f(X+MaxX , Y+2*MaxY);
      glVertex2f(X+MaxX , Y);
   glEnd;



   glColor3f(Col.R, Col.G, Col.B);




   glBegin(GL_LINE_STRIP);
    glVertex2f(X , Y);
    glVertex2f(X , Y+2*MaxY);
    glVertex2f(X+MaxX , Y+2*MaxY);
    glVertex2f(X+MaxX , Y);
    glVertex2f(X , Y);

  glEnd;



  if peakDetected then
  begin

    Col.R:=255;
    Col.G:=255;
    Col.B:=255;

    glColor3f(Col.R, Col.G, Col.B);


  glBegin(GL_LINE_STRIP);
    for SampleIndex := 0 to High(AnalysisBufferAtPeakDetection) do
    begin
      if (SampleIndex>=peakInfo.BufferIndex) and (SampleIndex<=peakInfo.BufferIndexLast) then
      begin
        Col.R:=255;
        Col.G:=0;
        Col.B:=0;

        glColor3f(Col.R, Col.G, Col.B);
      end else
      begin
         Col.R:=255;
        Col.G:=255;
        Col.B:=255;

        glColor3f(Col.R, Col.G, Col.B);
      end;
      glVertex2f(X + MaxX * SampleIndex/High(AnalysisBufferAtPeakDetection),
                 Y + MaxY * (1 - AnalysisBufferAtPeakDetection[SampleIndex]/-Low(Smallint)));
    end;
  glEnd;



  end
  else
  begin

  Col.R:=255;
  Col.G:=255;
  Col.B:=255;

  glColor3f(Col.R, Col.G, Col.B);
{
  if (ParamStr(1) = '-black') or (ParamStr(1) = '-fsblack') then
    glColor3f(1, 1, 1);
}


  PreviewChannel.LockAnalysisBuffer();



  glBegin(GL_LINE_STRIP);
    for SampleIndex := 0 to High(PreviewChannel.AnalysisBuffer) do
    begin
      glVertex2f(X + MaxX * SampleIndex/High(PreviewChannel.AnalysisBuffer),
                 Y + MaxY * (1 - PreviewChannel.AnalysisBuffer[SampleIndex]/-Low(Smallint)));
    end;
  glEnd;

  PreviewChannel.UnlockAnalysisBuffer();

  end; // end drawing oscilloscope line without peak
end;

// Preview as in UScreenOptionsRecord.pas, except for drawing the oscilloscope line instead of volume and pitch
procedure TScreenOptionsBeatPlayPeakAnalysis.StopPreview;
var
  ChannelIndex: integer;
  Device: TAudioInputDevice;
begin
  if ((PreviewDeviceIndex >= 0) and
      (PreviewDeviceIndex <= High(AudioInputProcessor.DeviceList))) then
  begin
    Device := AudioInputProcessor.DeviceList[PreviewDeviceIndex];
    Device.Stop;
    for ChannelIndex := 0 to High(Device.CaptureChannel) do
      Device.LinkCaptureBuffer(ChannelIndex, nil);
  end;
  PreviewDeviceIndex := -1;
  peakDetected:=false;
end;


function AnalyzeBufferBeatDetails(AnalysisBuffer: array of smallint; AudioFormat: TAudioFormatInfo;
         timeBack: real; Threshold: integer; RiseRate: integer; MinPeakDuration: integer;
                               DropAfterPeak: integer; TestTimeAfterPeak: integer): TBeatPeakInfo;
var


  ToneValid: boolean;    // not really used but to have the function compatible with the code in URecord, AnalyzeBufferBeatOnly
  Tone:      integer;    // not really used but to have the function compatible with the code in URecord, AnalyzeBufferBeatOnly
  ToneAbs:   integer;    // not really used but to have the function compatible with the code in URecord, AnalyzeBufferBeatOnly

  Volume:      single;
  MaxVolume:   single;
  MeanVolume:  single;
  SampleIndex, SampleInterval, SampleIndexPeak, StartSampleIndex: integer;
  BaselineStart, BaselineInterval, BaselineSampleIndex: integer; // To compare the peak sample values to baseline
  HighestMaxIndex: integer; // The largest index above threshold, to define a peak length
  SampleLowerLimit, SampleUpperLimit: integer;
  detected, maximumdetected:    Boolean;
  RMSVolume, RMSVolumeBaseline, riserate_evaluated:   single;
  // Four potential criteria for a succesful beat note detetion (this is to discriminate against background noise)
  passesThreshold: Boolean; // 1) Passing an absolute sound intensity threshold (anyways mandatory)
  passesRiseRate: Boolean; // 2) Sufficiently quick rise rate (depending on user configuration in Ini variable)
  passesDuration: Boolean; // 3) Sufficient duration (not just single off measurement, depends on configuration in Ini variable)
  passesDropAfterPeak: Boolean; // 4) Sufficient quick fall after peak (depending on user configuration in Ini variable)

  DefaultreturnVal: TBeatPeakInfo;  // Values to return if we do not find a peak
  returnVal: TBeatPeakInfo;

begin





  DefaultreturnVal.BufferIndex:=-1;
  DefaultreturnVal.BufferIndexLast:=-1;
  DefaultreturnVal.Volume:=0;
  DefaultreturnVal.RiseRateMillisecond:=0;
  DefaultreturnVal.MinPeakMillisecond:=0;
  DefaultreturnVal.DropAfterPeak:=0;
  DefaultreturnVal.PeakValid:=false;

  returnVal.BufferIndex:=-1;
  returnVal.BufferIndexLast:=-1;
  returnVal.Volume:=0;
  returnVal.RiseRateMillisecond:=0;
  returnVal.MinPeakMillisecond:=0;
  returnVal.DropAfterPeak:=0;
  returnVal.PeakValid:=false;



  ToneValid := false;
  ToneAbs := -1;
  Tone    := -1;
  detected := false;


  passesThreshold:=false;
  passesRiseRate:=false;
  passesDuration:=false;
  passesDropAfterPeak:=false;

  if RiseRate = 0 then
     passesRiseRate := true; // No rise rate requirement, so passes anyways

  if MinPeakDuration = 0 then
     passesDuration := true; // No minimal duration, so test passed anyways

  if DropAfterPeak =0 then
     passesDropAfterPeak:= true;



  StartSampleIndex:=High(AnalysisBuffer)-Round(timeBack*AudioFormat.SampleRate);

    if(StartSampleIndex < 0) then
      StartSampleIndex := 0;

  for SampleIndex := StartSampleIndex to High(AnalysisBuffer) do
  begin

       passesThreshold:=false;
       passesRiseRate:=false;
       passesDuration:=false;
       passesDropAfterPeak:=false;

       if RiseRate = 0 then
          passesRiseRate := true; // No rise rate requirement, so passes anyways

       if MinPeakDuration = 0 then
          passesDuration := true; // No minimal duration, so test passed anyways

       if DropAfterPeak =0 then
          passesDropAfterPeak:= true;

      Volume := Abs(AnalysisBuffer[SampleIndex]) / (-Low(smallint)) *100;
      if  Volume > Threshold then
      begin
        passesThreshold:=true;
      end;

      returnVal.BufferIndex:=SampleIndex;
      returnVal.Volume:=Volume;

      // Before going further, check whether by any chance we already pass all criteria
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin
           detected:=true;

           Break;
      end;

      // First test passed, check for rise rate if necessary
      if passesThreshold and (not passesRiseRate) then begin
         BaselineStart:=SampleIndex-Round(0.005*AudioFormat.SampleRate);
         if BaselineStart >= Low(AnalysisBuffer) then
           begin
              BaselineInterval:=Round(0.004*AudioFormat.SampleRate);
              RMSVolumeBaseline:=0;
              for BaselineSampleIndex := BaselineStart to BaselineStart+BaselineInterval do
              begin
                  RMSVolumeBaseline :=
                     RMSVolumeBaseline+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
              end;
              RMSVolumeBaseline:=Sqrt(RMSVolumeBaseline/(BaselineInterval+1));

              BaselineStart:=SampleIndex;
              BaselineInterval:=Round(0.003*AudioFormat.SampleRate);
              if BaselineStart+BaselineInterval <= High(AnalysisBuffer) then
              begin

                RMSVolume:=0;
                for BaselineSampleIndex := BaselineStart to BaselineStart+BaselineInterval do
                begin
                  RMSVolume :=
                     RMSVolume+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
                end;
                  RMSVolume:=Sqrt(RMSVolume/(BaselineInterval+1));

                 riserate_evaluated:=(RMSVolume-RMSVolumeBaseline)*100;
                 // The idea is that we want to have quick rise but then also something that stays a bit constant or continues to rise
                 if  (riserate_evaluated>=RiseRate) and (RMSVolume*100.0 > Volume/2.0) then
                 begin
                   passesRiseRate:=true;
                end;

              end; // End we can get the peak RMS
           end;

      end;

      returnVal.RiseRateMillisecond:=riserate_evaluated;

      // Again, check whether by any chance we already pass all criteria
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin


           detected:=true;
           Break;
      end;

      // First two tests OK, but not (yet) the third one
      if passesThreshold and passesRiseRate and (not passesDuration) then
      begin
         SampleUpperLimit:=SampleIndex+Round(0.001*AudioFormat.SampleRate*MinPeakDuration*2);
         if SampleUpperLimit > High(AnalysisBuffer) then
             SampleUpperLimit := High(AnalysisBuffer);
         SampleLowerLimit:=SampleIndex+Round(0.001*AudioFormat.SampleRate*MinPeakDuration);
         if SampleLowerLimit > High(AnalysisBuffer) then
            SampleLowerLimit := High(AnalysisBuffer);
         maximumdetected:=false;
         HighestMaxIndex:=-1;
         for BaselineSampleIndex := SampleLowerLimit to SampleUpperLimit do
         begin
           if Abs(AnalysisBuffer[BaselineSampleIndex]) / (-Low(smallint)) *100 > Threshold then
           begin
                maximumdetected:=true;
                HighestMaxIndex:= BaselineSampleIndex;
           end;
         end;
         if maximumdetected then begin
            passesDuration:=true;
         end;

      end;

      returnVal.MinPeakMillisecond:=(HighestMaxIndex-SampleIndex)/AudioFormat.SampleRate*1000.0;
      returnVal.BufferIndexLast:=HighestMaxIndex;

      // Again, check whether by any chance we already pass all criteria
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin
           detected:=true;
           Break;
      end;

      //
      if passesThreshold and passesRiseRate and passesDuration and (not passesDropAfterPeak) then
      begin


          BaselineStart:=SampleIndex;
          BaselineInterval:=Round(0.003*AudioFormat.SampleRate);
          if BaselineStart+BaselineInterval <= High(AnalysisBuffer) then
           begin

              RMSVolumeBaseline:=0;
              for BaselineSampleIndex := BaselineStart to BaselineStart+BaselineInterval do
              begin
                  RMSVolumeBaseline :=
                     RMSVolumeBaseline+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
              end;
              RMSVolumeBaseline:=Sqrt(RMSVolumeBaseline/(BaselineInterval+1));
              SampleUpperLimit:=SampleIndex+Round((TestTimeAfterPeak/1000.0+0.005)*AudioFormat.SampleRate);
              SampleLowerLimit:=SampleIndex+Round(TestTimeAfterPeak/1000.0*AudioFormat.SampleRate);
              // Avoid indexing error by accessing non-existent points
              if SampleUpperLimit <= High(AnalysisBuffer) then
              begin
                 RMSVolume:=0;
                 for BaselineSampleIndex := SampleLowerLimit to SampleUpperLimit do
                 begin
                      RMSVolume:=RMSVolume+(AnalysisBuffer[BaselineSampleIndex])*(AnalysisBuffer[BaselineSampleIndex])/(-Low(smallint))/(-Low(smallint));
                 end;
                 RMSVolume:=Sqrt(RMSVolume/(SampleUpperLimit-SampleLowerLimit+1));



                 // Fall rate is relative to max peak intensity (here, RMSVolumeBaseline taken on 1ms peak from initial detection on)
                 if ((RMSVolumeBaseline - RMSVolume)/RMSVolumeBaseline)*100.0 >= DropAfterPeak then
                 begin
                   passesDropAfterPeak:=true;
                 end;
              end;

              returnVal.DropAfterPeak:=(RMSVolumeBaseline - RMSVolume)/RMSVolumeBaseline;

           end;



      end;

      // Final test if everything passes
      if passesThreshold and passesRiseRate and passesDuration and passesDropAfterPeak then
      begin
           detected:=true;
           Break;
      end;




  end;







  if detected then
    begin
       Result:=returnVal;
       ToneValid := true;
       ToneAbs:=48;
       Tone:=0;



    end else
    begin
      Result:=defaultreturnVal;
    end;




end;


end.
