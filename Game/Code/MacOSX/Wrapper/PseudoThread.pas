unit PseudoThread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface

type

	// Debugging threads with XCode doesn't seem to work.
	// We use PseudoThread in Debug mode to get proper debugging.
    TPseudoThread = class(TObject)
    private
	protected 
		Terminated,
		FreeOnTerminate : Boolean;
		procedure Execute; virtual; abstract;
		procedure Resume;
		procedure Suspend;
    public
		constructor Create(const suspended : Boolean);
    end;

implementation

{ TPseudoThread }

constructor TPseudoThread.Create(const suspended : Boolean);
begin
	if not suspended then begin
	    Execute;
	end;
end;

procedure TPseudoThread.Resume;
begin
	Execute;
end;

procedure TPseudoThread.Suspend;
begin
end;

end.
 
