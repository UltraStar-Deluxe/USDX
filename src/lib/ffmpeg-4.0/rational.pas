unit rational;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  ctypes;
type
  TAVRational = record
    num: csint; 
    den: csint;
  end;
function av_q2d(a: TAVRational): cdouble; {$IFDEF HasInline}inline;{$ENDIF}
function av_inv_q(q: TAVRational): TAVRational; {$IFDEF HasInline}inline;{$ENDIF}
implementation
function av_q2d(a: TAVRational): cdouble; {$IFDEF HasInline}inline;{$ENDIF}
begin
  av_q2d := a.num / a.den;
end;
function av_inv_q(q: TAVRational): TAVRational; {$IFDEF HasInline}inline;{$ENDIF}
begin
  av_inv_q.num := q.den;
  av_inv_q.den := q.num;
end;
end.
