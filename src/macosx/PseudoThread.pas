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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/macosx/PseudoThread.pas $
 * $Id: PseudoThread.pas 1577 2009-01-26 23:27:06Z k-m_schindler $
 *}

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
    FreeOnTerminate: boolean;
    procedure Execute; virtual; abstract;
    procedure Resume;
    procedure Suspend;
  public
   constructor Create(const suspended : boolean);
end;

implementation

{ TPseudoThread }

constructor TPseudoThread.Create(const suspended: boolean);
begin
  if not suspended then
  begin
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
 
