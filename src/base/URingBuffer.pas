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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/URingBuffer.pas $
 * $Id: URingBuffer.pas 1584 2009-02-04 17:45:38Z tobigun $
 *}

unit URingBuffer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils;

type
  TRingBuffer = class
    private
      RingBuffer: PByte;
      BufferCount: integer;
      BufferSize: integer;
      WritePos: integer;
      ReadPos: integer;
    public
      constructor Create(Size: integer);
      destructor Destroy; override;
      function Read(Buffer: Pointer; Count: integer): integer;
      function Write(Buffer: Pointer; Count: integer): integer;
      function Size(): integer;
      function Available(): integer;
      procedure Flush();
  end;

implementation

uses
  Math;

constructor TRingBuffer.Create(Size: integer);
begin
  BufferSize := Size;

  GetMem(RingBuffer, Size);
  if (RingBuffer = nil) then
    raise Exception.Create('No memory');
end;

destructor TRingBuffer.Destroy;
begin
  FreeMem(RingBuffer);
end;

function TRingBuffer.Read(Buffer: Pointer; Count: integer): integer;
var
  PartCount: integer;
  Src, Dst: PByte;
begin
  // adjust output count
  if (Count > BufferCount) then
  begin
    Count := BufferCount;
  end;

  // check if there is something to do
  if (Count <= 0) then
  begin
    Result := Count;
    Exit;
  end;

  // copy data to output buffer
  // first step: copy from the area between the read-position and the end of the buffer
  PartCount := Min(Count, BufferSize - ReadPos);
  Src := RingBuffer;
  Inc(Src, ReadPos);
  Dst := PByte(Buffer);
  Move(Src^, Dst^, PartCount);

  // second step: if we need more data, copy from the beginning of the buffer
  if (PartCount < Count) then
  begin
    Src := RingBuffer;
    Inc(Dst, PartCount);
    Move(Src^, Dst^, Count-PartCount);
  end;

  // mark the copied part of the buffer as free
  BufferCount := BufferCount - Count;
  ReadPos := (ReadPos + Count) mod BufferSize;

  Result := Count;
end;

function TRingBuffer.Write(Buffer: Pointer; Count: integer): integer;
var
  PartCount: integer;
  Src, Dst: PByte;
begin
  // check for a reasonable request
  if (Count <= 0) then
  begin
    Result := Count;
    Exit;
  end;

  // skip input data if the input buffer is bigger than the ring-buffer
  Src := PByte(Buffer);
  if (Count > BufferSize) then
  begin
    // adjust source pointer to the last BufferSize bytes
    Inc(Src, Count - BufferSize);
    Count := BufferSize;
  end;

  // first step: copy to the area between the write-position and the end of the buffer
  PartCount := Min(Count, BufferSize - WritePos);
  Dst := RingBuffer;
  Inc(Dst, WritePos);
  Move(Src^, Dst^, PartCount);

  // second step: copy data to front of buffer
  if (PartCount < Count) then
  begin
    Inc(Src, PartCount);
    Dst := RingBuffer;
    Move(Src^, Dst^, Count-PartCount);
  end;

  // update info
  BufferCount := Min(BufferCount + Count, BufferSize);
  WritePos := (WritePos + Count) mod BufferSize;
  // if the buffer is full, we have to reposition the read-position
  if (BufferCount = BufferSize) then
    ReadPos := WritePos;

  Result := Count;
end;

function TRingBuffer.Available(): integer;
begin
  Result := BufferCount;
end;

function TRingBuffer.Size(): integer;
begin
  Result := BufferSize;
end;

procedure TRingBuffer.Flush();
begin
  ReadPos := 0;
  WritePos := 0;
  BufferCount := 0;
end;

end.
