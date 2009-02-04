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
      RingBuffer: PByteArray;
      BufferCount: integer;
      BufferSize: integer;
      WritePos: integer;
      ReadPos: integer;
    public
      constructor Create(Size: integer);
      destructor Destroy; override;
      function Read(Buffer: PByteArray; Count: integer): integer;
      function Write(Buffer: PByteArray; Count: integer): integer;
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

function TRingBuffer.Read(Buffer: PByteArray; Count: integer): integer;
var
  PartCount: integer;
begin
  // adjust output count
  if (Count > BufferCount) then
  begin
    //DebugWriteln('Read too much: ' + inttostr(count) +',count:'+ inttostr(BufferCount) + '/size:' + inttostr(BufferSize));
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
  Move(RingBuffer[ReadPos], Buffer[0], PartCount);

  // second step: if we need more data, copy from the beginning of the buffer
  if (PartCount < Count) then
    Move(RingBuffer[0], Buffer[0], Count-PartCount);

  // mark the copied part of the buffer as free
  BufferCount := BufferCount - Count;
  ReadPos := (ReadPos + Count) mod BufferSize;

  Result := Count;
end;

function TRingBuffer.Write(Buffer: PByteArray; Count: integer): integer;
var
  PartCount: integer;
begin
  // check for a reasonable request
  if (Count <= 0) then
  begin
    Result := Count;
    Exit;
  end;

  // skip input data if the input buffer is bigger than the ring-buffer
  if (Count > BufferSize) then
  begin
    //DebugWriteln('Write skip data:' + inttostr(count) +',count:'+ inttostr(BufferCount) + '/size:' + inttostr(BufferSize));
    Buffer := @Buffer[Count - BufferSize];
    Count := BufferSize;
  end;

  // first step: copy to the area between the write-position and the end of the buffer
  PartCount := Min(Count, BufferSize - WritePos);
  Move(Buffer[0], RingBuffer[WritePos], PartCount);

  // second step: copy data to front of buffer
  if (PartCount < Count) then
    Move(Buffer[PartCount], RingBuffer[0], Count-PartCount);

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
