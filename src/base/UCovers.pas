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

unit UCovers;

{
  TODO:
  - adjust database to new song-loading (e.g. use SongIDs)
  - support for deletion of outdated covers
  - support for update of changed covers
  - use paths relative to the song for removable disks support
    (a drive might have a different drive-name the next time it is connected,
     so "H:/songs/..." will not match "I:/songs/...") 
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL,
  SQLite3,
  SQLiteTable3,
  SysUtils,
  Classes,
  UImage,
  UTexture,
  UPath;

type
  ECoverDBException = class(Exception)
  end;

  TCover = class
    private
      ID: int64;
      Filename: IPath;
    public
      constructor Create(ID: int64; Filename: IPath);
      function GetPreviewTexture(): TTexture;
      function GetTexture(): TTexture;
  end;

  TThumbnailInfo = record
    CoverWidth: integer;         // Original width of cover
    CoverHeight: integer;        // Original height of cover
    PixelFormat: TImagePixelFmt; // Pixel-format of thumbnail
  end;
  
  TCoverDatabase = class
    private
      DB: TSQLiteDatabase;
      procedure InitCoverDatabase();
      function CreateThumbnail(const Filename: IPath; var Info: TThumbnailInfo): PSDL_Surface;
      function LoadCover(CoverID: int64): TTexture;
      procedure DeleteCover(CoverID: int64);
      function FindCoverIntern(const Filename: IPath): int64;
      procedure Open();
      function GetVersion(): integer;
      procedure SetVersion(Version: integer);
    public
      constructor Create();
      destructor Destroy; override;
      function AddCover(const Filename: IPath): TCover;
      function FindCover(const Filename: IPath): TCover;
      function CoverExists(const Filename: IPath): boolean;
      function GetMaxCoverSize(): integer;
      procedure SetMaxCoverSize(Size: integer);
  end;

  TBlobWrapper = class(TCustomMemoryStream)
     function Write(const Buffer; Count: Integer): Integer; override;
  end;

var
  Covers: TCoverDatabase;

implementation

uses
  UMain,
  ULog,
  UPlatform,
  UIni,
  Math,
  DateUtils;

const
  COVERDB_FILENAME: UTF8String = 'cover.db';
  COVERDB_VERSION = 01; // 0.1
  COVER_TBL = 'Cover';
  COVER_THUMBNAIL_TBL = 'CoverThumbnail';
  COVER_IDX = 'Cover_Filename_IDX';

// Note: DateUtils.DateTimeToUnix() will throw an exception in FPC
function DateTimeToUnixTime(time: TDateTime): int64;
begin
  Result := Round((time - UnixDateDelta) * SecsPerDay);
end;

// Note: DateUtils.UnixToDateTime() will throw an exception in FPC
function UnixTimeToDateTime(timestamp: int64): TDateTime;
begin
  Result := timestamp / SecsPerDay + UnixDateDelta;
end;


{ TBlobWrapper }

function TBlobWrapper.Write(const Buffer; Count: Integer): Integer;
begin
  SetPointer(Pointer(Buffer), Count);
  Result := Count;
end;


{ TCover }

constructor TCover.Create(ID: int64; Filename: IPath);
begin
  Self.ID := ID;
  Self.Filename := Filename;
end;

function TCover.GetPreviewTexture(): TTexture;
begin
  Result := Covers.LoadCover(ID);
end;

function TCover.GetTexture(): TTexture;
begin
  Result := Texture.LoadTexture(Filename);
end;


{ TCoverDatabase }

constructor TCoverDatabase.Create();
begin
  inherited;

  Open();
  InitCoverDatabase();
end;

destructor TCoverDatabase.Destroy;
begin
  DB.Free;
  inherited;
end;

function TCoverDatabase.GetVersion(): integer;
begin
  Result := DB.GetTableValue('PRAGMA user_version');
end;

procedure TCoverDatabase.SetVersion(Version: integer);
begin
  DB.ExecSQL(Format('PRAGMA user_version = %d', [Version]));
end;

function TCoverDatabase.GetMaxCoverSize(): integer;
begin
  Result := ITextureSizeVals[Ini.TextureSize];
end;

procedure TCoverDatabase.SetMaxCoverSize(Size: integer);
var
  I: integer;
begin
  // search for first valid cover-size > Size
  for I := 0 to Length(ITextureSizeVals)-1 do
  begin
    if (Size <= ITextureSizeVals[I]) then
    begin
      Ini.TextureSize := I;
      Exit;
    end;
  end;

  // fall-back to highest size
  Ini.TextureSize := High(ITextureSizeVals);
end;

procedure TCoverDatabase.Open();
var
  Version: integer;
  Filename: IPath;
begin
  Filename := Platform.GetGameUserPath().Append(COVERDB_FILENAME);

  DB := TSQLiteDatabase.Create(Filename.ToUTF8());
  Version := GetVersion();

  // check version, if version is too old/new, delete database file
  if ((Version <> 0) and (Version <> COVERDB_VERSION)) then
  begin
    Log.LogInfo('Outdated cover-database file found', 'TCoverDatabase.Open');
    // close and delete outdated file
    DB.Free;
    if (not Filename.DeleteFile()) then
      raise ECoverDBException.Create('Could not delete ' + Filename.ToNative);
    // reopen
    DB := TSQLiteDatabase.Create(Filename.ToUTF8());
    Version := 0;
  end;

  // set version number after creation
  if (Version = 0) then
    SetVersion(COVERDB_VERSION);

  // speed-up disk-writing. The default FULL-synchronous mode is too slow.
  // With this option disk-writing is approx. 4 times faster but the database
  // might be corrupted if the OS crashes, although this is very unlikely.
  DB.ExecSQL('PRAGMA synchronous = OFF;');
  
  // the next line rather gives a slow-down instead of a speed-up, so we do not use it
  //DB.ExecSQL('PRAGMA temp_store = MEMORY;');
end;

procedure TCoverDatabase.InitCoverDatabase();
begin
  DB.ExecSQL('CREATE TABLE IF NOT EXISTS ['+COVER_TBL+'] (' +
               '[ID] INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
               '[Filename] TEXT  UNIQUE NOT NULL, ' +
               '[Date] INTEGER  NOT NULL, ' +
               '[Width] INTEGER  NOT NULL, ' +
               '[Height] INTEGER  NOT NULL ' +
             ')');

  DB.ExecSQL('CREATE INDEX IF NOT EXISTS ['+COVER_IDX+'] ON ['+COVER_TBL+'](' +
               '[Filename]  ASC' +
             ')');

  DB.ExecSQL('CREATE TABLE IF NOT EXISTS ['+COVER_THUMBNAIL_TBL+'] (' +
               '[ID] INTEGER  NOT NULL PRIMARY KEY, ' +
               '[Format] INTEGER  NOT NULL, ' +
               '[Width] INTEGER  NOT NULL, ' +
               '[Height] INTEGER  NOT NULL, ' +
               '[Data] BLOB  NULL' +
             ')');
end;

function TCoverDatabase.FindCoverIntern(const Filename: IPath): int64;
begin
  Result := DB.GetTableValue('SELECT [ID] FROM ['+COVER_TBL+'] ' +
                             'WHERE [Filename] = ?',
                             [Filename.ToUTF8]);
end;

function TCoverDatabase.FindCover(const Filename: IPath): TCover;
var
  CoverID: int64;
begin
  Result := nil;
  try
    CoverID := FindCoverIntern(Filename);
    if (CoverID > 0) then
      Result := TCover.Create(CoverID, Filename);
  except on E: Exception do
    Log.LogError(E.Message, 'TCoverDatabase.FindCover');
  end;
end;

function TCoverDatabase.CoverExists(const Filename: IPath): boolean;
begin
  Result := false;
  try
    Result := (FindCoverIntern(Filename) > 0);
  except on E: Exception do
    Log.LogError(E.Message, 'TCoverDatabase.CoverExists');
  end;
end;

function TCoverDatabase.AddCover(const Filename: IPath): TCover;
var
  CoverID: int64;
  Thumbnail: PSDL_Surface;
  CoverData: TBlobWrapper;
  FileDate: TDateTime;
  Info: TThumbnailInfo;
begin
  Result := nil;

  //if (not FileExists(Filename)) then
  //  Exit;

  // TODO: replace '\' with '/' in filename
  FileDate := Now(); //FileDateToDateTime(FileAge(Filename));

  Thumbnail := CreateThumbnail(Filename, Info);
  if (Thumbnail = nil) then
    Exit;

  CoverData := TBlobWrapper.Create;
  CoverData.Write(Thumbnail^.pixels, Thumbnail^.h * Thumbnail^.pitch);

  try
    // Note: use a transaction to speed-up file-writing.
    // Without data written by the first INSERT might be moved at the second INSERT. 
    DB.BeginTransaction();

    // add general cover info
    DB.ExecSQL('INSERT INTO ['+COVER_TBL+'] ' +
               '([Filename], [Date], [Width], [Height]) VALUES' +
               '(?, ?, ?, ?)',
               [Filename.ToUTF8, DateTimeToUnixTime(FileDate),
                Info.CoverWidth, Info.CoverHeight]);

    // get auto-generated cover ID
    CoverID := DB.GetLastInsertRowID();

    // add thumbnail info
    DB.ExecSQL('INSERT INTO ['+COVER_THUMBNAIL_TBL+'] ' +
               '([ID], [Format], [Width], [Height], [Data]) VALUES' +
               '(?, ?, ?, ?, ?)',
               [CoverID, Ord(Info.PixelFormat),
                Thumbnail^.w, Thumbnail^.h, CoverData]);

    Result := TCover.Create(CoverID, Filename);
  except on E: Exception do
    Log.LogError(E.Message, 'TCoverDatabase.AddCover');
  end;

  DB.Commit();
  CoverData.Free;
  SDL_FreeSurface(Thumbnail);
end;

function TCoverDatabase.LoadCover(CoverID: int64): TTexture;
var
  Width, Height: integer;
  PixelFmt: TImagePixelFmt;
  Data: PChar;
  DataSize: integer;
  Filename: IPath;
  Table: TSQLiteUniTable;
begin
  Table := nil;

  try
    Table := DB.GetUniTable(Format(
      'SELECT C.[Filename], T.[Format], T.[Width], T.[Height], T.[Data] ' +
      'FROM ['+COVER_TBL+'] C ' +
        'INNER JOIN ['+COVER_THUMBNAIL_TBL+'] T ' +
        'USING(ID) ' +
      'WHERE [ID] = %d', [CoverID]));

    Filename := Path(Table.FieldAsString(0));
    PixelFmt := TImagePixelFmt(Table.FieldAsInteger(1));
    Width    := Table.FieldAsInteger(2);
    Height   := Table.FieldAsInteger(3);

    Data := Table.FieldAsBlobPtr(4, DataSize);
    if (Data <> nil) and
       (PixelFmt = ipfRGB) then
    begin
      Result := Texture.CreateTexture(Data, Filename, Width, Height, 24)
    end
    else
    begin
      // FillChar() does not decrement the ref-count of ref-counted fields
      // -> reset Name field manually
      Result.Name := nil;
      FillChar(Result, SizeOf(TTexture), 0);
    end;
  except on E: Exception do
    Log.LogError(E.Message, 'TCoverDatabase.LoadCover');
  end;

  Table.Free;
end;

procedure TCoverDatabase.DeleteCover(CoverID: int64);
begin
  DB.ExecSQL(Format('DELETE FROM ['+COVER_TBL+'] WHERE [ID] = %d', [CoverID]));
  DB.ExecSQL(Format('DELETE FROM ['+COVER_THUMBNAIL_TBL+'] WHERE [ID] = %d', [CoverID]));
end;

(**
 * Returns a pointer to an array of bytes containing the texture data in the
 * requested size
 *)
function TCoverDatabase.CreateThumbnail(const Filename: IPath; var Info: TThumbnailInfo): PSDL_Surface;
var
  //TargetAspect, SourceAspect: double;
  //TargetWidth, TargetHeight: integer;
  Thumbnail: PSDL_Surface;
  MaxSize: integer;
begin
  Result := nil;

  MaxSize := GetMaxCoverSize();

  Thumbnail := LoadImage(Filename);
  if (not assigned(Thumbnail)) then
  begin
    Log.LogError('Could not load cover: "'+ Filename.ToNative +'"', 'TCoverDatabase.AddCover');
    Exit;
  end;

  // Convert pixel format as needed
  AdjustPixelFormat(Thumbnail, TEXTURE_TYPE_PLAIN);

  Info.CoverWidth  := Thumbnail^.w;
  Info.CoverHeight := Thumbnail^.h;
  Info.PixelFormat := ipfRGB;

  (* TODO: keep aspect ratio
  TargetAspect := Width / Height;
  SourceAspect := TexSurface.w / TexSurface.h;

  // Scale texture to covers dimensions (keep aspect)
  if (SourceAspect >= TargetAspect) then
  begin
    TargetWidth := Width;
    TargetHeight := Trunc(Width / SourceAspect);
  end
  else
  begin
    TargetHeight := Height;
    TargetWidth := Trunc(Height * SourceAspect);
  end;
  *)

  // TODO: do not scale if image is smaller
  ScaleImage(Thumbnail, MaxSize, MaxSize);
  
  Result := Thumbnail;
end;

end.

