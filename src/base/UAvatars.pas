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
 * $URL: $
 * $Id:  $
 *}

unit UAvatars;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  sdl2,
  SQLite3,
  SQLiteTable3,
  SysUtils,
  Classes,
  UImage,
  UIni,
  UTexture,
  UPath,
  UScale;

type
  ECoverDBException = class(Exception)
  end;

  TAvatar = class
    private
      ID: int64;
      Filename: IPath;
    public
      constructor Create(ID: int64; Filename: IPath);
      function GetPreviewTexture(): TTexture;
      function GetTexture(): TTexture;
  end;

  TAvatarThumbnailInfo = record
    AvatarWidth: integer;         // Original width of avatar
    AvatarHeight: integer;        // Original height of avatar
    PixelFormat: TImagePixelFmt; // Pixel-format of avatar thumbnail
  end;

  TAvatarDatabase = class
    private
      DB: TSQLiteDatabase;

      procedure InitAvatarDatabase();
      function CreateAvatarThumbnail(const Filename: IPath; var Info: TAvatarThumbnailInfo): PSDL_Surface;
      function LoadAvatar(AvatarID: int64): TTexture;
      procedure DeleteAvatar(AvatarID: int64);
      function FindAvatarIntern(const Filename: IPath): int64;
      procedure Open();
      function GetVersion(): integer;
      procedure SetVersion(Version: integer);
    public
      constructor Create();
      destructor Destroy; override;
      function AddAvatar(const Filename: IPath): TAvatar;
      function FindAvatar(const Filename: IPath): TAvatar;
      function AvatarExists(const Filename: IPath): boolean;
      function GetMaxAvatarSize(): integer;
      procedure SetMaxAvatarSize(Size: integer);
      procedure LoadAvatars();
  end;

  TBlobWrapper = class(TCustomMemoryStream)
     function Write(const Buffer; Count: Integer): Integer; override;
  end;

var
  Avatars: TAvatarDatabase;
  AvatarsList: array of IPath;
  NoAvatarTexture: array[1..UIni.IMaxPlayerCount] of TTexture;
  AvatarPlayerTextures: array[1..UIni.IMaxPlayerCount] of TTexture;

implementation

uses
  UMain,
  ULog,
  UPathUtils,
  UPlatform,
  UFilesystem,
  Math,
  DateUtils;

const
  AVATARDB_FILENAME: UTF8String = 'avatar.db';
  AVATARDB_VERSION = 01; // 0.1
  AVATAR_TBL = 'Avatar';
  AVATAR_THUMBNAIL_TBL = 'AvatarThumbnail';
  AVATAR_IDX = 'Avatar_Filename_IDX';

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

constructor TAvatar.Create(ID: int64; Filename: IPath);
begin
  Self.ID := ID;
  Self.Filename := Filename;
end;

function TAvatar.GetPreviewTexture(): TTexture;
begin
  Result := Avatars.LoadAvatar(ID);
  Result.ScaleMode := lsUniform;
end;

function TAvatar.GetTexture(): TTexture;
begin
  Result := Texture.LoadTexture(Filename);
  Result.ScaleMode := lsUniform;
end;


{ TCoverDatabase }

constructor TAvatarDatabase.Create();
begin
  inherited;

  Open();
  LoadAvatars();
  InitAvatarDatabase();
end;

destructor TAvatarDatabase.Destroy;
begin
  DB.Free;
  inherited;
end;

function TAvatarDatabase.GetVersion(): integer;
begin
  Result := DB.GetTableValue('PRAGMA user_version');
end;

procedure TAvatarDatabase.SetVersion(Version: integer);
begin
  DB.ExecSQL(Format('PRAGMA user_version = %d', [Version]));
end;

function TAvatarDatabase.GetMaxAvatarSize(): integer;
begin
  Result := ITextureSizeVals[Ini.TextureSize];
end;

procedure TAvatarDatabase.SetMaxAvatarSize(Size: integer);
var
  I: integer;
begin
  // search for first valid avatar-size > Size
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

procedure TAvatarDatabase.Open();
var
  Version: integer;
  Filename: IPath;
begin
  Filename := Platform.GetGameUserPath().Append(AVATARDB_FILENAME);

  DB := TSQLiteDatabase.Create(Filename.ToUTF8());
  Version := GetVersion();

  // check version, if version is too old/new, delete database file
  if ((Version <> 0) and (Version <> AVATARDB_VERSION)) then
  begin
    Log.LogInfo('Outdated avatar-database file found', 'TAvatarDatabase.Open');
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
    SetVersion(AVATARDB_VERSION);

  // speed-up disk-writing. The default FULL-synchronous mode is too slow.
  // With this option disk-writing is approx. 4 times faster but the database
  // might be corrupted if the OS crashes, although this is very unlikely.
  DB.ExecSQL('PRAGMA synchronous = OFF;');

  // the next line rather gives a slow-down instead of a speed-up, so we do not use it
  //DB.ExecSQL('PRAGMA temp_store = MEMORY;');
end;

procedure TAvatarDatabase.InitAvatarDatabase();
begin
  DB.ExecSQL('CREATE TABLE IF NOT EXISTS ['+AVATAR_TBL+'] (' +
               '[ID] INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
               '[Filename] TEXT  UNIQUE NOT NULL, ' +
               '[Date] INTEGER  NOT NULL, ' +
               '[Width] INTEGER  NOT NULL, ' +
               '[Height] INTEGER  NOT NULL ' +
             ')');

  DB.ExecSQL('CREATE INDEX IF NOT EXISTS ['+AVATAR_IDX+'] ON ['+AVATAR_TBL+'](' +
               '[Filename]  ASC' +
             ')');

  DB.ExecSQL('CREATE TABLE IF NOT EXISTS ['+AVATAR_THUMBNAIL_TBL+'] (' +
               '[ID] INTEGER  NOT NULL PRIMARY KEY, ' +
               '[Format] INTEGER  NOT NULL, ' +
               '[Width] INTEGER  NOT NULL, ' +
               '[Height] INTEGER  NOT NULL, ' +
               '[Data] BLOB  NULL' +
             ')');
end;

function TAvatarDatabase.FindAvatarIntern(const Filename: IPath): int64;
begin
  Result := DB.GetTableValue('SELECT [ID] FROM ['+AVATAR_TBL+'] ' +
                             'WHERE [Filename] = ?',
                             [Filename.ToUTF8]);
end;

function TAvatarDatabase.FindAvatar(const Filename: IPath): TAvatar;
var
  AvatarID: int64;
begin
  Result := nil;
  try
    AvatarID := FindAvatarIntern(Filename);
    if (AvatarID > 0) then
      Result := TAvatar.Create(AvatarID, Filename);
  except on E: Exception do
    Log.LogError(E.Message, 'TAvatarDatabase.FindAvatar');
  end;
end;

function TAvatarDatabase.AvatarExists(const Filename: IPath): boolean;
begin
  Result := false;
  try
    Result := (FindAvatarIntern(Filename) > 0);
  except on E: Exception do
    Log.LogError(E.Message, 'TAvatarDatabase.CoverExists');
  end;
end;

function TAvatarDatabase.AddAvatar(const Filename: IPath): TAvatar;
var
  AvatarID: int64;
  Thumbnail: PSDL_Surface;
  AvatarData: TBlobWrapper;
  FileDate: TDateTime;
  Info: TAvatarThumbnailInfo;
begin
  Result := nil;

  //if (not FileExists(Filename)) then
  //  Exit;

  // TODO: replace '\' with '/' in filename
  FileDate := Now(); //FileDateToDateTime(FileAge(Filename));

  Thumbnail := CreateAvatarThumbnail(Filename, Info);
  if (Thumbnail = nil) then
    Exit;
  SDL_LockSurface(Thumbnail);
  if not assigned(Thumbnail^.pixels) then
  begin
    Log.LogError('Failed to lock surface', 'TAvatarDatabase.AddAvatar');
    SDL_FreeSurface(Thumbnail);
    Exit;
  end;

  AvatarData := TBlobWrapper.Create;
  AvatarData.Write(Thumbnail^.pixels, Thumbnail^.h * Thumbnail^.pitch);

  try
    // Note: use a transaction to speed-up file-writing.
    // Without data written by the first INSERT might be moved at the second INSERT.
    DB.BeginTransaction();

    // add general cover info
    DB.ExecSQL('INSERT INTO ['+AVATAR_TBL+'] ' +
               '([Filename], [Date], [Width], [Height]) VALUES' +
               '(?, ?, ?, ?)',
               [Filename.ToUTF8, DateTimeToUnixTime(FileDate),
                Info.AvatarWidth, Info.AvatarHeight]);

    // get auto-generated avatar ID
    AvatarID := DB.GetLastInsertRowID();

    // add thumbnail info
    DB.ExecSQL('INSERT INTO ['+AVATAR_THUMBNAIL_TBL+'] ' +
               '([ID], [Format], [Width], [Height], [Data]) VALUES' +
               '(?, ?, ?, ?, ?)',
               [AvatarID, Ord(Info.PixelFormat),
                Thumbnail^.w, Thumbnail^.h, AvatarData]);

    Result := TAvatar.Create(AvatarID, Filename);
  except on E: Exception do
    Log.LogError(E.Message, 'TAvatarDatabase.AddAvatar');
  end;

  DB.Commit();
  AvatarData.Free;
  SDL_FreeSurface(Thumbnail);
end;

function TAvatarDatabase.LoadAvatar(AvatarID: int64): TTexture;
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
      'FROM ['+AVATAR_TBL+'] C ' +
        'INNER JOIN ['+AVATAR_THUMBNAIL_TBL+'] T ' +
        'USING(ID) ' +
      'WHERE [ID] = %d', [AvatarID]));

    Filename := Path(Table.FieldAsString(0));
    PixelFmt := TImagePixelFmt(Table.FieldAsInteger(1));
    Width    := Table.FieldAsInteger(2);
    Height   := Table.FieldAsInteger(3);

    Data := Table.FieldAsBlobPtr(4, DataSize);
    if (Data <> nil) and
       (PixelFmt = ipfRGB) then
    begin
      Result := Texture.CreateTexture(Data, Filename, Width, Height);
      Result.ScaleMode := lsUniform;
    end
    else
    begin
      // FillChar() does not decrement the ref-count of ref-counted fields
      // -> reset Name field manually
      Result.Name := nil;
      FillChar(Result, SizeOf(TTexture), 0);
    end;
  except on E: Exception do
    Log.LogError(E.Message, 'TAvatarDatabase.LoadAvatar');
  end;

  Table.Free;
end;

procedure TAvatarDatabase.DeleteAvatar(AvatarID: int64);
begin
  DB.ExecSQL(Format('DELETE FROM ['+AVATAR_TBL+'] WHERE [ID] = %d', [AvatarID]));
  DB.ExecSQL(Format('DELETE FROM ['+AVATAR_THUMBNAIL_TBL+'] WHERE [ID] = %d', [AvatarID]));
end;

(**
 * Returns a pointer to an array of bytes containing the texture data in the
 * requested size
 *)
function TAvatarDatabase.CreateAvatarThumbnail(const Filename: IPath; var Info: TAvatarThumbnailInfo): PSDL_Surface;
var
  //TargetAspect, SourceAspect: double;
  //TargetWidth, TargetHeight: integer;
  Thumbnail: PSDL_Surface;
  MaxSize: integer;
begin
  Result := nil;

  MaxSize := GetMaxAvatarSize();

  Thumbnail := LoadImage(Filename);
  if (not assigned(Thumbnail)) then
  begin
    Log.LogError('Could not load avatar: "'+ Filename.ToNative +'"', 'TAvatarDatabase.AddAvatar');
    Exit;
  end;

  // Convert pixel format as needed
  AdjustPixelFormat(Thumbnail, TEXTURE_TYPE_PLAIN);

  Info.AvatarWidth  := Thumbnail^.w;
  Info.AvatarHeight := Thumbnail^.h;
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

procedure TAvatarDatabase.LoadAvatars;
var
  Len:  Integer;
  IterJPG, IterPNG: IFileIterator;
  FileInfo: TFileInfo;
begin
  // first position for no-avatar
  SetLength(AvatarsList, 1);

  // jpg
  IterJPG := FileSystem.FileFind(AvatarsPath.Append('*.jpg'), 0);

  while (IterJPG.HasNext) do
  begin
    Len := Length(AvatarsList);
    SetLength(AvatarsList, Len + 1);

    FileInfo := IterJPG.Next;

    AvatarsList[High(AvatarsList)] := AvatarsPath.Append(FileInfo.Name);
  end;


  // png
  IterPNG := FileSystem.FileFind(AvatarsPath.Append('*.png'), 0);

  while (IterPNG.HasNext) do
  begin
    Len := Length(AvatarsList);
    SetLength(AvatarsList, Len + 1);

    FileInfo := IterPNG.Next;

    AvatarsList[High(AvatarsList)] := AvatarsPath.Append(FileInfo.Name);
  end;
  
end;

end.

