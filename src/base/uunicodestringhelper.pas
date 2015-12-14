unit UUnicodeStringHelper;

{$mode objfpc}

interface

uses
  UUnicodeUtils;

{**
 * Checks if the given string is a valid UTF-8 string.
 * If an ANSI encoded string (with char codes >= 128) is passed, the
 * function will most probably return false, as most ANSI strings sequences
 * are illegal in UTF-8.
 *}
function IsUTF8StringH(const str: RawByteString): boolean;

{**
 * Returns true if the system uses UTF-8 as default string type
 * (filesystem or API calls).
 * This is always true on Mac OS X and always false on Win32. On Unix it depends
 * on the LC_CTYPE setting.
 * Do not use AnsiToUTF8() or UTF8ToAnsi() if this function returns true.
 *}
function IsNativeUTF8H(): boolean;

implementation

function IsUTF8StringH(const str: RawByteString): boolean;
begin
  Result := IsUTF8String(str);
end;

function IsNativeUTF8H(): boolean;
begin
  Result := IsNativeUTF8();
end;

end.

