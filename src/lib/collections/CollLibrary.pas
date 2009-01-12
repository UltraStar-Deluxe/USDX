unit CollLibrary;

(*****************************************************************************
 * Copyright 2003 by Matthew Greet
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details. (http://opensource.org/licenses/lgpl-license.php)
 * 
 * See http://www.warmachine.u-net.com/delphi_collections for updates and downloads.
 *
 * $Version: v1.0.3 $
 * $Revision: 1.0.1.1 $
 * $Log: D:\QVCS Repositories\Delphi Collections\CollLibrary.qbt $
 * 
 *   Initial version.
 * 
 * Revision 1.0.1.1  by: Matthew Greet  Rev date: 24/10/03 16:48:16
 *   v1.0 branch.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 06/04/03 10:40:32
 *   Initial revision.
 * 
 * FPC compatibility fixes by: UltraStar Deluxe Team
 *
 * $Endlog$
 *****************************************************************************)

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}
 
interface

uses
    Collections, CollArray, CollHash, CollList, CollPArray, CollWrappers;

type
    TMiscCollectionLibrary = class
    public
        class function ClassNameToClassType(ClassName: String): TAbstractCollectionClass;
        class function EqualIID(const IID1, IID2: TGUID): Boolean;
        class function HashCode(Value: String): Integer;
        class procedure ShuffleArray(var ItemArray: array of ICollectable);
        class procedure ShuffleList(const List: IList);
    end;

implementation

{ TMiscCollectionLibrary }
class function TMiscCollectionLibrary.ClassNameToClassType(ClassName: String): TAbstractCollectionClass;
begin
    if ClassName = 'TArray' then
        Result := TArray
    else if ClassName = 'THashSet' then
        Result := THashSet
    else if ClassName = 'THashMap' then
        Result := THashMap
    else if ClassName = 'THashIntegerMap' then
        Result := THashIntegerMap
    else if ClassName = 'THashStringMap' then
        Result := THashStringMap
    else if ClassName = 'TListSet' then
        Result := TListSet
    else if ClassName = 'TListMap' then
        Result := TListMap
    else if ClassName = 'TPArrayBag' then
        Result := TPArrayBag
    else if ClassName = 'TPArraySet' then
        Result := TPArraySet
    else if ClassName = 'TPArrayList' then
        Result := TPArrayList
    else if ClassName = 'TPArrayMap' then
        Result := TPArrayMap
    else
        Result := nil;
end;

class function TMiscCollectionLibrary.EqualIID(const IID1, IID2: TGUID): Boolean;
begin
    Result := (IID1.D1 = IID2.D1) and (IID1.D2 = IID2.D2) and (IID1.D3 = IID2.D3) and
        (IID1.D4[0] = IID2.D4[0]) and (IID1.D4[1] = IID2.D4[1]) and
        (IID1.D4[2] = IID2.D4[2]) and (IID1.D4[3] = IID2.D4[3]) and
        (IID1.D4[4] = IID2.D4[4]) and (IID1.D4[5] = IID2.D4[5]) and
        (IID1.D4[6] = IID2.D4[6]) and (IID1.D4[7] = IID2.D4[7]);
end;

class function TMiscCollectionLibrary.HashCode(Value: String): Integer;
var
    I: Integer;
begin
    Result := 0;
    for I := 1 to Length(Value) do
        Result := (Result shl 1) xor Ord(Value[I]);
end;

class procedure TMiscCollectionLibrary.ShuffleArray(var ItemArray: array of ICollectable);
var
    Item: ICollectable;
    ArraySize, I, Index: Integer;
begin
    Randomize;
    ArraySize := Length(ItemArray);
    for I := 0 to ArraySize - 1 do
    begin
        Index := (I + Random(ArraySize - 1) + 1) mod ArraySize;
        Item := ItemArray[I];
        ItemArray[I] := ItemArray[Index];
        ItemArray[Index] := Item;
    end;
end;

class procedure TMiscCollectionLibrary.ShuffleList(const List: IList);
var
    ListSize, I: Integer;
begin
    Randomize;
    ListSize := List.GetSize;
    for I := 0 to ListSize - 1 do
    begin
        List.Exchange(I, (I + Random(ListSize - 1) + 1) mod ListSize);
    end;
end;


end.
