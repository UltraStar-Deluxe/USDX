unit CollArray;

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
 * $Revision: 1.2 $
 * $Log: D:\QVCS Repositories\Delphi Collections\CollArray.qbt $
 * 
 *   Colllection implementations based on arrays.
 * 
 * Revision 1.2  by: Matthew Greet  Rev date: 12/06/04 20:02:16
 *   Capacity property.
 * 
 * Revision 1.1  by: Matthew Greet  Rev date: 06/04/03 10:30:36
 *   Size property dropped. 
 *   Unused abstract functions still implemented.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 01/03/03 10:50:02
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
    Collections;

type
    TArray = class(TAbstractList)
    private
        FArray: array of ICollectable;
    protected
        function TrueGetItem(Index: Integer): ICollectable; override;
        procedure TrueSetItem(Index: Integer; const Value: ICollectable); override;
        procedure TrueAppend(const Item: ICollectable); override;
        procedure TrueClear; override;
        function TrueDelete(Index: Integer): ICollectable; override;
        procedure TrueInsert(Index: Integer; const Item: ICollectable); override;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        constructor Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean = false); override;
        constructor Create(Size: Integer; NaturalItemsOnly: Boolean = false); overload; virtual;
        constructor Create(const Collection: ICollection); override;
        destructor Destroy; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetFixedSize: Boolean; override;
        function GetSize: Integer; override;
    end;

implementation

constructor TArray.Create(NaturalItemsOnly: Boolean);
begin
    Create(0, NaturalItemsOnly);
end;

constructor TArray.Create(Size: Integer; NaturalItemsOnly: Boolean = false);
begin
    inherited Create(NaturalItemsOnly);
    SetLength(FArray, Size);
end;

constructor TArray.Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
var
    Item: ICollectable;
    ItemError: TCollectionError;
    I: Integer;
begin
    inherited Create(ItemArray, NaturalItemsOnly);
    SetLength(FArray, Length(ItemArray));
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        Item := ItemArray[I];
        ItemError := ItemAllowed(Item);
        if ItemError <> ceOK then
        begin
            CollectionError(ItemError);
        end
        else
            Items[I] := Item;
    end;
end;

constructor TArray.Create(const Collection: ICollection);
var
    Iterator: IIterator;
    I: Integer;
begin
    inherited Create(Collection);
    SetLength(FArray, Collection.GetSize);
    Iterator := Collection.GetIterator;
    I := 0;
    while not Iterator.EOF do
    begin
        Items[I] := Iterator.CurrentItem;
        Inc(I);
        Iterator.Next;
    end;
end;

destructor TArray.Destroy;
var
    I: Integer;
begin
    // Delete interface references to all items
    for I := Low(FArray) to High(FArray) do
    begin
        FArray[I] := nil;
    end;
    inherited Destroy;
end;

function TArray.TrueGetItem(Index: Integer): ICollectable;
begin
    Result := FArray[Index];
end;

procedure TArray.TrueSetItem(Index: Integer; const Value: ICollectable);
begin
    FArray[Index] := Value;
end;

procedure TArray.TrueAppend(const Item: ICollectable);
begin
    // Ignored as collection is fixed size
end;

procedure TArray.TrueClear;
begin
    // Ignored as collection is fixed size
end;

function TArray.TrueDelete(Index: Integer): ICollectable;
begin
    // Ignored as collection is fixed size
end;

procedure TArray.TrueInsert(Index: Integer; const Item: ICollectable);
begin
    // Ignored as collection is fixed size
end;

function TArray.GetCapacity: Integer;
begin
    Result := Size;
end;

procedure TArray.SetCapacity(Value: Integer);
begin
    // Ignored
end;

function TArray.GetFixedSize: Boolean;
begin
    Result := true;
end;

function TArray.GetSize: Integer;
begin
    Result := Length(FArray);
end;

end.
