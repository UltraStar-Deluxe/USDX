unit CollPArray;

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
 * $Revision: 1.2.1.2 $
 * $Log: D:\QVCS Repositories\Delphi Collections\CollPArray.qbt $
 * 
 *   Collection implementations based on TList.
 * 
 * Revision 1.2.1.2  by: Matthew Greet  Rev date: 12/06/04 20:08:30
 *   Capacity property.
 * 
 * Revision 1.2.1.1  by: Matthew Greet  Rev date: 14/02/04 17:46:10
 *   v1.0 branch.
 * 
 * Revision 1.2  by: Matthew Greet  Rev date: 28/04/03 15:07:14
 *   Correctly handles nil items.
 * 
 * Revision 1.1  by: Matthew Greet  Rev date: 06/04/03 10:43:16
 *   Added TPArrayMap and TExposedPArrayList.
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
    Classes,
    Collections;

type
    TPArrayBag = class(TAbstractBag)
    private
        FList: TList;
    protected
        function TrueAdd(const Item: ICollectable): Boolean; override;
        procedure TrueClear; override;
        function TrueRemove(const Item: ICollectable): ICollectable; override;
        function TrueRemoveAll(const Item: ICollectable): ICollection; override;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        destructor Destroy; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetIterator: IIterator; override;
        function GetSize: Integer; override;
        function TrueContains(const Item: ICollectable): Boolean; override;
    end;

    TPArraySet = class(TAbstractSet)
    private
        FList: TList;
    protected
        function GetPosition(const Item: ICollectable): TCollectionPosition; override;
        procedure TrueAdd2(Position: TCollectionPosition; const Item: ICollectable); override;
        procedure TrueClear; override;
        function TrueGet(Position: TCollectionPosition): ICollectable; override;
        procedure TrueRemove2(Position: TCollectionPosition); override;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        destructor Destroy; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetIterator: IIterator; override;
        function GetSize: Integer; override;
    end;

    TPArrayList = class(TAbstractList)
    private
        FList: TList;
    protected
        function TrueGetItem(Index: Integer): ICollectable; override;
        procedure TrueSetItem(Index: Integer; const Item: ICollectable); override;
        procedure TrueAppend(const Item: ICollectable); override;
        procedure TrueClear; override;
        function TrueDelete(Index: Integer): ICollectable; override;
        procedure TrueInsert(Index: Integer; const Item: ICollectable); override;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        destructor Destroy; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetIterator: IIterator; override;
        function GetSize: Integer; override;
    end;

    TPArrayMap = class(TAbstractMap)
    private
        FList: TList;
    protected
        function GetAssociationIterator: IMapIterator; override;
        function GetKeyPosition(const Key: ICollectable): TCollectionPosition; override;
        procedure TrueClear; override;
        function TrueGet(Position: TCollectionPosition): IAssociation; override;
        function TruePut(Position: TCollectionPosition; const Association: IAssociation): IAssociation; override;
        function TrueRemove2(Position: TCollectionPosition): IAssociation; override;
    public
        constructor Create(NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean); override;
        destructor Destroy; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetSize: Integer; override;
    end;

    // Same as TPArrayList but raises method visibilities so items can be manually
    // appended or inserted without resetting sort flag.
    TExposedPArrayList = class(TPArrayList)
    public
        procedure TrueAppend(const Item: ICollectable); override;
        procedure TrueInsert(Index: Integer; const Item: ICollectable); override;
    end;


implementation

type
    TPArrayIterator = class(TAbstractIterator)
    private
        FList: TList;
        FIndex: Integer;
    protected
        constructor Create(List: TList; AllowRemove: Boolean);
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    end;

    TPArrayAssociationIterator = class(TAbstractAssociationIterator)
    private
        FList: TList;
        FIndex: Integer;
    protected
        constructor Create(List: TList; AllowRemove: Boolean);
        function TrueFirst: IAssociation; override;
        function TrueNext: IAssociation; override;
        procedure TrueRemove; override;
    end;

    TPArrayPosition = class(TCollectionPosition)
    private
        FIndex: Integer;
    public
        constructor Create(Found: Boolean; Index: Integer);
        property Index: Integer read FIndex;
    end;

constructor TPArrayBag.Create(NaturalItemsOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FList := TList.Create;
end;

destructor TPArrayBag.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TPArrayBag.TrueAdd(const Item: ICollectable): Boolean;
begin
    FList.Add(Pointer(Item));
    Result := true;
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    if Item <> nil then
        Item._AddRef;
end;

procedure TPArrayBag.TrueClear;
var
    Item: ICollectable;
    I: Integer;
begin
    // Delete all interface references
    for I := 0 to FList.Count - 1 do
    begin
        Item := ICollectable(FList[I]);
        FList[I] := nil;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        if Item <> nil then
            Item._Release;
    end;
    FList.Clear;
end;

function TPArrayBag.TrueContains(const Item: ICollectable): Boolean;
var
    I: Integer;
    Success: Boolean;
begin
    // Sequential search
    I := 0;
    Success := false;
    while (I < FList.Count) and not Success do
    begin
        Success := Comparator.Equals(Item, ICollectable(FList[I]));
        Inc(I);
    end;
    Result := Success;
end;

function TPArrayBag.TrueRemove(const Item: ICollectable): ICollectable;
var
    Item2: ICollectable;
    I: Integer;
    Found: Boolean;
begin
    // Sequential search
    I := 0;
    Found := false;
    Result := nil;
    while (I < FList.Count) and not Found do
    begin
        Item2 := ICollectable(FList[I]);
        if Comparator.Equals(Item, Item2) then
        begin
            Found := true;
            Result := Item2;
            FList.Delete(I);
            // Storing interface reference as a pointer does not update reference
            // count automatically, so this must be done manually
            if Item2 <> nil then
                Item2._Release;
        end
        else
            Inc(I);
    end;
end;

function TPArrayBag.TrueRemoveAll(const Item: ICollectable): ICollection;
var
    ResultCollection: TPArrayBag;
    Item2: ICollectable;
    I: Integer;
begin
    // Sequential search
    I := 0;
    ResultCollection := TPArrayBag.Create;
    while I < FList.Count do
    begin
        Item2 := ICollectable(FList[I]);
        if Comparator.Equals(Item, Item2) then
        begin
            ResultCollection.Add(Item2);
            FList.Delete(I);
            // Storing interface reference as a pointer does not update reference
            // count automatically, so this must be done manually
            if Item <> nil then
                Item._Release;
        end
        else
            Inc(I);
    end;
    Result := ResultCollection;
end;

function TPArrayBag.GetCapacity: Integer;
begin
    Result := FList.Capacity;
end;

procedure TPArrayBag.SetCapacity(Value: Integer);
begin
    FList.Capacity := Value;
end;

function TPArrayBag.GetIterator: IIterator;
begin
    Result := TPArrayIterator.Create(FList, true);
end;

function TPArrayBag.GetSize: Integer;
begin
    Result := FList.Count;
end;

constructor TPArraySet.Create(NaturalItemsOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FList := TList.Create;
end;

destructor TPArraySet.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TPArraySet.GetPosition(const Item: ICollectable): TCollectionPosition;
var
    I: Integer;
    Success: Boolean;
begin
    // Sequential search
    I := 0;
    Success := false;
    while (I < FList.Count) do
    begin
        Success := Comparator.Equals(Item, ICollectable(FList[I]));
        if Success then
            break;
        Inc(I);
    end;
    Result := TPArrayPosition.Create(Success, I);
end;

procedure TPArraySet.TrueAdd2(Position: TCollectionPosition; const Item: ICollectable);
begin
    FList.Add(Pointer(Item));
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Item._AddRef;
end;

procedure TPArraySet.TrueClear;
var
    Item: ICollectable;
    I: Integer;
begin
    // Delete all interface references
    for I := 0 to FList.Count - 1 do
    begin
        Item := ICollectable(FList[I]);
        FList[I] := nil;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Item._Release;
    end;
    FList.Clear;
end;

function TPArraySet.TrueGet(Position: TCollectionPosition): ICollectable;
begin
    Result := ICollectable(FList.Items[TPArrayPosition(Position).Index]);
end;

procedure TPArraySet.TrueRemove2(Position: TCollectionPosition);
var
    Item: ICollectable;
begin
    Item := ICollectable(FList[TPArrayPosition(Position).Index]);
    FList.Delete(TPArrayPosition(Position).Index);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Item._Release;
end;

function TPArraySet.GetCapacity: Integer;
begin
    Result := FList.Capacity;
end;

procedure TPArraySet.SetCapacity(Value: Integer);
begin
    FList.Capacity := Value;
end;

function TPArraySet.GetIterator: IIterator;
begin
    Result := TPArrayIterator.Create(FList, true);
end;

function TPArraySet.GetSize: Integer;
begin
    Result := FList.Count;
end;

constructor TPArrayList.Create(NaturalItemsOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FList := TList.Create;
end;

destructor TPArrayList.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TPArrayList.TrueGetItem(Index: Integer): ICollectable;
begin
    Result := ICollectable(FList.Items[Index]);
end;

procedure TPArrayList.TrueSetItem(Index: Integer; const Item: ICollectable);
var
    OldItem: ICollectable;
begin
    OldItem := ICollectable(FList[Index]);
    FList[Index] := Pointer(Item);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    if Item <> nil then
        Item._AddRef;
    if OldItem <> nil then
        OldItem._Release;
end;

procedure TPArrayList.TrueAppend(const Item: ICollectable);
begin
    FList.Add(Pointer(Item));
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    if Item <> nil then
        Item._AddRef;
end;

procedure TPArrayList.TrueClear;
var
    Item: ICollectable;
    I: Integer;
begin
    // Delete all interface references
    for I := 0 to FList.Count - 1 do
    begin
        Item := ICollectable(FList[I]);
        FList[I] := nil;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        if Item <> nil then
            Item._Release;
    end;
    FList.Clear;
end;

function TPArrayList.TrueDelete(Index: Integer): ICollectable;
begin
    Result := ICollectable(FList[Index]);
    FList.Delete(Index);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    if Result <> nil then
        Result._Release;
end;

procedure TPArrayList.TrueInsert(Index: Integer; const Item: ICollectable);
begin
    FList.Insert(Index, Pointer(Item));
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    if Item <> nil then
        Item._AddRef;
end;

function TPArrayList.GetCapacity: Integer;
begin
    Result := FList.Capacity;
end;

procedure TPArrayList.SetCapacity(Value: Integer);
begin
    FList.Capacity := Value;
end;

function TPArrayList.GetIterator: IIterator;
begin
    Result := TPArrayIterator.Create(FList, true);
end;

function TPArrayList.GetSize: Integer;
begin
    Result := FList.Count;
end;

constructor TPArrayMap.Create(NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly, NaturalKeysOnly);
    FList := TList.Create;
end;

destructor TPArrayMap.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TPArrayMap.GetAssociationIterator: IMapIterator;
begin
    Result := TPArrayAssociationIterator.Create(FList, true);
end;

function TPArrayMap.GetKeyPosition(const Key: ICollectable): TCollectionPosition;
var
    I: Integer;
    Success: Boolean;
begin
    // Sequential search
    I := 0;
    Success := false;
    while (I < FList.Count) do
    begin
        Success := KeyComparator.Equals(Key, IAssociation(FList[I]).GetKey);
        if Success then
            break;
        Inc(I);
    end;
    Result := TPArrayPosition.Create(Success, I);
end;

procedure TPArrayMap.TrueClear;
var
    Association: IAssociation;
    I: Integer;
begin
    // Delete all interface references
    for I := 0 to FList.Count - 1 do
    begin
        Association := IAssociation(FList[I]);
        FList[I] := nil;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Association._Release;
    end;
    FList.Clear;
end;

function TPArrayMap.TrueGet(Position: TCollectionPosition): IAssociation;
begin
    Result := IAssociation(FList.Items[TPArrayPosition(Position).Index]);
end;

function TPArrayMap.TruePut(Position: TCollectionPosition; const Association: IAssociation): IAssociation;
var
    OldAssociation: IAssociation;
    Index: Integer;
begin
    if Position.Found then
    begin
        Index := (Position as TPArrayPosition).Index;
        OldAssociation := IAssociation(FList[Index]);
        FList[Index] := Pointer(Association);
    end
    else
    begin
        OldAssociation := nil;
        FList.Add(Pointer(Association));
    end;
    Result := OldAssociation;
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._AddRef;
    if OldAssociation <> nil then
        OldAssociation._Release;
end;

function TPArrayMap.TrueRemove2(Position: TCollectionPosition): IAssociation;
var
    OldAssociation: IAssociation;
begin
    OldAssociation := IAssociation(FList[TPArrayPosition(Position).Index]);
    FList.Delete(TPArrayPosition(Position).Index);
    Result := OldAssociation;
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    OldAssociation._Release;
end;

function TPArrayMap.GetCapacity: Integer;
begin
    Result := FList.Capacity;
end;

procedure TPArrayMap.SetCapacity(Value: Integer);
begin
    FList.Capacity := Value;
end;

function TPArrayMap.GetSize: Integer;
begin
    Result := FList.Count;
end;

procedure TExposedPArrayList.TrueAppend(const Item: ICollectable);
begin
    inherited TrueAppend(Item);
end;

procedure TExposedPArrayList.TrueInsert(Index: Integer; const Item: ICollectable);
begin
    inherited TrueInsert(Index, Item);
end;

{ TPArrayIterator }
constructor TPArrayIterator.Create(List: TList; AllowRemove: Boolean);
begin
    inherited Create(AllowRemove);
    FList := List;
    FIndex := -1;
end;

function TPArrayIterator.TrueFirst: ICollectable;
begin
    FIndex := 0;
    if FIndex < FList.Count then
        Result := ICollectable(FList[FIndex])
    else
        Result := nil;
end;

function TPArrayIterator.TrueNext: ICollectable;
begin
    Inc(FIndex);
    if FIndex < FList.Count then
        Result := ICollectable(FList[FIndex])
    else
        Result := nil;
end;

procedure TPArrayIterator.TrueRemove;
var
    Item: ICollectable;
begin
    Item := ICollectable(FList[FIndex]);
    FList.Delete(FIndex);
    Dec(FIndex);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Item._Release;
end;

{ TPArrayAssociationIterator }
constructor TPArrayAssociationIterator.Create(List: TList; AllowRemove: Boolean);
begin
    inherited Create(AllowRemove);
    FList := List;
    FIndex := -1;
end;

function TPArrayAssociationIterator.TrueFirst: IAssociation;
begin
    FIndex := 0;
    if FIndex < FList.Count then
        Result := IAssociation(FList[FIndex])
    else
        Result := nil;
end;

function TPArrayAssociationIterator.TrueNext: IAssociation;
begin
    Inc(FIndex);
    if FIndex < FList.Count then
        Result := IAssociation(FList[FIndex])
    else
        Result := nil;
end;

procedure TPArrayAssociationIterator.TrueRemove;
var
    Association: IAssociation;
begin
    Association := IAssociation(FList[FIndex]);
    FList.Delete(FIndex);
    Dec(FIndex);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._Release;
end;

{ TPArrayPosition }
constructor TPArrayPosition.Create(Found: Boolean; Index: Integer);
begin
    inherited Create(Found);
    FIndex := Index;
end;

end.
