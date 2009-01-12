unit CollHash;

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
 * $Revision: 1.1.1.2 $
 * $Log: D:\QVCS Repositories\Delphi Collections\CollHash.qbt $
 * 
 *   Collection implementations based on hash tables.
 * 
 * Revision 1.1.1.2  by: Matthew Greet  Rev date: 12/06/04 20:04:30
 *   Capacity property.
 * 
 * Revision 1.1.1.1  by: Matthew Greet  Rev date: 24/10/03 16:48:16
 *   v1.0 branch.
 * 
 * Revision 1.1  by: Matthew Greet  Rev date: 06/04/03 10:40:16
 *   Added integer map and string map versions. 
 *   THashSet uses its own implementation, not THashMap. 
 *   DefaulMaxLoadFactor changed.
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
    Classes, Math,
    Collections;

const
    DefaultTableSize = 100;
    MaxLoadFactorMin = 0.01;        // Minimum allowed value for MaxLoadFactor property.
    DefaultMaxLoadFactor = 5.0;

type
    THashMap = class(TAbstractMap)
    private
        FArray: TListArray;
        FCapacity: Integer;
        FMaxLoadFactor: Double;
        FSize: Integer;
        FTableSize: Integer;
    protected
        function GetAssociationIterator: IMapIterator; override;
        procedure SetMaxLoadFactor(Value: Double); virtual;
        procedure SetTableSize(Value: Integer); virtual;
        procedure ChangeCapacity(Value: TListArray); virtual;
        procedure CheckLoadFactor(AlwaysChangeCapacity: Boolean); virtual;
        function GetHash(const Key: ICollectable): Integer; virtual;
        function GetKeyPosition(const Key: ICollectable): TCollectionPosition; override;
        procedure Rehash;
        procedure TrueClear; override;
        function TrueGet(Position: TCollectionPosition): IAssociation; override;
        function TruePut(Position: TCollectionPosition; const Association: IAssociation): IAssociation; override;
        function TrueRemove2(Position: TCollectionPosition): IAssociation; override;
    public
        constructor Create(NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean); override;
        destructor Destroy; override;
        class function GetAlwaysNaturalKeys: Boolean; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetNaturalKeyIID: TGUID; override;
        function GetSize: Integer; override;
        property MaxLoadFactor: Double read FMaxLoadFactor write SetMaxLoadFactor;
        property TableSize: Integer read FTableSize write SetTableSize;
    end;

    THashSet = class(TAbstractSet)
    private
        FArray: TListArray;
        FCapacity: Integer;
        FMaxLoadFactor: Double;
        FSize: Integer;
        FTableSize: Integer;
    protected
        procedure SetMaxLoadFactor(Value: Double); virtual;
        procedure SetTableSize(Value: Integer); virtual;
        procedure ChangeCapacity(Value: TListArray); virtual;
        procedure CheckLoadFactor(AlwaysChangeCapacity: Boolean); virtual;
        function GetHash(const Item: ICollectable): Integer; virtual;
        function GetPosition(const Item: ICollectable): TCollectionPosition; override;
        procedure Rehash;
        procedure TrueAdd2(Position: TCollectionPosition; const Item: ICollectable); override;
        procedure TrueClear; override;
        function TrueGet(Position: TCollectionPosition): ICollectable; override;
        procedure TrueRemove2(Position: TCollectionPosition); override;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        destructor Destroy; override;
        class function GetAlwaysNaturalItems: Boolean; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetIterator: IIterator; override;
        function GetNaturalItemIID: TGUID; override;
        function GetSize: Integer; override;
        property MaxLoadFactor: Double read FMaxLoadFactor write SetMaxLoadFactor;
        property TableSize: Integer read FTableSize write SetTableSize;
    end;

    THashIntegerMap = class(TAbstractIntegerMap)
    private
        FArray: TListArray;
        FCapacity: Integer;
        FMaxLoadFactor: Double;
        FSize: Integer;
        FTableSize: Integer;
    protected
        function GetAssociationIterator: IIntegerMapIterator; override;
        procedure SetMaxLoadFactor(Value: Double); virtual;
        procedure SetTableSize(Value: Integer); virtual;
        procedure ChangeCapacity(Value: TListArray); virtual;
        procedure CheckLoadFactor(AlwaysChangeCapacity: Boolean); virtual;
        function GetHash(const Key: Integer): Integer; virtual;
        function GetKeyPosition(const Key: Integer): TCollectionPosition; override;
        procedure Rehash;
        procedure TrueClear; override;
        function TrueGet(Position: TCollectionPosition): IIntegerAssociation; override;
        function TruePut(Position: TCollectionPosition; const Association: IIntegerAssociation): IIntegerAssociation; override;
        function TrueRemove2(Position: TCollectionPosition): IIntegerAssociation; override;
    public
        constructor Create; override;
        constructor Create(NaturalItemsOnly: Boolean); override;
        constructor Create(NaturalItemsOnly: Boolean; TableSize: Integer; MaxLoadFactor: Double = DefaultMaxLoadFactor); overload; virtual;
        destructor Destroy; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetSize: Integer; override;
        property MaxLoadFactor: Double read FMaxLoadFactor write SetMaxLoadFactor;
        property TableSize: Integer read FTableSize write SetTableSize;
    end;

    THashStringMap = class(TAbstractStringMap)
    private
        FArray: TListArray;
        FCapacity: Integer;
        FMaxLoadFactor: Double;
        FSize: Integer;
        FTableSize: Integer;
    protected
        function GetAssociationIterator: IStringMapIterator; override;
        procedure SetMaxLoadFactor(Value: Double); virtual;
        procedure SetTableSize(Value: Integer); virtual;
        procedure ChangeCapacity(Value: TListArray); virtual;
        procedure CheckLoadFactor(AlwaysChangeCapacity: Boolean); virtual;
        function GetHash(const Key: String): Integer; virtual;
        function GetKeyPosition(const Key: String): TCollectionPosition; override;
        procedure Rehash;
        procedure TrueClear; override;
        function TrueGet(Position: TCollectionPosition): IStringAssociation; override;
        function TruePut(Position: TCollectionPosition; const Association: IStringAssociation): IStringAssociation; override;
        function TrueRemove2(Position: TCollectionPosition): IStringAssociation; override;
    public
        constructor Create; override;
        constructor Create(NaturalItemsOnly: Boolean); override;
        constructor Create(NaturalItemsOnly: Boolean; TableSize: Integer; MaxLoadFactor: Double = DefaultMaxLoadFactor); overload; virtual;
        destructor Destroy; override;
        function GetCapacity: Integer; override;
        procedure SetCapacity(Value: Integer); override;
        function GetSize: Integer; override;
        property MaxLoadFactor: Double read FMaxLoadFactor write SetMaxLoadFactor;
        property TableSize: Integer read FTableSize write SetTableSize;
    end;

implementation

const
    (* (sqrt(5) - 1)/2
        See Introduction to Algorithms in Pascal, 1995, by Thomas W. Parsons,
        published by John Wiley & Sons, Inc, ISBN 0-471-11600-9
    *)
    HashFactor = 0.618033988749894848204586834365638;

type
    THashIterator = class(TAbstractIterator)
    private
        FHashSet: THashSet;
        FHash: Integer;
        FChainIndex: Integer;
    protected
        constructor Create(HashSet: THashSet);
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    end;

    THashAssociationIterator = class(TAbstractAssociationIterator)
    private
        FHashMap: THashMap;
        FHash: Integer;
        FChainIndex: Integer;
    protected
        constructor Create(HashMap: THashMap);
        function TrueFirst: IAssociation; override;
        function TrueNext: IAssociation; override;
        procedure TrueRemove; override;
    end;

    THashIntegerIterator = class(TAbstractIntegerAssociationIterator)
    private
        FHashIntegerMap: THashIntegerMap;
        FHash: Integer;
        FChainIndex: Integer;
    protected
        constructor Create(HashIntegerMap: THashIntegerMap);
        function TrueFirst: IIntegerAssociation; override;
        function TrueNext: IIntegerAssociation; override;
        procedure TrueRemove; override;
    end;

    THashStringIterator = class(TAbstractStringAssociationIterator)
    private
        FHashStringMap: THashStringMap;
        FHash: Integer;
        FChainIndex: Integer;
    protected
        constructor Create(HashStringMap: THashStringMap);
        function TrueFirst: IStringAssociation; override;
        function TrueNext: IStringAssociation; override;
        procedure TrueRemove; override;
    end;

    THashPosition = class(TCollectionPosition)
    private
        FChain: TList;
        FIndex: Integer;
    public
        constructor Create(Found: Boolean; Chain: TList; Index: Integer);
        property Chain: TList read FChain;
        property Index: Integer read FIndex;
    end;

{ THashMap }
constructor THashMap.Create(NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean);
var
    I: Integer;
begin
    // Force use of natural keys
    inherited Create(NaturalItemsOnly, true);
    FTableSize := DefaultTableSize;
    FMaxLoadFactor := DefaultMaxLoadFactor;
    SetLength(FArray, FTableSize);
    for I := Low(FArray) to High(FArray) do
        FArray[I] := TList.Create;
    FCapacity := 0;
    FSize := 0;
    ChangeCapacity(FArray);
end;

destructor THashMap.Destroy;
var
    I: Integer;
begin
    for I := Low(FArray) to High(FArray) do
        FArray[I].Free;
    FArray := nil;
    inherited Destroy;
end;

class function THashMap.GetAlwaysNaturalKeys: Boolean;
begin
    Result := true;
end;

function THashMap.GetNaturalKeyIID: TGUID;
begin
    Result := HashableIID;
end;

function THashMap.GetAssociationIterator: IMapIterator;
begin
    Result := THashAssociationIterator.Create(Self);
end;

procedure THashMap.SetTableSize(Value: Integer);
begin
    if (FTableSize <> Value) and (Value >= 1) then
    begin
        FTableSize := Value;
        Rehash;
    end;
end;

procedure THashMap.SetMaxLoadFactor(Value: Double);
begin
    if (FMaxLoadFactor <> Value) and (Value >= MaxLoadFactorMin) then
    begin
        FMaxLoadFactor := Value;
        CheckLoadFactor(false);
    end;
end;

procedure THashMap.ChangeCapacity(Value: TListArray);
var
    Chain: TList;
    I, Total, ChainCapacity: Integer;
begin
    if FCapacity mod FTableSize = 0 then
        ChainCapacity := Trunc(FCapacity / FTableSize)
    else
        ChainCapacity := Trunc(FCapacity / FTableSize) + 1;
    Total := 0;
    for I := Low(Value) to High(Value) do
    begin
        Chain := Value[I];
        Chain.Capacity := ChainCapacity;
        Total := Total + Chain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashMap.CheckLoadFactor(AlwaysChangeCapacity: Boolean);
var
    LoadFactor: Double;
begin
    LoadFactor := Capacity / TableSize;
    if LoadFactor > MaxLoadFactor then
        TableSize := Trunc(Capacity / Max(MaxLoadFactor, MaxLoadFactorMin))
    else if AlwaysChangeCapacity then
        ChangeCapacity(FArray);
end;

function THashMap.GetHash(const Key: ICollectable): Integer;
var
    Hashable: IHashable;
    HashCode: Cardinal;
begin
    Key.QueryInterface(IHashable, Hashable);
    HashCode := Hashable.HashCode;
    Result := Trunc(Frac(HashCode * HashFactor) * TableSize);
end;

function THashMap.GetKeyPosition(const Key: ICollectable): TCollectionPosition;
var
    Chain: TList;
    I: Integer;
    Success: Boolean;
begin
    Chain := FArray[GetHash(Key)];
    Success := false;
    for I := 0 to Chain.Count - 1 do
    begin
        Success := KeyComparator.Equals(Key, IAssociation(Chain[I]).GetKey);
        if Success then
            Break;
    end;
    Result := THashPosition.Create(Success, Chain, I);
end;

procedure THashMap.Rehash;
var
    NewArray: TListArray;
    OldChain, NewChain: TList;
    Association: IAssociation;
    Total: Integer;
    I, J: Integer;
    Hash: Integer;
begin
    // Create new chains
    SetLength(NewArray, TableSize);
    for I := Low(NewArray) to High(NewArray) do
    begin
        NewChain := TList.Create;
        NewArray[I] := NewChain;
    end;
    ChangeCapacity(NewArray);

    // Transfer from old chains to new and drop old
    for I := Low(FArray) to High(FArray) do
    begin
        OldChain := FArray[I];
        for J := 0 to OldChain.Count - 1 do
        begin
            Association := IAssociation(OldChain[J]);
            Hash := GetHash(Association.GetKey);
            NewArray[Hash].Add(Pointer(Association));
        end;
        OldChain.Free;
    end;
    FArray := NewArray;

    // Find actual, new capacity
    Total := 0;
    for I := Low(FArray) to High(FArray) do
    begin
        NewChain := FArray[I];
        Total := Total + NewChain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashMap.TrueClear;
var
    Association: IAssociation;
    Chain: TList;
    I, J: Integer;
begin
    for I := Low(FArray) to High(FArray) do
    begin
        Chain := FArray[I];
        for J := 0 to Chain.Count - 1 do
        begin
            Association := IAssociation(Chain[J]);
            Chain[J] := nil;
            // Storing interface reference as a pointer does not update reference
            // count automatically, so this must be done manually
            Association._Release;
        end;
        Chain.Clear;
    end;
    FSize := 0;
end;

function THashMap.TrueGet(Position: TCollectionPosition): IAssociation;
var
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Result := IAssociation(HashPosition.Chain.Items[HashPosition.Index]);
end;

function THashMap.TruePut(Position: TCollectionPosition; const Association: IAssociation): IAssociation;
var
    HashPosition: THashPosition;
    OldAssociation: IAssociation;
begin
    HashPosition := THashPosition(Position);
    if HashPosition.Found then
    begin
        OldAssociation := IAssociation(HashPosition.Chain.Items[HashPosition.Index]);
        HashPosition.Chain.Items[HashPosition.Index] := Pointer(Association);
        Result := OldAssociation;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Association._AddRef;
        OldAssociation._Release;
    end
    else
    begin
        HashPosition.Chain.Add(Pointer(Association));
        Inc(FSize);
        Result := nil;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Association._AddRef;
    end;
end;

function THashMap.TrueRemove2(Position: TCollectionPosition): IAssociation;
var
    Association: IAssociation;
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Association := IAssociation(TrueGet(Position));
    HashPosition.Chain.Delete(HashPosition.Index);
    Dec(FSize);
    Result := Association;
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._Release;
end;

function THashMap.GetCapacity;
begin
    Result := FCapacity;
end;

procedure THashMap.SetCapacity(Value: Integer);
begin
    FCapacity := Value;
    CheckLoadFactor(true);
end;

function THashMap.GetSize: Integer;
begin
    Result := FSize;
end;

{ THashSet }
constructor THashSet.Create(NaturalItemsOnly: Boolean);
var
    I: Integer;
begin
    // Force use of natural items
    inherited Create(true);
    FTableSize := DefaultTableSize;
    FMaxLoadFactor := DefaultMaxLoadFactor;
    SetLength(FArray, FTableSize);
    for I := Low(FArray) to High(FArray) do
        FArray[I] := TList.Create;
    FSize := 0;
end;

destructor THashSet.Destroy;
var
    I: Integer;
begin
    for I := Low(FArray) to High(FArray) do
        FArray[I].Free;
    FArray := nil;
    inherited Destroy;
end;

procedure THashSet.SetTableSize(Value: Integer);
begin
    if (FTableSize <> Value) and (Value >= 1) then
    begin
        FTableSize := Value;
        Rehash;
    end;
end;

procedure THashSet.SetMaxLoadFactor(Value: Double);
begin
    if (FMaxLoadFactor <> Value) and (Value >= MaxLoadFactorMin) then
    begin
        FMaxLoadFactor := Value;
        CheckLoadFactor(false);
    end;
end;

procedure THashSet.ChangeCapacity(Value: TListArray);
var
    Chain: TList;
    I, Total, ChainCapacity: Integer;
begin
    if FCapacity mod FTableSize = 0 then
        ChainCapacity := Trunc(FCapacity / FTableSize)
    else
        ChainCapacity := Trunc(FCapacity / FTableSize) + 1;
    Total := 0;
    for I := Low(Value) to High(Value) do
    begin
        Chain := Value[I];
        Chain.Capacity := ChainCapacity;
        Total := Total + Chain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashSet.CheckLoadFactor(AlwaysChangeCapacity: Boolean);
var
    LoadFactor: Double;
begin
    LoadFactor := Capacity / TableSize;
    if LoadFactor > MaxLoadFactor then
        TableSize := Trunc(Capacity / Max(MaxLoadFactor, MaxLoadFactorMin))
    else if AlwaysChangeCapacity then
        ChangeCapacity(FArray);
end;

function THashSet.GetHash(const Item: ICollectable): Integer;
var
    Hashable: IHashable;
    HashCode: Cardinal;
begin
    Item.QueryInterface(IHashable, Hashable);
    HashCode := Hashable.HashCode;
    Result := Trunc(Frac(HashCode * HashFactor) * TableSize);
end;

function THashSet.GetPosition(const Item: ICollectable): TCollectionPosition;
var
    Chain: TList;
    I: Integer;
    Success: Boolean;
begin
    Chain := FArray[GetHash(Item)];
    Success := false;
    for I := 0 to Chain.Count - 1 do
    begin
        Success := Comparator.Equals(Item, ICollectable(Chain[I]));
        if Success then
            Break;
    end;
    Result := THashPosition.Create(Success, Chain, I);
end;

procedure THashSet.Rehash;
var
    NewArray: TListArray;
    OldChain, NewChain: TList;
    Item: ICollectable;
    Total: Integer;
    I, J: Integer;
    Hash: Integer;
begin
    // Create new chains
    SetLength(NewArray, TableSize);
    for I := Low(NewArray) to High(NewArray) do
    begin
        NewChain := TList.Create;
        NewArray[I] := NewChain;
    end;
    ChangeCapacity(NewArray);

    // Transfer from old chains to new and drop old
    for I := Low(FArray) to High(FArray) do
    begin
        OldChain := FArray[I];
        for J := 0 to OldChain.Count - 1 do
        begin
            Item := ICollectable(OldChain[J]);
            Hash := GetHash(Item);
            NewArray[Hash].Add(Pointer(Item));
        end;
        OldChain.Free;
    end;
    FArray := NewArray;

    // Find actual, new capacity
    Total := 0;
    for I := Low(FArray) to High(FArray) do
    begin
        NewChain := FArray[I];
        Total := Total + NewChain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashSet.TrueAdd2(Position: TCollectionPosition; const Item: ICollectable);
var
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    HashPosition.Chain.Add(Pointer(Item));
    Inc(FSize);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Item._AddRef;
end;

procedure THashSet.TrueClear;
var
    Item: ICollectable;
    Chain: TList;
    I, J: Integer;
begin
    for I := Low(FArray) to High(FArray) do
    begin
        Chain := FArray[I];
        for J := 0 to Chain.Count - 1 do
        begin
            Item := ICollectable(Chain[J]);
            Chain[J] := nil;
            // Storing interface reference as a pointer does not update reference
            // count automatically, so this must be done manually
            Item._Release;
        end;
        Chain.Clear;
    end;
    FSize := 0;
end;

function THashSet.TrueGet(Position: TCollectionPosition): ICollectable;
var
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Result := ICollectable(HashPosition.Chain.Items[HashPosition.Index]);
end;

procedure THashSet.TrueRemove2(Position: TCollectionPosition);
var
    Item: ICollectable;
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Item := TrueGet(Position);
    HashPosition.Chain.Delete(HashPosition.Index);
    Dec(FSize);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Item._Release;
end;

class function THashSet.GetAlwaysNaturalItems: Boolean;
begin
    Result := true;
end;

function THashSet.GetIterator: IIterator;
begin
    Result := THashIterator.Create(Self);
end;

function THashSet.GetNaturalItemIID: TGUID;
begin
    Result := HashableIID;
end;

function THashSet.GetCapacity;
begin
    Result := FCapacity;
end;

procedure THashSet.SetCapacity(Value: Integer);
begin
    FCapacity := Value;
    CheckLoadFactor(true);
end;

function THashSet.GetSize: Integer;
begin
    Result := FSize;
end;

{ THashIntegerMap }
constructor THashIntegerMap.Create;
begin
    Create(false, DefaultTableSize, DefaultMaxLoadFactor);
end;

constructor THashIntegerMap.Create(NaturalItemsOnly: Boolean);
begin
    Create(NaturalItemsOnly, DefaultTableSize, DefaultMaxLoadFactor);
end;

constructor THashIntegerMap.Create(NaturalItemsOnly: Boolean; TableSize: Integer; MaxLoadFactor: Double = DefaultMaxLoadFactor);
var
    I: Integer;
begin
    inherited Create(NaturalItemsOnly);
    SetLength(FArray, TableSize);
    for I := Low(FArray) to High(FArray) do
        FArray[I] := TList.Create;
    FTableSize := TableSize;
    FMaxLoadFactor := MaxLoadFactor;
    FSize := 0;
end;

destructor THashIntegerMap.Destroy;
var
    I: Integer;
begin
    for I := Low(FArray) to High(FArray) do
        FArray[I].Free;
    FArray := nil;
    inherited Destroy;
end;

function THashIntegerMap.GetAssociationIterator: IIntegerMapIterator;
begin
    Result := THashIntegerIterator.Create(Self);
end;

procedure THashIntegerMap.SetTableSize(Value: Integer);
begin
    if (FTableSize <> Value) and (Value >= 1) then
    begin
        FTableSize := Value;
        Rehash;
    end;
end;

procedure THashIntegerMap.SetMaxLoadFactor(Value: Double);
begin
    if (FMaxLoadFactor <> Value) and (Value >= MaxLoadFactorMin) then
    begin
        FMaxLoadFactor := Value;
        CheckLoadFactor(false);
    end;
end;

procedure THashIntegerMap.ChangeCapacity;
var
    Chain: TList;
    I, Total, ChainCapacity: Integer;
begin
    if FCapacity mod FTableSize = 0 then
        ChainCapacity := Trunc(FCapacity / FTableSize)
    else
        ChainCapacity := Trunc(FCapacity / FTableSize) + 1;
    Total := 0;
    for I := Low(Value) to High(Value) do
    begin
        Chain := Value[I];
        Chain.Capacity := ChainCapacity;
        Total := Total + Chain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashIntegerMap.CheckLoadFactor(AlwaysChangeCapacity: Boolean);
var
    LoadFactor: Double;
begin
    LoadFactor := Capacity / TableSize;
    if LoadFactor > MaxLoadFactor then
        TableSize := Trunc(Capacity / Max(MaxLoadFactor, MaxLoadFactorMin))
    else if AlwaysChangeCapacity then
        ChangeCapacity(FArray);
end;

function THashIntegerMap.GetHash(const Key: Integer): Integer;
begin
    Result := Trunc(Frac(Cardinal(Key) * HashFactor) * TableSize);
end;

function THashIntegerMap.GetKeyPosition(const Key: Integer): TCollectionPosition;
var
    Chain: TList;
    I: Integer;
    Success: Boolean;
begin
    Chain := FArray[GetHash(Key)];
    Success := false;
    for I := 0 to Chain.Count - 1 do
    begin
        Success := (Key = IIntegerAssociation(Chain[I]).GetKey);
        if Success then
            Break;
    end;
    Result := THashPosition.Create(Success, Chain, I);
end;

procedure THashIntegerMap.Rehash;
var
    NewArray: TListArray;
    OldChain, NewChain: TList;
    Association: IIntegerAssociation;
    Total: Integer;
    I, J: Integer;
    Hash: Integer;
begin
    // Create new chains
    SetLength(NewArray, TableSize);
    for I := Low(NewArray) to High(NewArray) do
    begin
        NewChain := TList.Create;
        NewArray[I] := NewChain;
    end;
    ChangeCapacity(NewArray);

    // Transfer from old chains to new and drop old
    for I := Low(FArray) to High(FArray) do
    begin
        OldChain := FArray[I];
        for J := 0 to OldChain.Count - 1 do
        begin
            Association := IIntegerAssociation(OldChain[J]);
            Hash := GetHash(Association.GetKey);
            NewArray[Hash].Add(Pointer(Association));
        end;
        OldChain.Free;
    end;
    FArray := NewArray;

    // Find actual, new capacity
    Total := 0;
    for I := Low(FArray) to High(FArray) do
    begin
        NewChain := FArray[I];
        Total := Total + NewChain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashIntegerMap.TrueClear;
var
    Association: IIntegerAssociation;
    Chain: TList;
    I, J: Integer;
begin
    for I := Low(FArray) to High(FArray) do
    begin
        Chain := FArray[I];
        for J := 0 to Chain.Count - 1 do
        begin
            Association := IIntegerAssociation(Chain[J]);
            Chain[J] := nil;
            // Storing interface reference as a pointer does not update reference
            // count automatically, so this must be done manually
            Association._Release;
        end;
        Chain.Clear;
    end;
    FSize := 0;
end;

function THashIntegerMap.TrueGet(Position: TCollectionPosition): IIntegerAssociation;
var
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Result := IIntegerAssociation(HashPosition.Chain.Items[HashPosition.Index]);
end;

function THashIntegerMap.TruePut(Position: TCollectionPosition; const Association: IIntegerAssociation): IIntegerAssociation;
var
    HashPosition: THashPosition;
    OldAssociation: IIntegerAssociation;
begin
    HashPosition := THashPosition(Position);
    if HashPosition.Found then
    begin
        OldAssociation := IIntegerAssociation(HashPosition.Chain.Items[HashPosition.Index]);
        HashPosition.Chain.Items[HashPosition.Index] := Pointer(Association);
        Result := OldAssociation;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Association._AddRef;
        OldAssociation._Release;
    end
    else
    begin
        HashPosition.Chain.Add(Pointer(Association));
        Inc(FSize);
        Result := nil;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Association._AddRef;
    end;
end;

function THashIntegerMap.TrueRemove2(Position: TCollectionPosition): IIntegerAssociation;
var
    Association: IIntegerAssociation;
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Association := IIntegerAssociation(TrueGet(Position));
    HashPosition.Chain.Delete(HashPosition.Index);
    Dec(FSize);
    Result := Association;
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._Release;
end;

function THashIntegerMap.GetCapacity;
begin
    Result := FCapacity;
end;

procedure THashIntegerMap.SetCapacity(Value: Integer);
begin
    FCapacity := Value;
    CheckLoadFactor(true);
end;

function THashIntegerMap.GetSize: Integer;
begin
    Result := FSize;
end;

{ THashStringMap }
constructor THashStringMap.Create;
begin
    Create(false, DefaultTableSize, DefaultMaxLoadFactor);
end;

constructor THashStringMap.Create(NaturalItemsOnly: Boolean);
begin
    Create(NaturalItemsOnly, DefaultTableSize, DefaultMaxLoadFactor);
end;

constructor THashStringMap.Create(NaturalItemsOnly: Boolean; TableSize: Integer; MaxLoadFactor: Double = DefaultMaxLoadFactor);
var
    I: Integer;
begin
    inherited Create(NaturalItemsOnly);
    SetLength(FArray, TableSize);
    for I := Low(FArray) to High(FArray) do
        FArray[I] := TList.Create;
    FTableSize := TableSize;
    FMaxLoadFactor := MaxLoadFactor;
    FSize := 0;
end;

destructor THashStringMap.Destroy;
var
    I: Integer;
begin
    for I := Low(FArray) to High(FArray) do
        FArray[I].Free;
    FArray := nil;
    inherited Destroy;
end;

function THashStringMap.GetAssociationIterator: IStringMapIterator;
begin
    Result := THashStringIterator.Create(Self);
end;

procedure THashStringMap.SetTableSize(Value: Integer);
begin
    if (FTableSize <> Value) and (Value >= 1) then
    begin
        FTableSize := Value;
        Rehash;
    end;
end;

procedure THashStringMap.SetMaxLoadFactor(Value: Double);
begin
    if (FMaxLoadFactor <> Value) and (Value >= MaxLoadFactorMin) then
    begin
        FMaxLoadFactor := Value;
        CheckLoadFactor(false);
    end;
end;

procedure THashStringMap.ChangeCapacity;
var
    Chain: TList;
    I, Total, ChainCapacity: Integer;
begin
    if FCapacity mod FTableSize = 0 then
        ChainCapacity := Trunc(FCapacity / FTableSize)
    else
        ChainCapacity := Trunc(FCapacity / FTableSize) + 1;
    Total := 0;
    for I := Low(Value) to High(Value) do
    begin
        Chain := Value[I];
        Chain.Capacity := ChainCapacity;
        Total := Total + Chain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashStringMap.CheckLoadFactor(AlwaysChangeCapacity: Boolean);
var
    LoadFactor: Double;
begin
    LoadFactor := Capacity / TableSize;
    if LoadFactor > MaxLoadFactor then
        TableSize := Trunc(Capacity / Max(MaxLoadFactor, MaxLoadFactorMin))
    else if AlwaysChangeCapacity then
        ChangeCapacity(FArray);
end;

function THashStringMap.GetHash(const Key: String): Integer;
var
    HashCode: Cardinal;
    I: Integer;
begin
    HashCode := 0;
    for I := 1 to Length(Key) do
        HashCode := (HashCode shl 1) xor Ord(Key[I]);
    Result := Trunc(Frac(HashCode * HashFactor) * TableSize);
end;

function THashStringMap.GetKeyPosition(const Key: String): TCollectionPosition;
var
    Chain: TList;
    I: Integer;
    Success: Boolean;
begin
    Chain := FArray[GetHash(Key)];
    Success := false;
    for I := 0 to Chain.Count - 1 do
    begin
        Success := (Key = IStringAssociation(Chain[I]).GetKey);
        if Success then
            Break;
    end;
    Result := THashPosition.Create(Success, Chain, I);
end;

procedure THashStringMap.Rehash;
var
    NewArray: TListArray;
    OldChain, NewChain: TList;
    Association: IStringAssociation;
    Total: Integer;
    I, J: Integer;
    Hash: Integer;
begin
    // Create new chains
    SetLength(NewArray, TableSize);
    for I := Low(NewArray) to High(NewArray) do
    begin
        NewChain := TList.Create;
        NewArray[I] := NewChain;
    end;
    ChangeCapacity(NewArray);

    // Transfer from old chains to new and drop old
    for I := Low(FArray) to High(FArray) do
    begin
        OldChain := FArray[I];
        for J := 0 to OldChain.Count - 1 do
        begin
            Association := IStringAssociation(OldChain[J]);
            Hash := GetHash(Association.GetKey);
            NewArray[Hash].Add(Pointer(Association));
        end;
        OldChain.Free;
    end;
    FArray := NewArray;

    // Find actual, new capacity
    Total := 0;
    for I := Low(FArray) to High(FArray) do
    begin
        NewChain := FArray[I];
        Total := Total + NewChain.Capacity;
    end;
    FCapacity := Total;
end;

procedure THashStringMap.TrueClear;
var
    Association: IStringAssociation;
    Chain: TList;
    I, J: Integer;
begin
    for I := Low(FArray) to High(FArray) do
    begin
        Chain := FArray[I];
        for J := 0 to Chain.Count - 1 do
        begin
            Association := IStringAssociation(Chain[J]);
            Chain[J] := nil;
            // Storing interface reference as a pointer does not update reference
            // count automatically, so this must be done manually
            Association._Release;
        end;
        Chain.Clear;
    end;
    FSize := 0;
end;

function THashStringMap.TrueGet(Position: TCollectionPosition): IStringAssociation;
var
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Result := IStringAssociation(HashPosition.Chain.Items[HashPosition.Index]);
end;

function THashStringMap.TruePut(Position: TCollectionPosition; const Association: IStringAssociation): IStringAssociation;
var
    HashPosition: THashPosition;
    OldAssociation: IStringAssociation;
begin
    HashPosition := THashPosition(Position);
    if HashPosition.Found then
    begin
        OldAssociation := IStringAssociation(HashPosition.Chain.Items[HashPosition.Index]);
        HashPosition.Chain.Items[HashPosition.Index] := Pointer(Association);
        Result := OldAssociation;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Association._AddRef;
        OldAssociation._Release;
    end
    else
    begin
        HashPosition.Chain.Add(Pointer(Association));
        Inc(FSize);
        Result := nil;
        // Storing interface reference as a pointer does not update reference
        // count automatically, so this must be done manually
        Association._AddRef;
    end;
end;

function THashStringMap.TrueRemove2(Position: TCollectionPosition): IStringAssociation;
var
    Association: IStringAssociation;
    HashPosition: THashPosition;
begin
    HashPosition := THashPosition(Position);
    Association := IStringAssociation(TrueGet(Position));
    HashPosition.Chain.Delete(HashPosition.Index);
    Dec(FSize);
    Result := Association;
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._Release;
end;

function THashStringMap.GetCapacity;
begin
    Result := FCapacity;
end;

procedure THashStringMap.SetCapacity(Value: Integer);
begin
    FCapacity := Value;
    CheckLoadFactor(true);
end;

function THashStringMap.GetSize: Integer;
begin
    Result := FSize;
end;

{ THashPosition }
constructor THashPosition.Create(Found: Boolean; Chain: TList; Index: Integer);
begin
    inherited Create(Found);
    FChain := Chain;
    FIndex := Index;
end;

{ THashIterator }
constructor THashIterator.Create(HashSet: THashSet);
begin
    inherited Create(true);
    FHashSet := HashSet;
    First;
end;

function THashIterator.TrueFirst: ICollectable;
var
    Chain: TList;
    Success: Boolean;
begin
    FHash := 0;
    FChainIndex := 0;
    Success := false;
    while FHash < FHashSet.TableSize do
    begin
        Chain := FHashSet.FArray[FHash];
        Success := Chain.Count > 0;
        if Success then
            Break;
        Inc(FHash);
    end;
    if Success then
        Result := ICollectable(FHashSet.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

function THashIterator.TrueNext: ICollectable;
var
    Chain: TList;
    Success: Boolean;
begin
    Success := false;
    Chain := FHashSet.FArray[FHash];
    repeat
        Inc(FChainIndex);
        if FChainIndex >= Chain.Count then
        begin
            Inc(FHash);
            FChainIndex := -1;
            if FHash < FHashSet.TableSize then
                Chain := FHashSet.FArray[FHash];
        end
        else
            Success := true;
    until Success or (FHash >= FHashSet.TableSize);
    if Success then
        Result := ICollectable(FHashSet.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

procedure THashIterator.TrueRemove;
var
    Item: ICollectable;
begin
    Item := ICollectable(FHashSet.FArray[FHash].Items[FChainIndex]);
    FHashSet.FArray[FHash].Delete(FChainIndex);
    Dec(FChainIndex);
    Dec(FHashSet.FSize);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Item._Release;
end;


{ THashAssociationIterator }
constructor THashAssociationIterator.Create(HashMap: THashMap);
begin
    inherited Create(true);
    FHashMap := HashMap;
    First;
end;

function THashAssociationIterator.TrueFirst: IAssociation;
var
    Chain: TList;
    Success: Boolean;
begin
    FHash := 0;
    FChainIndex := 0;
    Success := false;
    while FHash < FHashMap.TableSize do
    begin
        Chain := FHashMap.FArray[FHash];
        Success := Chain.Count > 0;
        if Success then
            Break;
        Inc(FHash);
    end;
    if Success then
        Result := IAssociation(FHashMap.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

function THashAssociationIterator.TrueNext: IAssociation;
var
    Chain: TList;
    Success: Boolean;
begin
    Success := false;
    Chain := FHashMap.FArray[FHash];
    repeat
        Inc(FChainIndex);
        if FChainIndex >= Chain.Count then
        begin
            Inc(FHash);
            FChainIndex := -1;
            if FHash < FHashMap.TableSize then
                Chain := FHashMap.FArray[FHash];
        end
        else
            Success := true;
    until Success or (FHash >= FHashMap.TableSize);
    if Success then
        Result := IAssociation(FHashMap.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

procedure THashAssociationIterator.TrueRemove;
var
    Association: IAssociation;
begin
    Association := IAssociation(FHashMap.FArray[FHash].Items[FChainIndex]);
    FHashMap.FArray[FHash].Delete(FChainIndex);
    Dec(FChainIndex);
    Dec(FHashMap.FSize);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._Release;
end;


{ THashIntegerIterator }
constructor THashIntegerIterator.Create(HashIntegerMap: THashIntegerMap);
begin
    inherited Create(true);
    FHashIntegerMap := HashIntegerMap;
    First;
end;

function THashIntegerIterator.TrueFirst: IIntegerAssociation;
var
    Chain: TList;
    Success: Boolean;
begin
    FHash := 0;
    FChainIndex := 0;
    Success := false;
    while FHash < FHashIntegerMap.TableSize do
    begin
        Chain := FHashIntegerMap.FArray[FHash];
        Success := Chain.Count > 0;
        if Success then
            Break;
        Inc(FHash);
    end;
    if Success then
        Result := IIntegerAssociation(FHashIntegerMap.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

function THashIntegerIterator.TrueNext: IIntegerAssociation;
var
    Chain: TList;
    Success: Boolean;
begin
    Success := false;
    Chain := FHashIntegerMap.FArray[FHash];
    repeat
        Inc(FChainIndex);
        if FChainIndex >= Chain.Count then
        begin
            Inc(FHash);
            FChainIndex := -1;
            if FHash < FHashIntegerMap.TableSize then
                Chain := FHashIntegerMap.FArray[FHash];
        end
        else
            Success := true;
    until Success or (FHash >= FHashIntegerMap.TableSize);
    if Success then
        Result := IIntegerAssociation(FHashIntegerMap.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

procedure THashIntegerIterator.TrueRemove;
var
    Association: IIntegerAssociation;
begin
    Association := IIntegerAssociation(FHashIntegerMap.FArray[FHash].Items[FChainIndex]);
    FHashIntegerMap.FArray[FHash].Delete(FChainIndex);
    Dec(FChainIndex);
    Dec(FHashIntegerMap.FSize);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._Release;
end;

{ THashStringIterator }
constructor THashStringIterator.Create(HashStringMap: THashStringMap);
begin
    inherited Create(true);
    FHashStringMap := HashStringMap;
    First;
end;

function THashStringIterator.TrueFirst: IStringAssociation;
var
    Chain: TList;
    Success: Boolean;
begin
    FHash := 0;
    FChainIndex := 0;
    Success := false;
    while FHash < FHashStringMap.TableSize do
    begin
        Chain := FHashStringMap.FArray[FHash];
        Success := Chain.Count > 0;
        if Success then
            Break;
        Inc(FHash);
    end;
    if Success then
        Result := IStringAssociation(FHashStringMap.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

function THashStringIterator.TrueNext: IStringAssociation;
var
    Chain: TList;
    Success: Boolean;
begin
    Success := false;
    Chain := FHashStringMap.FArray[FHash];
    repeat
        Inc(FChainIndex);
        if FChainIndex >= Chain.Count then
        begin
            Inc(FHash);
            FChainIndex := -1;
            if FHash < FHashStringMap.TableSize then
                Chain := FHashStringMap.FArray[FHash];
        end
        else
            Success := true;
    until Success or (FHash >= FHashStringMap.TableSize);
    if Success then
        Result := IStringAssociation(FHashStringMap.FArray[FHash].Items[FChainIndex])
    else
        Result := nil;
end;

procedure THashStringIterator.TrueRemove;
var
    Association: IStringAssociation;
begin
    Association := IStringAssociation(FHashStringMap.FArray[FHash].Items[FChainIndex]);
    FHashStringMap.FArray[FHash].Delete(FChainIndex);
    Dec(FChainIndex);
    Dec(FHashStringMap.FSize);
    // Storing interface reference as a pointer does not update reference
    // count automatically, so this must be done manually
    Association._Release;
end;


end.
