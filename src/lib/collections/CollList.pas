unit CollList;

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
 * $Log: D:\QVCS Repositories\Delphi Collections\CollList.qbt $
 * 
 *   Collection implementations based on sorted TPArrayList instances.
 * 
 * Revision 1.1.1.2  by: Matthew Greet  Rev date: 12/06/04 20:05:54
 *   Capacity property.
 * 
 * Revision 1.1.1.1  by: Matthew Greet  Rev date: 14/02/04 17:45:38
 *   v1.0 branch.
 * 
 * Revision 1.1  by: Matthew Greet  Rev date: 06/04/03 10:41:52
 *   Uses TExposedPArrayList to improve performance.
 * 
 * Revision 1.0  by: Matthew Greet  Rev date: 01/03/03 10:50:02
 *   Initial revision.
 * 
 * FPC compatibility fixes by: UltraStar Deluxe Team
 *
 * $Endlog$
 *****************************************************************************)

interface

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}

uses
    Collections, CollPArray;

type
    TListSet = class(TAbstractSet)
    private
        FList: TExposedPArrayList;
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
        function GetNaturalItemIID: TGUID; override;
        function GetSize: Integer; override;
    end;

    TListMap = class(TAbstractMap)
    private
        FList: TExposedPArrayList;
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
        procedure SetKeyComparator(const Value: IComparator); override;
        function GetNaturalKeyIID: TGUID; override;
        function GetSize: Integer; override;
    end;

implementation

type
    TListPosition = class(TCollectionPosition)
    private
        FSearchResult: TSearchResult;
    public
        constructor Create(Found: Boolean; SearchResult: TSearchResult);
        property SearchResult: TSearchResult read FSearchResult;
    end;

constructor TListSet.Create(NaturalItemsOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FList := TExposedPArrayList.Create(NaturalItemsOnly);
    FList.Comparator := Comparator;
    FList.Sort;
end;

destructor TListSet.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TListSet.GetPosition(const Item: ICollectable): TCollectionPosition;
var
    SearchResult: TSearchResult;
begin
    SearchResult := FList.Search(Item);
    Result := TListPosition.Create((SearchResult.ResultType = srFoundAtIndex), SearchResult);
end;

procedure TListSet.TrueAdd2(Position: TCollectionPosition; const Item: ICollectable);
var
    SearchResult: TSearchResult;
    Index: Integer;
begin
    SearchResult := TListPosition(Position).SearchResult;
    Index := SearchResult.Index;
    if SearchResult.ResultType = srBeforeIndex then
        FList.TrueInsert(Index, Item)
    else
        FList.TrueAppend(Item);
end;

procedure TListSet.TrueClear;
begin
    FList.Clear;
end;

function TListSet.TrueGet(Position: TCollectionPosition): ICollectable;
begin
    Result := FList.Items[TListPosition(Position).SearchResult.Index];
end;

procedure TListSet.TrueRemove2(Position: TCollectionPosition);
begin
    FList.Delete(TListPosition(Position).SearchResult.Index);
end;

function TListSet.GetCapacity: Integer;
begin
    Result := FList.Capacity;
end;

procedure TListSet.SetCapacity(Value: Integer);
begin
    FList.Capacity := Value;
end;

function TListSet.GetIterator: IIterator;
begin
    Result := FList.GetIterator;
end;

function TListSet.GetNaturalItemIID: TGUID;
begin
    Result := ComparableIID;
end;

function TListSet.GetSize: Integer;
begin
    Result := FList.Size;
end;

constructor TListMap.Create(NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly, NaturalKeysOnly);
    FList := TExposedPArrayList.Create(false);
    FList.Comparator := AssociationComparator;
    FList.Sort;
end;

destructor TListMap.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;

function TListMap.GetAssociationIterator: IMapIterator;
begin
    Result := TAssociationIterator.Create(FList.GetIterator);
end;

function TListMap.GetKeyPosition(const Key: ICollectable): TCollectionPosition;
var
    Association: IAssociation;
    SearchResult: TSearchResult;
begin
    Association := TAssociation.Create(Key, nil);
    SearchResult := FList.Search(Association);
    Result := TListPosition.Create((SearchResult.ResultType = srFoundAtIndex), SearchResult);
end;

procedure TListMap.TrueClear;
begin
    FList.Clear;
end;

function TListMap.TrueGet(Position: TCollectionPosition): IAssociation;
begin
    Result := (FList.Items[TListPosition(Position).SearchResult.Index]) as IAssociation;
end;

function TListMap.TruePut(Position: TCollectionPosition; const Association: IAssociation): IAssociation;
var
    SearchResult: TSearchResult;
    Index: Integer;
begin
    SearchResult := TListPosition(Position).SearchResult;
    Index := SearchResult.Index;
    if SearchResult.ResultType = srFoundAtIndex then
    begin
        Result := (FList.Items[Index]) as IAssociation;
        FList.Items[Index] := Association;
    end
    else if SearchResult.ResultType = srBeforeIndex then
        FList.TrueInsert(Index, Association)
    else
        FList.TrueAppend(Association);
end;

function TListMap.TrueRemove2(Position: TCollectionPosition): IAssociation;
begin
    Result := (FList.Items[TListPosition(Position).SearchResult.Index]) as IAssociation;
    FList.Delete(TListPosition(Position).SearchResult.Index);
end;

procedure TListMap.SetKeyComparator(const Value: IComparator);
begin
    inherited SetKeyComparator(Value);
    FList.Sort;
end;

function TListMap.GetCapacity: Integer;
begin
    Result := FList.Capacity;
end;

procedure TListMap.SetCapacity(Value: Integer);
begin
    FList.Capacity := Value;
end;

function TListMap.GetNaturalKeyIID: TGUID;
begin
    Result := ComparableIID;
end;

function TListMap.GetSize: Integer;
begin
    Result := FList.Size;
end;

constructor TListPosition.Create(Found: Boolean; SearchResult: TSearchResult);
begin
    inherited Create(Found);
    FSearchResult := SearchResult;
end;

end.
