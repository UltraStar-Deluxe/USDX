unit Collections;
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
 * $Version: v1.0 $
 * $Revision: 1.1.1.4 $
 * $Log: D:\QVCS Repositories\Delphi Collections\Collections.qbt $
 * 
 *   Main unit containing all interface and abstract class definitions.
 * 
 * Revision 1.1.1.4  by: Matthew Greet  Rev date: 14/03/05 23:26:32
 *   Fixed RemoveAll for TAbstractList for sorted lists.
 * 
 * Revision 1.1.1.3  by: Matthew Greet  Rev date: 14/10/04 16:31:18
 *   Fixed memory lean in ContainsKey of TAbstractStringMap and
 *   TAbstractIntegerMap.
 * 
 * Revision 1.1.1.2  by: Matthew Greet  Rev date: 12/06/04 20:03:26
 *   Capacity property. 
 *   Memory leak fixed.
 * 
 * Revision 1.1.1.1  by: Matthew Greet  Rev date: 13/02/04 16:12:10
 *   v1.0 branch.
 * 
 * Revision 1.1  by: Matthew Greet  Rev date: 06/04/03 10:36:30
 *   Added integer map and string map collection types with supporting
 *   classes. 
 *   Add clone and filter functions with supporting classes. 
 *   Added nil not allowed collection error. 
 *   Properties appear in collection interfaces as well as abstract
 *   classes.
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
    Classes, SysUtils;

const
    EquatableIID: TGUID = '{EAC823A7-0B90-11D7-8120-0002E3165EF8}';
    HashableIID: TGUID = '{98998440-4C3E-11D7-8120-0002E3165EF8}';
    ComparableIID: TGUID = '{9F4C96C0-0CF0-11D7-8120-0002E3165EF8}';
    MappableIID: TGUID = '{DAEC8CA0-0DBB-11D7-8120-0002E3165EF8}';
    StringMappableIID: TGUID = '{3CC61F40-5F92-11D7-8120-0002E3165EF8}';
    IntegerMappableIID: TGUID = '{774FC760-5F92-11D7-8120-0002E3165EF8}';

type
    TDefaultComparator = class;
    TNaturalComparator = class;
    ICollectable = interface;

    TCollectableArray = array of ICollectable;
    TIntegerArray = array of Integer;
    TStringArray = array of String;
    TListArray = array of TList;

    TCollectionError = (ceOK, ceDuplicate, ceDuplicateKey, ceFixedSize, ceNilNotAllowed, ceNotNaturalItem, ceOutOfRange);
    TCollectionErrors = set of TCollectionError;

    TSearchResultType = (srNotFound, srFoundAtIndex, srBeforeIndex, srAfterEnd);

    TCollectionType = (ctBag, ctSet, ctList, ctMap, ctIntegerMap, ctStringMap);

    TCollectionFilterFunc = function (const Item: ICollectable): Boolean of object;
    TCollectionCompareFunc = function (const Item1, Item2: ICollectable): Integer of object;

    TSearchResult = record
        ResultType: TSearchResultType;
        Index: Integer;
    end;

    ICollectable = interface
        ['{98998441-4C3E-11D7-8120-0002E3165EF8}']
        function GetInstance: TObject;
    end;

    IEquatable = interface
        ['{EAC823A7-0B90-11D7-8120-0002E3165EF8}']
        function GetInstance: TObject;
        function Equals(const Item: ICollectable): Boolean;
    end;

    IHashable = interface(IEquatable)
        ['{98998440-4C3E-11D7-8120-0002E3165EF8}']
        function HashCode: Integer;
    end;

    IComparable = interface(IEquatable)
        ['{9F4C96C0-0CF0-11D7-8120-0002E3165EF8}']
        function CompareTo(const Item: ICollectable): Integer;
    end;

    IMappable = interface(IEquatable)
        ['{DAEC8CA0-0DBB-11D7-8120-0002E3165EF8}']
        function GetKey: ICollectable;
    end;

    IStringMappable = interface(IEquatable)
        ['{3CC61F40-5F92-11D7-8120-0002E3165EF8}']
        function GetKey: String;
    end;

    IIntegerMappable = interface(IEquatable)
        ['{774FC760-5F92-11D7-8120-0002E3165EF8}']
        function GetKey: Integer;
    end;

    IComparator = interface
        ['{1F20CD60-10FE-11D7-8120-0002E3165EF8}']
        function GetInstance: TObject;
        function Compare(const Item1, Item2: ICollectable): Integer;
        function Equals(const Item1, Item2: ICollectable): Boolean; overload;
        function Equals(const Comparator: IComparator): Boolean; overload;
    end;

    IFilter = interface
        ['{27FE44C0-638E-11D7-8120-0002E3165EF8}']
        function Accept(const Item: ICollectable): Boolean;
    end;

    IIterator = interface
        ['{F6930500-1113-11D7-8120-0002E3165EF8}']
        function GetAllowRemoval: Boolean;
        function CurrentItem: ICollectable;
        function EOF: Boolean;
        function First: ICollectable;
        function Next: ICollectable;
        function Remove: Boolean;
    end;

    IMapIterator = interface(IIterator)
        ['{848CC0E0-2A31-11D7-8120-0002E3165EF8}']
        function CurrentKey: ICollectable;
    end;

    IIntegerMapIterator = interface(IIterator)
        ['{C7169780-606C-11D7-8120-0002E3165EF8}']
        function CurrentKey: Integer;
    end;

    IStringMapIterator = interface(IIterator)
        ['{1345ED20-5F93-11D7-8120-0002E3165EF8}']
        function CurrentKey: String;
    end;

    IAssociation = interface(ICollectable)
        ['{556CD700-4DB3-11D7-8120-0002E3165EF8}']
        function GetKey: ICollectable;
        function GetValue: ICollectable;
    end;

    IIntegerAssociation = interface(ICollectable)
        ['{ED954420-5F94-11D7-8120-0002E3165EF8}']
        function GetKey: Integer;
        function GetValue: ICollectable;
    end;

    IStringAssociation = interface(ICollectable)
        ['{FB87D2A0-5F94-11D7-8120-0002E3165EF8}']
        function GetKey: String;
        function GetValue: ICollectable;
    end;

    IAssociationComparator = interface(IComparator)
        ['{EA9BE6E0-A852-11D8-B93A-0002E3165EF8}']
        function GetKeyComparator: IComparator;
        procedure SetKeyComparator(Value: IComparator);
        property KeyComparator: IComparator read GetKeyComparator write SetKeyComparator;
    end;

    IIntegerAssociationComparator = interface(IComparator)
        ['{EA9BE6E1-A852-11D8-B93A-0002E3165EF8}']
    end;

    IStringAssociationComparator = interface(IComparator)
        ['{EA9BE6E2-A852-11D8-B93A-0002E3165EF8}']
    end;

    ICollection = interface
        ['{EAC823AC-0B90-11D7-8120-0002E3165EF8}']
        function GetAsArray: TCollectableArray;
        function GetCapacity: Integer;
        procedure SetCapacity(Value: Integer);
        function GetComparator: IComparator;
        procedure SetComparator(const Value: IComparator);
        function GetDuplicates: Boolean;
        function GetFixedSize: Boolean;
        function GetIgnoreErrors: TCollectionErrors;
        procedure SetIgnoreErrors(Value: TCollectionErrors);
        function GetInstance: TObject;
        function GetIterator: IIterator; overload;
        function GetIterator(const Filter: IFilter): IIterator; overload;
        function GetIterator(FilterFunc: TCollectionFilterFunc): IIterator; overload;
        function GetNaturalItemIID: TGUID;
        function GetNaturalItemsOnly: Boolean;
        function GetSize: Integer;
        function GetType: TCollectionType;
        function Add(const Item: ICollectable): Boolean; overload;
        function Add(const ItemArray: array of ICollectable): Integer; overload;
        function Add(const Collection: ICollection): Integer; overload;
        function Clear: Integer;
        function Clone: ICollection;
        function Contains(const Item: ICollectable): Boolean; overload;
        function Contains(const ItemArray: array of ICollectable): Boolean; overload;
        function Contains(const Collection: ICollection): Boolean; overload;
        function Equals(const Collection: ICollection): Boolean;
        function Find(const Filter: IFilter): ICollectable; overload;
        function Find(FilterFunc: TCollectionFilterFunc): ICollectable; overload;
        function FindAll(const Filter: IFilter = nil): ICollection; overload;
        function FindAll(FilterFunc: TCollectionFilterFunc): ICollection; overload;
        function IsEmpty: Boolean;
        function IsNaturalItem(const Item: ICollectable): Boolean;
        function IsNilAllowed: Boolean;
        function ItemAllowed(const Item: ICollectable): TCollectionError;
        function ItemCount(const Item: ICollectable): Integer; overload;
        function ItemCount(const ItemArray: array of ICollectable): Integer; overload;
        function ItemCount(const Collection: ICollection): Integer; overload;
        function Matching(const ItemArray: array of ICollectable): ICollection; overload;
        function Matching(const Collection: ICollection): ICollection; overload;
        function Remove(const Item: ICollectable): ICollectable; overload;
        function Remove(const ItemArray: array of ICollectable): ICollection; overload;
        function Remove(const Collection: ICollection): ICollection; overload;
        function RemoveAll(const Item: ICollectable): ICollection; overload;
        function RemoveAll(const ItemArray: array of ICollectable): ICollection; overload;
        function RemoveAll(const Collection: ICollection): ICollection; overload;
        function Retain(const ItemArray: array of ICollectable): ICollection; overload;
        function Retain(const Collection: ICollection): ICollection; overload;
        property AsArray: TCollectableArray read GetAsArray;
        property Capacity: Integer read GetCapacity write SetCapacity;
        property Comparator: IComparator read GetComparator write SetComparator;
        property FixedSize: Boolean read GetFixedSize;
        property IgnoreErrors: TCollectionErrors read GetIgnoreErrors write SetIgnoreErrors;
        property NaturalItemIID: TGUID read GetNaturalItemIID;
        property NaturalItemsOnly: Boolean read GetNaturalItemsOnly;
        property Size: Integer read GetSize;
    end;

    IBag = interface(ICollection)
        ['{C29C9560-2D59-11D7-8120-0002E3165EF8}']
        function CloneAsBag: IBag;
    end;

    ISet = interface(ICollection)
        ['{DD7888E2-0BB1-11D7-8120-0002E3165EF8}']
        function CloneAsSet: ISet;
        function Complement(const Universe: ISet): ISet;
        function Intersect(const Set2: ISet): ISet;
        function Union(const Set2: ISet): ISet;
    end;

    IList = interface(ICollection)
        ['{EE81AB60-0B9F-11D7-8120-0002E3165EF8}']
        function GetDuplicates: Boolean;
        procedure SetDuplicates(Value: Boolean);
        function GetItem(Index: Integer): ICollectable;
        procedure SetItem(Index: Integer; const Item: ICollectable);
        function GetSorted: Boolean;
        procedure SetSorted(Value: Boolean);
        function CloneAsList: IList;
        function Delete(Index: Integer): ICollectable;
        procedure Exchange(Index1, Index2: Integer);
        function First: ICollectable;
        function IndexOf(const Item: ICollectable): Integer;
        function Insert(Index: Integer; const Item: ICollectable): Boolean; overload;
        function Insert(Index: Integer; const ItemArray: array of ICollectable): Integer; overload;
        function Insert(Index: Integer; const Collection: ICollection): Integer; overload;
        function Last: ICollectable;
        procedure Sort(const Comparator: IComparator); overload;
        procedure Sort(CompareFunc: TCollectionCompareFunc); overload;
        property Duplicates: Boolean read GetDuplicates write SetDuplicates;
        property Items[Index: Integer]: ICollectable read GetItem write SetItem; default;
        property Sorted: Boolean read GetSorted write SetSorted;
    end;

    IMap = interface(ICollection)
        ['{AD458280-2A6B-11D7-8120-0002E3165EF8}']
        function GetItem(const Key: ICollectable): ICollectable;
        procedure SetItem(const Key, Item: ICollectable);
        function GetKeyComparator: IComparator;
        procedure SetKeyComparator(const Value: IComparator);
        function GetKeyIterator: IIterator;
        function GetKeys: ISet;
        function GetMapIterator: IMapIterator;
        function GetMapIteratorByKey(const Filter: IFilter): IMapIterator; overload;
        function GetMapIteratorByKey(FilterFunc: TCollectionFilterFunc): IMapIterator; overload;
        function GetNaturalKeyIID: TGUID;
        function GetNaturalKeysOnly: Boolean;
        function GetValues: ICollection;
        function CloneAsMap: IMap;
        function ContainsKey(const Key: ICollectable): Boolean; overload;
        function ContainsKey(const KeyArray: array of ICollectable): Boolean; overload;
        function ContainsKey(const Collection: ICollection): Boolean; overload;
        function Get(const Key: ICollectable): ICollectable;
        function IsNaturalKey(const Key: ICollectable): Boolean;
        function KeyAllowed(const Key: ICollectable): TCollectionError;
        function MatchingKey(const KeyArray: array of ICollectable): ICollection; overload;
        function MatchingKey(const Collection: ICollection): ICollection; overload;
        function Put(const Item: ICollectable): ICollectable; overload;
        function Put(const Key, Item: ICollectable): ICollectable; overload;
        function Put(const ItemArray: array of ICollectable): ICollection; overload;
        function Put(const Collection: ICollection): ICollection; overload;
        function Put(const Map: IMap): ICollection; overload;
        function RemoveKey(const Key: ICollectable): ICollectable; overload;
        function RemoveKey(const KeyArray: array of ICollectable): ICollection; overload;
        function RemoveKey(const Collection: ICollection): ICollection; overload;
        function RetainKey(const KeyArray: array of ICollectable): ICollection; overload;
        function RetainKey(const Collection: ICollection): ICollection; overload;
        property KeyComparator: IComparator read GetKeyComparator write SetKeyComparator;
        property Items[const Key: ICollectable]: ICollectable read GetItem write SetItem; default;
        property NaturalKeyIID: TGUID read GetNaturalKeyIID;
        property NaturalKeysOnly: Boolean read GetNaturalKeysOnly;
    end;

    IIntegerMap = interface(ICollection)
        ['{93DBA9A0-606C-11D7-8120-0002E3165EF8}']
        function GetItem(const Key: Integer): ICollectable;
        procedure SetItem(const Key: Integer; const Item: ICollectable);
        function GetKeys: ISet;
        function GetMapIterator: IIntegerMapIterator;
        function GetValues: ICollection;
        function CloneAsIntegerMap: IIntegerMap;
        function ContainsKey(const Key: Integer): Boolean; overload;
        function ContainsKey(const KeyArray: array of Integer): Boolean; overload;
        function Get(const Key: Integer): ICollectable;
        function Put(const Item: ICollectable): ICollectable; overload;
        function Put(const Key: Integer; const Item: ICollectable): ICollectable; overload;
        function Put(const ItemArray: array of ICollectable): ICollection; overload;
        function Put(const Collection: ICollection): ICollection; overload;
        function Put(const Map: IIntegerMap): ICollection; overload;
        function RemoveKey(const Key: Integer): ICollectable; overload;
        function RemoveKey(const KeyArray: array of Integer): ICollection; overload;
        function RetainKey(const KeyArray: array of Integer): ICollection; overload;
        property Items[const Key: Integer]: ICollectable read GetItem write SetItem; default;
    end;

    IStringMap = interface(ICollection)
        ['{20531A20-5F92-11D7-8120-0002E3165EF8}']
        function GetItem(const Key: String): ICollectable;
        procedure SetItem(const Key: String; const Item: ICollectable);
        function GetKeys: ISet;
        function GetMapIterator: IStringMapIterator;
        function GetValues: ICollection;
        function CloneAsStringMap: IStringMap;
        function ContainsKey(const Key: String): Boolean; overload;
        function ContainsKey(const KeyArray: array of String): Boolean; overload;
        function Get(const Key: String): ICollectable;
        function Put(const Item: ICollectable): ICollectable; overload;
        function Put(const Key: String; const Item: ICollectable): ICollectable; overload;
        function Put(const ItemArray: array of ICollectable): ICollection; overload;
        function Put(const Collection: ICollection): ICollection; overload;
        function Put(const Map: IStringMap): ICollection; overload;
        function RemoveKey(const Key: String): ICollectable; overload;
        function RemoveKey(const KeyArray: array of String): ICollection; overload;
        function RetainKey(const KeyArray: array of String): ICollection; overload;
        property Items[const Key: String]: ICollectable read GetItem write SetItem; default;
    end;

    TCollectionPosition = class
    private
        FFound: Boolean;
    public
        constructor Create(Found: Boolean);
        property Found: Boolean read FFound;
    end;

    TAbstractComparator = class(TInterfacedObject, IComparator)
    public
        class function GetDefaultComparator: IComparator;
        class function GetNaturalComparator: IComparator;
        class function GetReverseNaturalComparator: IComparator;
        function GetInstance: TObject;
        function Compare(const Item1, Item2: ICollectable): Integer; virtual; abstract;
        function Equals(const Item1, Item2: ICollectable): Boolean; overload; virtual; abstract;
        function Equals(const Comparator: IComparator): Boolean; overload; virtual;
    end;

    TDefaultComparator = class(TAbstractComparator)
    protected
        constructor Create;
    public
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
    end;

    TNaturalComparator = class(TAbstractComparator)
    protected
        constructor Create;
    public
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
    end;

    TReverseNaturalComparator = class(TAbstractComparator)
    protected
        constructor Create;
    public
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
    end;

    TAssociation = class(TInterfacedObject, ICollectable, IAssociation)
    private
        FKey: ICollectable;
        FValue: ICollectable;
    public
        constructor Create(const Key, Value: ICollectable); virtual;
        destructor Destroy; override;
        function GetInstance: TObject; virtual;
        function GetKey: ICollectable;
        function GetValue: ICollectable;
    end;

    TIntegerAssociation = class(TInterfacedObject, ICollectable, IIntegerAssociation)
    private
        FKey: Integer;
        FValue: ICollectable;
    public
        constructor Create(const Key: Integer; const Value: ICollectable); virtual;
        destructor Destroy; override;
        function GetInstance: TObject; virtual;
        function GetKey: Integer;
        function GetValue: ICollectable;
    end;

    TStringAssociation = class(TInterfacedObject, ICollectable, IStringAssociation)
    private
        FKey: String;
        FValue: ICollectable;
    public
        constructor Create(const Key: String; const Value: ICollectable); virtual;
        destructor Destroy; override;
        function GetInstance: TObject; virtual;
        function GetKey: String;
        function GetValue: ICollectable;
    end;

    TAssociationComparator = class(TAbstractComparator, IAssociationComparator)
    private
        FKeyComparator: IComparator;
    public
        constructor Create(NaturalKeys: Boolean = false);
        destructor Destroy; override;
        function GetKeyComparator: IComparator;
        procedure SetKeyComparator(Value: IComparator);
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
        property KeyComparator: IComparator read GetKeyComparator write SetKeyComparator;
    end;

    TIntegerAssociationComparator = class(TAbstractComparator, IIntegerAssociationComparator)
    public
        constructor Create;
        destructor Destroy; override;
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
    end;

    TStringAssociationComparator = class(TAbstractComparator, IStringAssociationComparator)
    public
        constructor Create;
        destructor Destroy; override;
        function Compare(const Item1, Item2: ICollectable): Integer; override;
        function Equals(const Item1, Item2: ICollectable): Boolean; override;
    end;



    TAbstractCollection = class(TInterfacedObject, ICollection)
    private
        FCreated: Boolean;          // Required to avoid passing destroyed object reference to exception
        FComparator: IComparator;
        FIgnoreErrors: TCollectionErrors;
        FNaturalItemsOnly: Boolean;
    protected
        procedure CollectionError(ErrorType: TCollectionError);
        procedure InitFrom(const Collection: ICollection); overload; virtual;
        function TrueAdd(const Item: ICollectable): Boolean; virtual; abstract;
        procedure TrueClear; virtual; abstract;
        function TrueContains(const Item: ICollectable): Boolean; virtual; abstract;
        function TrueItemCount(const Item: ICollectable): Integer; virtual;
        function TrueRemove(const Item: ICollectable): ICollectable; virtual; abstract;
        function TrueRemoveAll(const Item: ICollectable): ICollection; virtual; abstract;
    public
        constructor Create; overload; virtual;
        constructor Create(NaturalItemsOnly: Boolean); overload; virtual;
        constructor Create(const ItemArray: array of ICollectable); overload; virtual;
        constructor Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean); overload; virtual;
        constructor Create(const Collection: ICollection); overload; virtual;
        destructor Destroy; override;
        class function GetAlwaysNaturalItems: Boolean; virtual;
        function GetAsArray: TCollectableArray; virtual;
        function GetCapacity: Integer; virtual; abstract;
        procedure SetCapacity(Value: Integer); virtual; abstract;
        function GetComparator: IComparator; virtual;
        procedure SetComparator(const Value: IComparator); virtual;
        function GetDuplicates: Boolean; virtual;
        function GetFixedSize: Boolean; virtual;
        function GetIgnoreErrors: TCollectionErrors;
        procedure SetIgnoreErrors(Value: TCollectionErrors);
        function GetInstance: TObject;
        function GetIterator: IIterator; overload; virtual; abstract;
        function GetIterator(const Filter: IFilter): IIterator; overload; virtual;
        function GetIterator(FilterFunc: TCollectionFilterFunc): IIterator; overload; virtual;
        function GetNaturalItemIID: TGUID; virtual; abstract;
        function GetNaturalItemsOnly: Boolean; virtual;
        function GetSize: Integer; virtual; abstract;
        function GetType: TCollectionType; virtual; abstract;
        function Add(const Item: ICollectable): Boolean; overload; virtual;
        function Add(const ItemArray: array of ICollectable): Integer; overload; virtual;
        function Add(const Collection: ICollection): Integer; overload; virtual;
        procedure AfterConstruction; override;
        procedure BeforeDestruction; override;
        function Clear: Integer; virtual;
        function Clone: ICollection; virtual;
        function Contains(const Item: ICollectable): Boolean; overload; virtual;
        function Contains(const ItemArray: array of ICollectable): Boolean; overload; virtual;
        function Contains(const Collection: ICollection): Boolean; overload; virtual;
        function Equals(const Collection: ICollection): Boolean; virtual;
        function Find(const Filter: IFilter): ICollectable; overload; virtual;
        function Find(FilterFunc: TCollectionFilterFunc): ICollectable; overload; virtual;
        function FindAll(const Filter: IFilter): ICollection; overload; virtual;
        function FindAll(FilterFunc: TCollectionFilterFunc): ICollection; overload; virtual;
        function IsEmpty: Boolean; virtual;
        function IsNaturalItem(const Item: ICollectable): Boolean; virtual;
        function IsNilAllowed: Boolean; virtual; abstract;
        function ItemAllowed(const Item: ICollectable): TCollectionError; virtual;
        function ItemCount(const Item: ICollectable): Integer; overload; virtual;
        function ItemCount(const ItemArray: array of ICollectable): Integer; overload; virtual;
        function ItemCount(const Collection: ICollection): Integer; overload; virtual;
        function Matching(const ItemArray: array of ICollectable): ICollection; overload; virtual;
        function Matching(const Collection: ICollection): ICollection; overload; virtual;
        function Remove(const Item: ICollectable): ICollectable; overload; virtual;
        function Remove(const ItemArray: array of ICollectable): ICollection; overload; virtual;
        function Remove(const Collection: ICollection): ICollection; overload; virtual;
        function RemoveAll(const Item: ICollectable): ICollection; overload; virtual;
        function RemoveAll(const ItemArray: array of ICollectable): ICollection; overload; virtual;
        function RemoveAll(const Collection: ICollection): ICollection; overload; virtual;
        function Retain(const ItemArray: array of ICollectable): ICollection; overload; virtual;
        function Retain(const Collection: ICollection): ICollection; overload; virtual;
        property AsArray: TCollectableArray read GetAsArray;
        property Capacity: Integer read GetCapacity write SetCapacity;
        property Comparator: IComparator read GetComparator write SetComparator;
        property FixedSize: Boolean read GetFixedSize;
        property IgnoreErrors: TCollectionErrors read GetIgnoreErrors write SetIgnoreErrors;
        property NaturalItemIID: TGUID read GetNaturalItemIID;
        property NaturalItemsOnly: Boolean read GetNaturalItemsOnly;
        property Size: Integer read GetSize;
    end;

    TAbstractBag = class(TAbstractCollection, IBag)
    public
        function CloneAsBag: IBag; virtual;
        function GetNaturalItemIID: TGUID; override;
        function GetType: TCollectionType; override;
        function IsNilAllowed: Boolean; override;
    end;

    TAbstractSet = class (TAbstractCollection, ISet)
    protected
        function GetPosition(const Item: ICollectable): TCollectionPosition; virtual; abstract;
        function TrueAdd(const Item: ICollectable): Boolean; override;
        procedure TrueAdd2(Position: TCollectionPosition; const Item: ICollectable); virtual; abstract;
        function TrueContains(const Item: ICollectable): Boolean; override;
        function TrueGet(Position: TCollectionPosition): ICollectable; virtual; abstract;
        function TrueRemove(const Item: ICollectable): ICollectable; override;
        procedure TrueRemove2(Position: TCollectionPosition); virtual; abstract;
        function TrueRemoveAll(const Item: ICollectable): ICollection; override;
    public
        function GetDuplicates: Boolean; override;
        function GetNaturalItemIID: TGUID; override;
        function GetType: TCollectionType; override;
        function CloneAsSet: ISet; virtual;
        function Complement(const Universe: ISet): ISet; overload; virtual;
        function Intersect(const Set2: ISet): ISet; overload; virtual;
        function IsNilAllowed: Boolean; override;
        function Union(const Set2: ISet): ISet; overload; virtual;
    end;

    TAbstractList = class(TAbstractCollection, IList)
    private
        FDuplicates: Boolean;
        FSorted: Boolean;
    protected
        function BinarySearch(const Item: ICollectable): TSearchResult; virtual;
        procedure InitFrom(const Collection: ICollection); override;
        procedure QuickSort(Lo, Hi: Integer; const Comparator: IComparator); overload; virtual;
        procedure QuickSort(Lo, Hi: Integer; CompareFunc: TCollectionCompareFunc); overload; virtual;
        function SequentialSearch(const Item: ICollectable; const SearchComparator: IComparator = nil): TSearchResult; virtual;
        function TrueContains(const Item: ICollectable): Boolean; override;
        function TrueGetItem(Index: Integer): ICollectable; virtual; abstract;
        procedure TrueSetItem(Index: Integer; const Item: ICollectable); virtual; abstract;
        function TrueAdd(const Item: ICollectable): Boolean; override;
        procedure TrueAppend(const Item: ICollectable); virtual; abstract;
        function TrueDelete(Index: Integer): ICollectable; virtual; abstract;
        procedure TrueInsert(Index: Integer; const Item: ICollectable); virtual; abstract;
        function TrueItemCount(const Item: ICollectable): Integer; override;
        function TrueRemove(const Item: ICollectable): ICollectable; override;
        function TrueRemoveAll(const Item: ICollectable): ICollection; override;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        function GetDuplicates: Boolean; override;
        procedure SetDuplicates(Value: Boolean); virtual;
        function GetItem(Index: Integer): ICollectable; virtual;
        procedure SetItem(Index: Integer; const Item: ICollectable); virtual;
        function GetIterator: IIterator; override;
        function GetNaturalItemIID: TGUID; override;
        function GetSorted: Boolean; virtual;
        procedure SetSorted(Value: Boolean); virtual;
        function GetType: TCollectionType; override;
        function CloneAsList: IList; virtual;
        function Delete(Index: Integer): ICollectable; virtual;
        procedure Exchange(Index1, Index2: Integer); virtual;
        function First: ICollectable; virtual;
        function IndexOf(const Item: ICollectable): Integer; virtual;
        function Insert(Index: Integer; const Item: ICollectable): Boolean; overload; virtual;
        function Insert(Index: Integer; const ItemArray: array of ICollectable): Integer; overload; virtual;
        function Insert(Index: Integer; const Collection: ICollection): Integer; overload; virtual;
        function IsNilAllowed: Boolean; override;
        function Last: ICollectable; virtual;
        function Search(const Item: ICollectable; const SearchComparator: IComparator = nil): TSearchResult; virtual;
        procedure Sort(const SortComparator: IComparator = nil); overload; virtual;
        procedure Sort(CompareFunc: TCollectionCompareFunc); overload; virtual;
        property Duplicates: Boolean read GetDuplicates write SetDuplicates;
        property Items[Index: Integer]: ICollectable read GetItem write SetItem; default;
        property Sorted: Boolean read GetSorted write SetSorted;
    end;

    TAbstractMap = class(TAbstractCollection, IMap)
    private
        FAssociationComparator: IAssociationComparator;
        FKeyComparator: IComparator;
        FNaturalKeysOnly: Boolean;
    protected
        function GetAssociationIterator: IMapIterator; virtual; abstract;
        function GetKeyPosition(const Key: ICollectable): TCollectionPosition; virtual; abstract;
        procedure InitFrom(const Collection: ICollection); override;
        function TrueAdd(const Item: ICollectable): Boolean; override;
        function TrueContains(const Item: ICollectable): Boolean; override;
        function TrueGet(Position: TCollectionPosition): IAssociation; virtual; abstract;
        function TruePut(Position: TCollectionPosition; const Association: IAssociation): IAssociation; virtual; abstract;
        function TrueRemove(const Item: ICollectable): ICollectable; override;
        function TrueRemove2(Position: TCollectionPosition): IAssociation; virtual; abstract;
        function TrueRemoveAll(const Item: ICollectable): ICollection; override;
        property AssociationComparator: IAssociationComparator read FAssociationComparator;
    public
        constructor Create; override;
        constructor Create(NaturalItemsOnly: Boolean); override;
        constructor Create(NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean); overload; virtual;
        constructor Create(const ItemArray: array of ICollectable); overload; override;
        constructor Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean); overload; override;
        constructor Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean); overload; virtual;
        constructor Create(const KeyArray, ItemArray: array of ICollectable); overload; virtual;
        constructor Create(const KeyArray, ItemArray: array of ICollectable; NaturalItemsOnly: Boolean); overload; virtual;
        constructor Create(const KeyArray, ItemArray: array of ICollectable; NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean); overload; virtual;
//      Don't use this parameter signature as it hits a compiler bug in D5.
//        constructor Create(const KeyArray, ItemArray: TCollectableArray; NaturalItemsOnly: Boolean = false; NaturalKeysOnly: Boolean = true); overload; virtual;
        constructor Create(const Map: IMap); overload; virtual;
        destructor Destroy; override;
        class function GetAlwaysNaturalKeys: Boolean; virtual;
        function GetItem(const Key: ICollectable): ICollectable; virtual;
        procedure SetItem(const Key, Item: ICollectable); virtual;
        function GetIterator: IIterator; override;
        function GetKeyComparator: IComparator; virtual;
        procedure SetKeyComparator(const Value: IComparator); virtual;
        function GetKeyIterator: IIterator; virtual;
        function GetKeys: ISet; virtual;
        function GetMapIterator: IMapIterator; virtual;
        function GetMapIteratorByKey(const Filter: IFilter): IMapIterator; overload; virtual;
        function GetMapIteratorByKey(FilterFunc: TCollectionFilterFunc): IMapIterator; overload; virtual;
        function GetNaturalItemIID: TGUID; override;
        function GetNaturalKeyIID: TGUID; virtual;
        function GetNaturalKeysOnly: Boolean; virtual;
        function GetType: TCollectionType; override;
        function GetValues: ICollection; virtual;
        function Clone: ICollection; override;
        function CloneAsMap: IMap; virtual;
        function ContainsKey(const Key: ICollectable): Boolean; overload; virtual;
        function ContainsKey(const KeyArray: array of ICollectable): Boolean; overload; virtual;
        function ContainsKey(const Collection: ICollection): Boolean; overload; virtual;
        function Get(const Key: ICollectable): ICollectable; virtual;
        function KeyAllowed(const Key: ICollectable): TCollectionError; virtual;
        function IsNaturalKey(const Key: ICollectable): Boolean; virtual;
        function IsNilAllowed: Boolean; override;
        function MatchingKey(const KeyArray: array of ICollectable): ICollection; overload; virtual;
        function MatchingKey(const Collection: ICollection): ICollection; overload; virtual;
        function Put(const Item: ICollectable): ICollectable; overload; virtual;
        function Put(const Key, Item: ICollectable): ICollectable; overload; virtual;
        function Put(const ItemArray: array of ICollectable): ICollection; overload; virtual;
        function Put(const Collection: ICollection): ICollection; overload; virtual;
        function Put(const Map: IMap): ICollection; overload; virtual;
        function RemoveKey(const Key: ICollectable): ICollectable; overload; virtual;
        function RemoveKey(const KeyArray: array of ICollectable): ICollection; overload; virtual;
        function RemoveKey(const Collection: ICollection): ICollection; overload; virtual;
        function RetainKey(const KeyArray: array of ICollectable): ICollection; overload; virtual;
        function RetainKey(const Collection: ICollection): ICollection; overload; virtual;
        property KeyComparator: IComparator read GetKeyComparator write SetKeyComparator;
        property Items[const Key: ICollectable]: ICollectable read GetItem write SetItem; default;
        property NaturalKeyIID: TGUID read GetNaturalKeyIID;
        property NaturalKeysOnly: Boolean read GetNaturalKeysOnly;
    end;

    TAbstractIntegerMap = class(TAbstractCollection, IIntegerMap)
    private
        FAssociationComparator: IIntegerAssociationComparator;
    protected
        function GetAssociationIterator: IIntegerMapIterator; virtual; abstract;
        function GetKeyPosition(const Key: Integer): TCollectionPosition; virtual; abstract;
        function TrueAdd(const Item: ICollectable): Boolean; override;
        function TrueContains(const Item: ICollectable): Boolean; override;
        function TrueGet(Position: TCollectionPosition): IIntegerAssociation; virtual; abstract;
        function TruePut(Position: TCollectionPosition; const Association: IIntegerAssociation): IIntegerAssociation; virtual; abstract;
        function TrueRemove(const Item: ICollectable): ICollectable; override;
        function TrueRemove2(Position: TCollectionPosition): IIntegerAssociation; virtual; abstract;
        function TrueRemoveAll(const Item: ICollectable): ICollection; override;
        property AssociationComparator: IIntegerAssociationComparator read FAssociationComparator;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        constructor Create(const ItemArray: array of ICollectable); overload; override;
        constructor Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean); overload; override;
        constructor Create(const KeyArray: array of Integer; const ItemArray: array of ICollectable); overload; virtual;
        constructor Create(const KeyArray: array of Integer; const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean); overload; virtual;
        constructor Create(const Map: IIntegerMap); overload; virtual;
        destructor Destroy; override;
        function GetItem(const Key: Integer): ICollectable; virtual;
        procedure SetItem(const Key: Integer; const Item: ICollectable); virtual;
        function GetIterator: IIterator; override;
        function GetKeys: ISet; virtual;
        function GetMapIterator: IIntegerMapIterator; virtual;
        function GetNaturalItemIID: TGUID; override;
        function GetType: TCollectionType; override;
        function GetValues: ICollection; virtual;
        function Clone: ICollection; override;
        function CloneAsIntegerMap: IIntegerMap; virtual;
        function ContainsKey(const Key: Integer): Boolean; overload; virtual;
        function ContainsKey(const KeyArray: array of Integer): Boolean; overload; virtual;
        function Get(const Key: Integer): ICollectable; virtual;
        function IsNilAllowed: Boolean; override;
        function Put(const Item: ICollectable): ICollectable; overload; virtual;
        function Put(const Key: Integer; const Item: ICollectable): ICollectable; overload; virtual;
        function Put(const ItemArray: array of ICollectable): ICollection; overload; virtual;
        function Put(const Collection: ICollection): ICollection; overload; virtual;
        function Put(const Map: IIntegerMap): ICollection; overload; virtual;
        function RemoveKey(const Key: Integer): ICollectable; overload; virtual;
        function RemoveKey(const KeyArray: array of Integer): ICollection; overload; virtual;
        function RetainKey(const KeyArray: array of Integer): ICollection; overload; virtual;
        property Items[const Key: Integer]: ICollectable read GetItem write SetItem; default;
    end;

    TAbstractStringMap = class(TAbstractCollection, IStringMap)
    private
        FAssociationComparator: IStringAssociationComparator;
    protected
        function GetAssociationIterator: IStringMapIterator; virtual; abstract;
        function GetKeyPosition(const Key: String): TCollectionPosition; virtual; abstract;
        function TrueAdd(const Item: ICollectable): Boolean; override;
        function TrueContains(const Item: ICollectable): Boolean; override;
        function TrueGet(Position: TCollectionPosition): IStringAssociation; virtual; abstract;
        function TruePut(Position: TCollectionPosition; const Association: IStringAssociation): IStringAssociation; virtual; abstract;
        function TrueRemove(const Item: ICollectable): ICollectable; override;
        function TrueRemove2(Position: TCollectionPosition): IStringAssociation; virtual; abstract;
        function TrueRemoveAll(const Item: ICollectable): ICollection; override;
        property AssociationComparator: IStringAssociationComparator read FAssociationComparator;
    public
        constructor Create(NaturalItemsOnly: Boolean); override;
        constructor Create(const ItemArray: array of ICollectable); overload; override;
        constructor Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean); overload; override;
        constructor Create(const KeyArray: array of String; const ItemArray: array of ICollectable); overload; virtual;
        constructor Create(const KeyArray: array of String; const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean); overload; virtual;
        constructor Create(const Map: IStringMap); overload; virtual;
        destructor Destroy; override;
        function GetItem(const Key: String): ICollectable; virtual;
        procedure SetItem(const Key: String; const Item: ICollectable); virtual;
        function GetIterator: IIterator; override;
        function GetKeys: ISet; virtual;
        function GetMapIterator: IStringMapIterator; virtual;
        function GetNaturalItemIID: TGUID; override;
        function GetType: TCollectionType; override;
        function GetValues: ICollection; virtual;
        function Clone: ICollection; override;
        function CloneAsStringMap: IStringMap; virtual;
        function ContainsKey(const Key: String): Boolean; overload; virtual;
        function ContainsKey(const KeyArray: array of String): Boolean; overload; virtual;
        function Get(const Key: String): ICollectable; virtual;
        function IsNilAllowed: Boolean; override;
        function Put(const Item: ICollectable): ICollectable; overload; virtual;
        function Put(const Key: String; const Item: ICollectable): ICollectable; overload; virtual;
        function Put(const ItemArray: array of ICollectable): ICollection; overload; virtual;
        function Put(const Collection: ICollection): ICollection; overload; virtual;
        function Put(const Map: IStringMap): ICollection; overload; virtual;
        function RemoveKey(const Key: String): ICollectable; overload; virtual;
        function RemoveKey(const KeyArray: array of String): ICollection; overload; virtual;
        function RetainKey(const KeyArray: array of String): ICollection; overload; virtual;
        property Items[const Key: String]: ICollectable read GetItem write SetItem; default;
    end;

    TAbstractCollectionClass = class of TAbstractCollection;
    TAbstractBagClass = class of TAbstractBag;
    TAbstractSetClass = class of TAbstractSet;
    TAbstractListClass = class of TAbstractList;
    TAbstractMapClass = class of TAbstractMap;
    TAbstractIntegerMapClass = class of TAbstractIntegerMap;
    TAbstractStringMapClass = class of TAbstractStringMap;

    TAbstractIterator = class(TInterfacedObject, IIterator)
    private
        FAllowRemoval: Boolean;
        FEOF: Boolean;
        FItem: ICollectable;
    protected
        constructor Create(AllowRemoval: Boolean = true);
        function TrueFirst: ICollectable; virtual; abstract;
        function TrueNext: ICollectable; virtual; abstract;
        procedure TrueRemove; virtual; abstract;
    public
        procedure AfterConstruction; override;
        function GetAllowRemoval: Boolean; virtual;
        function CurrentItem: ICollectable; virtual;
        function EOF: Boolean; virtual;
        function First: ICollectable; virtual;
        function Next: ICollectable; virtual;
        function Remove: Boolean; virtual;
        property AllowRemoval: Boolean read GetAllowRemoval;
    end;

    TAbstractListIterator = class(TAbstractIterator)
    private
        FCollection: TAbstractList;
        FIndex: Integer;
    protected
        constructor Create(Collection: TAbstractList);
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    end;

    TAbstractMapIterator = class(TAbstractIterator, IMapIterator)
    public
        function CurrentKey: ICollectable; virtual; abstract;
    end;

    TAbstractAssociationIterator = class(TInterfacedObject, IIterator, IMapIterator)
    private
        FAllowRemoval: Boolean;
        FEOF: Boolean;
        FAssociation: IAssociation;
    protected
        constructor Create(AllowRemoval: Boolean = true);
        function TrueFirst: IAssociation; virtual; abstract;
        function TrueNext: IAssociation; virtual; abstract;
        procedure TrueRemove; virtual; abstract;
    public
        procedure AfterConstruction; override;
        function GetAllowRemoval: Boolean; virtual;
        function CurrentKey: ICollectable; virtual;
        function CurrentItem: ICollectable; virtual;
        function EOF: Boolean; virtual;
        function First: ICollectable; virtual;
        function Next: ICollectable; virtual;
        function Remove: Boolean; virtual;
        property AllowRemoval: Boolean read GetAllowRemoval;
    end;

    TAbstractIntegerAssociationIterator = class(TInterfacedObject, IIterator, IIntegerMapIterator)
    private
        FAllowRemoval: Boolean;
        FEOF: Boolean;
        FAssociation: IIntegerAssociation;
    protected
        constructor Create(AllowRemoval: Boolean = true);
        function TrueFirst: IIntegerAssociation; virtual; abstract;
        function TrueNext: IIntegerAssociation; virtual; abstract;
        procedure TrueRemove; virtual; abstract;
    public
        procedure AfterConstruction; override;
        function GetAllowRemoval: Boolean; virtual;
        function CurrentKey: Integer; virtual;
        function CurrentItem: ICollectable; virtual;
        function EOF: Boolean; virtual;
        function First: ICollectable; virtual;
        function Next: ICollectable; virtual;
        function Remove: Boolean; virtual;
        property AllowRemoval: Boolean read GetAllowRemoval;
    end;

    TAbstractStringAssociationIterator = class(TInterfacedObject, IIterator, IStringMapIterator)
    private
        FAllowRemoval: Boolean;
        FEOF: Boolean;
        FAssociation: IStringAssociation;
    protected
        constructor Create(AllowRemoval: Boolean = true);
        function TrueFirst: IStringAssociation; virtual; abstract;
        function TrueNext: IStringAssociation; virtual; abstract;
        procedure TrueRemove; virtual; abstract;
    public
        procedure AfterConstruction; override;
        function GetAllowRemoval: Boolean; virtual;
        function CurrentKey: String; virtual;
        function CurrentItem: ICollectable; virtual;
        function EOF: Boolean; virtual;
        function First: ICollectable; virtual;
        function Next: ICollectable; virtual;
        function Remove: Boolean; virtual;
        property AllowRemoval: Boolean read GetAllowRemoval;
    end;

    TAssociationIterator = class(TAbstractIterator, IMapIterator)
    private
        FIterator: IIterator;
    protected
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    public
        constructor Create(const Iterator: IIterator);
        destructor Destroy; override;
        function CurrentItem: ICollectable; override;
        function CurrentKey: ICollectable; virtual;
    end;

    TAssociationKeyIterator = class(TAbstractIterator)
    private
        FIterator: IMapIterator;
    protected
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    public
        constructor Create(const Iterator: IMapIterator);
        destructor Destroy; override;
    end;

    TAbstractFilter = class(TInterfacedObject, IFilter)
    public
        function Accept(const Item: ICollectable): Boolean; virtual; abstract;
    end;

    TFilterIterator = class(TAbstractIterator)
    private
        FIterator: IIterator;
        FFilter: IFilter;
    protected
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    public
        constructor Create(const Iterator: IIterator; const Filter: IFilter; AllowRemoval: Boolean = true); virtual;
        destructor Destroy; override;
    end;

    TFilterFuncIterator = class(TAbstractIterator)
    private
        FIterator: IIterator;
        FFilterFunc: TCollectionFilterFunc;
    protected
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    public
        constructor Create(const Iterator: IIterator; FilterFunc: TCollectionFilterFunc; AllowRemoval: Boolean = true); virtual;
        destructor Destroy; override;
    end;

    TKeyFilterMapIterator = class(TAbstractMapIterator)
    private
        FIterator: IMapIterator;
        FFilter: IFilter;
    protected
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    public
        constructor Create(const Iterator: IMapIterator; const Filter: IFilter; AllowRemoval: Boolean = true); virtual;
        destructor Destroy; override;
        function CurrentKey: ICollectable; override;
    end;

    TKeyFilterFuncMapIterator = class(TAbstractMapIterator)
    private
        FIterator: IMapIterator;
        FFilterFunc: TCollectionFilterFunc;
    protected
        function TrueFirst: ICollectable; override;
        function TrueNext: ICollectable; override;
        procedure TrueRemove; override;
    public
        constructor Create(const Iterator: IMapIterator; FilterFunc: TCollectionFilterFunc; AllowRemoval: Boolean = true); virtual;
        destructor Destroy; override;
        function CurrentKey: ICollectable; override;
    end;


    ECollectionError = class(Exception)
    private
        FCollection: ICollection;
        FErrorType: TCollectionError;
    public
        constructor Create(const Msg: String; const Collection: ICollection; ErrorType: TCollectionError);
        property Collection: ICollection read FCollection;
        property ErrorType: TCollectionError read FErrorType;
    end;

implementation

uses
    Math,
    CollArray, CollHash, CollList, CollPArray, CollWrappers;

var
    FDefaultComparator: IComparator;
    FNaturalComparator: IComparator;
    FReverseNaturalComparator: IComparator;

{ TCollectionPosition }
constructor TCollectionPosition.Create(Found: Boolean);
begin
    FFound := Found;
end;

{ TAbstractComparator }
class function TAbstractComparator.GetDefaultComparator: IComparator;
begin
    if FDefaultComparator = nil then
        FDefaultComparator := TDefaultComparator.Create;
    Result := FDefaultComparator;
end;

class function TAbstractComparator.GetNaturalComparator: IComparator;
begin
    if FNaturalComparator = nil then
        FNaturalComparator := TNaturalComparator.Create;
    Result := FNaturalComparator;
end;

class function TAbstractComparator.GetReverseNaturalComparator: IComparator;
begin
    if FReverseNaturalComparator = nil then
        FReverseNaturalComparator := TReverseNaturalComparator.Create;
    Result := FReverseNaturalComparator;
end;

function TAbstractComparator.GetInstance: TObject;
begin
    Result := Self;
end;

function TAbstractComparator.Equals(const Comparator: IComparator): Boolean;
begin
    Result := (Self = Comparator.GetInstance); 
end;

{ TDefaultComparator }
constructor TDefaultComparator.Create;
begin
    // Empty
end;

function TDefaultComparator.Compare(const Item1, Item2: ICollectable): Integer;
var
    Value1, Value2: Integer;
begin
    if Item1 <> nil then
        Value1 := Integer(Pointer(Item1))
    else
        Value1 := Low(Integer);
    if Item2 <> nil then
        Value2 := Integer(Pointer(Item2))
    else
        Value2 := Low(Integer);
    if (Value1 < Value2) then
        Result := -1
    else if (Value1 > Value2) then
        Result := 1
    else
        Result := 0;
end;

function TDefaultComparator.Equals(const Item1, Item2: ICollectable): Boolean;
begin
    Result := (Item1 = Item2);
end;

{ TNaturalComparator }
constructor TNaturalComparator.Create;
begin
    // Empty
end;

function TNaturalComparator.Compare(const Item1, Item2: ICollectable): Integer;
begin
    if (Item1 = nil) and (Item2 <> nil) then
        Result := -1
    else if (Item1 <> nil) and (Item2 = nil) then
        Result := 1
    else if (Item1 = nil) and (Item2 = nil) then
        Result := 0
    else
        Result := (Item1 as IComparable).CompareTo(Item2);
end;

function TNaturalComparator.Equals(const Item1, Item2: ICollectable): Boolean;
begin
    if (Item1 = nil) or (Item2 = nil) then
        Result := (Item1 = Item2)
    else
    begin
        Result := (Item1 as IEquatable).Equals(Item2);
    end;
end;

{ TReverseNaturalComparator }
constructor TReverseNaturalComparator.Create;
begin
    // Empty
end;

function TReverseNaturalComparator.Compare(const Item1, Item2: ICollectable): Integer;
begin
    if (Item1 = nil) and (Item2 <> nil) then
        Result := 1
    else if (Item1 <> nil) and (Item2 = nil) then
        Result := -1
    else if (Item1 = nil) and (Item2 = nil) then
        Result := 0
    else
        Result := -(Item1 as IComparable).CompareTo(Item2);
end;

function TReverseNaturalComparator.Equals(const Item1, Item2: ICollectable): Boolean;
begin
    if (Item1 = nil) or (Item2 = nil) then
        Result := (Item1 = Item2)
    else
        Result := (Item1 as IEquatable).Equals(Item2);
end;

{ TAssociation }
constructor TAssociation.Create(const Key, Value: ICollectable);
begin
    FKey := Key;
    FValue := Value;
end;

destructor TAssociation.Destroy;
begin
    FKey := nil;
    FValue := nil;
    inherited Destroy;
end;

function TAssociation.GetInstance: TObject;
begin
    Result := Self;
end;

function TAssociation.GetKey: ICollectable;
begin
    Result := FKey;
end;

function TAssociation.GetValue: ICollectable;
begin
    Result := FValue;
end;


{ TIntegerAssociation }
constructor TIntegerAssociation.Create(const Key: Integer; const Value: ICollectable);
begin
    FKey := Key;
    FValue := Value;
end;

destructor TIntegerAssociation.Destroy;
begin
    FValue := nil;
    inherited Destroy;
end;

function TIntegerAssociation.GetInstance: TObject;
begin
    Result := Self;
end;

function TIntegerAssociation.GetKey: Integer;
begin
    Result := FKey;
end;

function TIntegerAssociation.GetValue: ICollectable;
begin
    Result := FValue;
end;


{ TStringAssociation }
constructor TStringAssociation.Create(const Key: String; const Value: ICollectable);
begin
    FKey := Key;
    FValue := Value;
end;

destructor TStringAssociation.Destroy;
begin
    FValue := nil;
    inherited Destroy;
end;

function TStringAssociation.GetInstance: TObject;
begin
    Result := Self;
end;

function TStringAssociation.GetKey: String;
begin
    Result := FKey;
end;

function TStringAssociation.GetValue: ICollectable;
begin
    Result := FValue;
end;


{ TAbstractIterator }
constructor TAbstractIterator.Create(AllowRemoval: Boolean);
begin
    inherited Create;
    FAllowRemoval := AllowRemoval;
    FEOF := true;
    FItem := nil;
end;

procedure TAbstractIterator.AfterConstruction;
begin
    inherited AfterConstruction;
    First;
end;

function TAbstractIterator.GetAllowRemoval: Boolean;
begin
    Result := FAllowRemoval;
end;

function TAbstractIterator.CurrentItem: ICollectable;
begin
    Result := FItem;
end;

function TAbstractIterator.EOF: Boolean;
begin
    Result := FEOF;
end;

function TAbstractIterator.First: ICollectable;
begin
    FEOF := false;
    FItem := TrueFirst;
    if FItem = nil then
        FEOF := true;
    Result := FItem;
end;

function TAbstractIterator.Next: ICollectable;
begin
    if not FEOF then
    begin
        FItem := TrueNext;
        if FItem = nil then
            FEOF := true;
    end;
    Result := FItem;
end;

function TAbstractIterator.Remove: Boolean;
begin
    if (FItem <> nil) and FAllowRemoval then
    begin
        TrueRemove;
        FItem := nil;
        Result := true;
    end
    else
        Result := false;
end;

{ TAbstractAssociationIterator }
constructor TAbstractAssociationIterator.Create(AllowRemoval: Boolean);
begin
    inherited Create;
    FAllowRemoval := AllowRemoval;
    FEOF := true;
    FAssociation := nil;
end;

procedure TAbstractAssociationIterator.AfterConstruction;
begin
    inherited AfterConstruction;
    First;
end;

function TAbstractAssociationIterator.GetAllowRemoval: Boolean;
begin
    Result := FAllowRemoval;
end;

function TAbstractAssociationIterator.CurrentKey: ICollectable;
begin
    if FAssociation <> nil then
        Result := FAssociation.GetKey
    else
        Result := nil;
end;

function TAbstractAssociationIterator.CurrentItem: ICollectable;
begin
    if FAssociation <> nil then
        Result := FAssociation.GetValue
    else
        Result := nil;
end;

function TAbstractAssociationIterator.EOF: Boolean;
begin
    Result := FEOF;
end;

function TAbstractAssociationIterator.First: ICollectable;
begin
    FAssociation := TrueFirst;
    if FAssociation <> nil then
    begin
        Result := FAssociation.GetValue;
        FEOF := false;
    end
    else
    begin
        Result := nil;
        FEOF := true;
    end;
end;

function TAbstractAssociationIterator.Next: ICollectable;
begin
    if not FEOF then
    begin
        FAssociation := TrueNext;
        if FAssociation <> nil then
            Result := FAssociation.GetValue
        else
        begin
            Result := nil;
            FEOF := true;
        end;
    end;
end;

function TAbstractAssociationIterator.Remove: Boolean;
begin
    if (FAssociation <> nil) and FAllowRemoval then
    begin
        TrueRemove;
        FAssociation := nil;
        Result := true;
    end
    else
        Result := false;
end;

{ TAbstractIntegerAssociationIterator }
constructor TAbstractIntegerAssociationIterator.Create(AllowRemoval: Boolean);
begin
    inherited Create;
    FAllowRemoval := AllowRemoval;
    FEOF := true;
    FAssociation := nil;
end;

procedure TAbstractIntegerAssociationIterator.AfterConstruction;
begin
    inherited AfterConstruction;
    First;
end;

function TAbstractIntegerAssociationIterator.GetAllowRemoval: Boolean;
begin
    Result := FAllowRemoval;
end;

function TAbstractIntegerAssociationIterator.CurrentKey: Integer;
begin
    if FAssociation <> nil then
        Result := FAssociation.GetKey
    else
        Result := 0;
end;

function TAbstractIntegerAssociationIterator.CurrentItem: ICollectable;
begin
    if FAssociation <> nil then
        Result := FAssociation.GetValue
    else
        Result := nil;
end;

function TAbstractIntegerAssociationIterator.EOF: Boolean;
begin
    Result := FEOF;
end;

function TAbstractIntegerAssociationIterator.First: ICollectable;
begin
    FAssociation := TrueFirst;
    if FAssociation <> nil then
    begin
        Result := FAssociation.GetValue;
        FEOF := false;
    end
    else
    begin
        Result := nil;
        FEOF := true;
    end;
end;

function TAbstractIntegerAssociationIterator.Next: ICollectable;
begin
    if not FEOF then
    begin
        FAssociation := TrueNext;
        if FAssociation <> nil then
            Result := FAssociation.GetValue
        else
        begin
            Result := nil;
            FEOF := true;
        end;
    end;
end;

function TAbstractIntegerAssociationIterator.Remove: Boolean;
begin
    if (FAssociation <> nil) and FAllowRemoval then
    begin
        TrueRemove;
        FAssociation := nil;
        Result := true;
    end
    else
        Result := false;
end;

{ TAbstractStringAssociationIterator }
constructor TAbstractStringAssociationIterator.Create(AllowRemoval: Boolean);
begin
    inherited Create;
    FAllowRemoval := AllowRemoval;
    FEOF := true;
    FAssociation := nil;
end;

procedure TAbstractStringAssociationIterator.AfterConstruction;
begin
    inherited AfterConstruction;
    First;
end;

function TAbstractStringAssociationIterator.GetAllowRemoval: Boolean;
begin
    Result := FAllowRemoval;
end;

function TAbstractStringAssociationIterator.CurrentKey: String;
begin
    if FAssociation <> nil then
        Result := FAssociation.GetKey
    else
        Result := '';
end;

function TAbstractStringAssociationIterator.CurrentItem: ICollectable;
begin
    if FAssociation <> nil then
        Result := FAssociation.GetValue
    else
        Result := nil;
end;

function TAbstractStringAssociationIterator.EOF: Boolean;
begin
    Result := FEOF;
end;

function TAbstractStringAssociationIterator.First: ICollectable;
begin
    FAssociation := TrueFirst;
    if FAssociation <> nil then
    begin
        Result := FAssociation.GetValue;
        FEOF := false;
    end
    else
    begin
        Result := nil;
        FEOF := true;
    end;
end;

function TAbstractStringAssociationIterator.Next: ICollectable;
begin
    if not FEOF then
    begin
        FAssociation := TrueNext;
        if FAssociation <> nil then
            Result := FAssociation.GetValue
        else
        begin
            Result := nil;
            FEOF := true;
        end;
    end;
end;

function TAbstractStringAssociationIterator.Remove: Boolean;
begin
    if (FAssociation <> nil) and FAllowRemoval then
    begin
        TrueRemove;
        FAssociation := nil;
        Result := true;
    end
    else
        Result := false;
end;

{ TAssociationIterator }
constructor TAssociationIterator.Create(const Iterator: IIterator);
begin
    inherited Create(Iterator.GetAllowRemoval);
    FIterator := Iterator;
end;

destructor TAssociationIterator.Destroy;
begin
    FIterator := nil;
    inherited Destroy;
end;

function TAssociationIterator.TrueFirst: ICollectable;
var
    Association: IAssociation;
begin
    Association := FIterator.First as IAssociation;
    if Association <> nil then
        Result := Association.GetValue
    else
        Result := nil;
end;

function TAssociationIterator.TrueNext: ICollectable;
var
    Association: IAssociation;
begin
    Association := (FIterator.Next as IAssociation);
    if Association <> nil then
        Result := Association.GetValue
    else
        Result := nil;
end;

procedure TAssociationIterator.TrueRemove;
begin
    FIterator.Remove;
end;

function TAssociationIterator.CurrentItem: ICollectable;
var
    Association: IAssociation;
begin
    Association := FIterator.CurrentItem as IAssociation;
    if Association <> nil then
        Result := Association.GetValue
    else
        Result := nil;
end;

function TAssociationIterator.CurrentKey: ICollectable;
var
    Association: IAssociation;
begin
    Association := FIterator.CurrentItem  as IAssociation;
    if Association <> nil then
        Result := Association.GetKey
    else
        Result := nil;
end;

{ TAssociationComparator }
constructor TAssociationComparator.Create(NaturalKeys: Boolean);
begin
    inherited Create;
    if NaturalKeys then
        FKeyComparator := TAbstractComparator.GetNaturalComparator
    else
        FKeyComparator := TAbstractComparator.GetDefaultComparator;
end;

destructor TAssociationComparator.Destroy;
begin
    FKeyComparator := nil;
    inherited Destroy;
end;

function TAssociationComparator.GetKeyComparator: IComparator;
begin
    Result := FKeyComparator;
end;

procedure TAssociationComparator.SetKeyComparator(Value: IComparator);
begin
    FKeyComparator := Value;
end;

function TAssociationComparator.Compare(const Item1, Item2: ICollectable): Integer;
begin
    Result := KeyComparator.Compare((Item1 as IAssociation).GetKey, (Item2 as IAssociation).GetKey);
end;

function TAssociationComparator.Equals(const Item1, Item2: ICollectable): Boolean;
begin
    Result := KeyComparator.Equals((Item1 as IAssociation).GetKey, (Item2 as IAssociation).GetKey);
end;

{ TIntegerAssociationComparator }
constructor TIntegerAssociationComparator.Create;
begin
    inherited Create;
end;

destructor TIntegerAssociationComparator.Destroy;
begin
    inherited Destroy;
end;

function TIntegerAssociationComparator.Compare(const Item1, Item2: ICollectable): Integer;
var
    Key1, Key2: Integer;
begin
    Key1 := (Item1 as IIntegerAssociation).GetKey;
    Key2 := (Item2 as IIntegerAssociation).GetKey;
    if Key1 < Key2 then
        Result := -1
    else if Key1 > Key2 then
        Result := 1
    else
        Result := 0;
end;

function TIntegerAssociationComparator.Equals(const Item1, Item2: ICollectable): Boolean;
begin
    Result := ((Item1 as IIntegerAssociation).GetKey = (Item2 as IIntegerAssociation).GetKey);
end;

{ TStringAssociationComparator }
constructor TStringAssociationComparator.Create;
begin
    inherited Create;
end;

destructor TStringAssociationComparator.Destroy;
begin
    inherited Destroy;
end;

function TStringAssociationComparator.Compare(const Item1, Item2: ICollectable): Integer;
var
    Key1, Key2: String;
begin
    Key1 := (Item1 as IStringAssociation).GetKey;
    Key2 := (Item2 as IStringAssociation).GetKey;
    if Key1 < Key2 then
        Result := -1
    else if Key1 > Key2 then
        Result := 1
    else
        Result := 0;
end;

function TStringAssociationComparator.Equals(const Item1, Item2: ICollectable): Boolean;
begin
    Result := ((Item1 as IStringAssociation).GetKey = (Item2 as IStringAssociation).GetKey);
end;

{ TAssociationKeyIterator }
constructor TAssociationKeyIterator.Create(const Iterator: IMapIterator);
begin
    inherited Create(Iterator.GetAllowRemoval);
    FIterator := Iterator;
end;

destructor TAssociationKeyIterator.Destroy;
begin
    FIterator := nil;
    inherited Destroy;
end;

function TAssociationKeyIterator.TrueFirst: ICollectable;
begin
    FIterator.First;
    Result := FIterator.CurrentKey;
end;

function TAssociationKeyIterator.TrueNext: ICollectable;
begin
    FIterator.Next;
    Result := FIterator.CurrentKey;
end;

procedure TAssociationKeyIterator.TrueRemove;
begin
    FIterator.Remove;
end;

{ TFilterIterator }
constructor TFilterIterator.Create(const Iterator: IIterator; const Filter: IFilter; AllowRemoval: Boolean = true);
begin
    FIterator := Iterator;
    FFilter := Filter;
end;

destructor TFilterIterator.Destroy;
begin
    FIterator := nil;
    FFilter := nil;
end;

function TFilterIterator.TrueFirst: ICollectable;
var
    Item: ICollectable;
begin
    Item := FIterator.First;
    while not FIterator.EOF do
    begin
        if FFilter.Accept(Item) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

function TFilterIterator.TrueNext: ICollectable;
var
    Item: ICollectable;
begin
    Item := FIterator.Next;
    while not FIterator.EOF do
    begin
        if FFilter.Accept(Item) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

procedure TFilterIterator.TrueRemove;
begin
    FIterator.Remove;
end;

{ TFilterFuncIterator }
constructor TFilterFuncIterator.Create(const Iterator: IIterator; FilterFunc: TCollectionFilterFunc; AllowRemoval: Boolean = true);
begin
    FIterator := Iterator;
    FFilterFunc := FilterFunc;
end;

destructor TFilterFuncIterator.Destroy;
begin
    FIterator := nil;
    FFilterFunc := nil;
end;

function TFilterFuncIterator.TrueFirst: ICollectable;
var
    Item: ICollectable;
begin
    Item := FIterator.First;
    while not FIterator.EOF do
    begin
        if FFilterFunc(Item) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

function TFilterFuncIterator.TrueNext: ICollectable;
var
    Item: ICollectable;
begin
    Item := FIterator.Next;
    while not FIterator.EOF do
    begin
        if FFilterFunc(Item) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

procedure TFilterFuncIterator.TrueRemove;
begin
    FIterator.Remove;
end;

{ TKeyFilterMapIterator }
constructor TKeyFilterMapIterator.Create(const Iterator: IMapIterator; const Filter: IFilter; AllowRemoval: Boolean = true);
begin
    FIterator := Iterator;
    FFilter := Filter;
end;

destructor TKeyFilterMapIterator.Destroy;
begin
    FIterator := nil;
    FFilter := nil;
end;

function TKeyFilterMapIterator.TrueFirst: ICollectable;
var
    Key, Item: ICollectable;
begin
    Item := FIterator.First;
    while not FIterator.EOF do
    begin
        Key := FIterator.CurrentKey;
        if FFilter.Accept(Key) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

function TKeyFilterMapIterator.TrueNext: ICollectable;
var
    Key, Item: ICollectable;
begin
    Item := FIterator.Next;
    while not FIterator.EOF do
    begin
        Key := FIterator.CurrentKey;
        if FFilter.Accept(Key) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

procedure TKeyFilterMapIterator.TrueRemove;
begin
    FIterator.Remove;
end;

function TKeyFilterMapIterator.CurrentKey: ICollectable;
begin
    Result := FIterator.CurrentKey;
end;

{ TKeyFilterFuncMapIterator }
constructor TKeyFilterFuncMapIterator.Create(const Iterator: IMapIterator; FilterFunc: TCollectionFilterFunc; AllowRemoval: Boolean = true);
begin
    FIterator := Iterator;
    FFilterFunc := FilterFunc;
end;

destructor TKeyFilterFuncMapIterator.Destroy;
begin
    FIterator := nil;
    FFilterFunc := nil;
end;

function TKeyFilterFuncMapIterator.TrueFirst: ICollectable;
var
    Key, Item: ICollectable;
begin
    Item := FIterator.First;
    while not FIterator.EOF do
    begin
        Key := FIterator.CurrentKey;
        if FFilterFunc(Key) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

function TKeyFilterFuncMapIterator.TrueNext: ICollectable;
var
    Key, Item: ICollectable;
begin
    Item := FIterator.Next;
    while not FIterator.EOF do
    begin
        Key := FIterator.CurrentKey;
        if FFilterFunc(Key) then
            break
        else
            Item := FIterator.Next;
    end;
    Result := Item;
end;

procedure TKeyFilterFuncMapIterator.TrueRemove;
begin
    FIterator.Remove;
end;

function TKeyFilterFuncMapIterator.CurrentKey: ICollectable;
begin
    Result := FIterator.CurrentKey;
end;


{ TAbstractCollection }
constructor TAbstractCollection.Create;
begin
    Create(false);
end;

constructor TAbstractCollection.Create(NaturalItemsOnly: Boolean);
begin
    FCreated := false;
    inherited Create;
    FNaturalItemsOnly := NaturalItemsOnly or GetAlwaysNaturalItems;
    if FNaturalItemsOnly then
        FComparator := TAbstractComparator.GetNaturalComparator
    else
        FComparator := TAbstractComparator.GetDefaultComparator;
    FIgnoreErrors := [ceDuplicate];
end;

constructor TAbstractCollection.Create(const ItemArray: array of ICollectable);
begin
    Create(ItemArray, false);
end;

// Fixed size collections must override this.
constructor TAbstractCollection.Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
var
    I: Integer;
begin
    Create(NaturalItemsOnly);
    if not FixedSize then
    begin
        Capacity := Length(ItemArray);
        for I := Low(ItemArray) to High(ItemArray) do
        begin
            Add(ItemArray[I]);
        end;
    end;
end;

// Fixed size collections must override this.
constructor TAbstractCollection.Create(const Collection: ICollection);
var
    Iterator: IIterator;
begin
    Create(Collection.GetNaturalItemsOnly);
    InitFrom(Collection);
    if not FixedSize then
    begin
        Capacity := Collection.GetSize;
        Iterator := Collection.GetIterator;
        while not Iterator.EOF do
        begin
            Add(Iterator.CurrentItem);
            Iterator.Next;
        end;
    end;
end;

destructor TAbstractCollection.Destroy;
begin
    FCreated := false;
    FComparator := nil;
    inherited Destroy;
end;

procedure TAbstractCollection.CollectionError(ErrorType: TCollectionError);
var
    Msg: String;
begin
    if not (ErrorType in FIgnoreErrors) then
    begin
        case ErrorType of
        ceDuplicate: Msg := 'Collection does not allow duplicates.';
        ceDuplicateKey: Msg := 'Collection does not allow duplicate keys.';
        ceFixedSize: Msg := 'Collection has fixed size.';
        ceNilNotAllowed: Msg := 'Collection does not allow nil.';
        ceNotNaturalItem: Msg := 'Collection only accepts natural items.';
        ceOutOfRange: Msg := 'Index out of collection range.';
        end;
        // If exception is thrown during construction, collection cannot be
        // passed to it as destructor is automatically called and this leaves an
        // interface reference to a destroyed object and crashes.
        if FCreated then
            raise ECollectionError.Create(Msg, Self, ErrorType)
        else
            raise ECollectionError.Create(Msg, nil, ErrorType);
    end;
end;

procedure TAbstractCollection.InitFrom(const Collection: ICollection);
begin
    Comparator := Collection.GetComparator;
    IgnoreErrors := Collection.GetIgnoreErrors;
end;

// Implementations should override this if possible
function TAbstractCollection.TrueItemCount(const Item: ICollectable): Integer;
var
    Iterator: IIterator;
    Total: Integer;
begin
    Total := 0;
    Iterator := GetIterator;
    while not Iterator.EOF do
    begin
        if FComparator.Equals(Item, Iterator.CurrentItem) then
            Inc(Total);
        Iterator.Next;
    end;
    Result := Total;
end;

class function TAbstractCollection.GetAlwaysNaturalItems: Boolean;
begin
    Result := false;
end;

function TAbstractCollection.GetAsArray: TCollectableArray;
var
    Iterator: IIterator;
    Working: TCollectableArray;
    I: Integer;
begin
    SetLength(Working, Size);
    I := 0;
    Iterator := GetIterator;
    while not Iterator.EOF do
    begin
        Working[I] := Iterator.CurrentItem;
        Inc(I);
        Iterator.Next;
    end;
    Result := Working;
end;

function TAbstractCollection.GetComparator: IComparator;
begin
    Result := FComparator;
end;

function TAbstractCollection.GetDuplicates: Boolean;
begin
    Result := true; // Sets and lists override this.
end;

procedure TAbstractCollection.SetComparator(const Value: IComparator);
begin
    if Value = nil then
    begin
        if NaturalItemsOnly then
            FComparator := TAbstractComparator.GetNaturalComparator
        else
            FComparator := TAbstractComparator.GetDefaultComparator;
    end
    else
        FComparator := Value;
end;

function TAbstractCollection.GetFixedSize: Boolean;
begin
    Result := false;
end;

function TAbstractCollection.GetIgnoreErrors: TCollectionErrors;
begin
    Result := FIgnoreErrors;
end;

procedure TAbstractCollection.SetIgnoreErrors(Value: TCollectionErrors);
begin
    FIgnoreErrors := Value;
end;

function TAbstractCollection.GetInstance: TObject;
begin
    Result := Self;
end;

function TAbstractCollection.GetIterator(const Filter: IFilter): IIterator;
var
    Iterator: IIterator;
begin
    Iterator := GetIterator;
    Result := TFilterIterator.Create(Iterator, Filter, Iterator.GetAllowRemoval);
end;

function TAbstractCollection.GetIterator(FilterFunc: TCollectionFilterFunc): IIterator;
var
    Iterator: IIterator;
begin
    Iterator := GetIterator;
    Result := TFilterFuncIterator.Create(Iterator, FilterFunc, Iterator.GetAllowRemoval);
end;

function TAbstractCollection.GetNaturalItemsOnly: Boolean;
begin
    Result := FNaturalItemsOnly;
end;

function TAbstractCollection.Add(const Item: ICollectable): Boolean;
var
    ItemError: TCollectionError;
    Success: Boolean;
begin
    ItemError := ItemAllowed(Item); // Can be natural items only error or nil not allowed error
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Success := false;
    end
    else if FixedSize then
    begin
        CollectionError(ceFixedSize);
        Success := false;
    end
    else
    begin
        Success := TrueAdd(Item);
    end;
    Result := Success;
end;

function TAbstractCollection.Add(const ItemArray: array of ICollectable): Integer;
var
    Item: ICollectable;
    ItemError: TCollectionError;
    I, Count: Integer;
    Success: Boolean;
begin
    Count := 0;
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
    end
    else
    begin
        for I := Low(ItemArray) to High(ItemArray) do
        begin
            Item := ItemArray[I];
            ItemError := ItemAllowed(Item);
            if ItemError <> ceOK then
            begin
                CollectionError(ItemError);
                Success := false;
            end
            else
            begin
                Success := TrueAdd(Item);
            end;
            if Success then
                Inc(Count);
        end;
    end;
    Result := Count;
end;

function TAbstractCollection.Add(const Collection: ICollection): Integer;
var
    Iterator: IIterator;
    Item: ICollectable;
    ItemError: TCollectionError;
    Count: Integer;
    Success: Boolean;
begin
    Count := 0;
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;
        ItemError := ItemAllowed(Item);
        if ItemError <> ceOK then
        begin
            CollectionError(ItemError);
            Success := false;
        end
        else if FixedSize then
        begin
            CollectionError(ceFixedSize);
            Success := false;
        end
        else
        begin
            Success := TrueAdd(Item);
        end;
        if Success then
            Inc(Count);
        Iterator.Next;
    end;
    Result := Count;
end;

procedure TAbstractCollection.AfterConstruction;
begin
    inherited AfterConstruction;
    FCreated := true;
end;

procedure TAbstractCollection.BeforeDestruction;
begin
    if not FixedSize then
        TrueClear;
    inherited BeforeDestruction;
end;

function TAbstractCollection.Clear: Integer;
begin
    if not FixedSize then
    begin
        Result := Size;
        TrueClear;
    end
    else
    begin
        CollectionError(ceFixedSize);
        Result := 0;
    end;
end;

function TAbstractCollection.Clone: ICollection;
begin
    Result := (TAbstractCollectionClass(ClassType)).Create(Self);
end;

function TAbstractCollection.Contains(const Item: ICollectable): Boolean;
var
    ItemError: TCollectionError;
    Success: Boolean;
begin
    ItemError := ItemAllowed(Item);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Success := false;
    end
    else
    begin
        Success := TrueContains(Item);
    end;
    Result := Success;
end;

function TAbstractCollection.Contains(const ItemArray: array of ICollectable): Boolean;
var
    I: Integer;
    Success: Boolean;
begin
    Success := true;
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        Success := Success and Contains(ItemArray[I]);
        if not Success then
            break;
    end;
    Result := Success;
end;

function TAbstractCollection.Contains(const Collection: ICollection): Boolean;
var
    Iterator: IIterator;
    Success: Boolean;
begin
    Success := true;
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Success := Success and Contains(Iterator.CurrentItem);
        if not Success then
            break;
        Iterator.Next;
    end;
    Result := Success;
end;

function TAbstractCollection.Equals(const Collection: ICollection): Boolean;
var
    Iterator: IIterator;
    Success: Boolean;
begin
    if Collection.GetType <> GetType then
        Result := false
    else if Collection.Size <> Size then
        Result := false
    else if not Collection.Comparator.Equals(Comparator) then
        Result := false
    else if not Collection.GetDuplicates and not GetDuplicates then
    begin
        // Not equal if any item not found in parameter collection
        Success := true;
        Iterator := GetIterator;
        while not Iterator.EOF and Success do
        begin
            Success := Collection.Contains(Iterator.CurrentItem);
            Iterator.Next;
        end;
        Result := Success;
    end
    else
    begin
        // Not equal if any item count not equal to item count in parameter collection
        Success := true;
        Iterator := GetIterator;
        while not Iterator.EOF and Success do
        begin
            Success := (ItemCount(Iterator.CurrentItem) = Collection.ItemCount(Iterator.CurrentItem));
            Iterator.Next;
        end;
        Result := Success;
    end;
end;

function TAbstractCollection.Find(const Filter: IFilter): ICollectable;
begin
    Result := GetIterator(Filter).First;
end;

function TAbstractCollection.Find(FilterFunc: TCollectionFilterFunc): ICollectable;
begin
    Result := GetIterator(FilterFunc).First;
end;

function TAbstractCollection.FindAll(const Filter: IFilter): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Self.GetIterator(Filter);
    while not Iterator.EOF do
    begin
        ResultCollection.Add(Iterator.CurrentItem);
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.FindAll(FilterFunc: TCollectionFilterFunc): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Self.GetIterator(FilterFunc);
    while not Iterator.EOF do
    begin
        ResultCollection.Add(Iterator.CurrentItem);
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.IsEmpty: Boolean;
begin
    Result := (Size = 0);
end;

function TAbstractCollection.IsNaturalItem(const Item: ICollectable): Boolean;
var
    Temp: IUnknown;
begin
    if Item <> nil then
        Result := (Item.QueryInterface(NaturalItemIID, Temp) <> E_NOINTERFACE)
    else
        Result := false;
end;

function TAbstractCollection.ItemAllowed(const Item: ICollectable): TCollectionError;
begin
    if NaturalItemsOnly and not IsNaturalItem(Item) then
        Result := ceNotNaturalItem
    else if not IsNilAllowed and (Item = nil) then
        Result := ceNilNotAllowed
    else
        Result := ceOK;
end;

function TAbstractCollection.ItemCount(const Item: ICollectable): Integer;
var
    ItemError: TCollectionError;
begin
    ItemError := ItemAllowed(Item);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Result := 0;
    end
    else if GetDuplicates then
    begin
        Result := TrueItemCount(Item);
    end
    else
    begin
        // Where duplicates are not allowed, TrueContains will be faster than TrueItemCount.
        if TrueContains(Item) then
            Result := 1
        else
            Result := 0;
    end;
end;

function TAbstractCollection.ItemCount(const ItemArray: array of ICollectable): Integer;
var
    I: Integer;
    Total: Integer;
begin
    Total := 0;
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        Total := Total + ItemCount(ItemArray[I]);
    end;
    Result := Total;
end;

function TAbstractCollection.ItemCount(const Collection: ICollection): Integer;
var
    Iterator: IIterator;
    Total: Integer;
begin
    Total := 0;
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Total := Total + ItemCount(Iterator.CurrentItem);
        Iterator.Next;
    end;
    Result := Total;
end;

function TAbstractCollection.Matching(const ItemArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        if Contains(ItemArray[I]) then
            ResultCollection.Add(ItemArray[I]);
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.Matching(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        if Contains(Iterator.CurrentItem) then
            ResultCollection.Add(Iterator.CurrentItem);
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.Remove(const Item: ICollectable): ICollectable;
var
    ItemError: TCollectionError;
begin
    ItemError := ItemAllowed(Item);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Result := nil;
    end
    else if FixedSize then
    begin
        CollectionError(ceFixedSize);
        Result := nil;
    end
    else
    begin
        Result := TrueRemove(Item);
    end;
end;

function TAbstractCollection.Remove(const ItemArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        ResultCollection.Add(Remove(ItemArray[I]));
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.Remove(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        ResultCollection.Add(Remove(Iterator.CurrentItem));
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.RemoveAll(const Item: ICollectable): ICollection;
var
    ItemError: TCollectionError;
begin
    ItemError := ItemAllowed(Item);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Result := nil;
    end
    else if FixedSize then
    begin
        CollectionError(ceFixedSize);
        Result := nil;
    end
    else
    begin
        Result := TrueRemoveAll(Item);
    end;
end;

function TAbstractCollection.RemoveAll(const ItemArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        ResultCollection.Add(RemoveAll(ItemArray[I]));
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.RemoveAll(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        ResultCollection.Add(RemoveAll(Iterator.CurrentItem));
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.Retain(const ItemArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
    Item: ICollectable;
    I: Integer;
    Found, Success: Boolean;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := GetIterator;
    while not Iterator.EOF do
    begin
        // Converting the array to a map would be faster but I don't want to
        // couple base class code to a complex collection.
        Found := false;
        for I := Low(ItemArray) to High(ItemArray) do
        begin
            Item := Iterator.CurrentItem;
            Found := Comparator.Equals(Item, ItemArray[I]);
            if Found then
                break;
        end;
        if not Found then
        begin
            Success := Iterator.Remove;
            if Success then
                ResultCollection.Add(Item);
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractCollection.Retain(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
    Item: ICollectable;
    Success: Boolean;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;
        if not Collection.Contains(Item) then
        begin
            Success := Iterator.Remove;
            if Success then
                ResultCollection.Add(Item);
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

{ TAbstractBag }
function TAbstractBag.CloneAsBag: IBag;
begin
    Result := (TAbstractBagClass(ClassType)).Create(Self);
end;

function TAbstractBag.GetNaturalItemIID: TGUID;
begin
    Result := EquatableIID;
end;

function TAbstractBag.GetType: TCollectionType;
begin
    Result := ctBag;
end;

function TAbstractBag.IsNilAllowed: Boolean;
begin
    Result := true;
end;

{ TAbstractSet }
function TAbstractSet.TrueAdd(const Item: ICollectable): Boolean;
var
    Position: TCollectionPosition;
begin
    // Adds if not already present otherwise fails
    Position := GetPosition(Item);
    try
        if Position.Found then
        begin
            CollectionError(ceDuplicate);
            Result := false;
        end
        else
        begin
            TrueAdd2(Position, Item);
            Result := true;
        end;
    finally
        Position.Free;
    end;
end;

function TAbstractSet.TrueContains(const Item: ICollectable): Boolean;
var
    Position: TCollectionPosition;
begin
    Position := GetPosition(Item);
    try
        Result := Position.Found;
    finally
        Position.Free;
    end;
end;

function TAbstractSet.TrueRemove(const Item: ICollectable): ICollectable;
var
    Position: TCollectionPosition;
begin
    Position := GetPosition(Item);
    try
        if Position.Found then
        begin
            Result := TrueGet(Position);
            TrueRemove2(Position);
        end
        else
            Result := nil;
    finally
        Position.Free;
    end;
end;

function TAbstractSet.TrueRemoveAll(const Item: ICollectable): ICollection;
var
    ResultCollection: ICollection;
    RemovedItem: ICollectable;
begin
    ResultCollection := TPArrayBag.Create;
    RemovedItem := TrueRemove(Item);
    if RemovedItem <> nil then
        ResultCollection.Add(RemovedItem);
    Result := ResultCollection;
end;

function TAbstractSet.GetDuplicates: Boolean;
begin
    Result := false;
end;

function TAbstractSet.GetNaturalItemIID: TGUID;
begin
    Result := EquatableIID;
end;

function TAbstractSet.GetType: TCollectionType;
begin
    Result := ctSet;
end;

function TAbstractSet.CloneAsSet: ISet;
begin
    Result := (TAbstractSetClass(ClassType)).Create(Self);
end;

function TAbstractSet.Complement(const Universe: ISet): ISet;
var
    ResultSet: ISet;
    Iterator: IIterator;
    Item: ICollectable;
begin
    // Return items in universe not found in self.
    ResultSet := TAbstractSetClass(ClassType).Create(NaturalItemsOnly);
    Iterator := Universe.GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;
        if not Contains(Item) then
            ResultSet.Add(Item);
        Iterator.Next;
    end;
    Result := ResultSet;
end;

function TAbstractSet.Intersect(const Set2: ISet): ISet;
var
    ResultSet: ISet;
    Iterator: IIterator;
    Item: ICollectable;
begin
    // Return items found in self and parameter.
    ResultSet := TAbstractSetClass(ClassType).Create(NaturalItemsOnly);
    Iterator := GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;
        if Contains(Item) and Set2.Contains(Item) then
            ResultSet.Add(Iterator.CurrentItem);
        Iterator.Next;
    end;
    Result := ResultSet;
end;

function TAbstractSet.IsNilAllowed: Boolean;
begin
    Result := false;
end;

function TAbstractSet.Union(const Set2: ISet): ISet;
var
    ResultSet: ISet;
    Iterator: IIterator;
    Item: ICollectable;
begin
    // Return items found in self or parameter.
    ResultSet := CloneAsSet;
    Iterator := Set2.GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;
        if not Contains(Item) and Set2.Contains(Item) then
            ResultSet.Add(Iterator.CurrentItem);
        Iterator.Next;
    end;
    Result := ResultSet;
end;

{ TAbstractList }
constructor TAbstractList.Create(NaturalItemsOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FDuplicates := true;
    FSorted := false;
end;

procedure TAbstractList.InitFrom(const Collection: ICollection);
var
    List: IList;
begin
    inherited InitFrom(Collection);
    if Collection.QueryInterface(IList, List) = S_OK then
    begin
        FDuplicates := List.GetDuplicates;
        FSorted := List.GetSorted;
    end;
end;

function TAbstractList.TrueAdd(const Item: ICollectable): Boolean;
var
    SearchResult: TSearchResult;
begin
    Result := True;
    if Sorted then
    begin
        // Insert in appropriate place to maintain sort order, unless duplicate
        // not allowed.
        SearchResult := BinarySearch(Item);
        case SearchResult.ResultType of
        srBeforeIndex: TrueInsert(SearchResult.Index, Item);
        srFoundAtIndex: begin
            if Duplicates then
                TrueInsert(SearchResult.Index, Item)
            else
            begin
                CollectionError(ceDuplicate);
                Result := false;
            end;
        end;
        srAfterEnd: TrueAppend(Item);
        end;
    end
    else
    begin
        // Add to end, unless duplicate not allowed.
        if not Duplicates and (SequentialSearch(Item, Comparator).ResultType = srFoundAtIndex) then
        begin
            CollectionError(ceDuplicate);
            Result := false;
        end
        else
            TrueAppend(Item);
    end;
end;

function TAbstractList.TrueContains(const Item: ICollectable): Boolean;
begin
    if Sorted then
        Result := BinarySearch(Item).ResultType = srFoundAtIndex
    else
        Result := SequentialSearch(Item, Comparator).ResultType = srFoundAtIndex
end;

function TAbstractList.TrueItemCount(const Item: ICollectable): Integer;
var
    SearchResult: TSearchResult;
    Count: Integer;
begin
    if Sorted then
    begin
        // If sorted, use binary search.
        Count := 0;
        SearchResult := BinarySearch(Item);
        if SearchResult.ResultType = srFoundAtIndex then
        begin
            repeat
                Inc(Count);
            until not Comparator.Equals(Item, Items[SearchResult.Index]);
        end;
        Result := Count;
    end
    else
        // Resort to sequential search for unsorted
        Result := inherited TrueItemCount(Item);
end;

function TAbstractList.TrueRemove(const Item: ICollectable): ICollectable;
var
    SearchResult: TSearchResult;
begin
    Result := nil;
    if Sorted then
    begin
        SearchResult := BinarySearch(Item);
        if SearchResult.ResultType = srFoundAtIndex then
        begin
            Result := TrueDelete(SearchResult.Index);
        end;
    end
    else
    begin
        SearchResult := SequentialSearch(Item);
        if SearchResult.ResultType = srFoundAtIndex then
            Result := TrueDelete(SearchResult.Index);
    end;
end;

function TAbstractList.TrueRemoveAll(const Item: ICollectable): ICollection;
var
    ResultCollection: ICollection;
    SearchResult: TSearchResult;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create;
    if Sorted then
    begin
        SearchResult := BinarySearch(Item);
        if SearchResult.ResultType = srFoundAtIndex then
        begin
            repeat
                ResultCollection.Add(TrueDelete(SearchResult.Index));
            until not Comparator.Equals(Item, Items[SearchResult.Index]);
        end;
    end
    else
    begin
        I := 0;
        while I < Size do
        begin
            if Comparator.Equals(Item, Items[I]) then
            begin
                ResultCollection.Add(TrueDelete(I));
            end
            else
                Inc(I);
        end;
    end;
    Result := ResultCollection;
end;

procedure TAbstractList.QuickSort(Lo, Hi: Integer; const Comparator: IComparator);
var
    I, J, Mid: Integer;
begin
    repeat
        I := Lo;
        J := Hi;
        Mid := (Lo + Hi) div 2;
        repeat
            while Comparator.Compare(Items[I], Items[Mid]) < 0 do
                Inc(I);
            while Comparator.Compare(Items[J], Items[Mid]) > 0 do
                Dec(J);
            if I <= J then
            begin
                Exchange(I, J);
                if Mid = I then
                    Mid := J
                else if Mid = J then
                    Mid := I;
                Inc(I);
                Dec(J);
            end;
        until I > J;
        if Lo < J then
            QuickSort(Lo, J, Comparator);
        Lo := I;
    until I >= Hi;
end;

procedure TAbstractList.QuickSort(Lo, Hi: Integer; CompareFunc: TCollectionCompareFunc);
var
    I, J, Mid: Integer;
begin
    repeat
        I := Lo;
        J := Hi;
        Mid := (Lo + Hi) div 2;
        repeat
            while CompareFunc(Items[I], Items[Mid]) < 0 do
                Inc(I);
            while CompareFunc(Items[J], Items[Mid]) > 0 do
                Dec(J);
            if I <= J then
            begin
                Exchange(I, J);
                if Mid = I then
                    Mid := J
                else if Mid = J then
                    Mid := I;
                Inc(I);
                Dec(J);
            end;
        until I > J;
        if Lo < J then
            QuickSort(Lo, J, CompareFunc);
        Lo := I;
    until I >= Hi;
end;

function TAbstractList.GetDuplicates: Boolean;
begin
    Result := FDuplicates;
end;

procedure TAbstractList.SetDuplicates(Value: Boolean);
var
    Iterator: IIterator;
    Failed: Boolean;
begin
    Failed := false;
    // If trying to set no duplicates, check there are no existing duplicates.
    if not Value then
    begin
        Iterator := GetIterator;
        while not Iterator.EOF and not Failed do
        begin
            Failed := (ItemCount(Iterator.CurrentItem) > 1);
            Iterator.Next;
        end;
        if Failed then
            CollectionError(ceDuplicate);
    end;
    if not Failed then
        FDuplicates := Value;
end;

function TAbstractList.GetItem(Index: Integer): ICollectable;
begin
    if (Index < 0) or (Index >= Size) then
    begin
        CollectionError(ceOutOfRange);
        Result := nil;
    end
    else
        Result := TrueGetItem(Index);
end;

procedure TAbstractList.SetItem(Index: Integer; const Item: ICollectable);
var
    SearchResult: TSearchResult;
begin
    if (Index < 0) or (Index >= Size) then
    begin
        CollectionError(ceOutOfRange)
    end
    else if not Duplicates then
    begin
        // Find any duplicates
        if Sorted then
        begin
            SearchResult := BinarySearch(Item);
            case SearchResult.ResultType of
            srBeforeIndex, srAfterEnd: begin        // If item is not present
                FSorted := false;
                TrueSetItem(Index, Item);
            end;
            srFoundAtIndex: begin                   // If item is already present
                CollectionError(ceDuplicate);
            end;
            end;
        end
        else
        begin
            // If item is already present
            if SequentialSearch(Item, Comparator).ResultType = srFoundAtIndex then
            begin
                CollectionError(ceDuplicate);
            end
            else
            begin
                TrueSetItem(Index, Item);
            end;
        end;
    end
    else
    begin
        FSorted := false;
        TrueSetItem(Index, Item);
    end;
end;

function TAbstractList.GetIterator: IIterator;
begin
    Result := TAbstractListIterator.Create(Self);
end;

function TAbstractList.GetNaturalItemIID: TGUID;
begin
    Result := ComparableIID;
end;

function TAbstractList.GetSorted: Boolean;
begin
    Result := FSorted;
end;

procedure TAbstractList.SetSorted(Value: Boolean);
begin
    if Value then
        Sort;
end;

function TAbstractList.GetType: TCollectionType;
begin
    Result := ctList;
end;

function TAbstractList.BinarySearch(const Item: ICollectable): TSearchResult;
var
    Lo, Hi, Mid: Integer;
    CompareResult: Integer;
    Success: Boolean;
begin
    if Size = 0 then
    begin
        Result.ResultType := srAfterEnd;
        Exit;
    end;
    Lo := 0;
    Hi := Size - 1;
    Success := false;
    repeat
        Mid := (Lo + Hi) div 2;
        CompareResult := Comparator.Compare(Item, Items[Mid]);
        if CompareResult = 0 then
            Success := true
        else if CompareResult > 0 then
            Lo := Mid + 1
        else
            Hi := Mid - 1;
    until (Lo > Hi) or Success;
    if Success then
    begin
        // Move index back if in cluster of duplicates
        while (Mid > 0) and Comparator.Equals(Item, Items[Mid - 1]) do
            Dec(Mid);
        Result.ResultType := srFoundAtIndex;
        Result.Index := Mid;
    end
    else if CompareResult < 0 then
    begin
        Result.ResultType := srBeforeIndex;
        Result.Index := Mid;
    end
    else if Hi < Size - 1 then
    begin
        Result.ResultType := srBeforeIndex;
        Result.Index := Mid + 1;
    end
    else
        Result.ResultType := srAfterEnd;
end;

function TAbstractList.CloneAsList: IList;
begin
    Result := (TAbstractListClass(ClassType)).Create(Self);
end;

function TAbstractList.Delete(Index: Integer): ICollectable;
begin
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
        Result := nil;
    end
    else if (Index < 0) or (Index >= Size) then
    begin
        CollectionError(ceOutOfRange);
        Result := nil;
    end
    else
    begin
        Result := TrueDelete(Index);
    end;
end;

procedure TAbstractList.Exchange(Index1, Index2: Integer);
var
    Item: ICollectable;
begin
    if (Index1 < 0) or (Index1 >= Size) then
        CollectionError(ceOutOfRange);
    if (Index2 < 0) or (Index2 >= Size) then
        CollectionError(ceOutOfRange);
    FSorted := false;
    Item := ICollectable(Items[Index1]);
    Items[Index1] := Items[Index2];
    Items[Index2] := Item;
end;

function TAbstractList.First: ICollectable;
begin
    if Size > 0 then
        Result := Items[0]
    else
        Result := nil;
end;

function TAbstractList.IndexOf(const Item: ICollectable): Integer;
var
    SearchResult: TSearchResult;
begin
    if Sorted then
        SearchResult := BinarySearch(Item)
    else
        SearchResult := SequentialSearch(Item, Comparator);
    if SearchResult.ResultType = srFoundAtIndex then
        Result := SearchResult.Index
    else
        Result := -1;
end;

function TAbstractList.Insert(Index: Integer; const Item: ICollectable): Boolean;
var
    ItemError: TCollectionError;
begin
    ItemError := ItemAllowed(Item);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Result := false;
    end
    else if FixedSize then
    begin
        CollectionError(ceFixedSize);
        Result := false;
    end
    else if (Index < 0) or (Index > Size) then
    begin
        CollectionError(ceOutOfRange);
        Result := false;
    end
    else
    begin
        FSorted := false;
        if Index = Size then
            TrueAdd(Item)
        else
            TrueInsert(Index, Item);
        Result := true;
    end;
end;

function TAbstractList.Insert(Index: Integer; const ItemArray: array of ICollectable): Integer;
var
    Item: ICollectable;
    ItemError: TCollectionError;
    I, NewIndex, Count: Integer;
    Success: Boolean;
begin
    Count := 0;
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
    end
    else if (Index < 0) or (Index > Size) then
    begin
        CollectionError(ceOutOfRange);
    end
    else
    begin
        // Insert entire array in place in correct order
        NewIndex := Index;
        for I := Low(ItemArray) to High(ItemArray) do
        begin
            Item := ItemArray[I];
            ItemError := ItemAllowed(Item);
            if ItemError <> ceOK then
            begin
                CollectionError(ItemError);
            end
            else
            begin
                Success := Insert(NewIndex, Item);
                if Success then
                begin
                    Inc(NewIndex);
                    Inc(Count);
                end;
            end;
        end;
    end;
    Result := Count;
end;

function TAbstractList.Insert(Index: Integer; const Collection: ICollection): Integer;
var
    Iterator: IIterator;
    Item: ICollectable;
    ItemError: TCollectionError;
    NewIndex, Count: Integer;
    Success: Boolean;
begin
    Count := 0;
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
    end
    else if (Index < 0) or (Index > Size) then
    begin
        CollectionError(ceOutOfRange);
    end
    else
    begin
        // Insert entire collection in place in correct order
        NewIndex := Index;
        Iterator := Collection.GetIterator;
        while not Iterator.EOF do
        begin
            Item := Iterator.CurrentItem;
            ItemError := ItemAllowed(Item);
            if ItemError <> ceOK then
            begin
                CollectionError(ItemError);
            end
            else
            begin
                Success := Insert(NewIndex, Item);
                if Success then
                begin
                    Inc(NewIndex);
                    Inc(Count);
                end;
            end;
            Iterator.Next;
        end;
    end;
    Result := Count;
end;

function TAbstractList.IsNilAllowed: Boolean;
begin
    Result := true;
end;

function TAbstractList.Last: ICollectable;
begin
    if Size > 0 then
        Result := Items[Size - 1]
    else
        Result := nil;
end;

function TAbstractList.Search(const Item: ICollectable; const SearchComparator: IComparator = nil): TSearchResult;
begin
    if Sorted and (SearchComparator = nil) then
        Result := BinarySearch(Item)
    else
        Result := SequentialSearch(Item, SearchComparator);
end;

function TAbstractList.SequentialSearch(const Item: ICollectable; const SearchComparator: IComparator): TSearchResult;
var
    WorkingComparator: IComparator;
    I: Integer;
    Success: Boolean;
begin
    if SearchComparator = nil then
        WorkingComparator := Comparator
    else
        WorkingComparator := SearchComparator;
    Result.ResultType := srNotFound;
    I := 0;
    Success := false;
    while (I < Size) and not Success do
    begin
        if WorkingComparator.Equals(Item, Items[I]) then
        begin
            Result.ResultType := srFoundAtIndex;
            Result.Index := I;
            Success := true;
        end
        else
            Inc(I);
    end;
end;

procedure TAbstractList.Sort(const SortComparator: IComparator);
begin
    if SortComparator = nil then
    begin
        if Size > 0 then
            QuickSort(0, Size - 1, Comparator);
        FSorted := true;
    end
    else
    begin
        if Size > 0 then
            QuickSort(0, Size - 1, SortComparator);
        FSorted := false;
    end;
end;

procedure TAbstractList.Sort(CompareFunc: TCollectionCompareFunc);
begin
    if Size > 0 then
        QuickSort(0, Size - 1, CompareFunc);
    FSorted := false;
end;

{ TAbstractMap }
constructor TAbstractMap.Create;
begin
    Create(false, true);
end;

constructor TAbstractMap.Create(NaturalItemsOnly: Boolean);
begin
    Create(NaturalItemsOnly, true);
end;

constructor TAbstractMap.Create(NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FNaturalKeysOnly := NaturalKeysOnly or GetAlwaysNaturalKeys;
    FAssociationComparator := TAssociationComparator.Create(FNaturalKeysOnly);
    if FNaturalKeysOnly then
        FKeyComparator := TAbstractComparator.GetNaturalComparator
    else
        FKeyComparator := TAbstractComparator.GetDefaultComparator;
end;

constructor TAbstractMap.Create(const ItemArray: array of ICollectable);
begin
    Create(ItemArray, true, true);
end;

constructor TAbstractMap.Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
begin
    Create(ItemArray, true, true);
end;

constructor TAbstractMap.Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean);
var
    I: Integer;
begin
    Create(true, NaturalKeysOnly);
    if not FixedSize then
    begin
        Capacity := Length(ItemArray);
        for I := Low(ItemArray) to High(ItemArray) do
        begin
            Add(ItemArray[I]);
        end;
    end;
end;

constructor TAbstractMap.Create(const KeyArray, ItemArray: array of ICollectable);
begin
    Create(KeyArray, ItemArray, false, true);
end;

constructor TAbstractMap.Create(const KeyArray, ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
begin
    Create(KeyArray, ItemArray, NaturalItemsOnly, true);
end;

constructor TAbstractMap.Create(const KeyArray, ItemArray: array of ICollectable; NaturalItemsOnly: Boolean; NaturalKeysOnly: Boolean);
var
    I, Lo, Hi: Integer;
begin
    Create(NaturalItemsOnly, NaturalKeysOnly);
    if not FixedSize then
    begin
        Capacity := Min(Length(KeyArray), Length(ItemArray));
        Lo := Max(Low(KeyArray), Low(ItemArray));
        Hi := Min(High(KeyArray), High(ItemArray));
        for I := Lo to Hi do
        begin
            Put(KeyArray[I], ItemArray[I]);
        end;
    end;
end;

constructor TAbstractMap.Create(const Map: IMap);
var
    MapIterator: IMapIterator;
begin
    Create(Map.GetNaturalItemsOnly, Map.GetNaturalKeysOnly);
    InitFrom(Map);
    if not FixedSize then
    begin
        Capacity := Map.GetSize;
        MapIterator := Map.GetMapIterator;
        while not MapIterator.EOF do
        begin
            Put(MapIterator.CurrentKey, MapIterator.CurrentItem);
            MapIterator.Next;
        end;
    end;
end;

destructor TAbstractMap.Destroy;
begin
    FKeyComparator := nil;
    FAssociationComparator := nil;
    inherited Destroy;
end;

procedure TAbstractMap.InitFrom(const Collection: ICollection);
var
    Map: IMap;
begin
    inherited InitFrom(Collection);
    if Collection.QueryInterface(IMap, Map) = S_OK then
    begin
        FNaturalKeysOnly := Map.GetNaturalKeysOnly or GetAlwaysNaturalKeys;
        KeyComparator := Map.GetKeyComparator;
    end;
end;

function TAbstractMap.TrueAdd(const Item: ICollectable): Boolean;
var
    Position: TCollectionPosition;
    Mappable: IMappable;
begin
    if IsNaturalItem(Item) then
    begin
        Mappable := Item as IMappable;
        Position := GetKeyPosition(Mappable.GetKey);
        try
            if Position.Found then
            begin
                CollectionError(ceDuplicateKey);
                Result := false;
            end
            else
            begin
                TruePut(Position, TAssociation.Create(Mappable.GetKey, Item));
                Result := true;
            end;
        finally
            Position.Free;
        end;
    end
    else
    begin
        CollectionError(ceNotNaturalItem);
        Result := false;
    end;
end;

function TAbstractMap.TrueContains(const Item: ICollectable): Boolean;
var
    Item2: ICollectable;
    Success: Boolean;
    Iterator: IIterator;
begin
    Iterator := GetIterator;
    Success := false;
    while not Iterator.EOF and not Success do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
            Success := true;
        Iterator.Next;
    end;
    Result := Success;
end;

function TAbstractMap.TrueRemove(const Item: ICollectable): ICollectable;
var
    Item2: ICollectable;
    Iterator: IMapIterator;
    Found: Boolean;
begin
    // Sequential search
    Found := false;
    Result := nil;
    Iterator := GetAssociationIterator;
    while not Iterator.EOF and not Found do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
        begin
            Result := Item2;
            Iterator.Remove;
            Found := true;
        end;
        Iterator.Next;
    end;
end;

function TAbstractMap.TrueRemoveAll(const Item: ICollectable): ICollection;
var
    ResultCollection: ICollection;
    Item2: ICollectable;
    Iterator: IMapIterator;
begin
    // Sequential search
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := GetAssociationIterator;
    while not Iterator.EOF do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
        begin
            ResultCollection.Add(Item2);
            Iterator.Remove;
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

class function TAbstractMap.GetAlwaysNaturalKeys: Boolean;
begin
    Result := false;
end;

function TAbstractMap.GetItem(const Key: ICollectable): ICollectable;
begin
    Result := Get(Key);
end;

procedure TAbstractMap.SetItem(const Key, Item: ICollectable);
begin
    Put(Key, Item);
end;

function TAbstractMap.GetIterator: IIterator;
begin
    Result := GetAssociationIterator;
end;

function TAbstractMap.GetKeyComparator: IComparator;
begin
    Result := FKeyComparator;
end;

procedure TAbstractMap.SetKeyComparator(const Value: IComparator);
begin
    FKeyComparator := Value;
    FAssociationComparator.KeyComparator := Value;
end;

function TAbstractMap.GetKeyIterator: IIterator;
begin
    Result := TAssociationKeyIterator.Create(GetAssociationIterator);
end;

function TAbstractMap.GetKeys: ISet;
var
    ResultCollection: TPArraySet;
    KeyIterator: IIterator;
begin
    ResultCollection := TPArraySet.Create(NaturalKeysOnly);
    ResultCollection.SetComparator(GetKeyComparator);
    KeyIterator := GetKeyIterator;
    while not KeyIterator.EOF do
    begin
        ResultCollection.Add(KeyIterator.CurrentItem);
        KeyIterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractMap.GetMapIterator: IMapIterator;
begin
    Result := GetAssociationIterator;
end;

function TAbstractMap.GetMapIteratorByKey(const Filter: IFilter): IMapIterator;
var
    Iterator: IMapIterator;
begin
    Iterator := GetMapIterator;
    Result := TKeyFilterMapIterator.Create(Iterator, Filter, Iterator.GetAllowRemoval);
end;

function TAbstractMap.GetMapIteratorByKey(FilterFunc: TCollectionFilterFunc): IMapIterator;
var
    Iterator: IMapIterator;
begin
    Iterator := GetMapIterator;
    Result := TKeyFilterFuncMapIterator.Create(Iterator, FilterFunc, Iterator.GetAllowRemoval);
end;

function TAbstractMap.GetNaturalItemIID: TGUID;
begin
    Result := MappableIID;
end;

function TAbstractMap.GetNaturalKeyIID: TGUID;
begin
    Result := EquatableIID;
end;

function TAbstractMap.GetNaturalKeysOnly: Boolean;
begin
    Result := FNaturalKeysOnly;
end;

function TAbstractMap.GetType: TCollectionType;
begin
    Result := ctMap;
end;

function TAbstractMap.GetValues: ICollection;
var
    ResultCollection: ICollection;
    ValueIterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    ValueIterator := GetIterator;
    while not ValueIterator.EOF do
    begin
        ResultCollection.Add(ValueIterator.CurrentItem);
        ValueIterator.Next;
    end;
    Result := ResultCollection;
end;

// Overrides TAbstractCollection function, otherwise Create(ICollection) is
// called, which cannot access keys.
function TAbstractMap.Clone: ICollection;
begin
    Result := (TAbstractMapClass(ClassType)).Create(Self);
end;

function TAbstractMap.CloneAsMap: IMap;
begin
    Result := (TAbstractMapClass(ClassType)).Create(Self);
end;

function TAbstractMap.ContainsKey(const Key: ICollectable): Boolean;
var
    Position: TCollectionPosition;
begin
    Position := GetKeyPosition(Key);
    try
        Result := Position.Found;
    finally
        Position.Free;
    end;
end;

function TAbstractMap.ContainsKey(const KeyArray: array of ICollectable): Boolean;
var
    I: Integer;
    Success: Boolean;
begin
    Success := true;
    for I := Low(KeyArray) to High(KeyArray) do
    begin
        Success := Success and ContainsKey(KeyArray[I]);
        if not Success then
            break;
    end;
    Result := Success;
end;

function TAbstractMap.ContainsKey(const Collection: ICollection): Boolean;
var
    Iterator: IIterator;
    Success: Boolean;
begin
    Success := true;
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Success := Success and ContainsKey(Iterator.CurrentItem);
        if not Success then
            break;
        Iterator.Next;
    end;
    Result := Success;
end;

function TAbstractMap.Get(const Key: ICollectable): ICollectable;
var
    KeyError: TCollectionError;
    Position: TCollectionPosition;
begin
    KeyError := KeyAllowed(Key);
    if KeyError <> ceOK then
    begin
        CollectionError(KeyError);
        Result := nil;
    end
    else
    begin
        Position := GetKeyPosition(Key);
        try
            if Position.Found then
                Result := TrueGet(Position).GetValue
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractMap.KeyAllowed(const Key: ICollectable): TCollectionError;
begin
    if NaturalKeysOnly and not IsNaturalKey(Key) then
        Result := ceNotNaturalItem
    else if Key = nil then
        Result := ceNilNotAllowed
    else
        Result := ceOK;
end;

function TAbstractMap.IsNaturalKey(const Key: ICollectable): Boolean;
var
    Temp: IUnknown;
begin
    if Key.QueryInterface(NaturalKeyIID, Temp) <> E_NOINTERFACE then
        Result := true
    else
        Result := false;
end;

function TAbstractMap.IsNilAllowed: Boolean;
begin
    Result := true;
end;

function TAbstractMap.MatchingKey(const KeyArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(KeyArray) to High(KeyArray) do
    begin
        if ContainsKey(KeyArray[I]) then
            ResultCollection.Add(KeyArray[I]);
    end;
    Result := ResultCollection;
end;

function TAbstractMap.MatchingKey(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Iterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        if ContainsKey(Iterator.CurrentItem) then
            ResultCollection.Add(Iterator.CurrentItem);
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractMap.Put(const Item: ICollectable): ICollectable;
var
    Mappable: IMappable;
    OldAssociation, NewAssociation: IAssociation;
    Position: TCollectionPosition;
begin
    if not IsNaturalItem(Item) then
    begin
        CollectionError(ceNotNaturalItem);
        Result := nil;
    end
    else
    begin
        Item.QueryInterface(IMappable, Mappable);
        Position := GetKeyPosition(Mappable.GetKey);
        try
            NewAssociation := TAssociation.Create(Mappable.GetKey, Item);
            OldAssociation := TruePut(Position, NewAssociation);
            if OldAssociation <> nil then
                Result := OldAssociation.GetValue
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractMap.Put(const Key, Item: ICollectable): ICollectable;
var
    OldAssociation, NewAssociation: IAssociation;
    ItemError, KeyError: TCollectionError;
    Position: TCollectionPosition;
begin
    ItemError := ItemAllowed(Item);
    KeyError := KeyAllowed(Key);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Result := nil;
    end
    else if KeyError <> ceOK then
    begin
        CollectionError(KeyError);
        Result := nil;
    end
    else
    begin
        // Find appropriate place, then place key-item association there
        Position := GetKeyPosition(Key);
        try
            NewAssociation := TAssociation.Create(Key, Item);
            OldAssociation := TruePut(Position, NewAssociation);
            if OldAssociation <> nil then
                Result := OldAssociation.GetValue
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractMap.Put(const ItemArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    Mappable: IMappable;
    OldAssociation, NewAssociation: IAssociation;
    Position: TCollectionPosition;
    Item: ICollectable;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        Item := ItemArray[I];
        if not IsNaturalItem(Item) then
        begin
            CollectionError(ceNotNaturalItem);
        end
        else
        begin
            // Find appropriate place, then place key-item association there
            Item.QueryInterface(IMappable, Mappable);
            Position := GetKeyPosition(Mappable.GetKey);
            try
                NewAssociation := TAssociation.Create(Mappable.GetKey, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
    end;
    Result := ResultCollection;
end;

function TAbstractMap.Put(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Mappable: IMappable;
    OldAssociation, NewAssociation: IAssociation;
    Position: TCollectionPosition;
    Iterator: IIterator;
    Item: ICollectable;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;;
        if not IsNaturalItem(Item) then
        begin
            CollectionError(ceNotNaturalItem);
        end
        else
        begin
            // Find appropriate place, then place key-item association there
            Item.QueryInterface(IMappable, Mappable);
            Position := GetKeyPosition(Mappable.GetKey);
            try
                NewAssociation := TAssociation.Create(Mappable.GetKey, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractMap.Put(const Map: IMap): ICollection;
var
    ResultCollection: ICollection;
    OldAssociation, NewAssociation: IAssociation;
    ItemError, KeyError: TCollectionError;
    Position: TCollectionPosition;
    MapIterator: IMapIterator;
    Key, Item: ICollectable;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    MapIterator := Map.GetMapIterator;
    while not MapIterator.EOF do
    begin
        Key := MapIterator.CurrentKey;
        Item := MapIterator.CurrentItem;

        ItemError := ItemAllowed(Item);
        KeyError := KeyAllowed(Key);
        if ItemError <> ceOK then
        begin
            CollectionError(ItemError);
        end
        else if KeyError <> ceOK then
        begin
            CollectionError(KeyError);
        end
        else
        begin
            // Find appropriate place, then place key-item association there
            Position := GetKeyPosition(Key);
            try
                NewAssociation := TAssociation.Create(Key, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
        MapIterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractMap.RemoveKey(const Key: ICollectable): ICollectable;
var
    KeyError: TCollectionError;
    Position: TCollectionPosition;
    OldAssociation: IAssociation;
begin
    KeyError := KeyAllowed(Key);
    if KeyError <> ceOK then
    begin
        CollectionError(KeyError);
        Result := nil;
    end
    else
    begin
        Position := GetKeyPosition(Key);
        try
            if Position.Found then
            begin
                OldAssociation := TrueRemove2(Position);
                Result := OldAssociation.GetValue
            end
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractMap.RemoveKey(const KeyArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    OldAssociation: IAssociation;
    KeyError: TCollectionError;
    Position: TCollectionPosition;
    Key: ICollectable;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(KeyArray) to High(KeyArray) do
    begin
        Key := KeyArray[I];
        KeyError := KeyAllowed(Key);
        if KeyError <> ceOK then
        begin
            CollectionError(KeyError);
        end
        else
        begin
            Position := GetKeyPosition(Key);
            try
                if Position.Found then
                begin
                    OldAssociation := TrueRemove2(Position);
                    ResultCollection.Add(OldAssociation.GetValue);
                end;
            finally
                Position.Free;
            end;
        end;
    end;
    Result := ResultCollection;
end;

function TAbstractMap.RemoveKey(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    OldAssociation: IAssociation;
    KeyError: TCollectionError;
    Position: TCollectionPosition;
    Key: ICollectable;
    Iterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Key := Iterator.CurrentItem;
        KeyError := KeyAllowed(Key);
        if KeyError <> ceOK then
        begin
            CollectionError(KeyError);
        end
        else
        begin
            Position := GetKeyPosition(Key);
            try
                if Position.Found then
                begin
                    OldAssociation := TrueRemove2(Position);
                    ResultCollection.Add(OldAssociation.GetValue);
                end;
            finally
                Position.Free;
            end;
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractMap.RetainKey(const KeyArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    MapIterator: IMapIterator;
    I: Integer;
    Found: Boolean;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
    end
    else
    begin
        MapIterator := GetMapIterator;
        while not MapIterator.EOF do
        begin
            // Converting the array to a map would be faster but I don't want to
            // couple base class code to a complex collection.
            Found := false;
            for I := Low(KeyArray) to High(KeyArray) do
            begin
                Found := KeyComparator.Equals(MapIterator.CurrentKey, KeyArray[I]);
                if Found then
                    break;
            end;
            if not Found then
            begin
                ResultCollection.Add(MapIterator.CurrentItem);
                MapIterator.Remove;
            end;
            MapIterator.Next;
        end;
        Result := ResultCollection;
    end;
end;

function TAbstractMap.RetainKey(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    MapIterator: IMapIterator;
    Key: ICollectable;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
    end
    else
    begin
        MapIterator := GetMapIterator;
        while not MapIterator.EOF do
        begin
            Key := MapIterator.CurrentKey;
            if not Collection.Contains(Key) then
            begin
                ResultCollection.Add(MapIterator.CurrentItem);
                MapIterator.Remove;
            end;
            MapIterator.Next;
        end;
    end;
    Result := ResultCollection;
end;


{ TAbstractIntegerMap }
constructor TAbstractIntegerMap.Create(NaturalItemsOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FAssociationComparator := TIntegerAssociationComparator.Create;
end;

constructor TAbstractIntegerMap.Create(const ItemArray: array of ICollectable);
begin
    Create(ItemArray, true);
end;

constructor TAbstractIntegerMap.Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
begin
    inherited Create(ItemArray, true);
end;

constructor TAbstractIntegerMap.Create(const KeyArray: array of Integer; const ItemArray: array of ICollectable);
begin
    Create(KeyArray, ItemArray, false);
end;

constructor TAbstractIntegerMap.Create(const KeyArray: array of Integer; const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
var
    I, Lo, Hi: Integer;
begin
    Create(NaturalItemsOnly);
    Capacity := Min(Length(KeyArray), Length(ItemArray));
    if not FixedSize then
    begin
        Lo := Max(Low(KeyArray), Low(ItemArray));
        Hi := Min(High(KeyArray), High(ItemArray));
        for I := Lo to Hi do
        begin
            Put(KeyArray[I], ItemArray[I]);
        end;
    end;
end;

constructor TAbstractIntegerMap.Create(const Map: IIntegerMap);
var
    MapIterator: IIntegerMapIterator;
begin
    Create(Map.GetNaturalItemsOnly);
    InitFrom(Map);
    Capacity := Map.GetSize;
    if not FixedSize then
    begin
        MapIterator := Map.GetMapIterator;
        while not MapIterator.EOF do
        begin
            Put(MapIterator.CurrentKey, MapIterator.CurrentItem);
            MapIterator.Next;
        end;
    end;
end;

destructor TAbstractIntegerMap.Destroy;
begin
    FAssociationComparator := nil;
    inherited Destroy;
end;

function TAbstractIntegerMap.TrueAdd(const Item: ICollectable): Boolean;
var
    Position: TCollectionPosition;
    Mappable: IIntegerMappable;
begin
    if IsNaturalItem(Item) then
    begin
        Mappable := Item as IIntegerMappable;
        Position := GetKeyPosition(Mappable.GetKey);
        try
            if Position.Found then
            begin
                CollectionError(ceDuplicateKey);
                Result := false;
            end
            else
            begin
                TruePut(Position, TIntegerAssociation.Create(Mappable.GetKey, Item));
                Result := true;
            end;
        finally
            Position.Free;
        end;
    end
    else
    begin
        CollectionError(ceNotNaturalItem);
        Result := false;
    end;
end;

function TAbstractIntegerMap.TrueContains(const Item: ICollectable): Boolean;
var
    Item2: ICollectable;
    Success: Boolean;
    Iterator: IIterator;
begin
    Iterator := GetIterator;
    Success := false;
    while not Iterator.EOF and not Success do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
            Success := true;
        Iterator.Next;
    end;
    Result := Success;
end;

function TAbstractIntegerMap.TrueRemove(const Item: ICollectable): ICollectable;
var
    Item2: ICollectable;
    Iterator: IIntegerMapIterator;
    Found: Boolean;
begin
    // Sequential search
    Found := false;
    Result := nil;
    Iterator := GetAssociationIterator;
    while not Iterator.EOF and not Found do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
        begin
            Result := Item2;
            Iterator.Remove;
            Found := true;
        end;
        Iterator.Next;
    end;
end;

function TAbstractIntegerMap.TrueRemoveAll(const Item: ICollectable): ICollection;
var
    ResultCollection: ICollection;
    Item2: ICollectable;
    Iterator: IIntegerMapIterator;
begin
    // Sequential search
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := GetAssociationIterator;
    while not Iterator.EOF do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
        begin
            ResultCollection.Add(Item2);
            Iterator.Remove;
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractIntegerMap.GetItem(const Key: Integer): ICollectable;
begin
    Result := Get(Key);
end;

procedure TAbstractIntegerMap.SetItem(const Key: Integer; const Item: ICollectable);
begin
    Put(Key, Item);
end;

function TAbstractIntegerMap.GetIterator: IIterator;
begin
    Result := GetAssociationIterator;
end;

function TAbstractIntegerMap.GetKeys: ISet;
var
    ResultCollection: TPArraySet;
    MapIterator: IIntegerMapIterator;
begin
    ResultCollection := TPArraySet.Create(true);
    MapIterator := GetMapIterator;
    while not MapIterator.EOF do
    begin
        ResultCollection.Add(TIntegerWrapper.Create(MapIterator.CurrentKey) as ICollectable);
        MapIterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractIntegerMap.GetMapIterator: IIntegerMapIterator;
begin
    Result := GetAssociationIterator;
end;

function TAbstractIntegerMap.GetNaturalItemIID: TGUID;
begin
    Result := IntegerMappableIID;
end;

function TAbstractIntegerMap.GetType: TCollectionType;
begin
    Result := ctIntegerMap;
end;

function TAbstractIntegerMap.GetValues: ICollection;
var
    ResultCollection: ICollection;
    ValueIterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    ValueIterator := GetIterator;
    while not ValueIterator.EOF do
    begin
        ResultCollection.Add(ValueIterator.CurrentItem);
        ValueIterator.Next;
    end;
    Result := ResultCollection;
end;

// Overrides TAbstractCollection function, otherwise Create(ICollection) is
// called, which cannot access keys.
function TAbstractIntegerMap.Clone: ICollection;
begin
    Result := (TAbstractIntegerMapClass(ClassType)).Create(Self);
end;

function TAbstractIntegerMap.CloneAsIntegerMap: IIntegerMap;
begin
    Result := (TAbstractIntegerMapClass(ClassType)).Create(Self);
end;

function TAbstractIntegerMap.ContainsKey(const Key: Integer): Boolean;
var
    Position: TCollectionPosition;
begin
    Position := GetKeyPosition(Key);
    try
        Result := Position.Found;
    finally
        Position.Free;
    end;
end;

function TAbstractIntegerMap.ContainsKey(const KeyArray: array of Integer): Boolean;
var
    I: Integer;
    Success: Boolean;
begin
    Success := true;
    for I := Low(KeyArray) to High(KeyArray) do
    begin
        Success := Success and ContainsKey(KeyArray[I]);
        if not Success then
            break;
    end;
    Result := Success;
end;

function TAbstractIntegerMap.Get(const Key: Integer): ICollectable;
var
    Position: TCollectionPosition;
begin
    Position := GetKeyPosition(Key);
    try
        if Position.Found then
            Result := TrueGet(Position).GetValue
        else
            Result := nil;
    finally
        Position.Free;
    end;
end;

function TAbstractIntegerMap.IsNilAllowed: Boolean;
begin
    Result := true;
end;

function TAbstractIntegerMap.Put(const Item: ICollectable): ICollectable;
var
    Mappable: IIntegerMappable;
    OldAssociation, NewAssociation: IIntegerAssociation;
    Position: TCollectionPosition;
begin
    if not IsNaturalItem(Item) then
    begin
        CollectionError(ceNotNaturalItem);
        Result := nil;
    end
    else
    begin
        Item.QueryInterface(IIntegerMappable, Mappable);
        Position := GetKeyPosition(Mappable.GetKey);
        try
            NewAssociation := TIntegerAssociation.Create(Mappable.GetKey, Item);
            OldAssociation := TruePut(Position, NewAssociation);
            if OldAssociation <> nil then
                Result := OldAssociation.GetValue
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractIntegerMap.Put(const Key: Integer; const Item: ICollectable): ICollectable;
var
    OldAssociation, NewAssociation: IIntegerAssociation;
    ItemError: TCollectionError;
    Position: TCollectionPosition;
begin
    ItemError := ItemAllowed(Item);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Result := nil;
    end
    else
    begin
        Position := GetKeyPosition(Key);
        try
            NewAssociation := TIntegerAssociation.Create(Key, Item);
            OldAssociation := TruePut(Position, NewAssociation);
            if OldAssociation <> nil then
                Result := OldAssociation.GetValue
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractIntegerMap.Put(const ItemArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    Mappable: IIntegerMappable;
    OldAssociation, NewAssociation: IIntegerAssociation;
    Position: TCollectionPosition;
    Item: ICollectable;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        Item := ItemArray[I];
        if not IsNaturalItem(Item) then
        begin
            CollectionError(ceNotNaturalItem);
        end
        else
        begin
            Item.QueryInterface(IIntegerMappable, Mappable);
            Position := GetKeyPosition(Mappable.GetKey);
            try
                NewAssociation := TIntegerAssociation.Create(Mappable.GetKey, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
    end;
    Result := ResultCollection;
end;

function TAbstractIntegerMap.Put(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Mappable: IIntegerMappable;
    OldAssociation, NewAssociation: IIntegerAssociation;
    Position: TCollectionPosition;
    Iterator: IIterator;
    Item: ICollectable;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;;
        if not IsNaturalItem(Item) then
        begin
            CollectionError(ceNotNaturalItem);
        end
        else
        begin
            Item.QueryInterface(IIntegerMappable, Mappable);
            Position := GetKeyPosition(Mappable.GetKey);
            try
                NewAssociation := TIntegerAssociation.Create(Mappable.GetKey, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractIntegerMap.Put(const Map: IIntegerMap): ICollection;
var
    ResultCollection: ICollection;
    OldAssociation, NewAssociation: IIntegerAssociation;
    ItemError: TCollectionError;
    Position: TCollectionPosition;
    MapIterator: IIntegerMapIterator;
    Item: ICollectable;
    Key: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    MapIterator := Map.GetMapIterator;
    while not MapIterator.EOF do
    begin
        Key := MapIterator.CurrentKey;
        Item := MapIterator.CurrentItem;

        ItemError := ItemAllowed(Item);
        if ItemError <> ceOK then
        begin
            CollectionError(ItemError);
        end
        else
        begin
            Position := GetKeyPosition(Key);
            try
                NewAssociation := TIntegerAssociation.Create(Key, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
        MapIterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractIntegerMap.RemoveKey(const Key: Integer): ICollectable;
var
    Position: TCollectionPosition;
    OldAssociation: IIntegerAssociation;
begin
    Position := GetKeyPosition(Key);
    try
        if Position.Found then
        begin
            OldAssociation := TrueRemove2(Position);
            Result := OldAssociation.GetValue
        end
        else
            Result := nil;
    finally
        Position.Free;
    end;
end;

function TAbstractIntegerMap.RemoveKey(const KeyArray: array of Integer): ICollection;
var
    ResultCollection: ICollection;
    OldAssociation: IIntegerAssociation;
    Position: TCollectionPosition;
    Key: Integer;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(KeyArray) to High(KeyArray) do
    begin
        Key := KeyArray[I];
        Position := GetKeyPosition(Key);
        try
            if Position.Found then
            begin
                OldAssociation := TrueRemove2(Position);
                ResultCollection.Add(OldAssociation.GetValue);
            end;
        finally
            Position.Free;
        end;
    end;
    Result := ResultCollection;
end;

function TAbstractIntegerMap.RetainKey(const KeyArray: array of Integer): ICollection;
var
    ResultCollection: ICollection;
    MapIterator: IIntegerMapIterator;
    I: Integer;
    Found: Boolean;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
    end
    else
    begin
        MapIterator := GetMapIterator;
        while not MapIterator.EOF do
        begin
            // Converting the array to a map would be faster but I don't want to
            // couple base class code to a complex collection.
            Found := false;
            for I := Low(KeyArray) to High(KeyArray) do
            begin
                Found := (MapIterator.CurrentKey = KeyArray[I]);
                if Found then
                    break;
            end;
            if not Found then
            begin
                ResultCollection.Add(MapIterator.CurrentItem);
                MapIterator.Remove;
            end;
            MapIterator.Next;
        end;
        Result := ResultCollection;
    end;
end;


{ TAbstractStringMap }
constructor TAbstractStringMap.Create(NaturalItemsOnly: Boolean);
begin
    inherited Create(NaturalItemsOnly);
    FAssociationComparator := TStringAssociationComparator.Create;
end;

constructor TAbstractStringMap.Create(const ItemArray: array of ICollectable);
begin
    Create(ItemArray, true);
end;

constructor TAbstractStringMap.Create(const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
begin
    inherited Create(ItemArray, true);
end;

constructor TAbstractStringMap.Create(const KeyArray: array of String; const ItemArray: array of ICollectable);
begin
    Create(KeyArray, ItemArray, false);
end;

constructor TAbstractStringMap.Create(const KeyArray: array of String; const ItemArray: array of ICollectable; NaturalItemsOnly: Boolean);
var
    I, Lo, Hi: Integer;
begin
    Create(NaturalItemsOnly);
    Capacity := Min(Length(KeyArray), Length(ItemArray));
    if not FixedSize then
    begin
        Lo := Max(Low(KeyArray), Low(ItemArray));
        Hi := Min(High(KeyArray), High(ItemArray));
        for I := Lo to Hi do
        begin
            Put(KeyArray[I], ItemArray[I]);
        end;
    end;
end;

constructor TAbstractStringMap.Create(const Map: IStringMap);
var
    MapIterator: IStringMapIterator;
begin
    Create(Map.GetNaturalItemsOnly);
    InitFrom(Map);
    Capacity := Map.GetSize;
    if not FixedSize then
    begin
        MapIterator := Map.GetMapIterator;
        while not MapIterator.EOF do
        begin
            Put(MapIterator.CurrentKey, MapIterator.CurrentItem);
            MapIterator.Next;
        end;
    end;
end;

destructor TAbstractStringMap.Destroy;
begin
    FAssociationComparator := nil;
    inherited Destroy;
end;

function TAbstractStringMap.TrueAdd(const Item: ICollectable): Boolean;
var
    Position: TCollectionPosition;
    Mappable: IStringMappable;
begin
    if IsNaturalItem(Item) then
    begin
        Mappable := Item as IStringMappable;
        Position := GetKeyPosition(Mappable.GetKey);
        try
            if Position.Found then
            begin
                CollectionError(ceDuplicateKey);
                Result := false;
            end
            else
            begin
                TruePut(Position, TStringAssociation.Create(Mappable.GetKey, Item));
                Result := true;
            end;
        finally
            Position.Free;
        end;
    end
    else
    begin
        CollectionError(ceNotNaturalItem);
        Result := false;
    end;
end;

function TAbstractStringMap.TrueContains(const Item: ICollectable): Boolean;
var
    Item2: ICollectable;
    Success: Boolean;
    Iterator: IIterator;
begin
    Iterator := GetIterator;
    Success := false;
    while not Iterator.EOF and not Success do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
            Success := true;
        Iterator.Next;
    end;
    Result := Success;
end;

function TAbstractStringMap.TrueRemove(const Item: ICollectable): ICollectable;
var
    Item2: ICollectable;
    Iterator: IStringMapIterator;
    Found: Boolean;
begin
    // Sequential search
    Found := false;
    Result := nil;
    Iterator := GetAssociationIterator;
    while not Iterator.EOF and not Found do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
        begin
            Result := Item2;
            Iterator.Remove;
            Found := true;
        end;
        Iterator.Next;
    end;
end;

function TAbstractStringMap.TrueRemoveAll(const Item: ICollectable): ICollection;
var
    ResultCollection: ICollection;
    Item2: ICollectable;
    Iterator: IIterator;
begin
    // Sequential search
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := GetAssociationIterator;
    while not Iterator.EOF do
    begin
        Item2 := Iterator.CurrentItem;
        if Comparator.Equals(Item, Item2) then
        begin
            ResultCollection.Add(Item2);
            Iterator.Remove;
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractStringMap.GetItem(const Key: String): ICollectable;
begin
    Result := Get(Key);
end;

procedure TAbstractStringMap.SetItem(const Key: String; const Item: ICollectable);
begin
    Put(Key, Item);
end;

function TAbstractStringMap.GetIterator: IIterator;
begin
    Result := GetAssociationIterator;
end;

function TAbstractStringMap.GetKeys: ISet;
var
    ResultCollection: TPArraySet;
    MapIterator: IStringMapIterator;
begin
    ResultCollection := TPArraySet.Create(true);
    MapIterator := GetMapIterator;
    while not MapIterator.EOF do
    begin
        ResultCollection.Add(TStringWrapper.Create(MapIterator.CurrentKey) as ICollectable);
        MapIterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractStringMap.GetMapIterator: IStringMapIterator;
begin
    Result := GetAssociationIterator;
end;

function TAbstractStringMap.GetNaturalItemIID: TGUID;
begin
    Result := StringMappableIID;
end;

function TAbstractStringMap.GetType: TCollectionType;
begin
    Result := ctStringMap;
end;

function TAbstractStringMap.GetValues: ICollection;
var
    ResultCollection: ICollection;
    ValueIterator: IIterator;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    ValueIterator := GetIterator;
    while not ValueIterator.EOF do
    begin
        ResultCollection.Add(ValueIterator.CurrentItem);
        ValueIterator.Next;
    end;
    Result := ResultCollection;
end;

// Overrides TAbstractCollection function, otherwise Create(ICollection) is
// called, which cannot access keys.
function TAbstractStringMap.Clone: ICollection;
begin
    Result := (TAbstractStringMapClass(ClassType)).Create(Self);
end;

function TAbstractStringMap.CloneAsStringMap: IStringMap;
begin
    Result := (TAbstractStringMapClass(ClassType)).Create(Self);
end;

function TAbstractStringMap.ContainsKey(const Key: String): Boolean;
var
    Position: TCollectionPosition;
begin
    Position := GetKeyPosition(Key);
    try
        Result := Position.Found;
    finally
        Position.Free;
    end;
end;

function TAbstractStringMap.ContainsKey(const KeyArray: array of String): Boolean;
var
    I: Integer;
    Success: Boolean;
begin
    Success := true;
    for I := Low(KeyArray) to High(KeyArray) do
    begin
        Success := Success and ContainsKey(KeyArray[I]);
        if not Success then
            break;
    end;
    Result := Success;
end;

function TAbstractStringMap.Get(const Key: String): ICollectable;
var
    Position: TCollectionPosition;
begin
    Position := GetKeyPosition(Key);
    try
        if Position.Found then
            Result := TrueGet(Position).GetValue
        else
            Result := nil;
    finally
        Position.Free;
    end;
end;

function TAbstractStringMap.IsNilAllowed: Boolean;
begin
    Result := true;
end;

function TAbstractStringMap.Put(const Item: ICollectable): ICollectable;
var
    Mappable: IStringMappable;
    OldAssociation, NewAssociation: IStringAssociation;
    Position: TCollectionPosition;
begin
    if not IsNaturalItem(Item) then
    begin
        CollectionError(ceNotNaturalItem);
        Result := nil;
    end
    else
    begin
        Item.QueryInterface(IStringMappable, Mappable);
        Position := GetKeyPosition(Mappable.GetKey);
        try
            NewAssociation := TStringAssociation.Create(Mappable.GetKey, Item);
            OldAssociation := TruePut(Position, NewAssociation);
            if OldAssociation <> nil then
                Result := OldAssociation.GetValue
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractStringMap.Put(const Key: String; const Item: ICollectable): ICollectable;
var
    OldAssociation, NewAssociation: IStringAssociation;
    ItemError: TCollectionError;
    Position: TCollectionPosition;
begin
    ItemError := ItemAllowed(Item);
    if ItemError <> ceOK then
    begin
        CollectionError(ItemError);
        Result := nil;
    end
    else
    begin
        Position := GetKeyPosition(Key);
        try
            NewAssociation := TStringAssociation.Create(Key, Item);
            OldAssociation := TruePut(Position, NewAssociation);
            if OldAssociation <> nil then
                Result := OldAssociation.GetValue
            else
                Result := nil;
        finally
            Position.Free;
        end;
    end;
end;

function TAbstractStringMap.Put(const ItemArray: array of ICollectable): ICollection;
var
    ResultCollection: ICollection;
    Mappable: IStringMappable;
    OldAssociation, NewAssociation: IStringAssociation;
    Position: TCollectionPosition;
    Item: ICollectable;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(ItemArray) to High(ItemArray) do
    begin
        Item := ItemArray[I];
        if not IsNaturalItem(Item) then
        begin
            CollectionError(ceNotNaturalItem);
        end
        else
        begin
            Item.QueryInterface(IStringMappable, Mappable);
            Position := GetKeyPosition(Mappable.GetKey);
            try
                NewAssociation := TStringAssociation.Create(Mappable.GetKey, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
    end;
    Result := ResultCollection;
end;

function TAbstractStringMap.Put(const Collection: ICollection): ICollection;
var
    ResultCollection: ICollection;
    Mappable: IStringMappable;
    OldAssociation, NewAssociation: IStringAssociation;
    Position: TCollectionPosition;
    Iterator: IIterator;
    Item: ICollectable;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    Iterator := Collection.GetIterator;
    while not Iterator.EOF do
    begin
        Item := Iterator.CurrentItem;;
        if not IsNaturalItem(Item) then
        begin
            CollectionError(ceNotNaturalItem);
        end
        else
        begin
            Item.QueryInterface(IStringMappable, Mappable);
            Position := GetKeyPosition(Mappable.GetKey);
            try
                NewAssociation := TStringAssociation.Create(Mappable.GetKey, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
        Iterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractStringMap.Put(const Map: IStringMap): ICollection;
var
    ResultCollection: ICollection;
    OldAssociation, NewAssociation: IStringAssociation;
    ItemError: TCollectionError;
    Position: TCollectionPosition;
    MapIterator: IStringMapIterator;
    Item: ICollectable;
    Key: String;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    MapIterator := Map.GetMapIterator;
    while not MapIterator.EOF do
    begin
        Key := MapIterator.CurrentKey;
        Item := MapIterator.CurrentItem;

        ItemError := ItemAllowed(Item);
        if ItemError <> ceOK then
        begin
            CollectionError(ItemError);
        end
        else
        begin
            Position := GetKeyPosition(Key);
            try
                NewAssociation := TStringAssociation.Create(Key, Item);
                OldAssociation := TruePut(Position, NewAssociation);
                if OldAssociation <> nil then
                    ResultCollection.Add(OldAssociation.GetValue);
            finally
                Position.Free;
            end;
        end;
        MapIterator.Next;
    end;
    Result := ResultCollection;
end;

function TAbstractStringMap.RemoveKey(const Key: String): ICollectable;
var
    Position: TCollectionPosition;
    OldAssociation: IStringAssociation;
begin
    Position := GetKeyPosition(Key);
    try
        if Position.Found then
        begin
            OldAssociation := TrueRemove2(Position);
            Result := OldAssociation.GetValue
        end
        else
            Result := nil;
    finally
        Position.Free;
    end;
end;

function TAbstractStringMap.RemoveKey(const KeyArray: array of String): ICollection;
var
    ResultCollection: ICollection;
    OldAssociation: IStringAssociation;
    Position: TCollectionPosition;
    Key: String;
    I: Integer;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    for I := Low(KeyArray) to High(KeyArray) do
    begin
        Key := KeyArray[I];
        Position := GetKeyPosition(Key);
        try
            if Position.Found then
            begin
                OldAssociation := TrueRemove2(Position);
                ResultCollection.Add(OldAssociation.GetValue);
            end;
        finally
            Position.Free;
        end;
    end;
    Result := ResultCollection;
end;

function TAbstractStringMap.RetainKey(const KeyArray: array of String): ICollection;
var
    ResultCollection: ICollection;
    MapIterator: IStringMapIterator;
    I: Integer;
    Found: Boolean;
begin
    ResultCollection := TPArrayBag.Create(NaturalItemsOnly);
    if FixedSize then
    begin
        CollectionError(ceFixedSize);
    end
    else
    begin
        MapIterator := GetMapIterator;
        while not MapIterator.EOF do
        begin
            // Converting the array to a map would be faster but I don't want to
            // couple base class code to a complex collection.
            Found := false;
            for I := Low(KeyArray) to High(KeyArray) do
            begin
                Found := (MapIterator.CurrentKey = KeyArray[I]);
                if Found then
                    break;
            end;
            if not Found then
            begin
                ResultCollection.Add(MapIterator.CurrentItem);
                MapIterator.Remove;
            end;
            MapIterator.Next;
        end;
        Result := ResultCollection;
    end;
end;


{ ECollectionError }
constructor ECollectionError.Create(const Msg: String; const Collection: ICollection; ErrorType: TCollectionError);
begin
    inherited Create(Msg);
    FCollection := Collection;
    FErrorType := ErrorType;
end;

{ TAbstractListIterator }
constructor TAbstractListIterator.Create(Collection: TAbstractList);
begin
    inherited Create(true);
    FCollection := Collection;
    First;
end;

function TAbstractListIterator.TrueFirst: ICollectable;
begin
    FIndex := 0;
    if FIndex < FCollection.GetSize then
        Result := FCollection.GetItem(FIndex)
    else
        Result := nil;
end;

function TAbstractListIterator.TrueNext: ICollectable;
begin
    Inc(FIndex);
    if FIndex < FCollection.GetSize then
        Result := FCollection.GetItem(FIndex)
    else
        Result := nil;
end;

procedure TAbstractListIterator.TrueRemove;
begin
    FCollection.Delete(FIndex);
    Dec(FIndex);
end;

end.
