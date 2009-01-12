unit CollWrappers;

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
 * $Revision: 1.1.1.1 $
 * $Log: D:\QVCS Repositories\Delphi Collections\CollWrappers.qbt $
 * 
 *   Various primitive type wrappers, adapters and abstract base classes for
 *   natural items.
 * 
 * Revision 1.1.1.1  by: Matthew Greet  Rev date: 24/10/03 16:48:16
 *   v1.0 branch.
 * 
 * Revision 1.1  by: Matthew Greet  Rev date: 06/04/03 10:51:04
 *   Primitive type wrapper interfaces added. 
 *   Abstract, template classes added. 
 *   All classes implement reference counting by descending from
 *   TInterfacedObject. 
 *   
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
    SysUtils,
    Collections;

type
    IAssociationWrapper = interface
        ['{54DF42E0-64F2-11D7-8120-0002E3165EF8}']
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetKey: ICollectable;
        function GetValue: TObject;
        property AutoDestroy: Boolean read GetAutoDestroy write SetAutoDestroy;
        property Key: ICollectable read GetKey;
        property Value: TObject read GetValue;
    end;

    IBoolean = interface
        ['{62D1D160-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: Boolean;
        property Value: Boolean read GetValue;
    end;

    ICardinal = interface
        ['{6AF7B1C0-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: Cardinal;
        property Value: Cardinal read GetValue;
    end;

    IChar = interface
        ['{73AD00E0-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: Char;
        property Value: Char read GetValue;
    end;

    IClass = interface
        ['{7A84B660-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: TClass;
        property Value: TClass read GetValue;
    end;

    IDouble = interface
        ['{815C6BE0-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: Double;
        property Value: Double read GetValue;
    end;

    IInteger = interface
        ['{88ECC300-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: Integer;
        property Value: Integer read GetValue;
    end;

    IIntegerAssociationWrapper = interface
        ['{8F582220-64F2-11D7-8120-0002E3165EF8}']
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetKey: Integer;
        function GetValue: TObject;
        property AutoDestroy: Boolean read GetAutoDestroy write SetAutoDestroy;
        property Key: Integer read GetKey;
        property Value: TObject read GetValue;
    end;

    IInterfaceWrapper = interface
        ['{962E5100-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: IUnknown;
        property Value: IUnknown read GetValue;
    end;

    IObject = interface
        ['{9C675580-64F2-11D7-8120-0002E3165EF8}']
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetValue: TObject;
        property Value: TObject read GetValue;
    end;

    IString = interface
        ['{A420DF80-64F2-11D7-8120-0002E3165EF8}']
        function GetValue: String;
        property Value: String read GetValue;
    end;

    IStringAssociationWrapper = interface
        ['{AB98CCA0-64F2-11D7-8120-0002E3165EF8}']
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetKey: String;
        function GetValue: TObject;
        property AutoDestroy: Boolean read GetAutoDestroy write SetAutoDestroy;
        property Key: String read GetKey;
        property Value: TObject read GetValue;
    end;

    TAbstractItem = class(TInterfacedObject, ICollectable)
    public
        function GetInstance: TObject; virtual;
    end;

    TAbstractIntegerMappable = class(TAbstractItem, IEquatable, IIntegerMappable)
    private
        FKey: Integer;
    protected
        function MakeKey: Integer; virtual; abstract;
    public
        procedure AfterConstruction; override;
        function Equals(const Item: ICollectable): Boolean; virtual;
        function GetKey: Integer; virtual;
    end;

    TAbstractMappable = class(TAbstractItem, IEquatable, IMappable)
    private
        FKey: ICollectable;
    protected
        function MakeKey: ICollectable; virtual; abstract;
    public
        destructor Destroy; override;
        procedure AfterConstruction; override;
        function Equals(const Item: ICollectable): Boolean; virtual;
        function GetKey: ICollectable; virtual;
    end;

    TAbstractStringMappable = class(TAbstractItem, IEquatable, IStringMappable)
    private
        FKey: String;
    protected
        function MakeKey: String; virtual; abstract;
    public
        procedure AfterConstruction; override;
        function Equals(const Item: ICollectable): Boolean; virtual;
        function GetKey: String; virtual;
    end;

    TAssociationWrapper = class(TAbstractItem, IEquatable, IMappable, IAssociationWrapper)
    private
        FAutoDestroy: Boolean;
        FKey: ICollectable;
        FValue: TObject;
    public
        constructor Create(const Key: ICollectable; Value: TObject); overload;
        constructor Create(Key: Integer; Value: TObject); overload;
        constructor Create(Key: String; Value: TObject); overload;
        constructor Create(Key, Value: TObject; AutoDestroyKey: Boolean = true); overload;
        destructor Destroy; override;
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetKey: ICollectable;
        function GetValue: TObject;
        function Equals(const Item: ICollectable): Boolean;
        property AutoDestroy: Boolean read GetAutoDestroy write SetAutoDestroy;
        property Key: ICollectable read GetKey;
        property Value: TObject read GetValue;
    end;

    TBooleanWrapper = class(TAbstractItem, IEquatable, IHashable, IComparable, IBoolean)
    private
        FValue: Boolean;
    public
        constructor Create(Value: Boolean);
        function GetValue: Boolean;
        function CompareTo(const Item: ICollectable): Integer;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        property Value: Boolean read GetValue;
    end;

    TCardinalWrapper = class(TAbstractItem, IEquatable, IHashable, IComparable, ICardinal)
    private
        FValue: Cardinal;
    public
        constructor Create(Value: Cardinal);
        function GetValue: Cardinal;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        function CompareTo(const Item: ICollectable): Integer;
        property Value: Cardinal read GetValue;
    end;

    TCharWrapper = class(TAbstractItem, IEquatable, IHashable, IComparable, IChar)
    private
        FValue: Char;
    public
        constructor Create(Value: Char);
        function GetValue: Char;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        function CompareTo(const Item: ICollectable): Integer;
        property Value: Char read GetValue;
    end;

    TClassWrapper = class(TAbstractItem, IEquatable, IHashable, IClass)
    private
        FValue: TClass;
    public
        constructor Create(Value: TClass);
        function GetValue: TClass;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        property Value: TClass read GetValue;
    end;

    TDoubleWrapper = class(TAbstractItem, IEquatable, IHashable, IComparable, IDouble)
    private
        FValue: Double;
    public
        constructor Create(Value: Double);
        function GetValue: Double;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        function CompareTo(const Item: ICollectable): Integer;
        property Value: Double read GetValue;
    end;

    TIntegerWrapper = class(TAbstractItem, IEquatable, IHashable, IComparable, IInteger)
    private
        FValue: Integer;
    public
        constructor Create(Value: Integer);
        function GetValue: Integer;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        function CompareTo(const Item: ICollectable): Integer;
        property Value: Integer read GetValue;
    end;

    TIntegerAssociationWrapper = class(TAbstractItem, IEquatable, IIntegerMappable, IIntegerAssociationWrapper)
    private
        FAutoDestroy: Boolean;
        FKey: Integer;
        FValue: TObject;
    public
        constructor Create(const Key: Integer; Value: TObject); overload;
        destructor Destroy; override;
        function Equals(const Item: ICollectable): Boolean;
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetKey: Integer;
        function GetValue: TObject;
        property AutoDestroy: Boolean read GetAutoDestroy write SetAutoDestroy;
        property Key: Integer read GetKey;
        property Value: TObject read GetValue;
    end;

    TInterfaceWrapper = class(TAbstractItem, IHashable, IEquatable, IInterfaceWrapper)
    private
        FValue: IUnknown;
    public
        constructor Create(const Value: IUnknown);
        destructor Destroy; override;
        function GetValue: IUnknown;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        property Value: IUnknown read GetValue;
    end;

    TObjectWrapper = class(TAbstractItem, IEquatable, IComparable, IHashable, IObject)
    private
        FAutoDestroy: Boolean;
        FValue: TObject;
    public
        constructor Create(Value: TObject); overload;
        destructor Destroy; override;
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetValue: TObject;
        function CompareTo(const Item: ICollectable): Integer;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        property AutoDestroy: Boolean read FAutoDestroy write FAutoDestroy;
        property Value: TObject read GetValue;
    end;

    TStringWrapper = class(TAbstractItem, IEquatable, IHashable, IComparable, IString)
    private
        FValue: String;
    public
        constructor Create(Value: String);
        function GetValue: String;
        function Equals(const Item: ICollectable): Boolean;
        function HashCode: Integer;
        function CompareTo(const Item: ICollectable): Integer;
        property Value: String read FValue;
    end;

    TStringAssociationWrapper = class(TAbstractItem, IEquatable, IStringMappable, IStringAssociationWrapper)
    private
        FAutoDestroy: Boolean;
        FKey: String;
        FValue: TObject;
    public
        constructor Create(const Key: String; Value: TObject); overload;
        destructor Destroy; override;
        function GetAutoDestroy: Boolean;
        procedure SetAutoDestroy(Value: Boolean);
        function GetKey: String;
        function GetValue: TObject;
        function Equals(const Item: ICollectable): Boolean;
        property AutoDestroy: Boolean read GetAutoDestroy write SetAutoDestroy;
        property Key: String read GetKey;
        property Value: TObject read GetValue;
    end;

implementation

{ TAbstractItem }
function TAbstractItem.GetInstance: TObject;
begin
    Result := Self;
end;


{ TAbstractIntegerMappable }
procedure TAbstractIntegerMappable.AfterConstruction;
begin
    inherited AfterConstruction;
    FKey := MakeKey;
end;

function TAbstractIntegerMappable.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self = Item.GetInstance);
end;

function TAbstractIntegerMappable.GetKey: Integer;
begin
    Result := FKey;
end;

{ TAbstractMappable }
destructor TAbstractMappable.Destroy;
begin
    FKey := nil;
    inherited Destroy;
end;

procedure TAbstractMappable.AfterConstruction;
begin
    inherited AfterConstruction;
    FKey := MakeKey;
end;

function TAbstractMappable.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self = Item.GetInstance);
end;

function TAbstractMappable.GetKey: ICollectable;
begin
    Result := FKey;
end;

{ TAbstractStringMappable }
procedure TAbstractStringMappable.AfterConstruction;
begin
    inherited AfterConstruction;
    FKey := MakeKey;
end;

function TAbstractStringMappable.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self = Item.GetInstance);
end;

function TAbstractStringMappable.GetKey: String;
begin
    Result := FKey;
end;

{ TAssociationWrapper }
constructor TAssociationWrapper.Create(const Key: ICollectable; Value: TObject);
begin
    inherited Create;
    FAutoDestroy := true;
    FKey := Key;
    FValue := Value;
end;

constructor TAssociationWrapper.Create(Key: Integer; Value: TObject);
begin
    Create(TIntegerWrapper.Create(Key) as ICollectable, Value);
end;

constructor TAssociationWrapper.Create(Key: String; Value: TObject);
begin
    Create(TStringWrapper.Create(Key) as ICollectable, Value);
end;

constructor TAssociationWrapper.Create(Key, Value: TObject; AutoDestroyKey: Boolean);
var
    KeyWrapper: TObjectWrapper;
begin
    KeyWrapper := TObjectWrapper.Create(Key);
    KeyWrapper.AutoDestroy := AutoDestroyKey;
    Create(KeyWrapper as ICollectable, Value);
end;

destructor TAssociationWrapper.Destroy;
begin
    if FAutoDestroy then
        FValue.Free;
    FKey := nil;
    inherited Destroy;
end;

function TAssociationWrapper.GetAutoDestroy: Boolean;
begin
    Result := FAutoDestroy;
end;

procedure TAssociationWrapper.SetAutoDestroy(Value: Boolean);
begin
    FAutoDestroy := Value;
end;

function TAssociationWrapper.GetKey: ICollectable;
begin
    Result := FKey;
end;

function TAssociationWrapper.GetValue: TObject;
begin
    Result := FValue;
end;

function TAssociationWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TAssociationWrapper).Value)
end;

{ TCardinalWrapper }
constructor TCardinalWrapper.Create(Value: Cardinal);
begin
    inherited Create;
    FValue := Value;
end;

function TCardinalWrapper.GetValue: Cardinal;
begin
    Result := FValue;
end;

function TCardinalWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TCardinalWrapper).Value)
end;

function TCardinalWrapper.HashCode: Integer;
begin
    Result := FValue;
end;

function TCardinalWrapper.CompareTo(const Item: ICollectable): Integer;
var
    Value2: Cardinal;
begin
    Value2 := (Item.GetInstance as TCardinalWrapper).Value;
    if Value < Value2 then
        Result := -1
    else if Value > Value2 then
        Result := 1
    else
        Result := 0;
end;

{ TBooleanWrapper }
constructor TBooleanWrapper.Create(Value: Boolean);
begin
    inherited Create;
    FValue := Value;
end;

function TBooleanWrapper.GetValue: Boolean;
begin
    Result := FValue;
end;

function TBooleanWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TBooleanWrapper).Value)
end;

function TBooleanWrapper.HashCode: Integer;
begin
    Result := Ord(FValue);
end;

function TBooleanWrapper.CompareTo(const Item: ICollectable): Integer;
var
    Value2: Boolean;
begin
    Value2 := (Item.GetInstance as TBooleanWrapper).Value;
    if not Value and Value2 then
        Result := -1
    else if Value and not Value2 then
        Result := 1
    else
        Result := 0;
end;

{ TCharWrapper }
constructor TCharWrapper.Create(Value: Char);
begin
    inherited Create;
    FValue := Value;
end;

function TCharWrapper.GetValue: Char;
begin
    Result := FValue;
end;

function TCharWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TCharWrapper).Value)
end;

function TCharWrapper.HashCode: Integer;
begin
    Result := Integer(FValue);
end;

function TCharWrapper.CompareTo(const Item: ICollectable): Integer;
var
    Value2: Char;
begin
    Value2 := (Item.GetInstance as TCharWrapper).Value;
    if Value < Value2 then
        Result := -1
    else if Value > Value2 then
        Result := 1
    else
        Result := 0;
end;

{ TClassWrapper }
constructor TClassWrapper.Create(Value: TClass);
begin
    inherited Create;
    FValue := Value;
end;

function TClassWrapper.GetValue: TClass;
begin
    Result := FValue;
end;

function TClassWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TClassWrapper).Value)
end;

function TClassWrapper.HashCode: Integer;
begin
    Result := Integer(FValue.ClassInfo);
end;

{ TDoubleWrapper }
constructor TDoubleWrapper.Create(Value: Double);
begin
    inherited Create;
    FValue := Value;
end;

function TDoubleWrapper.GetValue: Double;
begin
    Result := FValue;
end;

function TDoubleWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TDoubleWrapper).Value)
end;

function TDoubleWrapper.HashCode: Integer;
var
    DblAsInt: array[0..1] of Integer;
begin
    Double(DblAsInt) := Value;
    Result := DblAsInt[0] xor DblAsInt[1];
end;

function TDoubleWrapper.CompareTo(const Item: ICollectable): Integer;
var
    Value2: Double;
begin
    Value2 := (Item.GetInstance as TDoubleWrapper).Value;
    if Value < Value2 then
        Result := -1
    else if Value > Value2 then
        Result := 1
    else
        Result := 0;
end;

{ TIntegerWrapper }
constructor TIntegerWrapper.Create(Value: Integer);
begin
    inherited Create;
    FValue := Value;
end;

function TIntegerWrapper.GetValue: Integer;
begin
    Result := FValue;
end;

function TIntegerWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TIntegerWrapper).Value)
end;

function TIntegerWrapper.HashCode: Integer;
begin
    Result := FValue;
end;

function TIntegerWrapper.CompareTo(const Item: ICollectable): Integer;
var
    Value2: Integer;
begin
    Value2 := (Item.GetInstance as TIntegerWrapper).Value;
    if Value < Value2 then
        Result := -1
    else if Value > Value2 then
        Result := 1
    else
        Result := 0;
end;

{ TIntegerAssociationWrapper }
constructor TIntegerAssociationWrapper.Create(const Key: Integer; Value: TObject);
begin
    inherited Create;
    FAutoDestroy := true;
    FKey := Key;
    FValue := Value;
end;

destructor TIntegerAssociationWrapper.Destroy;
begin
    if FAutoDestroy then
        FValue.Free;
    inherited Destroy;
end;

function TIntegerAssociationWrapper.GetAutoDestroy: Boolean;
begin
    Result := FAutoDestroy;
end;

procedure TIntegerAssociationWrapper.SetAutoDestroy(Value: Boolean);
begin
    FAutoDestroy := Value;
end;

function TIntegerAssociationWrapper.GetValue: TObject;
begin
    Result := FValue;
end;

function TIntegerAssociationWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TIntegerAssociationWrapper).Value)
end;

function TIntegerAssociationWrapper.GetKey: Integer;
begin
    Result := FKey;
end;

{ TStringAssociationWrapper }
constructor TStringAssociationWrapper.Create(const Key: String; Value: TObject);
begin
    inherited Create;
    FAutoDestroy := true;
    FKey := Key;
    FValue := Value;
end;

destructor TStringAssociationWrapper.Destroy;
begin
    if FAutoDestroy then
        FValue.Free;
    inherited Destroy;
end;

function TStringAssociationWrapper.GetAutoDestroy: Boolean;
begin
    Result := FAutoDestroy;
end;

procedure TStringAssociationWrapper.SetAutoDestroy(Value: Boolean);
begin
    FAutoDestroy := Value;
end;

function TStringAssociationWrapper.GetValue: TObject;
begin
    Result := FValue;
end;

function TStringAssociationWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TStringAssociationWrapper).Value)
end;

function TStringAssociationWrapper.GetKey: String;
begin
    Result := FKey;
end;

{ TInterfaceWrapper }
constructor TInterfaceWrapper.Create(const Value: IUnknown);
begin
    inherited Create;
    FValue := Value;
end;

destructor TInterfaceWrapper.Destroy;
begin
    FValue := nil;
    inherited Destroy;
end;

function TInterfaceWrapper.GetValue: IUnknown;
begin
    Result := FValue;
end;

function TInterfaceWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TInterfaceWrapper).Value)
end;

function TInterfaceWrapper.HashCode: Integer;
begin
    Result := Integer(Pointer(FValue));
end;

{ TObjectWrapper }
constructor TObjectWrapper.Create(Value: TObject);
begin
    inherited Create;
    FAutoDestroy := true;
    FValue := Value;
end;

destructor TObjectWrapper.Destroy;
begin
    if FAutoDestroy then
        FValue.Free;
    inherited Destroy;
end;

function TObjectWrapper.GetAutoDestroy: Boolean;
begin
    Result := FAutoDestroy;
end;

procedure TObjectWrapper.SetAutoDestroy(Value: Boolean);
begin
    FAutoDestroy := Value;
end;

function TObjectWrapper.GetValue: TObject;
begin
    Result := FValue;
end;

function TObjectWrapper.CompareTo(const Item: ICollectable): Integer;
var
    Value1, Value2: Integer;
begin
    Value1 := Integer(Pointer(Self));
    if Item <> nil then
        Value2 := Integer(Pointer(Item))
    else
        Value2 := Low(Integer);
    if (Value1 < Value2) then
        Result := -1
    else if (Value1 > Value2) then
        Result := 1
    else
        Result := 0;
end;

function TObjectWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TObjectWrapper).Value)
end;

function TObjectWrapper.HashCode: Integer;
begin
    Result := Integer(Pointer(FValue));
end;

{ TStringWrapper }
constructor TStringWrapper.Create(Value: String);
begin
    inherited Create;
    FValue := Value;
end;

function TStringWrapper.GetValue: String;
begin
    Result := FValue;
end;

function TStringWrapper.Equals(const Item: ICollectable): Boolean;
begin
    Result := (Self.Value = (Item.GetInstance as TStringWrapper).Value)
end;

function TStringWrapper.HashCode: Integer;
var
    I: Integer;
begin
    Result := 0;
    for I := 1 to Length(FValue) do
        Result := (Result shl 1) xor Ord(FValue[I]);
end;

function TStringWrapper.CompareTo(const Item: ICollectable): Integer;
begin
    Result := CompareStr(Self.Value, (Item.GetInstance as TStringWrapper).Value)
end;


end.
