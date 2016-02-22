{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit curlpas; 

interface

uses
  lazcurl, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('lazcurl', @lazcurl.Register); 
end; 

initialization
  RegisterPackage('curlpas', @Register); 
end.
