{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazcommandpalette;

{$warn 5023 off : no warning about unused units}
interface

uses
  uLazCommandPalette, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uLazCommandPalette', @uLazCommandPalette.Register);
end;

initialization
  RegisterPackage('lazcommandpalette', @Register);
end.
