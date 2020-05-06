unit ptypes;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses Classes, SysUtils;

type
  { TLoxBool }
  TLoxBool = class(TObject)
    private
    FValue: Boolean;
    public
    constructor Create(v: Boolean);
    function Equals(obj: TObject): boolean; override;
    function ToString: ansistring; override;
    property value: Boolean read FValue;
  end;

  { TLoxNum }
  TLoxNum = class(TObject)
    private
    FValue: Double;
    public
    constructor Create(v: Double);
    function Equals(obj: TObject): boolean; override;
    function ToString: ansistring; override;
    property value: Double read FValue;
  end;

  { TLoxStr }
  TLoxStr = class(TObject)
    private
    FValue: String;
    public
    constructor Create(v: String);
    function Equals(obj: TObject): boolean; override;
    function ToString: ansistring; override;
    property value: String read FValue;
  end;

function ObjToStr(obj: TObject): ansistring;

implementation

uses Math;

constructor TLoxBool.Create(v: Boolean);
begin
  FValue := v;
end;

function TLoxBool.Equals(obj: TObject): boolean;
begin
  if obj = nil then
    Exit(false);
  if obj.ClassType <> TLoxBool then
    Exit(false);
  if self.FValue = TLoxBool(obj).FValue then
    Result := true
  else
    Result := false;
end;

function TLoxBool.ToString: ansistring;
begin
  Result := BoolToStr(FValue, true);
end;

constructor TLoxNum.Create(v: Double);
begin
  FValue := v;
end;

function TLoxNum.Equals(obj: TObject): boolean;
begin
  if obj = nil then
    Exit(false);
  if obj.ClassType <> TLoxNum then
    Exit(false);
  if Math.SameValue(self.FValue, TLoxNum(obj).FValue) then
    Result := true
  else
    Result := false;
end;

function TLoxNum.ToString: ansistring;
begin
  Result := Format('%f', [FValue]);
end;

constructor TLoxStr.Create(v: String);
begin
  FValue := v;
end;

function TLoxStr.Equals(obj: TObject): boolean;
begin
  if obj = nil then
    Exit(false);
  if obj.ClassType <> TLoxStr then
    Exit(false);
  if self.FValue = TLoxStr(obj).FValue then
    Result := true
  else
    Result := false;
end;

function TLoxStr.ToString: ansistring;
begin
  Result := Format('%s', [FValue]);
end;

function ObjToStr(obj: TObject): ansistring;
begin
  if obj = nil then
    Result := 'nil'
  else
    Result := obj.ToString;
end;

var
  lba, lbb, lbc: TLoxBool;
  lna, lnb, lnc: TLoxNum;
  lsa, lsb, lsc: TLoxStr;
initialization
begin
  WriteLn('DEBUG - Equality test.');
  lba := TLoxBool.Create(true);
  lbb := TLoxBool.Create(false);
  lbc := TLoxBool.Create(true);

  lna := TLoxNum.Create(1/3);
  lnb := TLoxNum.Create(1*1.00000000001/3.0);
  lnc := TLoxNum.Create(1/3);

  lsa := TLoxStr.Create('Hello');
  lsb := TLoxStr.Create('HellO');
  lsc := TLoxStr.Create('Hello');

  WriteLn(lba.value, ', ', lbb.value, ', ', lbc.value);
  if lba.Equals(lbb) then WriteLn('lba == lbb')
  else WriteLn('lba != lbb');
  if lba.Equals(lbc) then WriteLn('lba == lbc')
  else WriteLn('lba != lbc');

  WriteLn(lna.value, ', ', lnb.value, ', ', lnc.value);
  if lna.Equals(lnb) then WriteLn('lna == lnb')
  else WriteLn('lna != lnb');
  if lna.Equals(lnc) then WriteLn('lna == lnc')
  else WriteLn('lna != lnc');

  WriteLn(lsa.value, ', ', lsb.value, ', ', lsc.value);
  if lsa.Equals(lsb) then WriteLn('lsa == lsb')
  else WriteLn('lsa != lsb');
  if lsa.Equals(lsc) then WriteLn('lsa == lsc')
  else WriteLn('lsa != lsc');

  FreeAndNil(lba); FreeAndNil(lbb); FreeAndNil(lbc);
  FreeAndNil(lna); FreeAndNil(lnb); FreeAndNil(lnc);
  FreeAndNil(lsa); FreeAndNil(lsb); FreeAndNil(lsc);
end

finalization

end.

