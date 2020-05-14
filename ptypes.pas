unit ptypes;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses Classes, SysUtils;

type
  TLoxObject = class(TObject)
    function Clone: TLoxObject; virtual; abstract;
  end;

  { TLoxBool }
  TLoxBool = class(TLoxObject)
    private
    FValue: Boolean;
    public
    constructor Create(v: Boolean);
    function Clone: TLoxObject; override;
    function ToString: ansistring; override;
    property value: Boolean read FValue;
  end;

  { TLoxNum }
  TLoxNum = class(TLoxObject)
    private
    FValue: Double;
    public
    constructor Create(v: Double);
    function Clone: TLoxObject; override;
    function ToString: ansistring; override;
    property value: Double read FValue;

  end;

  { TLoxStr }
  TLoxStr = class(TLoxObject)
    private
    FValue: String;
    public
    constructor Create(v: String);
    function Clone: TLoxObject; override;
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

function TLoxBool.Clone: TLoxObject;
begin
  Result := TLoxBool.Create(self.FValue);
end;

function TLoxBool.ToString: ansistring;
begin
  Result := BoolToStr(FValue, true);
end;

constructor TLoxNum.Create(v: Double);
begin
  FValue := v;
end;

function TLoxNum.Clone: TLoxObject;
begin
  Result := TLoxNum.Create(self.FValue);
end;

function TLoxNum.ToString: ansistring;
begin
  Result := Format('%g', [FValue]);
end;

constructor TLoxStr.Create(v: String);
begin
  FValue := v;
end;

function TLoxStr.Clone: TLoxObject;
begin
  Result := TLoxStr.Create(self.FValue);
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
    Result := obj.ToString();
end;

initialization

finalization

end.

