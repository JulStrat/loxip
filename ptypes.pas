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
    function ToString: ansistring; override;
    property value: Boolean read FValue;
  end;

  { TLoxNum }
  TLoxNum = class(TObject)
    private
    FValue: Double;
    public
    constructor Create(v: Double);
    function ToString: ansistring; override;
    property value: Double read FValue;

  end;

  { TLoxStr }
  TLoxStr = class(TObject)
    private
    FValue: String;
    public
    constructor Create(v: String);
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

function TLoxBool.ToString: ansistring;
begin
  Result := BoolToStr(FValue, true);
end;

constructor TLoxNum.Create(v: Double);
begin
  FValue := v;
end;

function TLoxNum.ToString: ansistring;
begin
  Result := Format('%g', [FValue]);
end;

constructor TLoxStr.Create(v: String);
begin
  FValue := v;
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

