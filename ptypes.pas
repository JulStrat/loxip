unit ptypes;

{$ifdef FPC}
{$mode delphi}
{$endif}

{$DEFINE CLONE_BENCH}

interface

uses Classes, SysUtils;

type
  TLoxObject = class(TObject)
    function Clone: TLoxObject; virtual; abstract;
  end;

  { TO DO TLoxTrue, TLoxFalse global objects }
  { TLoxBool }
  TLoxBool = class(TLoxObject)
    private
    FValue: Boolean;
    public
    constructor Create(v: Boolean);
    destructor Destroy; override;
    function Clone: TLoxObject; override;
    function ToString: ansistring; override;
    property value: Boolean read FValue;
  end;

  { TLoxIntNum }
  {
  TLoxIntNum = class(TLoxObject)
    private
    FValue: LongInt;
    public
    constructor Create(v: LongInt);
    destructor Destroy; override;
    function Clone: TLoxObject; override;
    function ToString: ansistring; override;
    property value: LongInt read FValue;
  end;
  }
  { TLoxNum }
  TLoxNum = class(TLoxObject)
    private
    FValue: Double;
    public
    constructor Create(v: Double);
    destructor Destroy; override;
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
    destructor Destroy; override;
    function Clone: TLoxObject; override;
    function ToString: ansistring; override;
    property value: String read FValue;
  end;

function ObjToStr(obj: TObject): ansistring;
function LoxBool(v: boolean): TLoxBool;
procedure FreeObj(obj: TObject);

var
  TLoxTrue, TLoxFalse: TLoxBool;

implementation

{$IFDEF CLONE_BENCH}
var
  clonedNumber: LongInt;
  clonedTicks: QWord;
{$ENDIF}

constructor TLoxBool.Create(v: Boolean);
begin
  FValue := v;
end;

destructor TLoxBool.Destroy;
begin
  inherited Destroy;
end;

function TLoxBool.Clone: TLoxObject;
{$IFDEF CLONE_BENCH}
var
  delta: QWord;
{$ENDIF}
begin
  {$IFDEF CLONE_BENCH}
  Inc(clonedNumber);
  delta := GetTickCount64();
  {$ENDIF}
  Result := self;
  { Result := TLoxBool.Create(self.FValue); }
  {$IFDEF CLONE_BENCH}
  Inc(clonedTicks, GetTickCount64() - delta);
  {$ENDIF}
end;

function TLoxBool.ToString: ansistring;
begin
  Result := BoolToStr(FValue, true);
end;

{
constructor TLoxIntNum.Create(v: LongInt);
begin
  FValue := v;
end;

destructor TLoxIntNum.Destroy;
begin
  inherited Destroy;
end;

function TLoxIntNum.Clone: TLoxObject;
begin
  Result := TLoxIntNum.Create(self.FValue);
end;

function TLoxIntNum.ToString: ansistring;
begin
  Result := Format('%d', [self.FValue]);
end;
}

constructor TLoxNum.Create(v: Double);
begin
  FValue := v;
end;

destructor TLoxNum.Destroy;
begin
  inherited Destroy;
end;

function TLoxNum.Clone: TLoxObject;
{$IFDEF CLONE_BENCH}
var
  delta: QWord;
{$ENDIF}
begin
  {$IFDEF CLONE_BENCH}
  Inc(clonedNumber);
  delta := GetTickCount64();
  {$ENDIF}
  Result := TLoxNum.Create(self.FValue);
  {$IFDEF CLONE_BENCH}
  Inc(clonedTicks, GetTickCount64() - delta);
  {$ENDIF}
end;

function TLoxNum.ToString: ansistring;
begin
  Result := Format('%g', [FValue]);
end;

constructor TLoxStr.Create(v: String);
begin
  FValue := v;
end;

destructor TLoxStr.Destroy;
begin
  inherited Destroy;
end;

function TLoxStr.Clone: TLoxObject;
{$IFDEF CLONE_BENCH}
var
  delta: QWord;
{$ENDIF}
begin
  {$IFDEF CLONE_BENCH}
  Inc(clonedNumber);
  delta := GetTickCount64();
  {$ENDIF}
  Result := TLoxStr.Create(self.FValue);
  {$IFDEF CLONE_BENCH}
  Inc(clonedTicks, GetTickCount64() - delta);
  {$ENDIF}
end;

function TLoxStr.ToString: ansistring;
begin
  Result := self.FValue;
end;

function ObjToStr(obj: TObject): ansistring;
begin
  if obj = nil then
    Result := 'nil'
  else
    Result := obj.ToString();
end;

function LoxBool(v: boolean): TLoxBool;
begin
  if v then
    Result := TLoxTrue
  else  
    Result := TLoxFalse;  
end;

procedure FreeObj(obj: TObject);
begin
  if not (obj is TLoxBool) then
    FreeAndNil(obj);
end;

initialization
  {$IFDEF CLONE_BENCH}
  clonedNumber := 0;
  clonedTicks := 0;
  {$ENDIF}

  TLoxTrue := TLoxBool.Create(true);
  TLoxFalse := TLoxBool.Create(false);

finalization
  {$IFDEF CLONE_BENCH}
  WriteLn('TLoxObject - clonedNumber: ', clonedNumber);
  WriteLn('TLoxObject - clonedTicks: ', clonedTicks);
  {$ENDIF}

  FreeObj(TLoxTrue);
  FreeObj(TLoxFalse);

end.

