{
  Crafting Interpreters
  https://craftinginterpreters.com/
}
unit environment;

{$ifdef FPC}
{$mode delphi}
{$endif}

{$DEFINE ENVIR_BENCH}

interface

uses
  Classes, SysUtils, Generics.Collections
  , token;

type

  { TEnvironment }

  TEnvironment = class
  private
    FEnclosing: TEnvironment;
    FValues: TObjectDictionary<string, TObject>;
  public
    constructor Create; // overload;
    // constructor Create(encl: TEnvironment); overload;
    destructor Destroy; override;
    function Get(tok: TToken): TObject;
    procedure Assign(tok: TToken; val: TObject);
    procedure Define(vname: string; val: TObject);
    procedure Finalize;
    property enclosing: TEnvironment read FEnclosing write FEnclosing;
  end;

implementation

uses ptypes, rterror;

var
  blackHole: TObject;
  {$IFDEF ENVIR_BENCH}
  getTicks, assignTicks, defineTicks: QWord;
  {$ENDIF}

constructor TEnvironment.Create;
begin
  self.FEnclosing := nil;
  //FValues := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FValues := TObjectDictionary<string, TObject>.Create();
end;

{
constructor TEnvironment.Create(encl: TEnvironment);
begin
  self.FEnclosing := encl;
  FValues := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;
}

destructor TEnvironment.Destroy;
var
  key: String;
begin
  FreeAndNil(self.FValues);
  inherited Destroy;
end;

function TEnvironment.Get(tok: TToken): TObject;
var
  val: TObject;
  {$IFDEF ENVIR_BENCH}
  delta: QWord;
  {$ENDIF}
begin
  if FValues.TryGetValue(tok.lexeme, val) then
  begin
    {$IFDEF ENVIR_BENCH}
    delta := GetTickCount64();
    {$ENDIF}
    if val is TLoxObject then
      val := TLoxObject(val).Clone;
    {$IFDEF ENVIR_BENCH}
    Inc(getTicks, GetTickCount64() - delta);
    {$ENDIF}
    Exit(val);
  end;
  if self.FEnclosing <> nil then
    Exit(self.FEnclosing.Get(tok));
  raise ERuntimeError.Create(tok, Format('Undefined variable ''%s''.', [tok.lexeme]));
end;

procedure TEnvironment.Assign(tok: TToken; val: TObject);
var
  v: TObject;
  {$IFDEF ENVIR_BENCH}
  delta: QWord;
  {$ENDIF}
begin
  if FValues.TryGetValue(tok.lexeme, v) then
  begin
    {$IFDEF ENVIR_BENCH}
    delta := GetTickCount64();
    {$ENDIF}
    if val is TLoxObject then
      val := TLoxObject(val).Clone;
    FValues.Items[tok.lexeme] := val;
    {$IFDEF ENVIR_BENCH}
    Inc(assignTicks, GetTickCount64() - delta);
    {$ENDIF}
    Exit;
  end;
  if self.FEnclosing <> nil then
  begin
    self.FEnclosing.Assign(tok, val);
    Exit;
  end;

  raise ERuntimeError.Create(tok, Format('Undefined variable ''%s''.', [tok.lexeme]));
end;

procedure TEnvironment.Define(vname: string; val: TObject);
{$IFDEF ENVIR_BENCH}
var
  delta: QWord;
{$ENDIF}
//  pair: TPair<string, TObject>;
begin
  {$IFDEF ENVIR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}
  if val is TLoxObject then
    val := TLoxObject(val).Clone;

  FValues.AddOrSetValue(vname, val);
  {$IFDEF ENVIR_BENCH}
  Inc(defineTicks, GetTickCount64() - delta);
  {$ENDIF}
end;

procedure TEnvironment.Finalize;
var
  key: String;
begin
  {
  for key in self.FValues.keys do
    self.FValues[key] := blackHole;
  }
end;

initialization
  blackHole := TObject.Create;
  {$IFDEF ENVIR_BENCH}
  getTicks := 0;
  assignTicks := 0;
  defineTicks := 0;
  {$ENDIF}
finalization
  FreeAndNil(blackHole);
  {$IFDEF ENVIR_BENCH}
  WriteLn('TEnvironment - getTicks: ', getTicks);
  WriteLn('TEnvironment - assignTicks: ', assignTicks);
  WriteLn('TEnvironment - defineTicks: ', defineTicks);
  {$ENDIF}
end.
