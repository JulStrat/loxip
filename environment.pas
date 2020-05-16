{
  Crafting Interpreters
  https://craftinginterpreters.com/
}
unit environment;

{$ifdef FPC}
{$mode delphi}
{$endif}

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
    constructor Create; overload;
    constructor Create(encl: TEnvironment); overload;
    destructor Destroy; override;
    function Get(tok: TToken): TObject;
    procedure Assign(tok: TToken; val: TObject);
    procedure Define(vname: string; val: TObject);
  end;

implementation

uses ptypes, rterror;

constructor TEnvironment.Create;
begin
  self.FEnclosing := nil;
  FValues := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;

constructor TEnvironment.Create(encl: TEnvironment);
begin
  self.FEnclosing := encl;
  FValues := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
end;

destructor TEnvironment.Destroy;
begin
  FreeAndNil(self.FValues);
  inherited Destroy;
end;

function TEnvironment.Get(tok: TToken): TObject;
var
  val: TObject;
begin
  //writeln('DICT - ', self.FValues.Count);
  if FValues.TryGetValue(tok.lexeme, val) then
  begin
    if val is TLoxObject then
      val := TLoxObject(val).Clone;
    Exit(val);
  end;
  if self.FEnclosing <> nil then
    Exit(self.FEnclosing.Get(tok));
  raise ERuntimeError.Create(tok, Format('Undefined variable ''%s''.', [tok.lexeme]));
end;

procedure TEnvironment.Assign(tok: TToken; val: TObject);
var
  v: TObject;
begin
  if FValues.TryGetValue(tok.lexeme, v) then
  begin
    if val is TLoxObject then
      val := TLoxObject(val).Clone;
    FValues.Items[tok.lexeme] := val;
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
var
  pair: TPair<string, TObject>;
begin
  if val is TLoxObject then
    val := TLoxObject(val).Clone;

  FValues.AddOrSetValue(vname, val);
end;

initialization

finalization

end.
