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
    FValues: TDictionary<string, TObject>;
  public
    constructor Create; overload;
    constructor Create(encl: TEnvironment); overload;
    function Get(tok: TToken): TObject;
    procedure Assign(tok: TToken; val: TObject);
    procedure Define(vname: string; val: TObject);
  end;

implementation

uses rterror;

constructor TEnvironment.Create;
begin
  self.FEnclosing := nil;
  FValues := TDictionary<string, TObject>.Create();
end;

constructor TEnvironment.Create(encl: TEnvironment);
begin
  self.FEnclosing := encl;
  FValues := TDictionary<string, TObject>.Create();
end;

function TEnvironment.Get(tok: TToken): TObject;
var
  val: TObject;
begin
  if FValues.TryGetValue(tok.lexeme, val) then
    Exit(val);
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
begin
  FValues.AddOrSetValue(vname, val);
end;

initialization

finalization

end.
