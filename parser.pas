unit parser;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Generics.Collections,
  expression, token;

type
  TParseError = class(Exception);

  TParser = class
  private
    FTokens: TObjectList<TToken>;
    FCurrent: integer;

    function IsAtEnd(): boolean;
    function Check(tk: TTokenKind): boolean;
    function Advance: TToken;
    function Match(atk: array of TTokenKind): boolean;
    function Peek(): TToken;
    function Previous(): TToken;
    function Consume(tk: TTokenKind; msg: String): TToken;
    function Error(t: TToken; msg: String): TParseError;

    //function PeekNext: char;
    function Expression(): TExpression;
    function Equality(): TExpression;
    function Comparison(): TExpression;
    function Addition(): TExpression;
    function Multiplication(): TExpression;
    function Unary(): TExpression;
    function Primary(): TExpression;
  public
    constructor Create(tokens: TObjectList<TToken>);
    function Parse: TExpression;
    destructor Destroy; override;
  end;

implementation

uses lox;

constructor TParser.Create(tokens: TObjectList<TToken>);
begin
  FTokens := tokens;
  Fcurrent := 0;
end;

destructor TParser.Destroy;
begin
  // FTokens.Free();
  inherited;
end;

function TParser.Parse: TExpression;
begin
  try
    Exit(Expression())
  except
  on TParseError do
    Exit(nil)
  end;
end;

function TParser.IsAtEnd(): boolean;
begin
  Result := Peek().tokenKind = TTokenKind.tkEOF;
end;

function TParser.Check(tk: TTokenKind): boolean;
begin
  if IsAtEnd() then
    Result := false
  else
    Result := Peek().tokenKind = tk;
end;

function TParser.Match(atk: array of TTokenKind): boolean;
var
  tk: TTokenKind;
begin
  Result := false;
  for tk in atk do
    if Check(tk) then
    begin
      Advance();
      Result := true;
      break;
    end;
end;

function TParser.Consume(tk: TTokenKind; msg: String): TToken;
begin
   if Check(tk) then
     Exit(Advance());

   raise Error(Peek(), msg);
end;

function TParser.Advance(): TToken;
begin
  Result := FTokens[FCurrent];
  if not IsAtEnd() then Inc(FCurrent);
end;

function TParser.Peek(): TToken;
begin
  Result := FTokens[FCurrent];
end;

function TParser.Previous(): TToken;
begin
  Result := FTokens[FCurrent-1];
end;

function TParser.Error(t: TToken; msg: String): TParseError;
begin
   TLox.Error(t, msg);
   Result := TParseError.Create(msg);
end;

{
expression     → equality ;
}
function TParser.Expression(): TExpression;
begin
  Result := Equality()
end;

{
equality → comparison ( ( "!=" | "==" ) comparison )* ;
}
function TParser.Equality(): TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin
    expr := Comparison();

    while Match([TTokenKind.tkBANG_EQUAL, TTokenKind.tkEQUAL_EQUAL]) do
    begin
      op := Previous();
      right := Comparison();
      expr := TBinaryExpression.Create(op, expr, right);
    end;
    Result := expr;
end;

{
comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
}
function TParser.Comparison(): TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin
  expr := Addition();

  while match([TTokenKind.tkGREATER, TTokenKind.tkGREATER_EQUAL, TTokenKind.tkLESS, TTokenKind.tkLESS_EQUAL]) do
  begin
      op := Previous();
      right := Addition();
      expr := TBinaryExpression.Create(op, expr, right);
  end;

  Result := expr;
end;

function TParser.Addition(): TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin
  expr := Multiplication();

  while Match([TTokenKind.tkMINUS, TTokenKind.tkPLUS]) do
  begin
    op := Previous();
    right := Multiplication();
    expr := TBinaryExpression.Create(op, expr, right);
  end;

  Result := expr;
end;

function TParser.Multiplication(): TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin
  expr := Unary();

  while match([TTokenKind.tkSLASH, TTokenKind.tkSTAR]) do
  begin
    op := Previous();
    right := Unary();
    expr := TBinaryExpression.Create(op, expr, right);
  end;

  Result := expr;
end;

{
unary → ( "!" | "-" ) unary | primary ;
}
function TParser.Unary(): TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin

  if match([TTokenKind.tkBANG, TTokenKind.tkMINUS]) then
  begin
      op := Previous();
      right := Unary();
      Result := TUnaryExpression.Create(op, right);
  end
  else
    Result := Primary();
end;

{
primary → NUMBER | STRING | "false" | "true" | "nil"
        | "(" expression ")" ;
}
function TParser.Primary(): TExpression;
var
  expr: TExpression;
begin
    if match([TTokenKind.tkFALSE]) then
      Exit(TLiteralExpression.Create(false));
    if match([TTokenKind.tkTRUE]) then
      Exit(TLiteralExpression.Create(true));
    if match([TTokenKind.tkNIL]) then
      Exit(TLiteralExpression.Create(nil));

    if match([TTokenKind.tkNUMBER, TTokenKind.tkSTRING]) then
      Exit(TLiteralExpression.Create(Previous().literal));

    if match([TTokenKind.tkLEFT_PAREN]) then
    begin
      expr := Expression();
      Consume(TTokenKind.tkRIGHT_PAREN, 'Expect '')'' after expression.');
      Exit(TGroupingExpression.Create(expr));
    end;

    raise Error(peek(), 'Expect expression.');
end;

end.

