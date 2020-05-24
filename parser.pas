unit parser;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Generics.Collections,
  expression, statement, token;

type
  // TO DO !
  TParseError = class(Exception);

  { TParser }

  TParser = class
  private
    FTokens: TObjectList<TToken>;
    FCurrent: integer;

    function IsAtEnd: boolean;
    function Check(tk: TTokenKind): boolean;
    function Advance: TToken;
    function Match(atk: array of TTokenKind): boolean;
    function Peek: TToken;
    function Previous: TToken;
    function Consume(tk: TTokenKind; msg: string): TToken;
    function Error(t: TToken; msg: string): TParseError;
    procedure Synchronize;

    (* Expressions rules *)
    { expression : assignment }
    function Expression: TExpression;
    { assignment : IDENTIFIER "=" assignment | logical_or }
    function Assignment: TExpression;
    { logic_or : logic_and ( "or" logic_and )* }
    function LogicalOr: TExpression;
    { logic_and : equality ( "and" equality )* }
    function LogicalAnd: TExpression;
    { equality : comparison ( ( "!=" | "==" ) comparison )* }
    function Equality: TExpression;
    { comparison : addition ( ( ">" | ">=" | "<" | "<=" ) addition )* }
    function Comparison: TExpression;
    { addition : multiplication ( ( "-" | "+" ) multiplication )* }
    function Addition: TExpression;
    { multiplication : unary ( ( "/" | "*" ) unary )* }
    function Multiplication: TExpression;
    { unary : ( "!" | "-" ) unary | primary }
    function Unary: TExpression;
    { primary : NUMBER | STRING | "false" | "true" | "nil" |
      "(" expression ")" | IDENTIFIER }
    function Primary: TExpression;

    (* Statements rules *)
    { declaration : varDecl | statement }
    function Declaration: TStatement;
    { statement : exprStmt | ifStmt | whileStmt | forStmt | printStmt | printDotStm | block }
    function Statement: TStatement;
    { ifStmt : "if" "(" expression ")" statement ( "else" statement )? }
    function IfStatement: TStatement;

    { whileStmt : "while" "(" expression ")" statement }
    function WhileStatement: TStatement;
    { forStmt : "for" "(" ( varDecl | exprStmt | ";" )
                          expression? ";"
                          expression? ")" statement }
    function ForStatement: TStatement;

    { exprStmt : expression ";" }
    function ExprStatement: TStatement;
    { printStmt : "print" expression ";" }
    function PrintStatement: TStatement;
    { printDotStmt : "printdot" expression ";" }
    function PrintDOTStatement: TStatement;
    { varDecl : "var" IDENTIFIER ( "=" expression )? ";" }
    function VarDeclaration: TStatement;
    (* block: "{" declaration* "}" *)
    function Block: TObjectList<TStatement>;
  public
    constructor Create(tokens: TObjectList<TToken>);
    { do job }
    {
    function Parse: TExpression;
    }
    function Parse: TObjectList<TStatement>;
    destructor Destroy; override;
  end;

implementation

uses lox, ptypes, rterror;

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

{
function TParser.Parse: TExpression;
begin
  try
    Exit(Expression())
  except
  on TParseError do
    Exit(nil)
  end;
end;
}

function TParser.Parse: TObjectList<TStatement>;
var
  stm: TObjectList<TStatement>;
begin
  stm := TObjectList<TStatement>.Create(True); // Owns statements
  while not self.IsAtEnd() do
    { stm.Add(self.Statement()); }
    stm.Add(self.Declaration());
  Result := stm;
end;

function TParser.IsAtEnd: boolean;
begin
  Result := (Peek().tokenKind = TTokenKind.tkEOF);
end;

function TParser.Check(tk: TTokenKind): boolean;
begin
  if IsAtEnd() then
    Result := False
  else
    Result := Peek().tokenKind = tk;
end;

function TParser.Match(atk: array of TTokenKind): boolean;
var
  tk: TTokenKind;
begin
  Result := False;
  for tk in atk do
    if Check(tk) then
    begin
      Advance();
      Result := True;
      break;
    end;
end;

function TParser.Consume(tk: TTokenKind; msg: string): TToken;
begin
  if Check(tk) then
    Exit(Advance());

  raise Error(Peek(), msg);
end;

function TParser.Advance: TToken;
begin
  Result := FTokens[FCurrent];
  if not IsAtEnd() then
    Inc(FCurrent);
end;

function TParser.Peek: TToken;
begin
  Result := FTokens[FCurrent];
end;

function TParser.Previous: TToken;
begin
  Result := FTokens[FCurrent - 1];
end;

function TParser.Error(t: TToken; msg: string): TParseError;
begin
  TLox.Error(t, msg);
  Result := TParseError.Create(msg);
end;

procedure TParser.Synchronize;
begin
  self.Advance();

  while not self.IsAtEnd() do
  begin

    if self.Previous.tokenKind = TTokenKind.tkSEMICOLON then
      Exit();

    case self.Peek.tokenKind of
      TTokenKind.tkCLASS, TTokenKind.tkFUN, TTokenKind.tkVAR,
      TTokenKind.tkFOR, TTokenKind.tkIF, TTokenKind.tkWHILE,
      TTokenKind.tkPRINT, TTokenKind.tkPRINTDOT, TTokenKind.tkRETURN:
        Exit();
    end;

    self.Advance();
  end;
end;

function TParser.Expression: TExpression;
begin
  Result := Assignment(); // Equality();
end;

function TParser.Assignment: TExpression;
var
  expr, eval: Texpression;
  eq, nm: TToken;
begin
  // expr := self.Equality();
  expr := self.LogicalOr();

  if self.Match([TTokenKind.tkEQUAL]) then
  begin
    eq := self.Previous();
    eval := self.Assignment();
    if expr is TVariableExpression then
    begin
      nm := TVariableExpression(expr).varName;
      Exit(TAssignmentExpression.Create(nm, eval));
    end;
    self.Error(eq, 'Invalid assignment target.');
  end;

  Result := expr;
end;

function TParser.LogicalOr: TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin
  expr := self.LogicalAnd();

  while self.Match([TTokenKind.tkOR]) do
  begin
    op := self.Previous;
    right := self.LogicalAnd();
    expr := TLogicalExpression.Create(op, expr, right);
  end;

  Result := expr;
end;

function TParser.LogicalAnd: TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin
  expr := self.Equality();

  while self.Match([TTokenKind.tkAND]) do
  begin
    op := self.Previous;
    right := self.Equality();
    expr := TLogicalExpression.Create(op, expr, right);
  end;

  Result := expr;
end;

function TParser.Equality: TExpression;
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

function TParser.Comparison: TExpression;
var
  expr, right: TExpression;
  op: TToken;
begin
  expr := Addition();

  while match([TTokenKind.tkGREATER, TTokenKind.tkGREATER_EQUAL,
      TTokenKind.tkLESS, TTokenKind.tkLESS_EQUAL]) do
  begin
    op := Previous();
    right := Addition();
    expr := TBinaryExpression.Create(op, expr, right);
  end;

  Result := expr;
end;

function TParser.Addition: TExpression;
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

function TParser.Multiplication: TExpression;
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

function TParser.Unary: TExpression;
var
  right: TExpression;
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


// TO DO RWRT
function TParser.Primary: TExpression;
var
  expr: TExpression;
begin
  if match([TTokenKind.tkFALSE]) then
    Exit(TLiteralExpression.Create(TLoxBool.Create(False)));
  if match([TTokenKind.tkTRUE]) then
    Exit(TLiteralExpression.Create(TLoxBool.Create(True)));
  if match([TTokenKind.tkNIL]) then
    Exit(TLiteralExpression.Create(nil));

  if match([TTokenKind.tkNUMBER, TTokenKind.tkSTRING]) then
    Exit(TLiteralExpression.Create(Previous().literal));

  if match([TTokenKind.tkIDENTIFIER]) then
    Exit(TVariableExpression.Create(self.Previous()));

  if match([TTokenKind.tkLEFT_PAREN]) then
  begin
    expr := Expression();
    Consume(TTokenKind.tkRIGHT_PAREN, 'Expect '')'' after expression.');
    Exit(TGroupingExpression.Create(expr));
  end;

  raise Error(peek(), 'Expect expression.');
end;

function TParser.Declaration: TStatement;
begin
  try
    if self.Match(TTokenKind.tkVAR) then
      Exit(self.VarDeclaration());
    Exit(self.Statement());
  except
    on e: ERunTimeError do
    begin
      self.Synchronize();
      Exit(nil);
    end;
  end;
end;

// TO DO RWRT
function TParser.Statement: TStatement;
begin
  if self.Match([TTokenKind.tkIF]) then
    Exit(self.IfStatement());

  if self.Match([TTokenKind.tkWHILE]) then
    Exit(self.WhileStatement());

  if self.Match([TTokenKind.tkFOR]) then
    Exit(self.ForStatement());

  if self.Match([TTokenKind.tkPRINT]) then
    Exit(self.PrintStatement());

  if self.Match([TTokenKind.tkPRINTDOT]) then
    Exit(self.PrintDOTStatement());

  if self.Match([TTokenKind.tkLEFT_BRACE]) then
    Exit(TBlockStatement.Create(self.Block()));

  Exit(self.ExprStatement());
end;

function TParser.IfStatement: TStatement;
var
  cond: TExpression;
  thenStm, elseStm: TStatement;
begin
  cond := nil;
  thenStm := nil;
  elseStm := nil;

  self.Consume(TTokenKind.tkLEFT_PAREN, 'Expect ''('' after ''if''.');
  cond := self.Expression();
  self.Consume(TTokenKind.tkRIGHT_PAREN, 'Expect '')'' after if condition.');

  thenStm := self.Statement();

  if self.Match([TTokenKind.tkELSE]) then
    elseStm := self.Statement();

  Result := TIfStatement.Create(cond, thenStm, elseStm);
end;

function TParser.WhileStatement: TStatement;
var
  cond: TExpression;
  body: TStatement;
begin
  cond := nil;
  body := nil;

  self.Consume(TTokenKind.tkLEFT_PAREN, 'Expect ''('' after ''while''.');
  cond := self.Expression();
  self.Consume(TTokenKind.tkRIGHT_PAREN, 'Expect '')'' after condition.');

  body := self.Statement();
  Result := TWhileStatement.Create(cond, body);
end;

// TO DO RW
function TParser.ForStatement: TStatement;
var
  init: TStatement;
  cond, incr: TExpression;
  body: TStatement;
  block: TObjectList<TStatement>;
begin
  init := nil;
  cond := nil;
  incr := nil;
  body := nil;
  block := nil;

  self.Consume(TTokenKind.tkLEFT_PAREN, 'Expect ''('' after ''for''.');
  if self.Match([TTokenKind.tkSEMICOLON]) then
  else
    if self.Match([TTokenKind.tkVAR]) then
      init := self.VarDeclaration()
    else
      init := self.ExprStatement();

  if not self.Check(TTokenKind.tkSEMICOLON) then
    cond := self.Expression();
  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after loop condition.');

  if not self.Check(TTokenKind.tkRIGHT_PAREN) then
    incr := self.Expression();
  self.Consume(TTokenKind.tkRIGHT_PAREN, 'Expect '')'' after for clauses.');

  body := self.Statement();

  if incr <> nil then
  begin
    block := TObjectList<TStatement>.Create(True);
    block.Add(body);
    block.Add(TExpressionStatement.Create(incr));
    body := TBlockStatement.Create(block);
  end;

  if cond = nil then
    cond := TLiteralExpression.Create(TLoxBool.Create(true));
  body := TWhileStatement.Create(cond, body);

  if init <> nil then
  begin
    block := TObjectList<TStatement>.Create(True);
    block.Add(init);
    block.Add(body);
    body := TBlockStatement.Create(block);
  end;

  Result := body;
end;

function TParser.ExprStatement: TStatement;
var
  expr: TExpression;
begin
  expr := self.Expression();
  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after expression.');
  Result := TExpressionStatement.Create(expr);
end;

function TParser.PrintStatement: TStatement;
var
  expr: TExpression;
begin
  expr := self.Expression();
  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after expression.');
  Result := TPrintStatement.Create(expr);
end;

function TParser.PrintDOTStatement: TStatement;
var
  expr: TExpression;
begin
  expr := self.Expression();
  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after expression.');
  Result := TPrintDOTStatement.Create(expr);
end;

function TParser.VarDeclaration: TStatement;
var
  tok: TToken;
  init: TExpression;
begin
  tok := self.Consume(TTokenKind.tkIDENTIFIER, 'Expect variable name.');
  init := nil;

  if self.Match(TTokenKind.tkEQUAL) then
    init := self.Expression();

  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after variable declaration.');
  Result := TVariableStatement.Create(tok, init);
end;

function TParser.Block: TObjectList<TStatement>;
var
  bs: TObjectList<TStatement>;
begin
  bs := TObjectList<TStatement>.Create(True);

  while (not self.IsAtEnd()) and (not self.Check(TTokenKind.tkRIGHT_BRACE)) do
    bs.Add(self.Declaration());

  self.Consume(TTokenKind.tkRIGHT_BRACE, 'Expect ''}'' after block.');

  Result := bs;
end;

end.
