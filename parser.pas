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
  { TO DO implement break statement }
  { TO DO implement pow operator ^  }

  TParser = class(TObject)
  private
    FTokens: TObjectList<TToken>;
    FCurrent: integer;
    function Advance: TToken;
    function Peek: TToken;
    function Previous: TToken;
    function Check(tk: TTokenKind): boolean;
    function Consume(tk: TTokenKind; msg: string): TToken;
    function Match(atk: array of TTokenKind): boolean;
    function IsAtEnd: boolean;
    function Error(t: TToken; msg: string): TParseError;
    procedure Synchronize;

    { DECLARATIONS RULES }
    { declaration : varDecl | statement }
    function Declaration: TStatement;

    { varDecl : 'var' IDENTIFIER ( '=' expression )? ';' }
    function VarDeclaration: TStatement;

    { STATEMENTS RULES }
    { statement : exprStmt | ifStmt | whileStmt | forStmt | printStmt | printDotStm | block }
    function Statement: TStatement;

    { exprStmt : expression ';' }
    function ExprStatement: TStatement;

    { ifStmt : 'if' '(' expression ')' statement ( 'else' statement )? }
    function IfStatement: TStatement;

    { whileStmt : 'while' '(' expression ')' statement }
    function WhileStatement: TStatement;

    { forStmt : 'for' '(' ( varDecl | exprStmt | ';' )
                          expression? ';'
                          expression? ')' statement }
    function ForStatement: TStatement;

    { printStmt : 'print' expression ';' }
    function PrintStatement: TStatement;

    { printDotStmt : 'printdot' expression ';' }
    function PrintDOTStatement: TStatement;

    (* block: '{' declaration* '}' *)
    function Block: TObjectList<TStatement>;

    { EXPRESSIONS RULES }
    { expression : assignment }
    function Expression: TExpression;

    { assignment : IDENTIFIER '=' assignment | logical_or }
    function Assignment: TExpression;

    { logic_or : logic_and ( 'or' logic_and )* }
    function LogicalOr: TExpression;

    { logic_and : equality ( 'and' equality )* }
    function LogicalAnd: TExpression;

    { equality : comparison ( ( '!=' | '==' ) comparison )* }
    function Equality: TExpression;

    { comparison : addition ( ( '>' | '>=' | '<' | '<=' ) addition )* }
    function Comparison: TExpression;

    { addition : multiplication ( ( '-' | '+' ) multiplication )* }
    function Addition: TExpression;

    { multiplication : unary ( ( '/' | '*' ) unary )* }
    function Multiplication: TExpression;

    { unary : ( '!' | '-' ) unary | primary }
    function Unary: TExpression;

    { primary : NUMBER | STRING | 'false' | 'true' | 'nil' |
      '(' expression ')' | IDENTIFIER }
    function Primary: TExpression;

  public
    constructor Create(tokens: TObjectList<TToken>);
    destructor Destroy; override;
    function Parse: TObjectList<TStatement>;

  end;

implementation

uses lox, ptypes, rterror;

function TParser.Advance: TToken;
begin
  Result := FTokens[FCurrent];
  if not IsAtEnd() then
    Inc(FCurrent);
end;

function TParser.Peek: TToken;
begin
  Exit(FTokens[FCurrent]);
end;

function TParser.Previous: TToken;
begin
  Exit(FTokens[FCurrent - 1]);
end;

function TParser.Check(tk: TTokenKind): boolean;
begin
  if not IsAtEnd() then
    Exit(Peek().tokenKind = tk)
  else
    Exit(False);
end;

function TParser.Consume(tk: TTokenKind; msg: string): TToken;
begin
  if Check(tk) then
    Exit(Advance());

  raise Error(Peek(), msg);
end;

function TParser.Match(atk: array of TTokenKind): boolean;
var
  tk: TTokenKind;
begin
  for tk in atk do
    if Check(tk) then
    begin
      Advance();
      Exit(True);
    end;
  Exit(False);
end;

function TParser.IsAtEnd: boolean;
begin
  Exit(Peek().tokenKind = TTokenKind.tkEOF);
end;

function TParser.Error(t: TToken; msg: string): TParseError;
begin
  TLox.Error(t, msg);
  Exit(TParseError.Create(msg));
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

(*** DECLARATIONS RULES ***)
function TParser.Declaration: TStatement;
begin
  try
    if self.Match(TTokenKind.tkVAR) then
      Exit(self.VarDeclaration())
    else
      Exit(self.Statement());
  except
    on e: ERunTimeError do
    begin
      self.Synchronize();
      Exit(nil);
    end;
  end;
end;

function TParser.VarDeclaration: TStatement;
var
  tok: TToken;
  init: TExpression;
begin
  tok := nil;
  init := nil;

  tok := self.Consume(TTokenKind.tkIDENTIFIER, 'Expect variable name.');
  if self.Match(TTokenKind.tkEQUAL) then
    init := self.Expression();

  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after variable declaration.');
  Exit(TVariableStatement.Create(tok, init));
end;

(*** STATEMENTS RULES ***)
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

function TParser.ExprStatement: TStatement;
var
  expr: TExpression;
begin
  expr := nil;

  expr := self.Expression();
  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after expression.');
  Exit(TExpressionStatement.Create(expr));
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

  Exit(TIfStatement.Create(cond, thenStm, elseStm));
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
  Exit(TWhileStatement.Create(cond, body));
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
    cond := TLiteralExpression.Create(LoxBool(true)); (* not in Tokens. Free ? *)
  body := TWhileStatement.Create(cond, body);

  if init <> nil then
  begin
    block := TObjectList<TStatement>.Create(True);
    block.Add(init);
    block.Add(body);
    body := TBlockStatement.Create(block);
  end;

  Exit(body);
end;

function TParser.PrintStatement: TStatement;
var
  expr: TExpression;
begin
  expr := nil;

  expr := self.Expression();
  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after expression.');
  Exit(TPrintStatement.Create(expr));
end;

function TParser.PrintDOTStatement: TStatement;
var
  expr: TExpression;
begin
  expr := nil;

  expr := self.Expression();
  self.Consume(TTokenKind.tkSEMICOLON, 'Expect '';'' after expression.');
  Exit(TPrintDOTStatement.Create(expr));
end;

function TParser.Block: TObjectList<TStatement>;
var
  bs: TObjectList<TStatement>;
begin
  bs := nil;

  bs := TObjectList<TStatement>.Create(True);

  while (not self.IsAtEnd()) and (not self.Check(TTokenKind.tkRIGHT_BRACE)) do
    bs.Add(self.Declaration());

  self.Consume(TTokenKind.tkRIGHT_BRACE, 'Expect ''}'' after block.');

  Exit(bs);
end;

(*** EXPRESSIONS RULES ***)
function TParser.Expression: TExpression;
begin
  Exit(Assignment());
end;

function TParser.Assignment: TExpression;
var
  eq, nm: TToken;
  expr, eval: Texpression;
begin
  eq := nil;
  nm := nil;
  expr := nil;
  eval := nil;

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

  Exit(expr);
end;

function TParser.LogicalOr: TExpression;
var
  op: TToken;
  expr, right: TExpression;
begin
  op := nil;
  expr := nil;
  right := nil;

  expr := self.LogicalAnd();
  while self.Match([TTokenKind.tkOR]) do
  begin
    op := self.Previous;
    right := self.LogicalAnd();
    expr := TLogicalExpression.Create(op, expr, right);
  end;

  Exit(expr);
end;

function TParser.LogicalAnd: TExpression;
var
  op: TToken;
  expr, right: TExpression;
begin
  op := nil;
  expr := nil;
  right := nil;

  expr := self.Equality();
  while self.Match([TTokenKind.tkAND]) do
  begin
    op := self.Previous;
    right := self.Equality();
    expr := TLogicalExpression.Create(op, expr, right);
  end;

  Exit(expr);
end;

function TParser.Equality: TExpression;
var
  op: TToken;
  expr, right: TExpression;
begin
  op := nil;
  expr := nil;
  right := nil;

  expr := Comparison();
  while Match([TTokenKind.tkBANG_EQUAL, TTokenKind.tkEQUAL_EQUAL]) do
  begin
    op := Previous();
    right := Comparison();
    expr := TBinaryExpression.Create(op, expr, right);
  end;
  Exit(expr);
end;

function TParser.Comparison: TExpression;
var
  op: TToken;
  expr, right: TExpression;
begin
  op := nil;
  expr := nil;
  right := nil;

  expr := Addition();
  while match([TTokenKind.tkGREATER, TTokenKind.tkGREATER_EQUAL,
      TTokenKind.tkLESS, TTokenKind.tkLESS_EQUAL]) do
  begin
    op := Previous();
    right := Addition();
    expr := TBinaryExpression.Create(op, expr, right);
  end;

  Exit(expr);
end;

function TParser.Addition: TExpression;
var
  op: TToken;
  expr, right: TExpression;
begin
  op := nil;
  expr := nil;
  right := nil;

  expr := Multiplication();
  while Match([TTokenKind.tkMINUS, TTokenKind.tkPLUS]) do
  begin
    op := Previous();
    right := Multiplication();
    expr := TBinaryExpression.Create(op, expr, right);
  end;

  Exit(expr);
end;

function TParser.Multiplication: TExpression;
var
  op: TToken;
  expr, right: TExpression;
begin
  op := nil;
  expr := nil;
  right := nil;

  expr := Unary();
  while match([TTokenKind.tkSLASH, TTokenKind.tkSTAR]) do
  begin
    op := Previous();
    right := Unary();
    expr := TBinaryExpression.Create(op, expr, right);
  end;

  Exit(expr);
end;

function TParser.Unary: TExpression;
var
  op: TToken;
  right: TExpression;
begin
  op := nil;
  right := nil;

  if match([TTokenKind.tkBANG, TTokenKind.tkMINUS]) then
  begin
    op := Previous();
    right := Unary();
    Exit(TUnaryExpression.Create(op, right));
  end
  else
    Exit(Primary());
end;

// TO DO RWRT
function TParser.Primary: TExpression;
var
  expr: TExpression;
begin
  expr := nil;

  if match([TTokenKind.tkFALSE]) then
    Exit(TLiteralExpression.Create(LoxBool(False)));

  if match([TTokenKind.tkTRUE]) then
    Exit(TLiteralExpression.Create(LoxBool(True)));

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

(*** PUBLIC METHODS ***)
constructor TParser.Create(tokens: TObjectList<TToken>);
begin
  FTokens := tokens;
  Fcurrent := 0;
end;

destructor TParser.Destroy;
begin
  inherited;
end;

function TParser.Parse: TObjectList<TStatement>;
var
  stm: TObjectList<TStatement>;
begin
  stm := TObjectList<TStatement>.Create(True); (* Owns statements *)
  while not self.IsAtEnd() do
    stm.Add(self.Declaration());
  Exit(stm);
end;

end.
