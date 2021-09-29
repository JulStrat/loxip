unit interpreter;

{$ifdef FPC}
{$mode delphi}
{$endif}

{$IFOPT D+}
{$DEFINE DEBUG}
{$ENDIF}


interface

uses
  Classes, SysUtils, Generics.Collections
  , token, expression, statement, environment, rterror;

type
  { TInterpreter }

  TInterpreter = class(IExpressionVisitor, IStatementVisitor)
  private
    FEnvir: TEnvironment;
    function Evaluate(expr: TExpression): TObject;
    function IsTruthy(obj: TObject): boolean; inline;
    function IsEqual(left, right: TObject): boolean;
    function Stringify(obj: TObject): string;

  public
    constructor Create;
    destructor Destroy; override;
    { Expression visitor }
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitLogic(expr: TLogicalExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    function VisitVar(expr: TVariableExpression): TObject;
    function VisitAssign(expr: TAssignmentExpression): TObject;
    { Statement visitor }
    procedure VisitBlockStm(stm: TBlockStatement);
    procedure VisitExprStm(stm: TExpressionStatement);
    procedure VisitIfStm(stm: TIfStatement);
    procedure VisitWhileStm(stm: TWhileStatement);
    procedure VisitPrintStm(stm: TPrintStatement);
    procedure VisitPrintDOTStm(stm: TPrintDOTStatement);
    procedure VisitVarStm(stm: TVariableStatement);
    { helper }
    procedure Execute(stm: TStatement);
    { do job }
    procedure Interpret(stm: TObjectList<TStatement>);
  end;

implementation

uses ptypes, lox, astutils;

{ TInterpreter }

function TInterpreter.Evaluate(expr: TExpression): TObject;
begin
  {$IFDEF TRACE}
  WriteLn(Format('[DEBUG] (TInterpreter) Evaluating %s.', [expr.ClassName]));
  {$ENDIF}
  Result := nil;
  Result := expr.Accept(self);
end;

function TInterpreter.IsTruthy(obj: TObject): boolean;
begin
  { Ruby’s simple rule: false and nil are falsey and everything else is truthy. }
  if obj = nil then
    Exit(False);
  if obj is TLoxBool then
    Exit(TLoxBool(obj).Value);
  Exit(True);
end;

function TInterpreter.IsEqual(left, right: TObject): boolean;
begin
  if (left = nil) and (right = nil) then
    Exit(True);
  if left = nil then
    Exit(False);

  if left is TLoxBool then
    if right is TLoxBool then
      Exit(TLoxBool(left).Value = TLoxBool(right).Value)
    else
      Exit(False);

  if left is TLoxNum then
    if right is TLoxNum then
      Exit(TLoxNum(left).Value = TLoxNum(right).Value)
    else
      Exit(False);

  if left is TLoxStr then
    if right is TLoxStr then
      Exit(TLoxStr(left).Value = TLoxStr(right).Value)
    else
      Exit(False);

  Result := left.Equals(right);
end;

function TInterpreter.Stringify(obj: TObject): string;
begin
  Result := ObjToStr(obj);
end;

constructor TInterpreter.Create;
begin
  FEnvir := TEnvironment.Create;
end;

destructor TInterpreter.Destroy;
begin
  FreeObj(self.FEnvir);
  inherited;
end;

function TInterpreter.VisitLit(expr: TLiteralExpression): TObject;
begin
  if expr.Value is TLoxObject then
    Result := TLoxObject(expr.Value).Clone()
  else
    Result := expr.Value; { nil ? }
end;

function TInterpreter.VisitLogic(expr: TLogicalExpression): TObject;
var
  left: TObject;
begin
  //left := nil;
  left := self.Evaluate(expr.left);

  case expr.op.tokenKind of
    TTokenKind.tkOR:
      if self.IsTruthy(left) then
        Exit(left);
    TTokenKind.tkAND:
      if not self.IsTruthy(left) then
        Exit(left);
  end;

  FreeObj(left);
  Result := self.Evaluate(expr.right);
end;

function TInterpreter.VisitUn(expr: TUnaryExpression): TObject;
var
  right: TObject;
  obj: TObject;
  rte: ERunTimeError;
begin
  //right := nil;
  obj := nil;
  rte := nil;
  right := self.Evaluate(expr.right);

  case expr.op.tokenKind of
    TTokenKind.tkBANG:
      obj := LoxBool(not self.IsTruthy(right));

    TTokenKind.tkMINUS:
      if right is TLoxNum then
        obj := TLoxNum.Create(-TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operand must be a number.');
  end;

  FreeObj(right);
  Result := obj;
  if rte <> nil then
    raise rte;
end;

// TO DO CHECK OP with nil
function TInterpreter.VisitBin(expr: TBinaryExpression): TObject;
var
  left, right: TObject;
  obj: TObject;
  rte: ERunTimeError;
begin
  //left := nil;
  //right := nil;
  obj := nil;
  rte := nil;

  left := self.Evaluate(expr.left);
  right := self.Evaluate(expr.right);

  case expr.op.tokenKind of
    TTokenKind.tkPLUS: { + }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := TLoxNum.Create(TLoxNum(left).Value + TLoxNum(right).Value)
      else
      if (left is TLoxStr) or (right is TLoxStr) then
        obj := TLoxStr.Create(left.ToString() + right.ToString())
      else
        rte := ERuntimeError.Create(expr.op,
          'Operands must be numbers or either string.');

    TTokenKind.tkMINUS: { - }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := TLoxNum.Create(TLoxNum(left).Value - TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers.');

    TTokenKind.tkSTAR: { * }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := TLoxNum.Create(TLoxNum(left).Value * TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers.');

    TTokenKind.tkSLASH: { / }
      if (left is TLoxNum) and (right is TLoxNum) then
        if TLoxNum(right).Value <> 0 then
          obj := TLoxNum.Create(TLoxNum(left).Value / TLoxNum(right).Value)
        else
          rte := ERuntimeError.Create(expr.op, 'Division by zero.')
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers.');

    TTokenKind.tkGREATER: { > }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := LoxBool(TLoxNum(left).Value > TLoxNum(right).Value)
      else
      if (left is TLoxStr) and (right is TLoxStr) then
        obj := LoxBool(TLoxStr(left).Value > TLoxStr(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers or strings.');
    { TO DO Add bool comparison Rewrite comp proc }
    TTokenKind.tkGREATER_EQUAL: { >= }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := LoxBool(TLoxNum(left).Value >= TLoxNum(right).Value)
      else
      if (left is TLoxStr) and (right is TLoxStr) then
        obj := LoxBool(TLoxStr(left).Value >= TLoxStr(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers or strings.');

    TTokenKind.tkLESS: { < }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := LoxBool(TLoxNum(left).Value < TLoxNum(right).Value)
      else
      if (left is TLoxStr) and (right is TLoxStr) then
        obj := LoxBool(TLoxStr(left).Value < TLoxStr(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers or strings.');

    TTokenKind.tkLESS_EQUAL: { <= }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := LoxBool(TLoxNum(left).Value <= TLoxNum(right).Value)
      else
      if (left is TLoxStr) and (right is TLoxStr) then
        obj := LoxBool(TLoxStr(left).Value <= TLoxStr(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers or strings.');

    TTokenKind.tkEQUAL_EQUAL: { == }
      obj := LoxBool(self.IsEqual(left, right));

    TTokenKind.tkBANG_EQUAL:  { != }
      obj := LoxBool(not self.IsEqual(left, right));

  end;
  FreeObj(left);
  FreeObj(right);
  Result := obj;

  if rte <> nil then
    raise rte;
end;

function TInterpreter.VisitGroup(expr: TGroupingExpression): TObject;
begin
  Result := self.Evaluate(expr.expr);
end;

function TInterpreter.VisitVar(expr: TVariableExpression): TObject;
begin
  Result := FEnvir.Get(expr.varName);
end;

function TInterpreter.VisitAssign(expr: TAssignmentExpression): TObject;
var
  obj: TObject;
begin
  obj := self.Evaluate(expr.Value);
  self.FEnvir.Assign(expr.varName, obj);
  Result := obj;
end;

{ Statements }
procedure TInterpreter.VisitBlockStm(stm: TBlockStatement);
var
  prev: TEnvironment;
  s: TStatement;
begin
  prev := self.FEnvir;

  stm.environment.enclosing := prev;
  self.FEnvir := stm.environment;
  try
    for s in stm.block do
      self.Execute(s);
    //block.environment.Finalize;
  finally
    self.FEnvir := prev;
  end;

end;

procedure TInterpreter.VisitExprStm(stm: TExpressionStatement);
var
  r: TObject;
begin
  //r := nil;
  r := self.Evaluate(stm.expr);
  FreeObj(r);
end;

procedure TInterpreter.VisitIfStm(stm: TIfStatement);
var
  cond: TObject;
begin
  cond := self.Evaluate(stm.cond);
  if self.IsTruthy(cond) then
    self.Execute(stm.thenStm)
  else
    if stm.elseStm <> nil then
      self.Execute(stm.elseStm);
  FreeObj(cond);
end;

procedure TInterpreter.VisitWhileStm(stm: TWhileStatement);
var
  obj: TObject;
begin
  obj := self.Evaluate(stm.cond);
  while self.IsTruthy(obj) do
  begin
    self.Execute(stm.body);
    FreeObj(obj);
    obj := self.Evaluate(stm.cond);
  end;
  FreeObj(obj)
end;

procedure TInterpreter.VisitPrintStm(stm: TPrintStatement);
var
  obj: TObject;
  prn: TASTPrinter;
begin
  //obj := nil;
  prn := nil;
  try
  {$IFDEF DEBUG}
    prn := TASTPrinter.Create();
    WriteLn('[DEBUG] (TASTPrinter) ' + prn.Print(stm.expr, False));
    WriteLn('[DEBUG] (TASTPrinter RPN) ' + prn.Print(stm.expr, True));
  {$ENDIF}
    obj := self.Evaluate(stm.expr);
    WriteLn(ObjToStr(obj));
  finally
    FreeObj(obj);
    FreeObj(prn);
  end;
end;

procedure TInterpreter.VisitPrintDOTStm(stm: TPrintDOTStatement);
var
  dot: TStringList;
  dm: TASTDOTMaker;
begin
  dot := nil;
  dm := nil;
  try
    dm := TASTDOTMaker.Create();
    dot := dm.Make(stm.expr);
    WriteLn(dot.Text);
  finally
    FreeObj(dot);
    FreeObj(dm);
  end;
end;

procedure TInterpreter.VisitVarStm(stm: TVariableStatement);
var
  obj: TObject;
begin
  //obj := nil;
  if stm.expr <> nil then
    obj := self.Evaluate(stm.expr);
  self.FEnvir.Define(stm.token.lexeme, obj);
  FreeObj(obj);
end;

procedure TInterpreter.Execute(stm: TStatement);
begin
  {$IFDEF TRACE}
  WriteLn(Format('[DEBUG] (TInterpreter) Executing %s.', [stm.ClassName]));
  {$ENDIF}
  stm.Accept(self);
end;

procedure TInterpreter.Interpret(stm: TObjectList<TStatement>);
var
  s: TStatement;
begin
  try
    for s in stm do
      self.Execute(s);
  except
    on e: ERunTimeError do
      TLox.RunTimeError(e)
  end;
end;

end.
