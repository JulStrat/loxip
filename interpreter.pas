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
    function IsTruthy(obj: TObject): boolean;
    function IsEqual(left, right: TObject): boolean;
    function Stringify(obj: TObject): string;
  public
    constructor Create;
    destructor Destroy; override;
    { Expression visitor }
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    function VisitVar(expr: TVariableExpression): TObject;
    function VisitAssign(expr: TAssignmentExpression): TObject;
    { Statement visitor }
    procedure VisitBlockStm(stm: TBlockStatement);
    procedure VisitExprStm(stm: TExpressionStatement);
    procedure VisitPrintStm(stm: TPrintStatement);
    procedure VisitPrintDOTStm(stm: TPrintDOTStatement);
    procedure VisitVarStm(stm: TVariableStatement);
    { helper }
    procedure Execute(stm: TStatement);
    { do job }
    { procedure Interpret(expr: TExpression); }
    procedure Interpret(stm: TObjectList<TStatement>);
  end;

implementation

uses ptypes, lox, astutils;

{ TInterpreter }

function TInterpreter.Evaluate(expr: TExpression): TObject;
begin
  {$IFDEF DEBUG}
  WriteLn(Format('[DEBUG] (TInterpreter) Evaluating %s.', [expr.ClassName]));
  {$ENDIF}
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
  if (left = nil) or (right = nil) then
    Exit(False);
  if left.ClassType <> right.ClassType then
    Exit(False);
  if left.ClassType = TLoxBool then
    Exit(TLoxBool(left).Value = TLoxBool(right).Value);
  if left.ClassType = TLoxNum then
    Exit(TLoxNum(left).Value = TLoxNum(right).Value);
  if left.ClassType = TLoxStr then
    Exit(TLoxStr(left).Value = TLoxStr(right).Value);

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
  FreeAndNil(self.FEnvir);
  inherited;
end;

function TInterpreter.VisitLit(expr: TLiteralExpression): TObject;
begin
  Result := TLoxObject(expr.Value).Clone();
end;

function TInterpreter.VisitUn(expr: TUnaryExpression): TObject;
var
  right: TObject;
  obj: TObject;
  rte: ERunTimeError;
begin
  obj := nil;
  rte := nil;
  right := self.Evaluate(expr.right);

  case expr.op.tokenKind of
    TTokenKind.tkBANG:
      obj := TLoxBool.Create(not self.IsTruthy(right));

    TTokenKind.tkMINUS:
      if right is TLoxNum then
        obj := TLoxNum.Create(-TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operand must be a number.');
  end;

  FreeAndNil(right);
  Result := obj;
  if rte <> nil then
    raise rte;
end;

function TInterpreter.VisitBin(expr: TBinaryExpression): TObject;
var
  left, right: TObject;
  obj: TObject;
  rte: ERunTimeError;
begin
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
      if (left is TLoxStr) or (right is TLoxStr) then
        obj := TLoxStr.Create(left.ToString + right.ToString)
      else
        rte := ERuntimeError.Create(expr.op,
          'Operands must be numbers or at least one - string.');

    TTokenKind.tkGREATER: { > }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := TLoxBool.Create(TLoxNum(left).Value > TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers.');

    TTokenKind.tkGREATER_EQUAL: { >= }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := TLoxBool.Create(TLoxNum(left).Value >= TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers.');

    TTokenKind.tkLESS: { < }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := TLoxBool.Create(TLoxNum(left).Value < TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers.');

    TTokenKind.tkLESS_EQUAL: { <= }
      if (left is TLoxNum) and (right is TLoxNum) then
        obj := TLoxBool.Create(TLoxNum(left).Value <= TLoxNum(right).Value)
      else
        rte := ERuntimeError.Create(expr.op, 'Operands must be numbers.');

    TTokenKind.tkEQUAL_EQUAL: { == }
      obj := TLoxBool.Create(self.IsEqual(left, right));

    TTokenKind.tkBANG_EQUAL:  { != }
      obj := TLoxBool.Create(not self.IsEqual(left, right));

  end;
  FreeAndNil(left);
  FreeAndNil(right);
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
  Result := self.FEnvir.Get(expr.varName);
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
begin

end;

procedure TInterpreter.VisitExprStm(stm: TExpressionStatement);
var
  r: TObject;
begin
  r := self.Evaluate(stm.expr);
  if r is TLoxObject then
    FreeAndNil(r);
end;

procedure TInterpreter.VisitPrintStm(stm: TPrintStatement);
var
  obj: TObject;
  prn: TASTPrinter;
begin
  obj := nil;
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
    FreeAndNil(obj);
    FreeAndNil(prn);
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
    FreeAndNil(dot);
    FreeAndNil(dm);
  end;
end;

procedure TInterpreter.VisitVarStm(stm: TVariableStatement);
var
  obj: TObject;
begin
  obj := nil;
  if stm.expr <> nil then
    obj := self.Evaluate(stm.expr);
  self.FEnvir.Define(stm.token.lexeme, obj);
  FreeAndNil(obj);
end;

procedure TInterpreter.Execute(stm: TStatement);
begin
  stm.Accept(self);
end;

{
procedure TInterpreter.Interpret(expr: TExpression);
var
  obj: TObject;
begin
  try
    try
      obj := evaluate(expr);
      WriteLn(Stringify(obj));
    except
      on e: ERunTimeError do
        TLox.RunTimeError(e)
    end;
  finally
    ;
  end;
end;
}

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
