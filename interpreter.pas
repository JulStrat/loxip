unit interpreter;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils
  , token, expression, rterror;

type
  { TInterpreter }

  TInterpreter = class(IExpressionVisitor)
  private
    function Evaluate(expr: TExpression): TObject;
    function IsTruthy(obj: TObject): boolean;
    function IsEqual(left, right: TObject): boolean;
    function CheckNumberOperand(op: TToken; obj: TObject): ERunTimeError;
    function CheckNumberOperands(op: TToken; left, right: TObject): ERunTimeError;
    function CheckUnOperand(expr: TUnaryExpression): ERunTimeError;
    function CheckBinOperands(expr: TBinaryExpression): ERunTimeError;
    function Stringify(obj: TObject): string;
  public
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    procedure Interpret(expr: TExpression);
  end;

implementation

uses ptypes, lox;

{ TInterpreter }

function TInterpreter.Evaluate(expr: TExpression): TObject;
begin
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
  if left.ClassType <> right.ClassType then;
  Exit(False);
  if left.ClassType = TLoxBool then
    Exit(TLoxBool(left).Value = TLoxBool(right).Value);
  if left.ClassType = TLoxNum then
    Exit(TLoxNum(left).Value = TLoxNum(right).Value);
  if left.ClassType = TLoxStr then
    Exit(TLoxStr(left).Value = TLoxStr(right).Value);

  Result := left.Equals(right);

end;

function TInterpreter.CheckNumberOperand(op: TToken; obj: TObject): ERunTimeError;
begin
  if obj is TLoxNum then
    Result := nil
  else
    Result := ERuntimeError.Create(op, 'Operand must be a number.');
end;

function TInterpreter.CheckNumberOperands(op: TToken; left, right: TObject
  ): ERunTimeError;
begin
  if (left is TLoxNum) and (right is TLoxNum) then
    Result := nil
  else
    Result := ERuntimeError.Create(op, 'Operands must be numbers.');
end;

function TInterpreter.CheckUnOperand(expr: TUnaryExpression): ERunTimeError;
begin

end;

function TInterpreter.CheckBinOperands(expr: TBinaryExpression): ERunTimeError;
begin

end;

function TInterpreter.Stringify(obj: TObject): string;
begin
  Result := ObjToStr(obj);
end;

function TInterpreter.VisitLit(expr: TLiteralExpression): TObject;
begin
  Result := expr.Value;
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
    begin
      rte := CheckNumberOperand(expr.op, right);
      if rte = nil then
        obj := TLoxNum.Create(-TLoxNum(right).Value);
    end;
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
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxNum.Create(TLoxNum(left).Value + TLoxNum(right).Value);
    end;

    TTokenKind.tkMINUS: { - }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxNum.Create(TLoxNum(left).Value - TLoxNum(right).Value);
    end;

    TTokenKind.tkSTAR: { * }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxNum.Create(TLoxNum(left).Value * TLoxNum(right).Value);
    end;

    TTokenKind.tkSLASH: { / }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxNum.Create(TLoxNum(left).Value / TLoxNum(right).Value);
    end;

    TTokenKind.tkGREATER: { > }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxBool.Create(TLoxNum(left).Value > TLoxNum(right).Value);
    end;

    TTokenKind.tkGREATER_EQUAL: { >= }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxBool.Create(TLoxNum(left).Value >= TLoxNum(right).Value);
    end;

    TTokenKind.tkLESS: { < }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxBool.Create(TLoxNum(left).Value < TLoxNum(right).Value);
    end;

    TTokenKind.tkLESS_EQUAL: { <= }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxBool.Create(TLoxNum(left).Value <= TLoxNum(right).Value);
    end;

    TTokenKind.tkEQUAL_EQUAL: { == }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxBool.Create(TLoxNum(left).Value = TLoxNum(right).Value);
    end;

    TTokenKind.tkBANG_EQUAL:  { != }
    begin
      rte := self.CheckNumberOperands(expr.op, left, right);
      if rte = nil then
        obj := TLoxBool.Create(TLoxNum(left).Value <> TLoxNum(right).Value);
    end;

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

end.
