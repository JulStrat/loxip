unit interpreter;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils
  , expression;

type
  { TInterpreter }

  TInterpreter = class(IExpressionVisitor)
  private
    function Evaluate(expr: TExpression): TObject;
    function IsTruthy(obj: TObject): boolean;
    function IsEqual(left, right: TObject): boolean;
    procedure CheckNumberOperand(op: TToken; obj: TObject);
    procedure CheckNumberOperands(op: TToken; left, right: TObject);
    function Stringify(obj: TObject): string;
  public
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    procedure Interpret(expr: TExpression);
  end;

implementation

uses token, ptypes;

{ TInterpreter }

function TInterpreter.Evaluate(expr: TExpression): TObject;
begin
  Result := expr.Accept(self);
end;

function TInterpreter.IsTruthy(obj: TObject): boolean;
begin
  { Ruby’s simple rule: false and nil are falsey and everything else is truthy. }
  if obj = nil then Exit(false);
  if obj is TLoxBool then Exit(TLoxBool(obj).value);
  Exit(true);
end;

function TInterpreter.IsEqual(left, right: TObject): boolean;
begin

end;

procedure TInterpreter.CheckNumberOperand(op: TToken; obj: TObject);
begin

end;

procedure TInterpreter.CheckNumberOperands(op: TToken; left, right: TObject);
begin

end;

function TInterpreter.Stringify(obj: TObject): string;
begin

end;

function TInterpreter.VisitLit(expr: TLiteralExpression): TObject;
begin
  Result := expr.Value;
end;

function TInterpreter.VisitUn(expr: TUnaryExpression): TObject;
var
  right: TObject;
begin
  right := self.Evaluate(expr);

  case expr.op.tokenKind of
    TTokenKind.tkBANG: Exit(not self.IsTruthy(right));

    TTokenKind.tkMINUS: Exit(-Double(right));
  end;

  // Unreachable
  Result := nil;
end;

function TInterpreter.VisitBin(expr: TBinaryExpression): TObject;
begin

end;

function TInterpreter.VisitGroup(expr: TGroupingExpression): TObject;
begin
  Result := self.Evaluate(expr);
end;

procedure TInterpreter.Interpret(expr: TExpression);
begin

end;

end.
