unit astutils;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils
  , expression;

type
  TASTPrinter = class(TInterfacedObject, IExpressionVisitor)
    function VisitLit(expr: TLiteralExpression): variant;
    function VisitUn(expr: TUnaryExpression): variant;
    function VisitBin(expr: TBinaryExpression): variant;
    function VisitGroup(expr: TGroupingExpression): variant;
    function Print(expr: TExpression): string;
  end;

implementation

uses variants;

{ TASTPrinter }

function TASTPrinter.VisitLit(expr: TLiteralExpression): variant;
begin
  Result := VarToStr(expr.Value);
end;

function TASTPrinter.VisitUn(expr: TUnaryExpression): variant;
begin
  Result := '( ' + expr.op.lexeme + ' ' + expr.right.Accept(self) + ' )';
end;

function TASTPrinter.VisitBin(expr: TBinaryExpression): variant;
begin
  Result := '( ' + expr.op.lexeme + ' ' + expr.left.Accept(self) +
    ' ' + expr.right.Accept(self) + ' )';
end;

function TASTPrinter.VisitGroup(expr: TGroupingExpression): variant;
begin
  Result := '( group ' + expr.expr.Accept(self) + ' )';
end;

function TASTPrinter.Print(expr: TExpression): string;
begin
  Result := expr.Accept(self);
end;

end.


