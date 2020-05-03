unit astutils;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils
  , expression;

type
  { TASTPrinter }

  TASTPrinter = class(IExpressionVisitor)
  public
    function VisitLit(expr: TLiteralExpression): variant;
    function VisitUn(expr: TUnaryExpression): variant;
    function VisitBin(expr: TBinaryExpression): variant;
    function VisitGroup(expr: TGroupingExpression): variant;
    function Print(expr: TExpression): string;
  end;

  { TASTDOTMaker }

  TASTDOTMaker = class(IExpressionVisitor)
  private
    FNodeNum: integer;
    FDOT: TStringList;
  public
    function VisitLit(expr: TLiteralExpression): variant;
    function VisitUn(expr: TUnaryExpression): variant;
    function VisitBin(expr: TBinaryExpression): variant;
    function VisitGroup(expr: TGroupingExpression): variant;
    function Make(expr: TExpression): TStringList;
  end;


implementation

uses variants;

{ TASTDOTMaker }

function TASTDOTMaker.VisitLit(expr: TLiteralExpression): variant;
begin
  Result := self.FNodeNum;
  self.FDOT.Add(Format('%s [label="%s", shape=rectangle]',
    [VarToStr(Result), VarToStr(expr.Value)]));
end;

function TASTDOTMaker.VisitUn(expr: TUnaryExpression): variant;
begin
  Result := self.FNodeNum;
  self.FDOT.Add(Format('%s [label="%s"]', [VarToStr(Result), expr.op.lexeme]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%s -> %s', [VarToStr(expr.right.Accept(self)),
    VarToStr(Result)]));
end;

function TASTDOTMaker.VisitBin(expr: TBinaryExpression): variant;
begin
  Result := self.FNodeNum;
  self.FDOT.Add(Format('%s [label="%s"]', [VarToStr(Result), expr.op.lexeme]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%s -> %s', [VarToStr(expr.left.Accept(self)),
    VarToStr(Result)]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%s -> %s', [VarToStr(expr.right.Accept(self)),
    VarToStr(Result)]));
end;

function TASTDOTMaker.VisitGroup(expr: TGroupingExpression): variant;
begin
  Result := self.FNodeNum;
  self.FDOT.Add(Format('%s [label="group"]', [VarToStr(Result)]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%s -> %s', [VarToStr(expr.expr.Accept(self)),
    VarToStr(Result)]));
end;

function TASTDOTMaker.Make(expr: TExpression): TStringList;
begin
  self.FNodeNum := 0;
  self.FDOT := TStringList.Create;
  self.FDOT.Add('digraph astgraph {');
  self.FDOT.Add('node [shape=circle, fontsize=10, fontname="Courier"];');
  self.FDOT.Add('rankdir = BT;');
  expr.Accept(self);
  self.FDOT.Add('}');
  Result := self.FDOT;
end;

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
