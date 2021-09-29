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
  private
    FRPN: boolean;
  public
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitLogic(expr: TLogicalExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    function VisitVar(expr: TVariableExpression): TObject;
    function VisitAssign(expr: TAssignmentExpression): TObject;
    function Print(expr: TExpression; rpn: boolean = false): string;
  end;

  { TASTDOTMaker }

  TASTDOTMaker = class(IExpressionVisitor)
  private
    FNodeNum: integer;
    FDOT: TStringList;
  public
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitLogic(expr: TLogicalExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    function VisitVar(expr: TVariableExpression): TObject;
    function VisitAssign(expr: TAssignmentExpression): TObject;
    function Make(expr: TExpression): TStringList;
  end;

implementation

uses ptypes;

{ TASTDOTMaker }

function TASTDOTMaker.VisitLit(expr: TLiteralExpression): TObject;
begin
  Result := TObject(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="%s", shape=rectangle]',
    [integer(Result), ObjToStr(expr.Value)]));
end;

function TASTDOTMaker.VisitUn(expr: TUnaryExpression): TObject;
begin
  Result := TObject(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="%s"]', [integer(Result), expr.op.lexeme]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%d -> %d', [integer(expr.right.Accept(self)),
    integer(Result)]));
end;

function TASTDOTMaker.VisitBin(expr: TBinaryExpression): TObject;
begin
  Result := TObject(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="%s"]', [integer(Result), expr.op.lexeme]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%d -> %d', [integer(expr.left.Accept(self)),
    integer(Result)]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%d -> %d', [integer(expr.right.Accept(self)),
    integer(Result)]));
end;

function TASTDOTMaker.VisitLogic(expr: TLogicalExpression): TObject;
begin
  Result := self.VisitBin(expr);
end;

function TASTDOTMaker.VisitGroup(expr: TGroupingExpression): TObject;
begin
  Result := TObject(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="group"]', [integer(Result)]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%d -> %d', [integer(expr.expr.Accept(self)),
    integer(Result)]));
end;

function TASTDOTMaker.VisitVar(expr: TVariableExpression): TObject;
begin
  Result := TObject(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="%s", shape=rectangle]',
    [integer(Result), expr.varName.lexeme]));
end;

function TASTDOTMaker.VisitAssign(expr: TAssignmentExpression): TObject;
begin
  Result := TObject(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="assign"]', [integer(Result)]));

  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="%s"]', [self.FNodeNum, expr.varName.lexeme]));
  self.FDOT.Add(Format('%d -> %d', [self.FNodeNum, integer(Result)]));

  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%d -> %d', [integer(expr.value.Accept(self)),
    integer(Result)]));
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

function TASTPrinter.VisitLit(expr: TLiteralExpression): TObject;
begin
  if expr.Value is TLoxStr then
    Result := TLoxStr.Create(Format('"%s"', [TLoxStr(expr.Value).value]))
  else
    Result := TLoxStr.Create(ObjToStr(expr.Value));
end;

function TASTPrinter.VisitUn(expr: TUnaryExpression): TObject;
var
  s: string;
  o: TLoxStr;
begin
  o := TLoxStr(expr.right.Accept(self));
  if self.FRPN then
    s := Format('%s [unary]%s', [o.Value, expr.op.lexeme])
  else
    s := Format('(%s %s)', [expr.op.lexeme, o.Value]);
  FreeObj(o);
  Result := TLoxStr.Create(s);
end;

function TASTPrinter.VisitBin(expr: TBinaryExpression): TObject;
var
  s: string;
  l, r: TLoxStr;
begin
  l := TLoxStr(expr.left.Accept(self));
  r := TLoxStr(expr.right.Accept(self));
  if self.FRPN then
    s := Format('%s %s %s', [l.Value, r.Value, expr.op.lexeme])
  else
    s := Format('(%s %s %s)', [expr.op.lexeme, l.Value, r.Value]);
  FreeObj(l);
  FreeObj(r);
  Result := TLoxStr.Create(s);
end;

function TASTPrinter.VisitLogic(expr: TLogicalExpression): TObject;
begin
  Result := self.VisitBin(expr);
end;

function TASTPrinter.VisitGroup(expr: TGroupingExpression): TObject;
var
  s: string;
  e: TLoxStr;
begin
  e := TLoxStr(expr.expr.Accept(self));
  if self.FRPN then
    s := Format('%s', [e.Value])
  else
    s := Format('(group %s)', [e.Value]);
  FreeObj(e);
  Result := TLoxStr.Create(s);
end;

function TASTPrinter.VisitVar(expr: TVariableExpression): TObject;
begin
  Result := TLoxStr.Create(expr.varName.lexeme);
end;

function TASTPrinter.VisitAssign(expr: TAssignmentExpression): TObject;
var
  s: string;
  r: TLoxStr;
begin
  r := TLoxStr(expr.value.Accept(self));
  if self.FRPN then
    s := Format('%s %s assign', [expr.varName.lexeme, r.Value])
  else
    s := Format('(assign %s %s)', [expr.varName.lexeme, r.Value]);
  FreeObj(r);
  Result := TLoxStr.Create(s);
end;

function TASTPrinter.Print(expr: TExpression; rpn: boolean = false): string;
var
  o: TLoxStr;
begin
  self.FRPN := rpn;
  o := TLoxStr(expr.Accept(self));
  Result := o.Value;
  FreeObj(o);
end;

end.
