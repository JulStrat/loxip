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
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    function Print(expr: TExpression): string;
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
    function VisitGroup(expr: TGroupingExpression): TObject;
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

function TASTDOTMaker.VisitGroup(expr: TGroupingExpression): TObject;
begin
  Result := TObject(self.FNodeNum);
  self.FDOT.Add(Format('%d [label="group"]', [integer(Result)]));
  Inc(self.FNodeNum);
  self.FDOT.Add(Format('%s -> %d', [integer(expr.expr.Accept(self)),
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
  Result := TLoxStr.Create(ObjToStr(expr.Value));
end;

function TASTPrinter.VisitUn(expr: TUnaryExpression): TObject;
var
  s: string;
  o: TLoxStr;
begin
  o := TLoxStr(expr.right.Accept(self));
  s := '( ' + expr.op.lexeme + ' ' + o.Value + ' )';
  FreeAndNil(o);
  Result := TLoxStr.Create(s);
end;

function TASTPrinter.VisitBin(expr: TBinaryExpression): TObject;
var
  s: string;
  l, r: TLoxStr;
begin
  l := TLoxStr(expr.left.Accept(self));
  r := TLoxStr(expr.right.Accept(self));
  s := '( ' + expr.op.lexeme + ' ' + l.Value + ' ' + r.Value + ' )';
  FreeAndNil(l);
  FreeAndNil(r);
  Result := TLoxStr.Create(s);
end;

function TASTPrinter.VisitGroup(expr: TGroupingExpression): TObject;
var
  s: string;
  o: TLoxStr;
begin
  o := TLoxStr(expr.expr.Accept(self));
  s := '( group ' + o.Value + ' )';
  FreeAndNil(o);
  Result := TLoxStr.Create(s);
end;

function TASTPrinter.Print(expr: TExpression): string;
var
  o: TLoxStr;
begin
  o := TLoxStr(expr.Accept(self));
  Result := o.Value;
  FreeAndNil(o);
end;

end.
