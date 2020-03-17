program visitor;
{$ifdef FPC}
{$mode delphi}
{$endif}

uses
  Classes, SysUtils, variants;

type
TExpressionVisitor = class;

TExpression = class abstract
  function Accept(ev: TExpressionVisitor): String; virtual; abstract;
end;

TLiteralExpression = class(TExpression)
  FValue: variant;
  constructor Create(v: variant);
  function Accept(ev: TExpressionVisitor): String; override;
end;

TUnaryExpression = class(TExpression)
  FOp: Char;
  FRight: TExpression;
  constructor Create(o: Char; r: TExpression);
  function Accept(ev: TExpressionVisitor): String; override;
end;

TBinaryExpression = class(TExpression)
  FOp: Char;
  FLeft: TExpression;
  FRight: TExpression;
  constructor Create(o: Char; l, r: TExpression);
  function Accept(ev: TExpressionVisitor): String; override;
end;

TExpressionVisitor = class
  function Visit(exp: TExpression): String; virtual; overload;
  function Visit(exp: TLiteralExpression): String; virtual; overload;
  function Visit(exp: TUnaryExpression): String; virtual; overload;
  function Visit(exp: TBinaryExpression): String; virtual; overload;
end;

constructor TLiteralExpression.Create(v: variant);
begin
  self.FValue := v;
end;

function TLiteralExpression.Accept(ev: TExpressionVisitor): String;
begin
  Result := ev.Visit(self);
end;

constructor TUnaryExpression.Create(o: Char; r: TExpression);
begin
  self.FOp := o;
  self.FRight := r;
end;

function TUnaryExpression.Accept(ev: TExpressionVisitor): String;
begin
  Result := ev.Visit(self);
end;

constructor TBinaryExpression.Create(o: Char; l, r: TExpression);
begin
  self.FOp := o;
  self.FLeft := l;
  self.FRight := r;
end;

function TBinaryExpression.Accept(ev: TExpressionVisitor): String;
begin
  Result := ev.Visit(self);
end;

(* Visitor *)
function TExpressionVisitor.Visit(exp: TExpression): String;
begin
  Result := exp.Accept(self);
end;

function TExpressionVisitor.Visit(exp: TLiteralExpression): String;
begin
  Result := VarToStr(exp.FValue);
end;

function TExpressionVisitor.Visit(exp: TUnaryExpression): String;
begin
  Result := '(' + exp.FOp + '(' + Visit(exp.FRight) + ')' + ')';
end;

function TExpressionVisitor.Visit(exp: TBinaryExpression): String;
begin
  Result := '(' + Visit(exp.FLeft) + exp.FOp + Visit(exp.FRight) + ')';
end;

var
  ev: TExpressionVisitor;
  exp: TExpression;
begin
  ev := TExpressionVisitor.Create;
  exp := TUnaryExpression.Create('!',
    TBinaryExpression.Create('+',
    TBinaryExpression.Create('*', TLiteralExpression.Create(21), TLiteralExpression.Create(13)),
    TBinaryExpression.Create('/', TLiteralExpression.Create(1031), TLiteralExpression.Create(13)))
    );
  WriteLn(ev.Visit(exp));
  ReadLn;
end.

