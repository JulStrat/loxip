program visitor;
{$ifdef FPC}
{$mode delphi}
{$endif}

uses
  Classes, SysUtils, variants;

type
TExpressionVisitor = class;

TExpression = class abstract
  procedure Accept(ev: TExpressionVisitor); virtual; abstract;
end;

TLiteralExpression = class(TExpression)
  FValue: variant;
  constructor Create(v: variant);
  procedure Accept(ev: TExpressionVisitor); override;
  function ToString(): String; override;
end;

TUnaryExpression = class(TExpression)
  FOp: Char;
  FRight: TExpression;
  constructor Create(o: Char; r: TExpression);
  procedure Accept(ev: TExpressionVisitor); override;
  function ToString(): String; override;
end;

TBinaryExpression = class(TExpression)
  FOp: Char;
  FLeft: TExpression;
  FRight: TExpression;
  constructor Create(o: Char; l, r: TExpression);
  procedure Accept(ev: TExpressionVisitor); override;
  function ToString(): String; override;
end;

TExpressionVisitor = class
  procedure Visit(exp: TExpression); virtual; overload;
  procedure Visit(exp: TLiteralExpression); virtual; overload;
  procedure Visit(exp: TUnaryExpression); virtual; overload;
  procedure Visit(exp: TBinaryExpression); virtual; overload;
end;


constructor TLiteralExpression.Create(v: variant);
begin
  self.FValue := v;
end;

procedure TLiteralExpression.Accept(ev: TExpressionVisitor);
begin
  ev.Visit(self);
end;

function TLiteralExpression.ToString(): String;
begin
  Result := VarToStr(self.FValue);
end;

constructor TUnaryExpression.Create(o: Char; r: TExpression);
begin
  self.FOp := o;
  self.FRight := r;
end;

procedure TUnaryExpression.Accept(ev: TExpressionVisitor);
begin
  ev.Visit(self);
end;

function TUnaryExpression.ToString(): String;
begin
  Result := '(' + self.FOp + self.FRight.ToString + ')';
end;

constructor TBinaryExpression.Create(o: Char; l, r: TExpression);
begin
  self.FOp := o;
  self.FLeft := l;
  self.FRight := r;
end;

procedure TBinaryExpression.Accept(ev: TExpressionVisitor);
begin
  ev.Visit(self);
end;

function TBinaryExpression.ToString(): String;
begin
  Result := '(' + self.FLeft.ToString + self.FOp + self.FRight.ToString + ')';
end;

(* Visitor *)
procedure TExpressionVisitor.Visit(exp: TExpression);
begin
  exp.Accept(self);
end;

procedure TExpressionVisitor.Visit(exp: TLiteralExpression);
begin
  WriteLn(exp.ToString());
end;

procedure TExpressionVisitor.Visit(exp: TUnaryExpression);
begin
  WriteLn(exp.ToString());
end;

procedure TExpressionVisitor.Visit(exp: TBinaryExpression);
begin
  WriteLn(exp.ToString());
end;

var
  ev: TExpressionVisitor;

begin
  ev := TExpressionVisitor.Create;
  ev.Visit(
    TUnaryExpression('!',
    TBinaryExpression.Create('+',
    TBinaryExpression.Create('*', TLiteralExpression.Create(21), TLiteralExpression.Create(13)),
    TBinaryExpression.Create('/', TLiteralExpression.Create(1031), TLiteralExpression.Create(13)))
    ));
  ReadLn;
end.

