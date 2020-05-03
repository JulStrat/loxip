{
  Crafting Interpreters
  https://craftinginterpreters.com/
}

unit expression;

{$ifdef FPC}
{$mode delphi}
{$interfaces corba}
{$endif}

interface

uses
  Classes, SysUtils
  , token;

type
  IExpressionVisitor = interface;

  TExpression = class
    abstract
  public
    function Accept(ev: IExpressionVisitor): variant; virtual; abstract;
  end;

  { TLiteralExpression }

  TLiteralExpression = class(TExpression)
  private
    FValue: variant;
  public
    constructor Create(v: variant);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): variant; override;
    property Value: variant read FValue;
  end;

  { TUnaryExpression }

  TUnaryExpression = class(TExpression)
  private
    FOp: TToken;
    FRight: TExpression;
  public
    constructor Create(op: TToken; right: TExpression);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): variant; override;
    property op: TToken read FOp;
    property right: TExpression read FRight;
  end;

  { TBinaryExpression }

  TBinaryExpression = class(TExpression)
  private
    FOp: TToken;
    FLeft, FRight: TExpression;
  public
    constructor Create(op: TToken; left, right: TExpression);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): variant; override;
    property op: TToken read FOp;
    property left: TExpression read FLeft;
    property right: TExpression read FRight;
  end;

  { TGroupingExpression }

  TGroupingExpression = class(TExpression)
  private
    FExpr: TExpression;
  public
    constructor Create(expr: TExpression);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): variant; override;
    property expr: TExpression read FExpr;
  end;

  IExpressionVisitor = interface
['{8A3D7767-D1C5-49F5-BC15-F6C975FBAC67}']
    function VisitLit(expr: TLiteralExpression): variant;
    function VisitUn(expr: TUnaryExpression): variant;
    function VisitBin(expr: TBinaryExpression): variant;
    function VisitGroup(expr: TGroupingExpression): variant;
  end;

implementation

uses variants;

constructor TLiteralExpression.Create(v: variant);
begin
  self.FValue := v;
end;

destructor TLiteralExpression.Destroy;
begin
  inherited Destroy;
end;

function TLiteralExpression.Accept(ev: IExpressionVisitor): variant;
begin
  Writeln('Accepting literal - ', VarToStr(FValue));
  Result := ev.VisitLit(self);
end;

constructor TUnaryExpression.Create(op: TToken; right: TExpression);
begin
  self.FOp := op;
  self.FRight := right;
end;

destructor TUnaryExpression.Destroy;
begin
  FreeAndNil(self.FRight);
  inherited Destroy;
end;

function TUnaryExpression.Accept(ev: IExpressionVisitor): variant;
begin
  Writeln('Accepting unary - ', FOp.lexeme);
  Result := ev.VisitUn(self);
end;

constructor TBinaryExpression.Create(op: TToken; left, right: TExpression);
begin
  self.FOp := op;
  self.FLeft := left;
  self.FRight := right;
end;

destructor TBinaryExpression.Destroy;
begin
  FreeAndNil(self.FLeft);
  FreeAndNil(self.FRight);
  inherited Destroy;
end;

function TBinaryExpression.Accept(ev: IExpressionVisitor): variant;
begin
  Writeln('Accepting binary - ', FOp.lexeme);
  Result := ev.VisitBin(self);
end;

constructor TGroupingExpression.Create(expr: TExpression);
begin
  self.FExpr := expr;
end;

destructor TGroupingExpression.Destroy;
begin
  FreeAndNil(self.FExpr);
  inherited Destroy;
end;

function TGroupingExpression.Accept(ev: IExpressionVisitor): variant;
begin
  Writeln('Accepting grouping - ');
  Result := ev.VisitGroup(self);
end;

end.
