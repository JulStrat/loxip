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
    function Accept(ev: IExpressionVisitor): TObject; virtual; abstract;
  end;

  { TLiteralExpression }

  TLiteralExpression = class(TExpression)
  private
    FValue: TObject;
  public
    constructor Create(v: TObject);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): TObject; override;
    property Value: TObject read FValue;
  end;

  { TUnaryExpression }

  TUnaryExpression = class(TExpression)
  private
    FOp: TToken;
    FRight: TExpression;
  public
    constructor Create(op: TToken; right: TExpression);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): TObject; override;
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
    function Accept(ev: IExpressionVisitor): TObject; override;
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
    function Accept(ev: IExpressionVisitor): TObject; override;
    property expr: TExpression read FExpr;
  end;

  IExpressionVisitor = interface
['{8A3D7767-D1C5-49F5-BC15-F6C975FBAC67}']
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
  end;

implementation

uses ptypes;

constructor TLiteralExpression.Create(v: TObject);
begin
  self.FValue := v;
end;

destructor TLiteralExpression.Destroy;
begin
  inherited Destroy;
end;

function TLiteralExpression.Accept(ev: IExpressionVisitor): TObject;
var
  msg: String;
begin
  msg := Format('[DEBUG] (%s) Accepting literal expression - %s', [self.ClassName, ObjToStr(FValue)]);
  Writeln(msg);
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

function TUnaryExpression.Accept(ev: IExpressionVisitor): TObject;
var
  msg: String;
begin
  msg := Format('[DEBUG] (%s) Accepting unary expression - %s', [self.ClassName, self.FOp.lexeme]);
  Writeln(msg);
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

function TBinaryExpression.Accept(ev: IExpressionVisitor): TObject;
var
  msg: String;
begin
  msg := Format('[DEBUG] (%s) Accepting binary expression - %s', [self.ClassName, self.FOp.lexeme]);
  Writeln(msg);
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

function TGroupingExpression.Accept(ev: IExpressionVisitor): TObject;
var
  msg: String;
begin
  msg := Format('[DEBUG] (%s) Accepting grouping expression', [self.ClassName]);
  Writeln(msg);
  Result := ev.VisitGroup(self);
end;

end.
