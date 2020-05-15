{
  Crafting Interpreters
  https://craftinginterpreters.com/
}

unit expression;

{$ifdef FPC}
{$mode delphi}
{$interfaces corba}
{$endif}

{$IFOPT D+}
{$DEFINE DEBUG}
{$ENDIF}

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

  { TVariableExpression }

  TVariableExpression = class(TExpression)
  private
    // TO DO Rename
    FVarName: TToken;
  public
    constructor Create(varName: TToken);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): TObject; override;
    property varName: TToken read FVarName;
  end;

  { TAssignmentExpression }

  TAssignmentExpression = class(TExpression)
  private
    FVarName: TToken;
    // TO DO Rename
    FValue: TExpression;
  public
    constructor Create(varName: TToken; val: TExpression);
    destructor Destroy; override;
    function Accept(ev: IExpressionVisitor): TObject; override;
    property varName: TToken read FVarName;
    property Value: TExpression read FValue;
  end;

  IExpressionVisitor = interface
    ['{8A3D7767-D1C5-49F5-BC15-F6C975FBAC67}']
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    function VisitVar(expr: TVariableExpression): TObject;
    function VisitAssign(expr: TAssignmentExpression): TObject;
  end;

implementation

{$IFDEF TRACE}
uses ptypes;
(* ObjToStr *)
{$ENDIF}

constructor TLiteralExpression.Create(v: TObject);
begin
  self.FValue := v;
end;

destructor TLiteralExpression.Destroy;
begin
  //FreeAndNil(self.FValue);
  inherited Destroy;
end;

function TLiteralExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
var
  msg: string;
{$ENDIF}
begin
  {$IFDEF TRACE}
  msg := Format('[DEBUG] (%s) Accepting literal expression: %s',
    [self.ClassName, ObjToStr(FValue)]);
  Writeln(msg);
  {$ENDIF}
  Result := ev.VisitLit(self);
end;

constructor TUnaryExpression.Create(op: TToken; right: TExpression);
begin
  self.FOp := op;
  self.FRight := right;
end;

destructor TUnaryExpression.Destroy;
begin
  //FreeAndNil(self.FOp);
  FreeAndNil(self.FRight);
  inherited Destroy;
end;

function TUnaryExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
var
  msg: string;
{$ENDIF}
begin
  {$IFDEF TRACE}
  msg := Format('[DEBUG] (%s) Accepting unary expression: %s',
    [self.ClassName, self.FOp.lexeme]);
  Writeln(msg);
  {$ENDIF}
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
  //FreeAndNil(self.FOp);
  FreeAndNil(self.FLeft);
  FreeAndNil(self.FRight);
  inherited Destroy;
end;

function TBinaryExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
var
  msg: string;
{$ENDIF}
begin
  {$IFDEF TRACE}
  msg := Format('[DEBUG] (%s) Accepting binary expression: %s',
    [self.ClassName, self.FOp.lexeme]);
  Writeln(msg);
  {$ENDIF}
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
{$IFDEF TRACE}
var
  msg: string;
{$ENDIF}
begin
  {$IFDEF TRACE}
  msg := Format('[DEBUG] (%s) Accepting grouping expression', [self.ClassName]);
  Writeln(msg);
  {$ENDIF}
  Result := ev.VisitGroup(self);
end;

constructor TVariableExpression.Create(varName: TToken);
begin
  self.FVarName := varName;
end;

destructor TVariableExpression.Destroy;
begin
  //FreeAndNil(self.FVarName);
  inherited Destroy;
end;

function TVariableExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
var
  msg: string;
{$ENDIF}
begin
  {$IFDEF TRACE}
  msg := Format('[DEBUG] (%s) Accepting variable expression: %s',
    [self.ClassName, self.FVarName.lexeme]);
  Writeln(msg);
  {$ENDIF}
  Result := ev.VisitVar(self);
end;

constructor TAssignmentExpression.Create(varName: TToken; val: TExpression);
begin
  self.FVarName := varName;
  self.FValue := val;
end;

destructor TAssignmentExpression.Destroy;
begin
  //FreeAndNil(self.FVarName);
  FreeAndNil(self.FValue);
  inherited Destroy;
end;

function TAssignmentExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
var
  msg: string;
{$ENDIF}
begin
  {$IFDEF TRACE}
  msg := Format('[DEBUG] (%s) Accepting assignment expression: %s',
    [self.ClassName, self.FVarName.lexeme]);
  Writeln(msg);
  {$ENDIF}
  Result := ev.VisitAssign(self);
end;

end.
