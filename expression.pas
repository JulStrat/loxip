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

{$define EXPR_BENCH}

//{$DEFINE TRACE}

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
    property value: TObject read FValue;
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

  { TLogicalExpression }

  TLogicalExpression = class(TBinaryExpression)
  public
    function Accept(ev: IExpressionVisitor): TObject; override;
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
    property value: TExpression read FValue;
  end;

  IExpressionVisitor = interface
    ['{8A3D7767-D1C5-49F5-BC15-F6C975FBAC67}']
    function VisitLit(expr: TLiteralExpression): TObject;
    function VisitUn(expr: TUnaryExpression): TObject;
    function VisitBin(expr: TBinaryExpression): TObject;
    function VisitLogic(expr: TLogicalExpression): TObject;
    function VisitGroup(expr: TGroupingExpression): TObject;
    function VisitVar(expr: TVariableExpression): TObject;
    function VisitAssign(expr: TAssignmentExpression): TObject;
  end;

implementation


uses ptypes;

{$IFDEF EXPR_BENCH}
var litTicks
, unTicks
, binTicks
, logicTicks
, groupTicks
, varTicks
, assignTicks
: QWord;
{$ENDIF}

constructor TLiteralExpression.Create(v: TObject);
begin
  self.FValue := v;
end;

destructor TLiteralExpression.Destroy;
begin
  //FreeObj(self.FValue);
  inherited Destroy;
end;

function TLiteralExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
const
  __currMethodName = 'TLiteralExpression.Accept';
{$ENDIF}
{$IFDEF EXPR_BENCH}
var
  delta: QWord;
{$ENDIF}

begin
  {$IFDEF TRACE}
  Writeln(Format('[TRACE] (%s) Accepting literal expression: %s',
    [__currMethodName, ObjToStr(FValue)]));
  {$ENDIF}
  // CLONE HERE ?
  {$IFDEF EXPR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}

  Result := ev.VisitLit(self);

  {$IFDEF EXPR_BENCH}
  Inc(litTicks, GetTickCount64() - delta);
  {$ENDIF}

end;

constructor TUnaryExpression.Create(op: TToken; right: TExpression);
begin
  self.FOp := op;
  self.FRight := right;
end;

destructor TUnaryExpression.Destroy;
begin
  //FreeObj(self.FOp);
  FreeObj(self.FRight);
  inherited Destroy;
end;

function TUnaryExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
const
  __currMethodName = 'TUnaryExpression.Accept';
{$ENDIF}
{$IFDEF EXPR_BENCH}
var
  delta: QWord;
{$ENDIF}
begin
  {$IFDEF TRACE}
  Writeln(Format('[TRACE] (%s) Accepting unary expression: %s',
    [__currMethodName, self.FOp.lexeme]));
  {$ENDIF}
  {$IFDEF EXPR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}

  Result := ev.VisitUn(self);

  {$IFDEF EXPR_BENCH}
  Inc(unTicks, GetTickCount64() - delta);
  {$ENDIF}

end;

constructor TBinaryExpression.Create(op: TToken; left, right: TExpression);
begin
  self.FOp := op;
  self.FLeft := left;
  self.FRight := right;
end;

destructor TBinaryExpression.Destroy;
begin
  //FreeObj(self.FOp);
  FreeObj(self.FLeft);
  FreeObj(self.FRight);
  inherited Destroy;
end;

function TBinaryExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
const
  __currMethodName = 'TBinaryExpression.Accept';
{$ENDIF}
{$IFDEF EXPR_BENCH}
var
  delta: QWord;
{$ENDIF}

begin
  {$IFDEF TRACE}
  WriteLn(Format('[TRACE] (%s) Accepting binary expression: %s',
    [__currMethodName, self.FOp.lexeme]));
  {$ENDIF}
  {$IFDEF EXPR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}

  Result := ev.VisitBin(self);

  {$IFDEF EXPR_BENCH}
  Inc(binTicks, GetTickCount64() - delta);
  {$ENDIF}

end;

function TLogicalExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
const
  __currMethodName = 'TLogicalExpression.Accept';
{$ENDIF}
{$IFDEF EXPR_BENCH}
var
  delta: QWord;
{$ENDIF}
begin
  {$IFDEF TRACE}
  WriteLn(Format('[TRACE] (%s) Accepting logical expression: %s',
    [__currMethodName, self.FOp.lexeme]));
  {$ENDIF}
  {$IFDEF EXPR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}

  Result := ev.VisitLogic(self);

  {$IFDEF EXPR_BENCH}
  Inc(logicTicks, GetTickCount64() - delta);
  {$ENDIF}

end;

constructor TGroupingExpression.Create(expr: TExpression);
begin
  self.FExpr := expr;
end;

destructor TGroupingExpression.Destroy;
begin
  FreeObj(self.FExpr);
  inherited Destroy;
end;

function TGroupingExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
const
  __currMethodName = 'TGroupingExpression.Accept';
{$ENDIF}
{$IFDEF EXPR_BENCH}
var
  delta: QWord;
{$ENDIF}

begin
  {$IFDEF TRACE}
  WriteLn(Format('[TRACE] (%s) Accepting grouping expression', [__currMethodName]));
  {$ENDIF}
  {$IFDEF EXPR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}

  Result := ev.VisitGroup(self);

  {$IFDEF EXPR_BENCH}
  Inc(groupTicks, GetTickCount64() - delta);
  {$ENDIF}

end;

constructor TVariableExpression.Create(varName: TToken);
begin
  self.FVarName := varName;
end;

destructor TVariableExpression.Destroy;
begin
  //FreeObj(self.FVarName);
  inherited Destroy;
end;

function TVariableExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
const
  __currMethodName = 'TVariableExpression.Accept';
{$ENDIF}
{$IFDEF EXPR_BENCH}
var
  delta: QWord;
{$ENDIF}
begin
  {$IFDEF TRACE}
  WriteLn(Format('[TRACE] (%s) Accepting variable expression: %s',
    [__currMethodName, self.FVarName.lexeme]));
  {$ENDIF}
  {$IFDEF EXPR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}

  Result := ev.VisitVar(self);

  {$IFDEF EXPR_BENCH}
  Inc(varTicks, GetTickCount64() - delta);
  {$ENDIF}

end;

constructor TAssignmentExpression.Create(varName: TToken; val: TExpression);
begin
  self.FVarName := varName;
  self.FValue := val;
end;

destructor TAssignmentExpression.Destroy;
begin
  //FreeObj(self.FVarName);
  FreeObj(self.FValue);
  inherited Destroy;
end;

function TAssignmentExpression.Accept(ev: IExpressionVisitor): TObject;
{$IFDEF TRACE}
const
  __currMethodName = 'TAssignmentExpression.Accept';
{$ENDIF}
{$IFDEF EXPR_BENCH}
var
  delta: QWord;
{$ENDIF}
begin
  {$IFDEF TRACE}
  WriteLn(Format('[TRACE] (%s) Accepting assignment expression: %s',
    [__currMethodName, self.FVarName.lexeme]));
  {$ENDIF}
  {$IFDEF EXPR_BENCH}
  delta := GetTickCount64();
  {$ENDIF}

  Result := ev.VisitAssign(self);

  {$IFDEF EXPR_BENCH}
  Inc(assignTicks, GetTickCount64() - delta);
  {$ENDIF}

end;

initialization

  {$IFDEF EXPR_BENCH}
  litTicks := 0;
  unTicks := 0;
  binTicks := 0;
  logicTicks := 0;
  groupTicks := 0;
  varTicks := 0;
  assignTicks := 0;
  {$ENDIF}

finalization

  {$IFDEF EXPR_BENCH}
  WriteLn('TExpression - litTicks: ', litTicks);
  WriteLn('TExpression - unTicks: ', unTicks);
  WriteLn('TExpression - binTicks: ', binTicks);
  WriteLn('TExpression - logicTicks: ', logicTicks);
  WriteLn('TExpression - groupTicks: ', groupTicks);
  WriteLn('TExpression - varTicks: ', varTicks);
  WriteLn('TExpression - assignTicks: ', assignTicks);
  {$ENDIF}
end.


