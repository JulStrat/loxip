{
  Crafting Interpreters
  https://craftinginterpreters.com/
}

unit statement;

{$ifdef FPC}
{$mode delphi}
{$interfaces corba}
{$endif}

interface

uses
  Classes, SysUtils, Generics.Collections,
  expression, token;

type
  IStatementVisitor = interface;

  TStatement = class
    abstract
  public
    procedure Accept(ev: IStatementVisitor); virtual; abstract;
  end;

  { TBlockStatement }

  TBlockStatement = class(TStatement)
  private
    FBlock: TObjectList<TStatement>;
  public
    constructor Create(block: TObjectList<TStatement>);
    destructor Destroy; override;
    procedure Accept(ev: IStatementVisitor); override;
    property block: TObjectList<TStatement> read FBlock;
  end;

  { TExpressionStatement }

  TExpressionStatement = class(TStatement)
  private
    FExpr: TExpression;
  public
    constructor Create(expr: TExpression);
    destructor Destroy; override;
    procedure Accept(ev: IStatementVisitor); override;
    property expr: TExpression read FExpr;
  end;

  { TPrintStatement }

  TPrintStatement = class(TStatement)
  private
    FExpr: TExpression;
  public
    constructor Create(expr: TExpression);
    destructor Destroy; override;
    procedure Accept(ev: IStatementVisitor); override;
    property expr: TExpression read FExpr;
  end;

  TPrintDOTStatement = class(TPrintStatement)
  public
    procedure Accept(ev: IStatementVisitor); override;
  end;

  { TVariableStatement }

  TVariableStatement = class(TStatement)
  private
    FToken: TToken;
    FExpr: TExpression;
  public
    constructor Create(tok: TToken; expr: TExpression);
    destructor Destroy; override;
    procedure Accept(ev: IStatementVisitor); override;
    property token: TToken read FToken;
    property expr: TExpression read FExpr;
  end;

  IStatementVisitor = interface
    ['{CB339243-B3DD-4E3A-8E2C-A25503D1EF04}']
    procedure VisitBlockStm(stm: TBlockStatement);
    procedure VisitExprStm(stm: TExpressionStatement);
    procedure VisitPrintStm(stm: TPrintStatement);
    procedure VisitPrintDOTStm(stm: TPrintDOTStatement);
    procedure VisitVarStm(stm: TVariableStatement);
  end;

implementation

{ TBlockStatement }

constructor TBlockStatement.Create(block: TObjectList<TStatement>);
begin
  self.FBlock := block;
end;

destructor TBlockStatement.Destroy;
begin
  FreeAndNil(self.FBlock);
  inherited Destroy;
end;

procedure TBlockStatement.Accept(ev: IStatementVisitor);
begin
  ev.VisitBlockStm(self);
end;

{ TExpressionStatement }

constructor TExpressionStatement.Create(expr: TExpression);
begin
  self.FExpr := expr;
end;

destructor TExpressionStatement.Destroy;
begin
  FreeAndNil(self.FExpr);
  inherited Destroy;
end;

procedure TExpressionStatement.Accept(ev: IStatementVisitor);
begin
  ev.VisitExprStm(self);
end;

{ TPrintStatement }

constructor TPrintStatement.Create(expr: TExpression);
begin
  self.FExpr := expr;
end;

destructor TPrintStatement.Destroy;
begin
  FreeAndNil(self.FExpr);
  inherited Destroy;
end;

procedure TPrintStatement.Accept(ev: IStatementVisitor);
begin
  ev.VisitPrintStm(self);
end;

procedure TPrintDOTStatement.Accept(ev: IStatementVisitor);
begin
  ev.VisitPrintDOTStm(self);
end;

{ TVariableStatement }

constructor TVariableStatement.Create(tok: TToken; expr: TExpression);
begin
  self.FToken := tok;
  self.FExpr := expr;
end;

destructor TVariableStatement.Destroy;
begin
  //FreeAndNil(self.FToken);
  FreeAndNil(self.FExpr);
  inherited Destroy;
end;

procedure TVariableStatement.Accept(ev: IStatementVisitor);
begin
  ev.VisitVarStm(self);
end;

end.
