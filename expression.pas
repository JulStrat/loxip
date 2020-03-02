unit expression;

{$mode delphi}

interface

uses
  Classes, SysUtils, token;

type
  TExpression = class
    procedure Accept(visitor: TVisitor);
  end;

  TVisitor = class;
    procedure Visit(expr: TLiteralExpression);
    procedure Visit(expr: TUnaryExpression);
    procedure Visit(expr: TBinaryExpression);
    procedure Visit(expr: TGroupingExpression);
  end;

  TLiteralExpression = class(TExpression)
    FValue: variant;
  end;

  TUnaryExpression = class(TExpression)
    FToken: TToken;
    FRightExpr: TExpression;
  end;

  TBinaryExpression = class(TExpression)
    FLeftExpr: TExpression;
    FToken: TToken;
    FRightExpr: TExpression;
  end;

  TGroupingExpression = class(TExpression)
    FExpr: TExpression;
  end;

implementation

end.

