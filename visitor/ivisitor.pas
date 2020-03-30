{ Desederon Visitor 
  (C) I. Kakoulidis 2020 }
program visitor;
{$ifdef FPC}{$mode delphi}{$endif} 
uses Classes, SysUtils, variants; 
type 
  IExpressionVisitor = interface;
  TExpression = class abstract 
    function Accept(ev: IExpressionVisitor): variant; virtual; abstract;
  end;
  TLiteralExpression = class(TExpression) 
    FValue: variant; 
    constructor Create(v: variant); 
    function Accept(ev: IExpressionVisitor): variant; override;
  end;
  TUnaryExpression = class(TExpression)
    FOp: Char;
    FRight: TExpression;
    constructor Create(o: Char; r: TExpression); 
    function Accept(ev: IExpressionVisitor): variant; override;
  end; 
  TBinaryExpression = class(TExpression) 
    FOp: Char; 
    FLeft, FRight: TExpression;
    constructor Create(o: Char; l, r: TExpression);
    function Accept(ev: IExpressionVisitor): variant; override;
  end; 
  IExpressionVisitor = interface
    function VisitLit(exp: TLiteralExpression): variant;
    function VisitUn(exp: TUnaryExpression): variant;
    function VisitBin(exp: TBinaryExpression): variant;
  end; 
  constructor TLiteralExpression.Create(v: variant);
  begin 
    self.FValue := v;
  end;
  function TLiteralExpression.Accept(ev: IExpressionVisitor): variant;
  begin 
    Writeln ('Accepting literal - ', VarToStr(FValue));
    Result := ev.VisitLit(self);
  end;
  constructor TUnaryExpression.Create(o: Char; r: TExpression);
  begin 
    self.FOp := o; 
    self.FRight := r;
  end; 
  function TUnaryExpression.Accept(ev: IExpressionVisitor): variant;
  begin 
    Writeln ('Accepting unary - ', FOp);
    Result := ev.VisitUn(self);
  end; 
  constructor TBinaryExpression.Create(o: Char; l, r: TExpression);
  begin 
    self.FOp := o;
    self.FLeft := l;
    self.FRight := r;
  end;
  function TBinaryExpression.Accept(ev: IExpressionVisitor): variant;
  begin 
    Writeln ('Accepting binary - ', FOp);
    Result := ev.VisitBin(self);
  end; 
(* Visitor *)
type
  TExpressionVisitor = class(TInterfacedObject, IExpressionVisitor)
    function VisitLit(exp: TLiteralExpression): variant;
    function VisitUn(exp: TUnaryExpression): variant;
    function VisitBin(exp: TBinaryExpression): variant;
  end;
  function TExpressionVisitor.VisitLit(exp: TLiteralExpression): variant;
  begin 
    Result := VarToStr(exp.FValue);
  end;
  function TExpressionVisitor.VisitUn(exp: TUnaryExpression): variant;
  begin 
    Result := '(' + exp.FOp + '(' + exp.FRight.accept(self) + ')' + ')';
  end; 
  function TExpressionVisitor.VisitBin(exp: TBinaryExpression): variant;
  begin 
    Result := '(' + exp.FLeft.accept(self) + exp.FOp + exp.FRight.accept(self) + ')';
  end;
var 
  ev: TExpressionVisitor; 
  exp: TExpression;
begin 
  ev := TExpressionVisitor.Create;
  exp := TUnaryExpression.Create('!', 
  	 TBinaryExpression.Create('+', 
  	 	  TBinaryExpression.Create('*', 
  	 	    TLiteralExpression.Create(21), 
  	 	    TLiteralExpression.Create(13)), 
  	 	  TBinaryExpression.Create('/', 
  	 		   TLiteralExpression.Create(1031), 
  	 		   TLiteralExpression.Create(13)))); 
  	writeln (exp.accept(ev));
end. 
