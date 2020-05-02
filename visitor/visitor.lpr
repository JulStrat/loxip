{ Demederon Visitor
  (C) I. Kakoulidis 2020 }
program visitor;
{$ifdef FPC}{$mode delphi}{$endif} 
uses Classes, SysUtils, variants; 
type 
  TAExpressionVisitor = class;
  TExpression = class abstract 
    function Accept(ev: TAExpressionVisitor): variant; virtual; abstract;
  end;
  TLiteralExpression = class(TExpression) 
    FValue: variant; 
    constructor Create(v: variant); 
    function Accept(ev: TAExpressionVisitor): variant; override;
  end;
  TUnaryExpression = class(TExpression)
    FOp: Char;
    FRight: TExpression;
    constructor Create(o: Char; r: TExpression); 
    function Accept(ev: TAExpressionVisitor): variant; override;
  end; 
  TBinaryExpression = class(TExpression) 
    FOp: Char; 
    FLeft, FRight: TExpression;
    constructor Create(o: Char; l, r: TExpression);
    function Accept(ev: TAExpressionVisitor): variant; override;
  end; 
  TAExpressionVisitor = class abstract
    function VisitLit(exp: TLiteralExpression): variant; virtual; abstract;
    function VisitUn(exp: TUnaryExpression): variant; virtual; abstract;
    function VisitBin(exp: TBinaryExpression): variant; virtual; abstract;
  end; 
  constructor TLiteralExpression.Create(v: variant);
  begin 
    self.FValue := v;
  end;
  function TLiteralExpression.Accept(ev: TAExpressionVisitor): variant;
  begin 
    Writeln ('Accepting literal');
    Result := ev.VisitLit(self);
  end;
  constructor TUnaryExpression.Create(o: Char; r: TExpression);
  begin 
    self.FOp := o; 
    self.FRight := r;
  end; 
  function TUnaryExpression.Accept(ev: TAExpressionVisitor): variant;
  begin 
    Writeln ('Accepting unary');
    Result := ev.VisitUn(self);
  end; 
  constructor TBinaryExpression.Create(o: Char; l, r: TExpression);
  begin 
    self.FOp := o;
    self.FLeft := l;
    self.FRight := r;
  end;
  function TBinaryExpression.Accept(ev: TAExpressionVisitor): variant;
  begin 
    Writeln ('Accepting binary');
    Result := ev.VisitBin(self);
  end; 
(* Visitor *)
type
  TExpressionVisitor = class(TAExpressionVisitor)
    function VisitLit(exp: TLiteralExpression): variant; override;
    function VisitUn(exp: TUnaryExpression): variant; override;
    function VisitBin(exp: TBinaryExpression): variant; override;
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
  ReadLn;
end. 
