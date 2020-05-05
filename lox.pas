{
  Crafting Interpreters
  https://craftinginterpreters.com/
}

unit lox;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils
  , token;

type
  TLox = class
    class var
    hadError: boolean;
    hadRunTimeError: boolean;
    class procedure Run(Source: string); static;
    class procedure RunPrompt(); static;
    class procedure Error(line: integer; message: string); static; overload;
    class procedure Error(token: TToken; message: String); static; overload;
    class procedure Report(line: integer; where: string; message: string); static;
  end;

implementation

uses Generics.Collections
  , expression
  , scanner, parser, astutils;

class procedure TLox.Run(Source: string);
var
  scanner: TScanner;
  parser: TParser;
  tokens: TObjectList<TToken>;
  printer: TASTPrinter;
  dotmaker: TASTDOTMaker;
  tok: TToken;
  expr: TExpression;
begin
  scanner := TScanner.Create(Source);
  tokens := scanner.ScanTokens();

  (* Print tokens. Chapter - Scanning. *)
{
  for tok in tokens do
  begin
    WriteLn(tok.ToString());
  end;
}

  parser := TParser.Create(tokens);
  expr := parser.Parse;

  if hadError then
     Exit();

  printer := TASTPrinter.Create();
  WriteLn(printer.Print(expr));

  dotmaker := TASTDOTMaker.Create;
  WriteLn(dotmaker.Make(expr).Text);

(*
> 1 + 8 * 89 - - 12

( - ( + 1 ( * 8 89 ) ) ( - 12 ) )

digraph astgraph {
node [shape=circle, fontsize=10, fontname="Courier"];
rankdir = BT;
0 [label="-"]
1 [label="+"]
2 [label="1", shape=rectangle]
2 -> 1
3 [label="*"]
4 [label="8", shape=rectangle]
4 -> 3
5 [label="89", shape=rectangle]
5 -> 3
3 -> 1
1 -> 0
6 [label="-"]
7 [label="12", shape=rectangle]
7 -> 6
6 -> 0
}
*)
  (* Destructors test *)
{
  FreeAndNil(scanner);
  FreeAndNil(parser);
  FreeAndNil(expr);
  FreeAndNil(tokens);
}
end;

class procedure TLox.RunPrompt();
var
  inp: string;
begin
  WriteLn('Type quit to exit.');
  while True do
  begin
    Write('> ');
    ReadLn(inp);
    if inp = 'quit' then
      break;
    Run(inp);
    hadError := False;
  end;
end;

class procedure TLox.Error(line: integer; message: string);
begin
  Report(line, '', message);
end;

class procedure TLox.Report(line: integer; where: string; message: string);
begin
  WriteLn('[line - ', line, '] Error - ', where, ' : ', message);
  hadError := True;
end;

class procedure TLox.Error(token: TToken; message: String);
begin
  if token.tokenKind = TTokenKind.tkEOF then
     report(token.line, ' at end ', message)
  else
     report(token.line, ' at "' + token.lexeme + '" ', message);
end;

initialization
  TLox.hadError := False;
  TLox.hadRunTimeError := False;

end.

