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
  , token, interpreter, rterror;

type

  { TLox }

  TLox = class
    class var
    hadError: boolean;
    hadRunTimeError: boolean;
    inter: TInterpreter;
    class procedure Run(Source: string); static;
    class procedure RunPrompt(); static;
    class procedure Error(line: integer; msg: string); static; overload;
    class procedure Error(token: TToken; msg: String); static; overload;
    class procedure RunTimeError(rte: ERunTimeError); static;
    class procedure Report(line: integer; where: string; msg: string); static;
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
//{
  for tok in tokens do
  begin
    WriteLn(tok.ToString());
  end;
//}

  parser := TParser.Create(tokens);
  expr := parser.Parse;

  if hadError then
     Exit();

  printer := TASTPrinter.Create();
  WriteLn(printer.Print(expr));

  dotmaker := TASTDOTMaker.Create;
  WriteLn(dotmaker.Make(expr).Text);
  inter.Interpret(expr);

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

class procedure TLox.Error(line: integer; msg: string);
begin
  Report(line, '', msg);
end;

class procedure TLox.Report(line: integer; where: string; msg: string);
begin
  WriteLn(Format('[line %d] Error%s: %s', [line, where, msg]));
  hadError := True;
end;

class procedure TLox.Error(token: TToken; msg: String);
begin
  if token.tokenKind = TTokenKind.tkEOF then
     report(token.line, ' at end ', msg)
  else
     report(token.line, ' at "' + token.lexeme + '" ', msg);
end;

class procedure TLox.RunTimeError(rte: ERunTimeError);
var
  msg: string;
begin
  msg := Format('[ERROR] %s [line %d].', [rte.Message, rte.token.line]);
  WriteLn(msg);
  hadRunTimeError := true;
end;

initialization
  TLox.hadError := False;
  TLox.hadRunTimeError := False;
  TLox.inter := TInterpreter.Create;
end.

