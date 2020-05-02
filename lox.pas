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
  tok: TToken;
  expr: TExpression;
begin
  scanner := TScanner.Create(Source);
  tokens := scanner.ScanTokens();
{
  (* Print tokens. Chapter - Scanning. *)
  for tok in tokens do
  begin
    WriteLn(tok.ToString());
  end;
}
  parser := TParser.Create(tokens);
  printer := TASTPrinter.Create();
  expr := parser.Parse;

  if hadError then
     Exit();

  WriteLn(printer.Print(expr));
  (* Destructors test *)
  FreeAndNil(scanner);
  FreeAndNil(parser);
  FreeAndNil(expr);
  FreeAndNil(tokens);

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

