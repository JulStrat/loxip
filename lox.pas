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
    class procedure RunPrompt; static;
    class procedure Error(line: integer; msg: string); static; overload;
    class procedure Error(token: TToken; msg: String); static; overload;
    class procedure RunTimeError(rte: ERunTimeError); static;
    class procedure Report(line: integer; where: string; msg: string); static;
  end;

implementation

uses Generics.Collections
  , {expression,} statement
  , scanner, parser{, astutils};

class procedure TLox.Run(Source: string);
var
  scanner: TScanner;
  parser: TParser;
  tokens: TObjectList<TToken>;
  stm: TObjectList<TStatement>;
  counter: integer; // loop counter
  benchStart, benchEnd: QWord;
begin
  { TEST }
  // source := 'var x = 101.1; var y = 1000001.1; var z; z = x * x * x * x * x * x * x * x; // print z;';
  // WHILE statement test Fib
  // source := 'var a=0;var b=1;var c=0; while (c<1000) {if (c>=0) {print c+": "+a;} var temp=a;a=b;b=temp+b;c=c+1;}';
  // source := 'var st=clock();var a=0;var b=1;var c=0; while (c<100) {if (c>=5) {print a;} var temp=a;a=b;b=temp+b;c=c+1;} print clock()-st;';
  // FOR statement test
  // source := 'for (var i=0; i<=100; i=i+1) { if (i>=0) print i+": "+i*i; }';
  { TEST }
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
  stm := parser.Parse;

  { expr := parser.Parse; }

  if hadError then
     Exit();
  benchStart := GetTickCount64();
  { TEST }
  // for counter := 1 to 100000 do
  { TEST }
  inter.Interpret(stm);
  benchEnd := GetTickCount64();
  WriteLn(Format('Tick count - %d.', [benchEnd - benchStart]));

  (* Destructors test *)

  FreeAndNil(stm);
  FreeAndNil(tokens);
  FreeAndNil(scanner);
  FreeAndNil(parser);
end;

class procedure TLox.RunPrompt;
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
  SysUtils.DecimalSeparator := '.';

  TLox.hadError := False;
  TLox.hadRunTimeError := False;
  TLox.inter := TInterpreter.Create;
end.

