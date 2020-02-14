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
  Classes, SysUtils;

type
  TLox = class
    hadError: boolean; static;
    hadRunTimeError: boolean; static;
    class procedure Run(source: String); static;
    class procedure RunPrompt(); static;
    class procedure Error(line: integer; message: String); static;
    // class procedure Error(token: TToken; message: String);
    class procedure Report(line: integer; where: String; message: String); static;
  end;

implementation

uses scanner, token, Generics.Collections;

class procedure TLox.Run(source: String);
var
  scanner: TScanner;
  tokens: TObjectList<TToken>;
  tok: TToken;
begin
  scanner := TScanner.Create(source);
  tokens := scanner.ScanTokens();

  for tok in tokens do
  begin
    WriteLn(tok.ToString());
  end;
end;

class procedure TLox.RunPrompt();
var
  inp: String;
begin
  WriteLn('Type quit to exit.');
  while True do
  begin
    Write('> ');
    ReadLn(inp);
    if inp = 'quit' then
      break;
    Run(inp);
    hadError := false;
  end
end;

class procedure TLox.Error(line: integer; message: String);
begin
  Report(line, '', message);
end;

class procedure TLox.Report(line: integer; where: String; message: String);
begin
  WriteLn('[line - ', line, '] Error - ', where, ' : ', message);
  hadError := true;
end;

end.

