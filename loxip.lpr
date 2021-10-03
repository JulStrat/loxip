program loxip;

{$ifdef FPC}
{$mode delphi}
{$endif}

{$apptype console}
{$undef USE_FastMM4}

uses
  {$ifdef USE_FastMM4}
  FastMM4,
  {$endif}
  Classes, SysUtils,
  lox;

var
  script: TStringList;
  source: String;
// arg: .\tests\scanning\punctuators.lox
begin
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
  begin
    script := TStringList.Create;
    script.LoadFromFile(ParamStr(1));
    source := script.Text;
    TLox.Run(source);
    FreeAndNil(TLox.inter);
    if TLox.hadError then
      Halt(65);
  end
  else
  begin
    TLox.RunPrompt;
    FreeAndNil(TLox.inter);
  end;
end.

