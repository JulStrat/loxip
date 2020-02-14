program loxip;

uses
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
  end
  else
    TLox.RunPrompt;
end.

