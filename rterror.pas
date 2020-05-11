{
  Crafting Interpreters
  https://craftinginterpreters.com/
}
unit rterror;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, token;

type

  { ERunTimeError }

  ERunTimeError = class(Exception)
    private
      FToken: TToken;
    public
      constructor Create(tok: TToken; const msg: string);
      property token: TToken read FToken;
  end;

implementation

constructor ERunTimeError.Create(tok: TToken; const msg: string);
begin
  inherited Create(msg);
  FToken := tok;
end;

end.

