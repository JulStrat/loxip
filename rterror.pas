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
      constructor Create(t: TToken; const msg: string);
      property token: TToken read FToken;
  end;

implementation

{ ERunTimeError }

constructor ERunTimeError.Create(t: TToken; const msg: string);
begin
  inherited Create(msg);
  FToken := t;
end;

end.

