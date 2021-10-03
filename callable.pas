{
  Crafting Interpreters
  https://craftinginterpreters.com/
}
unit callable;

{$ifdef FPC}
{$mode delphi}
{$interfaces corba}
{$endif}

interface

uses
  Classes, SysUtils, Generics.Collections
  , interpreter;

type
  ILoxCallable = interface
    ['{DB18CF19-44FC-493F-B09D-B437835BA147}']
    function Arity(): integer;
    function Call(inter: TInterpreter; args: TObjectList<TObject>): TObject;
  end;

implementation

end.

