{
  Crafting Interpreters
  https://craftinginterpreters.com/
}
unit token;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TTokenKind = (
    // Single-character tokens.
    tkLEFT_PAREN, tkRIGHT_PAREN,    // ( , )
    tkLEFT_BRACE, tkRIGHT_BRACE,    // { , }
    tkCOMMA, tkDOT,                 // , , .
    tkMINUS, tkPLUS,                // - , +
    tkSEMICOLON, tkSTAR,            // ; , *
    tkSLASH,                        // /

    // One or two character tokens.
    tkBANG, tkBANG_EQUAL,
    tkEQUAL, tkEQUAL_EQUAL,
    tkLESS, tkLESS_EQUAL,
    tkGREATER, tkGREATER_EQUAL,

    // Literals.
    tkIDENTIFIER, tkSTRING, tkNUMBER,

    // Keywords.
    tkAND, tkOR, tkTRUE, tkFALSE,
    tkCLASS, tkSUPER, tkTHIS,
    tkIF, tkELSE, tkFOR, tkWHILE,
    tkFUN, tkRETURN,
    tkVAR, tkNIL,
    tkPRINT, tkPRINTDOT,

    // End of file.
    tkEOF);

  TToken = class
  private
    FTokenKind: TTokenKind;
    FLexeme: string;
    FLiteral: TObject;
    FLine: integer;
  public
    constructor Create(tokenKind: TTokenKind; lexeme: string;
      literal: TObject; line: integer);
    function ToString: string; override;
    property tokenKind: TTokenKind read FTokenKind;
    property lexeme: string read FLexeme;
    property literal: TObject read FLiteral;
    property line: integer read FLine;
  end;

var
  loxKeywords: TDictionary<string, TTokenKind>;

implementation

uses ptypes, TypInfo;

constructor TToken.Create(tokenKind: TTokenKind; lexeme: string;
  literal: TObject; line: integer);
begin
  FTokenKind := tokenKind;
  FLexeme := lexeme;
  FLiteral := literal;
  FLine := line;
end;

function TToken.ToString: string;
begin
  { Original format }
  Result := Format('%s %s %s', [GetEnumName(TypeInfo(TTokenKind), Ord(self.FTokenKind)).Substring(2),
    self.FLexeme, ObjToStr(self.FLiteral)]);
  { JSON }
  (*
  Result := Format('{ tokenKind: %s, lexeme: "%s", literal: %s }',
    [GetEnumName(TypeInfo(TTokenKind), Ord(self.FTokenKind)),
    self.FLexeme, ObjToStr(self.FLiteral)]);
  *)
end;

initialization
  loxKeywords := TDictionary<string, TTokenKind>.Create();
  loxKeywords.Add('and', TTokenKind.tkAND);
  loxKeywords.Add('or', TTokenKind.tkOR);
  loxKeywords.Add('true', TTokenKind.tkTRUE);
  loxKeywords.Add('false', TTokenKind.tkFALSE);

  loxKeywords.Add('class', TTokenKind.tkCLASS);
  loxKeywords.Add('super', TTokenKind.tkSUPER);
  loxKeywords.Add('this', TTokenKind.tkTHIS);

  loxKeywords.Add('if', TTokenKind.tkIF);
  loxKeywords.Add('else', TTokenKind.tkELSE);
  loxKeywords.Add('for', TTokenKind.tkFOR);
  loxKeywords.Add('while', TTokenKind.tkWHILE);

  loxKeywords.Add('fun', TTokenKind.tkFUN);
  loxKeywords.Add('return', TTokenKind.tkRETURN);

  loxKeywords.Add('var', TTokenKind.tkVAR);
  loxKeywords.Add('nil', TTokenKind.tkNIL);

  loxKeywords.Add('print', TTokenKind.tkPRINT);
  loxKeywords.Add('printdot', TTokenKind.tkPRINTDOT);

finalization
  loxKeywords.Free();

end.

