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
  Generics.Collections,
  TypInfo,
  Variants;

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
    tkFUNCT, tkRETURN,
    tkVAR, tkNIL,
    tkPRINT,

    // End of file.
    tkEOF);

  TToken = class
  private
    FTokenKind: TTokenKind;
    FLexeme: string;
    FLiteral: variant;
    FLine: integer;
  public
    constructor Create(tokenKind: TTokenKind; lexeme: string;
      literal: variant; line: integer);
    function ToString(): string; override;
    property tokenKind: TTokenKind read FTokenKind;
    property lexeme: string read FLexeme;
    property literal: variant read FLiteral;
    property line: integer read FLine;
  end;

var
  loxKeywords: TDictionary<string, TTokenKind>;

implementation

constructor TToken.Create(tokenKind: TTokenKind; lexeme: string;
  literal: variant; line: integer);
begin
  FTokenKind := tokenKind;
  FLexeme := lexeme;
  FLiteral := literal;
  FLine := line;
end;

{ Token to JSON }
function TToken.ToString: string;
begin
  Result := '{ tokenKind: ' + GetEnumName(TypeInfo(TTokenKind), Ord(tokenKind)) +
    ', lexeme: "' + lexeme + '", literal: ' + VarToStr(literal) + ' }';
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

  loxKeywords.Add('fun', TTokenKind.tkFUNCT);
  loxKeywords.Add('return', TTokenKind.tkRETURN);

  loxKeywords.Add('var', TTokenKind.tkVAR);
  loxKeywords.Add('nil', TTokenKind.tkNIL);

  loxKeywords.Add('print', TTokenKind.tkPRINT);

finalization
  loxKeywords.Free();

end.

