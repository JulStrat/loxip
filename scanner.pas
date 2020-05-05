{
  Crafting Interpreters
  https://craftinginterpreters.com/
}

unit scanner;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  token, ptypes,
  Generics.Collections,
  Classes, SysUtils;

type

  TScanner = class
  private
    FSource: string;
    FTokens: TObjectList<TToken>;
    FStart: integer;
    FCurrent: integer;
    FLine: integer;

    procedure ScanToken;
    procedure AddToken(tokenKind: TTokenKind); overload;
    procedure AddToken(tokenKind: TTokenKind; literal: TObject); overload;

    function IsAtEnd: boolean;
    function Advance: char;
    function Match(expected: char): boolean;
    function Peek: char;
    function PeekNext: char;

    procedure ReadString;
    procedure ReadNumber;
    procedure ReadIdentifier;

    function IsDigit(c: char): boolean;
    function IsAlpha(c: char): boolean;
    function IsAlphaNumeric(c: char): boolean;
  public
    constructor Create(src: string);
    function ScanTokens: TObjectList<TToken>;
    destructor Destroy; override;
  end;

implementation

uses lox;

constructor TScanner.Create(src: string);
begin
  FTokens := TObjectList<TToken>.Create();
  FSource := src;
  FStart := Low(src);
  Fcurrent := Low(src);
  FLine := 1;
end;

destructor TScanner.Destroy;
begin
  //FTokens.Free();
  inherited;
end;

function TScanner.IsAtEnd(): boolean;
begin
  //Result := FCurrent > FSource.Length;
  Result := FCurrent > High(FSource);
end;

function TScanner.Advance(): char;
begin
  Result := FSource[FCurrent];
  Inc(FCurrent);
end;

procedure TScanner.AddToken(tokenKind: TTokenKind);
begin
  AddToken(tokenKind, nil);
end;

procedure TScanner.AddToken(tokenKind: TTokenKind; literal: TObject);
var
  txt: string;
begin
  txt := System.Copy(FSource, FStart, FCurrent - FStart);
  FTokens.Add(TToken.Create(tokenKind, txt, literal, FLine));
end;

procedure TScanner.ScanToken();
var
  c: char;
begin
  c := Advance();

  case (c) of
    '(': AddToken(TTokenKind.tkLEFT_PAREN);
    ')': AddToken(TTokenKind.tkRIGHT_PAREN);
    '{': AddToken(TTokenKind.tkLEFT_BRACE);
    '}': AddToken(TTokenKind.tkRIGHT_BRACE);
    ',': AddToken(TTokenKind.tkCOMMA);
    '.': AddToken(TTokenKind.tkDOT);
    '-': AddToken(TTokenKind.tkMINUS);
    '+': AddToken(TTokenKind.tkPLUS);
    ';': AddToken(TTokenKind.tkSEMICOLON);
    '*': AddToken(TTokenKind.tkSTAR);
    '!': if Match('=') then
        AddToken(TTokenKind.tkBANG_EQUAL)
      else
        AddToken(TTokenKind.tkBANG);
    '=': if Match('=') then
        AddToken(TTokenKind.tkEQUAL_EQUAL)
      else
        AddToken(TTokenKind.tkEQUAL);
    '<': if Match('=') then
        AddToken(TTokenKind.tkLESS_EQUAL)
      else
        AddToken(TTokenKind.tkLESS);
    '>': if Match('=') then
        AddToken(TTokenKind.tkGREATER_EQUAL)
      else
        AddToken(TTokenKind.tkGREATER);
    '/':
    begin
      if (match('/')) then
      begin
        while (Peek() <> #10) and not isAtEnd() do
          Advance();
      end
      else
        AddToken(TTokenKind.tkSLASH);
    end;
    ' ', #13, #9: ;
    #10: Inc(FLine);
    '"': ReadString();
    '0'..'9': ReadNumber();
    'a'..'z', 'A'..'Z', '_': ReadIdentifier();
    else
      TLox.Error(FLine, 'Unexpected character.');
  end;

end;

procedure TScanner.ReadIdentifier();
var
  txt: string;
  tk: TTokenKind;
begin
  while (IsAlphaNumeric(Peek())) do
    Advance();

  txt := System.Copy(FSource, FStart, FCurrent - FStart);
  if not loxKeywords.TryGetValue(txt, tk) then
    tk := TTokenKind.tkIDENTIFIER;

  AddToken(tk);
end;

function TScanner.IsAlphaNumeric(c: char): boolean;
begin
  Result := isAlpha(c) or isDigit(c);
end;

function TScanner.IsAlpha(c: char): boolean;
begin
  Result := ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')) or
    (c = '_');
end;

function TScanner.IsDigit(c: char): boolean;
begin
  Result := (c >= '0') and (c <= '9');
end;

procedure TScanner.ReadNumber;
var
  cds: char;
begin
  while IsDigit(Peek()) do
    Advance();

  if (Peek() = '.') and isDigit(PeekNext()) then
  begin
    Advance();

    while IsDigit(Peek()) do
      Advance();
  end;

  cds := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AddToken(TTokenKind.tkNUMBER, TLoxNum.Create(StrToFloat(Copy(FSource, FStart, FCurrent - FStart))));
  finally
    FormatSettings.DecimalSeparator := cds;
  end;
end;

procedure TScanner.ReadString();
begin
  while (Peek() <> '"') and not IsAtEnd() do
  begin
    if (Peek() = #10) then
      Inc(FLine);

    Advance();
  end;

  if (isAtEnd()) then
  begin
    TLox.Error(FLine, 'Unterminated string.');
    Exit();
  end;

  Advance();
  AddToken(TTokenKind.tkSTRING, TLoxStr.Create(Copy(FSource, FStart + 1, FCurrent - FStart - 2)));
end;

function TScanner.Peek(): char;
begin
  if isAtEnd() then
    Result := #0
  else
    Result := FSource[FCurrent];
end;

function TScanner.PeekNext(): char;
begin
  if (FCurrent + 1) > High(FSource) then
    Result := #0
  else
    Result := FSource[FCurrent + 1];
end;

function TScanner.Match(expected: char): boolean;
begin
  if IsAtEnd() then
    Exit(False);
  if (FSource[FCurrent] <> expected) then
    Exit(False);

  Inc(FCurrent);
  Result := True;
end;

function TScanner.ScanTokens: TObjectList<TToken>;
begin
  while not IsAtEnd() do
  begin
    FStart := FCurrent;
    ScanToken();
  end;

  FTokens.Add(TToken.Create(TTokenKind.tkEOF, '', nil, FLine));
  Result := FTokens;
end;

end.
