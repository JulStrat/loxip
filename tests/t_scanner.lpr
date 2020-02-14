{
  Crafting Interpreters
  https://craftinginterpreters.com/
}

program t_scanner;

{$ifdef FPC}
{$mode delphi}
{$endif}

uses token,
  scanner;

var
  scn: TScanner;
  t: TToken;
  s: string;
  ltk: TTokenKind;

begin
  t := TToken.Create(TTokenKind.tkLEFT_PAREN, '(', nil, 1);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkRIGHT_PAREN, ')', nil, 2);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkLEFT_BRACE, '{', nil, 3);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkRIGHT_BRACE, '}', nil, 4);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkCOMMA, ',', nil, 5);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkDOT, '.', nil, 6);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkMINUS, '-', nil, 7);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkPLUS, '+', nil, 8);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkSEMICOLON, ';', nil, 9);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkSTAR, '*', nil, 10);
  WriteLn(t.ToString());


  t := TToken.Create(TTokenKind.tkSTRING, '123', 123, 11);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkSTRING, '123', '123', 12);
  WriteLn(t.ToString());

  t := TToken.Create(TTokenKind.tkNIL, '', nil, 13);
  WriteLn(t.ToString());

  // Test lox keywords
  s := 'var';
  if not loxKeywords.TryGetValue(s, ltk) then
    WriteLn(s, ' is IDENTIFIER.')
  else
    WriteLn(s, ' is KEYWORD - ', ltk);

  s := 'vir';
  if not loxKeywords.TryGetValue(s, ltk) then
    WriteLn(s, ' is IDENTIFIER.')
  else
    WriteLn(s, ' is KEYWORD - ', ltk);

  s := 'fun';
  if not loxKeywords.TryGetValue(s, ltk) then
    WriteLn(s, ' is IDENTIFIER.')
  else
    WriteLn(s, ' is KEYWORD - ', ltk);

  s := 'funa';
  if not loxKeywords.TryGetValue(s, ltk) then
    WriteLn(s, ' is IDENTIFIER.')
  else
    WriteLn(s, ' is KEYWORD - ', ltk);

  s := 'fin';
  if not loxKeywords.TryGetValue(s, ltk) then
    WriteLn(s, ' is IDENTIFIER.')
  else
    WriteLn(s, ' is KEYWORD - ', ltk);
// Scanner test
  scn := TScanner.Create(
    'andy formless fo _ _123 _abc ab123'#13#10 +
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_'
    );
  for t in scn.ScanTokens() do
  begin
    WriteLn(t.ToString());
  end;
  scn.Destroy;

  WriteLn('***');
  scn := TScanner.Create(
    'and class else false for fun if nil or return super this true var while'
    );
  for t in scn.ScanTokens() do
  begin
    WriteLn(t.ToString());
  end;
  scn.Destroy;

  WriteLn('***');
  scn := TScanner.Create(
    '123'#13#10 +
    '123.456'#13#10 +
    '.456'#13#10 +
    '123.'
    );
  for t in scn.ScanTokens() do
  begin
    WriteLn(t.ToString());
  end;
  scn.Destroy;

  WriteLn('***');
  scn := TScanner.Create(
    '(){};,+-*!===<=>=!=<>/.'
    );
  for t in scn.ScanTokens() do
  begin
    WriteLn(t.ToString());
  end;
  scn.Destroy;

  WriteLn('***');
  scn := TScanner.Create(
    '""'#13#10 +
    '"string"'
    );
  for t in scn.ScanTokens() do
  begin
    WriteLn(t.ToString());
  end;
  scn.Destroy;

  WriteLn('***');
  scn := TScanner.Create(
    'space    tabs				newlines '#13#10 +
    #13#10 +
    #13#10 +
    #13#10 +
    #13#10 +
    ':end'
    );
  for t in scn.ScanTokens() do
  begin
    WriteLn(t.ToString());
  end;
  scn.Destroy;

  WriteLn('***');
  scn := TScanner.Create(
    ''
    );
  for t in scn.ScanTokens() do
  begin
    WriteLn(t.ToString());
  end;
  scn.Destroy;


  ReadLn;
end.

