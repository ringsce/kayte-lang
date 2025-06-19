unit tokenDefs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TTokenType = (
    // Core types for lexer output
    tkEndOfLIne,
    tkEndOfFile,
    tkComment,
    tkUnknown,
    tkEOF,
    tkIdentifier,
    tkKeyword,
    tkNumberLiteral, // Unified for integers/floats if needed
    tkIntegerLiteral, // Specific for integers
    tkStringLiteral,
    tkBooleanLiteral,
    tkOperator,
    tkDelimiter,

    // Specific operator/delimiter tokens
    tkPlus, tkMinus, tkStar, tkSlash, tkEquals, tkLessThan, tkGreaterThan,
    tkLessEquals, tkGreaterEquals, tkNotEquals, tkAmpersand,
    tkAnd, tkOr, tkNot, tkIs, // Logical operators (if distinct from tkOperator)

    tkParenthesisOpen,
    tkParenthesisClose,
    tkComma,
    tkDot,
    tkColon,
    tkSemiColon, // If you use semicolons
    tkNewline,   // Or tokEndOfLine if you explicitly tokenise newlines
    // ... add any other token types used in your language
    // ... add specific keywords if you want, e.g.,
    tkIf, tkThen, tkElse, tkEnd, tkWhile, tkDo, tkPrint, tkLet, tkTrue, tkFalse // etc.
  );


  // TToken record to hold token information
  TToken = record
    TokenType: TTokenType;
    Line: Integer;  // Line number where the token was found
    Column: Integer; // Column number where the token starts
    LineNum: Integer;  // <-- Add this
    ColNum: Integer;   // <-- And this
    Lexeme: String; // <--- This is likely what you meant for the string value
  end;

  // --- FUNCTION DECLARATION ---
  function GetTokenType(const S: String): TTokenType;

  implementation

  // --- FUNCTION IMPLEMENTATION ---
  function GetTokenType(const S: String): TTokenType;
  begin
    // This is a simplified example; you'd build a more comprehensive check
    // using a hash map or a case statement for all keywords.
    if SameText(S, 'IF') then Result := tkIf
    else if SameText(S, 'THEN') then Result := tkThen
    else if SameText(S, 'ELSE') then Result := tkElse
    else if SameText(S, 'END') then Result := tkEnd
    else if SameText(S, 'WHILE') then Result := tkWhile
    else if SameText(S, 'DO') then Result := tkDo
    else if SameText(S, 'PRINT') then Result := tkPrint
    else if SameText(S, 'LET') then Result := tkLet
    else if SameText(S, 'TRUE') then Result := tkBooleanLiteral // Specific keyword for boolean true
    else if SameText(S, 'FALSE') then Result := tkBooleanLiteral // Specific keyword for boolean false
    else Result := tkIdentifier; // If not a keyword, it's an identifier
  end;

end.

