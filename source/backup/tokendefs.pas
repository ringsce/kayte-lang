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


implementation

end.

