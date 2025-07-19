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

    // --- New Tokens for Option Explicit ---
    tkOption,         // 'Option' keyword
    tkExplicit,       // 'Explicit' keyword
    tkOn,             // 'On' keyword
    tkOff,            // 'Off' keyword
    tkOptionExplicitOn,  // Composite token for "Option Explicit On"
    tkOptionExplicitOff  // Composite token for "Option Explicit Off"
    // --- End New Tokens ---// --- New Tokens for Option Explicit ---
    tkOption,         // 'Option' keyword
    tkExplicit,       // 'Explicit' keyword
    tkOn,             // 'On' keyword
    tkOff,            // 'Off' keyword
    tkOptionExplicitOn,  // Composite token for "Option Explicit On"
    tkOptionExplicitOff  // Composite token for "Option Explicit Off"
    // --- End New Tokens ---
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
  function GetTokenTypeName(ATokenType: TTokenType): String;
begin
  case ATokenType of
    tkUnknown: Result := 'UNKNOWN';
    tkEndOfFile: Result := 'END_OF_FILE';
    tkEndOfLine: Result := 'END_OF_LINE';
    tkComment: Result := 'COMMENT';
    tkKeyword: Result := 'KEYWORD';
    tkIdentifier: Result := 'IDENTIFIER';
    tkIntegerLiteral: Result := 'INTEGER_LITERAL';
    tkStringLiteral: Result := 'STRING_LITERAL';
    tkBooleanLiteral: Result := 'BOOLEAN_LITERAL';
    tkOperator: Result := 'OPERATOR';
    tkParenthesisOpen: Result := 'PARENTHESIS_OPEN';
    tkParenthesisClose: Result := 'PARENTHESIS_CLOSE';
    tkComma: Result := 'COMMA';
    tkDot: Result := 'DOT';
    tkColon: Result := 'COLON';
    tkOption: Result := 'OPTION_KEYWORD';
    tkExplicit: Result := 'EXPLICIT_KEYWORD';
    tkOn: Result := 'ON_KEYWORD';
    tkOff: Result := 'OFF_KEYWORD';
    tkOptionExplicitOn: Result := 'OPTION_EXPLICIT_ON_DIRECTIVE';
    tkOptionExplicitOff: Result := 'OPTION_EXPLICIT_OFF_DIRECTIVE';
    else Result := 'UNHANDLED_TOKEN_TYPE';
  end;
end;

end.

