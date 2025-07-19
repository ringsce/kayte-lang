unit TokenDefs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TTokenType = (
    // Core types for lexer output
    tkUnknown,
    tkEndOfLine,    // Represents end of a line
    tkEndOfFile,    // Represents end of the source file
    tkComment,      // Single-line comment (e.g., REM, ')
    tkIdentifier,   // Variable names, form names, control names, function names
    tkKeyword,      // General keyword (e.g., PRINT, SHOW, IF, THEN, END, SUB, FUNCTION, DIM, AS, REM, etc.)
    tkIntegerLiteral, // Numeric literal (e.g., 123, 456)
    tkStringLiteral,  // String literal (e.g., "Hello World")
    tkBooleanLiteral, // Boolean literal (TRUE, FALSE)
    tkOperator,       // Arithmetic, comparison, logical, and concatenation operators (+, -, *, /, =, <, >, <=, >=, <>, &, AND, OR, NOT, IS)

    // Specific delimiter tokens (these are distinct from generic tkOperator)
    tkParenthesisOpen,  // (
    tkParenthesisClose, // )
    tkComma,            // ,
    tkDot,              // .
    tkColon,            // : (for multi-statement lines)
    tkSemiColon,        // ; (if used for statement termination)

    // --- Tokens for Option Explicit Directive (composite tokens) ---
    tkOption,           // 'Option' keyword (as a standalone token if not part of directive)
    tkExplicit,         // 'Explicit' keyword (as a standalone token if not part of directive)
    tkOn,               // 'On' keyword (as a standalone token if not part of directive)
    tkOff,              // 'Off' keyword (as a standalone token if not part of directive)
    tkOptionExplicitOn, // Composite token for "Option Explicit On"
    tkOptionExplicitOff // Composite token for "Option Explicit Off"
    // --- End Option Explicit Directive Tokens ---
  );

  // TToken record to hold token information
  TToken = record
    TokenType: TTokenType;
    Line: Integer;    // Line number where the token was found (1-based)
    Column: Integer;  // Column number where the token starts (1-based)
    Lexeme: String;   // The actual text of the token
  end;

  // --- FUNCTION DECLARATION ---
  // This function provides a string representation of a TTokenType, useful for debugging.
  function GetTokenTypeName(ATokenType: TTokenType): String;

implementation

// --- FUNCTION IMPLEMENTATION ---
function GetTokenTypeName(ATokenType: TTokenType): String;
begin
  case ATokenType of
    tkUnknown: Result := 'UNKNOWN';
    tkEndOfLine: Result := 'END_OF_LINE';
    tkEndOfFile: Result := 'END_OF_FILE';
    tkComment: Result := 'COMMENT';
    tkIdentifier: Result := 'IDENTIFIER';
    tkKeyword: Result := 'KEYWORD';
    tkIntegerLiteral: Result := 'INTEGER_LITERAL';
    tkStringLiteral: Result := 'STRING_LITERAL';
    tkBooleanLiteral: Result := 'BOOLEAN_LITERAL';
    tkOperator: Result := 'OPERATOR';
    tkParenthesisOpen: Result := 'PARENTHESIS_OPEN';
    tkParenthesisClose: Result := 'PARENTHESIS_CLOSE';
    tkComma: Result := 'COMMA';
    tkDot: Result := 'DOT';
    tkColon: Result := 'COLON';
    tkSemiColon: Result := 'SEMICOLON';
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

