unit TokenDefs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  // Define the types of tokens our lexer can recognize
  TTokenType = (
    // Special tokens
    tkUnknown,        // For unrecognized tokens
    tkEndOfFile,      // End of input stream (replaces tkEOF)
    tkEndOfLine,      // Newline character
    tkComment,        // ' or REM comments
    tkEOF, // End of file

    // Literals
    tkIntegerLiteral, // 123, 456
    tkStringLiteral,  // "Hello, World!"
    tkBooleanLiteral, // TRUE, FALSE

    // Identifiers
    tkIdentifier,     // Variable names, function names, etc.

    // Keywords (basic set, expand as needed)
    tkKeyword,        // General keyword (e.g., PRINT, INPUT, DIM, IF, THEN, ELSE, WHILE, WEND, SUB, END, CALL, RETURN, SHOW, HIDE)

    // --- Specific Keywords for "Option Explicit On/Off" ---
    tkKeywordOption,  // OPTION
    tkKeywordExplicit,// EXPLICIT
    tkKeywordOn,      // ON
    tkKeywordOff,     // OFF
    // --- End Specific Keywords ---

    // Combined keywords for specific statements
    tkOptionExplicitOn, // OPTION EXPLICIT ON
    tkOptionExplicitOff,// OPTION EXPLICIT OFF

    // Operators
    tkOperator,       // +, -, *, /, =, <, >, <=, >=, <>, &, AND, OR, NOT, IS

    // Punctuation
    tkParenthesisOpen,  // (
    tkParenthesisClose, // )
    tkComma,            // ,
    tkDot,              // .
    tkColon             // :
  );

  // Represents a single token found by the lexer
  TToken = record
    TokenType: TTokenType;
    Lexeme: String;   // The actual text matched (e.g., "PRINT", "123", "myVar")
    Line: Integer;    // Line number where the token starts (0-indexed)
    Column: Integer;  // Column number where the token starts (0-indexed)
  end;

// Helper function to get a string representation of a token type (useful for error messages)
function GetTokenTypeName(TokenType: TTokenType): String;

// Array for easy lookup of token type names
const
    TokenTypeName: array[TTokenType] of String = (
    'UNKNOWN',
    'END_OF_FILE',
    'END_OF_LINE',
    'COMMENT',
    'EOF',   // <-- added for tkEOF
    'INTEGER_LITERAL',
    'STRING_LITERAL',
    'BOOLEAN_LITERAL',
    'IDENTIFIER',
    'KEYWORD',
    'KEYWORD_OPTION',
    'KEYWORD_EXPLICIT',
    'KEYWORD_ON',
    'KEYWORD_OFF',
    'OPTION_EXPLICIT_ON',
    'OPTION_EXPLICIT_OFF',
    'OPERATOR',
    'PARENTHESIS_OPEN',
    'PARENTHESIS_CLOSE',
    'COMMA',
    'DOT',
    'COLON'
  );

implementation

function GetTokenTypeName(TokenType: TTokenType): String;
begin
  Result := TokenTypeName[TokenType];
end;

end.

