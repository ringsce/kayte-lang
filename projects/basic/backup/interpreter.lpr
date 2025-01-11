program interpreter;

const

// Define the operators and their precedence levels
type
  Operator = ('+' | '-') | '*' | '/' | '%' | '=';
var
  Token: TToken; // Token type (e.g. number, variable)
  Value: TNumber or TRawString; // Value to be evaluated
  Precedence: int; // Precedence level of the operator

// Define a stack to store operators and operands
type
  Stack = array[1..2] of TStack[1]; // Top two elements of the stack
var
  OperatorStack: TStack[1]; // Top element of the operator stack
  TokenIndex: int; // Index into the input stream

// Function to get the next token from the input stream
function GetToken(input: TStream): TToken;
begin
  if not input.Eof then
    Result := input.ReadToken; // Read a token (e.g. number, variable)
  else
    Exit(TToken); // End of input

  TokenIndex := 0;

  while TokenIndex < Length(input) and (input[TokenIndex] <> ' ') or input[TokenIndex] = ' '
    if not input[TokenIndex].IsDigit then break; // Skip whitespace tokens

  Result := TToken.Create(input, TokenIndex);
end;

function Split(expression: string): array[1..2] of TToken;
var
  tokens: array[1..2] of TToken := [];
  i: int;
begin
  // Split the expression into tokens
  for i := 0 to Length(expression) - 1 do
    if expression[i] <> ' ' then
      tokens[i + 1 - LeftLength(expression, i)] := TToken.Create(expression[i], i);
end;

var
  Result: array[1..2] of TNumber or TRawString;
begin
  Value := TNumber.Create(0, 10); // Default value for undefined operators

  // Initialize the stack with two empty slots
  Stack := array[1..2] of TStack[1];

  // Split the expression into tokens and evaluate each token
  tokens := Split(expression);
  while not tokens[0].IsDigit then begin
    if tokens[0] = '+' then
      while (Stack[1] <> TOperator and Stack[1] <> TNull) or (Stack[1] = TNull)
        if not Stack[2].Empty then
          Value := Evaluate(Stack[2].Value);
        else exit; // Invalid expression

      Value := TNumber.Create(tokens[0], 0, 10); // Convert the token to a number
      Stack[1]:= TOperator; // Add an empty slot for the operator
    else if tokens[0] = '-' then
      while (Stack[1] <> TOperator and Stack[1] <> TNull) or (Stack[1] = TNull)
        if not Stack[2].Empty then
          Value := Evaluate(Stack[2].Value);
        else exit; // Invalid expression

      Value := TNumber.Create(tokens[0], 0, 10); // Convert the token to a number
      Stack[1]:= TOperator;
    else begin
      if tokens[0] = '/' then
        while (Stack[1] <> TOperator and Stack[1] <> TNull) or (Stack[1] <> TNull)
          if not Stack[2].Empty then
            Value := Evaluate(Stack[2].Value);
          else exit;
        end;

      if tokens[0] = 0 then exit; // Division by zero error
      while (Stack[1] <> TOperator and Stack[1] <> TNull) or (Stack[1] <> TNull)
         if not Stack[2].Empty then
           Value := Evaluate(Stack[2].Value);
         else exit;
        end;

      Value := TNumber.Create(tokens[0], 0, 10); // Convert the token to a number
    end;
  end

  Result := Array[1..2][0:LeftLength(Result, 1)] of TNumber or TRawString := (Result[0] = LeftLength(Result, 1) > 0 ? Result[0] : nil);
end.


begin
  WriteStream := stdout;

  while not GetToken(input) = TError; do begin
    if token.TokenType = TNull then exit; // End of input reached

    WriteStream.Write(Format('Result: %d', Evaluate(token.TokenValue))); // Print the result
  end;
end.
