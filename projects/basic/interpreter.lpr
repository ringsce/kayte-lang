program BasicInterpreter;

uses
  SysUtils;

type
  TTokenType = (ttNumber, ttOperator, ttEnd, ttError);
  TToken = record
    TokenType: TTokenType;
    Value: string;
  end;

  TStack = record
    Items: array of string;
    Top: Integer;
  end;

var
  Input: string;
  TokenIndex: Integer;
  OperatorStack, OperandStack: TStack;

procedure InitializeStack(var Stack: TStack);
begin
  SetLength(Stack.Items, 100); // Allocate a fixed size for simplicity
  Stack.Top := -1;
end;

procedure Push(var Stack: TStack; Item: string);
begin
  Inc(Stack.Top);
  Stack.Items[Stack.Top] := Item;
end;

function Pop(var Stack: TStack): string;
begin
  if Stack.Top >= 0 then
  begin
    Result := Stack.Items[Stack.Top];
    Dec(Stack.Top);
  end
  else
    Result := '';
end;

function Peek(var Stack: TStack): string;
begin
  if Stack.Top >= 0 then
    Result := Stack.Items[Stack.Top]
  else
    Result := '';
end;

function GetToken(const Expr: string; var Index: Integer): TToken;
var
  Start: Integer;
begin
  while (Index <= Length(Expr)) and (Expr[Index] = ' ') do
    Inc(Index);

  if Index > Length(Expr) then
  begin
    Result.TokenType := ttEnd;
    Exit;
  end;

  if CharInSet(Expr[Index], ['0'..'9']) then
  begin
    Start := Index;
    while (Index <= Length(Expr)) and CharInSet(Expr[Index], ['0'..'9']) do
      Inc(Index);
    Result.TokenType := ttNumber;
    Result.Value := Copy(Expr, Start, Index - Start);
  end
  else if CharInSet(Expr[Index], ['+', '-', '*', '/', '%']) then
  begin
    Result.TokenType := ttOperator;
    Result.Value := Expr[Index];
    Inc(Index);
  end
  else
  begin
    Result.TokenType := ttError;
    Result.Value := Expr[Index];
  end;
end;

function EvaluateOperator(const A, B: string; const Op: string): string;
var
  NumA, NumB: Integer;
begin
  NumA := StrToInt(A);
  NumB := StrToInt(B);

  case Op of
    '+': Result := IntToStr(NumA + NumB);
    '-': Result := IntToStr(NumA - NumB);
    '*': Result := IntToStr(NumA * NumB);
    '/': if NumB <> 0 then
           Result := IntToStr(NumA div NumB)
         else
           raise Exception.Create('Division by zero');
    '%': if NumB <> 0 then
           Result := IntToStr(NumA mod NumB)
         else
           raise Exception.Create('Modulo by zero');
  else
    raise Exception.Create('Unknown operator: ' + Op);
  end;
end;

procedure ProcessStacks;
var
  B, A, Op: string;
begin
  B := Pop(OperandStack);
  A := Pop(OperandStack);
  Op := Pop(OperatorStack);

  if (A <> '') and (B <> '') and (Op <> '') then
    Push(OperandStack, EvaluateOperator(A, B, Op))
  else
    raise Exception.Create('Invalid expression');
end;

procedure ParseExpression(const Expr: string);
var
  CurrentToken: TToken;
begin
  TokenIndex := 1;
  while True do
  begin
    CurrentToken := GetToken(Expr, TokenIndex);
    if CurrentToken.TokenType = ttEnd then
      Break;

    case CurrentToken.TokenType of
      ttNumber:
        Push(OperandStack, CurrentToken.Value);
      ttOperator:
        Push(OperatorStack, CurrentToken.Value);
      ttError:
        raise Exception.Create('Unexpected token: ' + CurrentToken.Value);
    end;
  end;

  // Process remaining operators in the stack
  while OperatorStack.Top >= 0 do
    ProcessStacks;
end;

function Interpret(const Expr: string): string;
begin
  InitializeStack(OperatorStack);
  InitializeStack(OperandStack);

  ParseExpression(Expr);

  if OperandStack.Top = 0 then
    Result := Pop(OperandStack)
  else
    raise Exception.Create('Invalid expression');
end;

// Main program
begin
  WriteLn('Enter an expression: ');
  ReadLn(Input);
  try
    WriteLn('Result: ', Interpret(Input));
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.

