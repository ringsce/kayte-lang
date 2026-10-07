unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl, Math, TokenDefs, Lexer, AST, BytecodeTypes, Assembler;

type
  // Struct name -> ordered list of its field names, recorded at parse
  // time by StructDefinition. There's no runtime struct/object value:
  // a struct instance is just sugar for one flat variable slot per
  // field, named "InstanceName.FieldName" (see DeclarationStatement and
  // the dotted-identifier handling in AssignmentStatement/Primary).
  TStructFieldMap = specialize TFPGMap<string, TStringList>;

  // A CALL whose target address is filled in once the whole program has
  // been parsed, since a SUB may be defined after (below) its first call.
  // An argument that's a plain variable: if the parameter it's passed to
  // turns out to be ByRef, the two BC_NOP placeholders after the call
  // become "copy the parameter's final value back into the variable".
  TCopyBack = record
    ArgIndex: Integer;
    InstrIndex: Integer; // first of the two placeholders
    Slot: Integer;       // the variable
  end;

  TCopyBacks = array of TCopyBack;

  TPendingCall = record
    InstrIndex: Integer; // the BC_CALL to patch
    SubKey: string;      // upper-cased SUB / FUNCTION name
    ArgCount: Integer;
    Line: Integer;
    WantValue: Boolean;  // used in an expression: must be a FUNCTION
    CopyBacks: TCopyBacks;
  end;

  // A single-statement parsing method, used to plug the right statement
  // vocabulary into ParseBlockBody (see its comment).
  TStatementProc = procedure of object;

  // Raised by TParser.Parse when the source had errors. Each error has
  // already been printed (with its line) by then; the message only says
  // how many there were.
  EKayteParseError = class(Exception);

  // A loop being compiled, for EXIT FOR / EXIT WHILE: its kind and the
  // jumps to patch to just past it.
  TLoopExits = record
    Kind: string;                    // 'FOR', 'WHILE' or 'DO'
    TryDepth: Integer;               // TRY regions open around the loop (see FRegions)
    Jumps: array of Integer;         // EXIT: to just past the loop
    ContinueJumps: array of Integer; // CONTINUE: to the loop's next iteration
  end;

  TIntegerArray = array of Integer;

  // A CLASS, as the pre-scan finds it (so code above the class can use it).
  // An object is an array tagged with the class name: field i is item i.
  // Methods are procedures named "CLASS.METHOD" with a hidden last
  // parameter ME (the object).
  //
  // With INHERITS, a class starts with its base class's fields (at the same
  // indexes) and has its methods unless it defines its own (overrides).
  TClassInfo = class
    Name: string;        // as declared: TYPENAME(object), and the tag tested at run time
    BaseName: string;    // INHERITS
    Base: TClassInfo;
    Problem: string;     // found by the pre-scan (unknown base, cycle ...), reported at CLASS
    IsType: Boolean;     // a QuickBASIC TYPE: DIM x AS it makes the object
    OwnFields, OwnMethods: TStringList; // declared in this class
    FieldTypes: TStringList; // a TYPE's fields' types, as written (INTEGER, STRING*20 ...)
    Fields: TStringList; // upper-cased, in order: the base's, then its own
    // Upper-cased member name -> its kind, parameter count (not counting
    // ME) and the class whose procedure it is: "S2@SHAPE" a SUB, "F0@SHAPE"
    // a FUNCTION or PROPERTY GET; "NAME$LET" -> "L1@..." for a PROPERTY
    // LET / SET.
    Methods: TStringList;
    Resolving, Resolved: Boolean;
    constructor Create(const AName: string);
    destructor Destroy; override;
    function Sig(const Member: string): string;
    function KeyOf(const Member: string): string; // its procedure: "OWNER.MEMBER"
    function IsA(C: TClassInfo): Boolean;         // C itself or a base class of it
  end;

  // A TRY block (or the CATCH of one with FINALLY) being compiled: leaving
  // it early (EXIT, CONTINUE, RETURN) ends its TRY and runs its FINALLY.
  TRegion = record
    HasFinally: Boolean;
    Calls: TIntegerArray; // BC_CALLs to the FINALLY code, to patch
  end;

  TChainMode = (cmExpr, cmStatement, cmTarget);

  // Where a name / element / member being compiled lives (see ChainExpr).
  TPlaceKind = (
    pkNone,     // nothing: it was a SUB call
    pkValue,    // a value already on the stack
    pkVar,      // variable Slot
    pkMeField,  // field Index of ME (inside a method)
    pkIndex,    // array and index on the stack
    pkMember,   // member Member of the object in variable Slot
    pkIndexVars); // array in variable Slot, index in variable Index
  TPlace = record
    Kind: TPlaceKind;
    Slot, Index: Integer;
    Member, MemberName: string;
    Cls: TClassInfo; // pkVar / pkMember: the object's class, when known (ME, MYBASE)
    Exact: Boolean;  // MYBASE: that class's own members, not overrides
  end;

  TMemberKind = (mkGet, mkSet, mkCall);
  TMemberAction = record
    Cls: TClassInfo;
    IsField: Boolean;
    Index: Integer; // field
    Key: string;    // method / property procedure
  end;
  TMemberActions = array of TMemberAction;

  TParser = class
  private
    FErrorCount: Integer;
    FLoops: array of TLoopExits; // innermost last
    FHiddenCount: Integer;       // for compiler-made variables (MOD, SELECT CASE)
    FLexer: TLexer;
    FCurrentToken: TToken;
    FPreviousToken: TToken;
    FAssembler: TAssembler;
    FStructs: TStructFieldMap;
    // While True, the expression-parsing methods below still parse (and
    // consume tokens for) an expression but emit no bytecode for it.
    // Used for expressions that are parsed only for their side effect on
    // the token stream (e.g. constructor arguments to a not-yet-modeled
    // NEW ClassName(...)) so no value is left dangling on the VM stack.
    FSuppressCodeGen: Boolean;
    // Name of the CLASS currently being parsed; only meaningful while
    // ClassBodyStatement is on the call stack (see ClassDefinition).
    FCurrentClassName: string;
    // True while parsing a SUB body (RETURN only emits code there).
    // The SUB / FUNCTION being compiled ('' at top level), its locals
    // (parameters, DIMs, compiler-made variables, a FUNCTION's result -
    // upper case), and the jumps that leave it (RETURN / EXIT SUB /
    // EXIT FUNCTION) to patch to its epilogue.
    FProc: string;
    FProcIsFunction: Boolean;
    FLocals: TStringList;
    FReturnJumps: TIntegerArray;
    // GOSUB inside the procedure: whether it has one, and the jumps (after a
    // GOSUB returns) that continue an unwind towards its exit code.
    FProcUsesGosub: Boolean;
    FGosubUnwinds: TIntegerArray;
    FFunctions: TStringList; // keys of the FUNCTIONs (they return values)
    FByRef: TStringList;     // procedure key -> its parameters' ByRef flags ('0' / '1' each)
    // Labels ("<scope>|<NAME>" -> address; scope is the SUB / FUNCTION key,
    // '' at the top level) and the GOTOs to patch once all are known.
    FLabels: TStringIntMap;
    FGotos: array of TPendingCall; // SubKey = label key, InstrIndex = the BC_JUMP
    // Upper-cased SUB name -> parameter count; the entry addresses live in
    // the program's SubroutineMap, which is saved with the bytecode.
    FSubParams: TStringIntMap;
    FPendingCalls: array of TPendingCall;
    // From the pre-scan (see Prescan): top-level SUB / FUNCTION names, the
    // classes, names the program declares or assigns somewhere (so "a(1)"
    // can be told apart from a call to an undefined FUNCTION), and whether
    // there's any TRY.
    FProcNames: TStringList;
    FClasses: TStringList; // upper name -> TClassInfo
    FKnownNames: TStringList;
    FHasTry: Boolean;
    FStructVars: TStringList; // variables DIMmed AS a STRUCT (upper case)
    FProcKind: string;        // 'SUB', 'FUNCTION' or 'PROPERTY' while compiling one
    FCatchStack: TIntegerArray;   // CATCH variables of the CATCH blocks open (THROW re-throws the last)
    FCatchSlots: TIntegerArray;   // every CATCH variable (e.Message is e)
    FWithSlots: TIntegerArray;    // WITH objects, innermost last
    FInitNext: Integer;           // the class being compiled: its initializer's jump to patch
    FRegions: array of TRegion;   // TRY regions open in this SUB / top level, innermost last
    FFinallyLines: TStringList;   // lines of TRYs that have a FINALLY (pre-scan)
    FInFinally: Integer;          // inside FINALLY blocks
    FFinallyLoopBase: Integer;    // loops open when the innermost FINALLY started
    FInLineIf: Boolean;           // the statements of a single-line IF (no labels there)
    // QuickBASIC mode (kayte --qbs)
    FQB: Boolean;
    FSharedNames: TStringList;    // DIM SHARED / COMMON SHARED / CONST names, seen in SUBs
    FProcShared: TStringList;     // SHARED inside the current SUB
    FProcStatic: TStringList;     // STATIC inside the current SUB
    FDataItems: TStringList;      // every DATA item: 'I<number>' or 'S<text>'
    FDataLabels: TStringIntMap;   // label -> DATA items before it (for RESTORE label)
    FRestores: array of TPendingCall; // RESTORE label: SubKey = label key, InstrIndex = its BC_LOAD_INT
    FUsesData: Boolean;
    FUsesOnError: Boolean;        // QuickBASIC ON ERROR (pre-scan): statements get ids for RESUME
    FStmtStarts, FStmtEnds: TIntegerArray; // top-level statements' addresses, by id
    FTrapLabels: array of TPendingCall;    // ON ERROR GOTO labels, by trap id (SubKey: the label)
    FTrapTries: TIntegerArray;     // BC_TRYs that install the trap: patched to its handler
    FResumeJumps, FResumeNextJumps: TIntegerArray; // RESUME / RESUME NEXT: to their tables
    FPrintFile: Integer;          // PRINT # / WRITE #: the file number's variable, else -1
    FVarTypes: TStringList;       // QuickBASIC: variable -> its AS type (for GET / PUT layouts)
    FGosubOnEnds: TIntegerArray;  // ON ... GOSUB: jumps past the statement

    procedure Advance;
    procedure Match(ExpectedType: TTokenType);
    function Check(TokenType: TTokenType): Boolean;
    function MatchAny(const Types: array of TTokenType): Boolean;

    // Parsing rules (non-terminals)
    function Statement: TStatementNode;
    procedure DispatchStatement;
    procedure DispatchStatementCore;
    procedure MidStatement;
    procedure OnErrorStatement;
    procedure ResumeStatement;
    procedure EmitTrap(Install: Boolean);
    procedure EmitErrorTrap;
    procedure ParseBlockBody(const Context: string; StatementHandler: TStatementProc);
    procedure ParseBlockUntil(const Context: string; const Terminators: array of string);
    function AtKeyword(const Word: string): Boolean;
    procedure StructDefinition;
    procedure DeclarationStatement;
    procedure AssignmentStatement;
    procedure PrintStatement;
    procedure InputStatement;
    procedure MsgBoxStatement;
    procedure CallStatement;
    procedure ProcessStatement;
    procedure QtStatement;
    procedure QmlStatement;
    procedure CommandStatement(Op: TByteCodeOp; Operand3: Integer = 0);
    procedure GoToStatement;
    procedure DoStatement;
    procedure LabelDefinition;
    procedure ResolveGotos;
    procedure GoSubStatement;
    procedure ReturnStatement;
    procedure IfStatement;
    procedure WhileStatement;
    procedure ForStatement;
    procedure WithStatement;
    procedure ForEachStatement(const Context: string);
    procedure ReDimStatement;
    procedure TryStatement;
    procedure ThrowStatement;
    procedure Prescan;
    function ClassOf(const Name: string): TClassInfo;
    function CurrentClass: TClassInfo;
    function IsKnownVar(const Name: string): Boolean;
    function AtStatementEnd: Boolean;
    function AtAssign: Boolean;
    function ParseArgs(BareArgs: Boolean; out CopyBacks: TCopyBacks): Integer;
    procedure EmitCall(const Key: string; ArgCount, Line: Integer; WantValue: Boolean; const CopyBacks: TCopyBacks);
    function ParseDims: Integer;
    procedure NewExpression;
    procedure BuiltinCall(Id, MinArgs, MaxArgs: Integer; const Name: string);
    procedure ChainExpr(IsStatement: Boolean);
    procedure Materialize(var P: TPlace);
    procedure AssignTo(var P: TPlace);
    procedure MethodCall(ObjSlot: Integer; Static: TClassInfo; Exact: Boolean; const MName: string;
      Line: Integer; IsStatement: Boolean; var P: TPlace);
    function FindMembers(const MName: string; Static: TClassInfo; Exact: Boolean; Kind: TMemberKind;
      ArgCount: Integer; WantValue: Boolean; out NoCheck: Boolean): TMemberActions;
    procedure EmitDispatch(ObjSlot: Integer; const Acts: TMemberActions; Static: Boolean;
      const MName: string; ValueSlot, ArgCount, Line: Integer; WantValue: Boolean; const CopyBacks: TCopyBacks);
    procedure StoreToName(const Name: string);
    procedure ChainParse(Mode: TChainMode; out Res: TPlace);
    function ParseTarget: TPlace;
    procedure BeginStore(var P: TPlace);
    procedure EndStore(var P: TPlace);
    procedure LoadPlace(const P: TPlace);
    procedure Stabilize(var P: TPlace);
    procedure PushRegion(HasFinally: Boolean);
    function PopRegion: TRegion;
    procedure LeaveRegions(Level: Integer);
    procedure ConstStatement;
    procedure PowerExpr;
    procedure TypeOfExpr;
    procedure EmitNewObject(Cls: TClassInfo);
    procedure FillArray(ArrSlot, Dims: Integer; Cls: TClassInfo);
    procedure EmitEsc(const Seq: string);
    procedure QBPrintStatement;
    procedure WriteStatement;
    procedure DataStatement;
    procedure ReadStatement;
    procedure RestoreStatement;
    procedure OnStatement;
    procedure SwapStatement;
    procedure EraseStatement;
    procedure ColorStatement;
    procedure LocateStatement;
    procedure TypeDefinition;
    function BeginClassInit(Cls: TClassInfo): Integer;
    procedure EndClassInit(SelfSlot: Integer);
    function BeginInitPiece(SelfSlot, Idx: Integer): Integer;
    procedure EndInitPiece(Over: Integer);
    procedure EmitGosub(const LabelName: string; Line: Integer);
    procedure EmitGoto(const LabelName: string; Line: Integer);
    procedure EmitGotoAt(const LabelName: string; Line: Integer; const Scope: string);
    procedure LineNumberLabel;
    function QBStatement(const Word: string): Boolean;
    procedure SkipToLineEnd;
    procedure EmitInputConvert(const Name: string);
    function ParseFileNumber(HashRequired: Boolean): Integer;
    function AtHash: Boolean;
    procedure EmitPiece(Kind: Integer; HasValue: Boolean);
    procedure PrintUsing;
    procedure OpenStatement;
    procedure CloseStatement;
    procedure GetPutStatement;
    function LayoutFor(const Spec, Name: string; Depth: Integer): string;
    procedure EmitStartup;
    procedure LoadName(const Name: string);
    function MeFieldIndex(const Name: string): Integer;
    procedure FieldDeclaration(Cls: TClassInfo);
    procedure EmitStr(const S: string);
    procedure EmitInt(V: Int64);
    procedure EmitFloat(D: Double);
    procedure EmitNumber(const Text: string);
    procedure SubDefinition;
    procedure ProcedureDefinition(IsFunction: Boolean; const PropAccessor: string = '');
    procedure CallExpression(const Name: string; Line: Integer; WantValue: Boolean; BareArgs: Boolean = False);
    procedure DeclareLocal(const Name: string);
    procedure ResolveCalls;
    procedure FunctionDefinition;
    procedure ClassDefinition;
    procedure PropertyDefinition;
    procedure FormDefinition;
    procedure ShowStatement;
    procedure HideStatement;
    procedure OptionStatement;  // NEW: Handle OPTION directives

    // Recursive-descent expression parsing methods.
    // Each one emits the bytecode for what it parses directly (postfix /
    // stack-machine order), leaving exactly one value on the VM's
    // evaluation stack - unless FSuppressCodeGen is set, in which case
    // they only consume tokens and emit nothing.
    procedure Expression;
    procedure SkipExpression; // Parse and discard (see FSuppressCodeGen)
    procedure SkipGenericParams; // Parse and discard "<T, U, ...>" (type erasure)
    procedure LogicalXor;
    procedure QBImp;
    procedure QBEqv;
    procedure EmitQB(Id, Argc: Integer);
    procedure LogicalOr;
    procedure IntDivExpr;
    procedure ContinueStatement;
    procedure PatchContinues(Target: Integer);
    procedure LogicalAnd;
    procedure LogicalNot;
    procedure ModExpr;
    procedure SelectStatement;
    procedure ExitStatement;
    procedure PushLoop(const Kind: string);
    procedure PopLoop;
    function HiddenVar(const Purpose: string): Integer;
    function EmitJumpAt(Op: TByteCodeOp): Integer;
    procedure PatchHere(InstrIndex: Integer);
    procedure Equality;
    procedure Comparison;
    procedure Term;
    procedure Factor;
    procedure Unary;
    procedure Primary;

    // Helper functions
    procedure Error(const Message: String);
    procedure ReportError(const Message: String);
    procedure NextToken;
    function PeekToken: TToken;

    // Bytecode helper functions
    function Op_Variable(const VarName: string): Integer;
    function Op_StringLiteral(const Literal: string): Integer;
    function Op_IntegerLiteral(const Value: Int64): Integer;

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    function Parse: TByteCodeProgram;
    // QuickBASIC compatibility (kayte --qbs); set the lexer's QBMode too.
    property QBMode: Boolean read FQB write FQB;
  end;

implementation

{ TClassInfo }

constructor TClassInfo.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
  Fields := TStringList.Create;
  Methods := TStringList.Create;
  OwnFields := TStringList.Create;
  OwnMethods := TStringList.Create;
  FieldTypes := TStringList.Create;
end;

destructor TClassInfo.Destroy;
begin
  Fields.Free;
  Methods.Free;
  OwnFields.Free;
  OwnMethods.Free;
  FieldTypes.Free;
  inherited Destroy;
end;

function TClassInfo.Sig(const Member: string): string;
begin
  Result := Methods.Values[UpperCase(Member)];
  if Pos('@', Result) > 0 then
    Result := Copy(Result, 1, Pos('@', Result) - 1);
end;

function TClassInfo.KeyOf(const Member: string): string;
var
  V: string;
begin
  V := Methods.Values[UpperCase(Member)];
  Result := Copy(V, Pos('@', V) + 1, MaxInt) + '.' + UpperCase(Member);
end;

function TClassInfo.IsA(C: TClassInfo): Boolean;
var
  K: TClassInfo;
begin
  K := Self;
  while K <> nil do
  begin
    if K = C then
      Exit(True);
    K := K.Base;
  end;
  Result := False;
end;

const
  // Built-in functions: name, BI_* id, fewest and most arguments (-1: any).
  // A trailing "$" (LEFT$, CHR$ ...) is accepted and ignored.
  BuiltinCount = 49;
  BuiltinNames: array[0..BuiltinCount - 1] of string = (
    'LEN', 'LEFT', 'RIGHT', 'MID', 'UCASE', 'LCASE', 'TRIM', 'LTRIM', 'RTRIM', 'INSTR',
    'REPLACE', 'STR', 'VAL', 'CHR', 'ASC', 'SPACE', 'ABS', 'SGN', 'MIN', 'MAX',
    'UBOUND', 'LBOUND', 'ARRAY', 'JOIN', 'SPLIT', 'TYPENAME', 'ISARRAY', 'ISNUMERIC', 'CINT', 'RND',
    'CSTR', 'CLNG', 'UPPER', 'LOWER', 'INT', 'FIX', 'SQR', 'STRING', 'HEX', 'OCT',
    'CDBL', 'CSNG', 'ROUND', 'SIN', 'COS', 'TAN', 'ATN', 'EXP', 'LOG');
  BuiltinIds: array[0..BuiltinCount - 1] of Integer = (
    BI_LEN, BI_LEFT, BI_RIGHT, BI_MID, BI_UCASE, BI_LCASE, BI_TRIM, BI_LTRIM, BI_RTRIM, BI_INSTR,
    BI_REPLACE, BI_STR, BI_VAL, BI_CHR, BI_ASC, BI_SPACE, BI_ABS, BI_SGN, BI_MIN, BI_MAX,
    BI_UBOUND, BI_LBOUND, BI_ARRAY, BI_JOIN, BI_SPLIT, BI_TYPENAME, BI_ISARRAY, BI_ISNUMERIC, BI_CINT, BI_RND,
    BI_STR, BI_CINT, BI_UCASE, BI_LCASE, BI_INT, BI_FIX, BI_SQR, BI_STRING, BI_HEX, BI_OCT,
    BI_CDBL, BI_CDBL, BI_ROUND, BI_SIN, BI_COS, BI_TAN, BI_ATN, BI_EXP, BI_LOG);
  BuiltinMin: array[0..BuiltinCount - 1] of Integer = (
    1, 2, 2, 2, 1, 1, 1, 1, 1, 2,
    3, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 1, 1, 1, 1, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 2, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1);
  BuiltinMax: array[0..BuiltinCount - 1] of Integer = (
    1, 2, 2, 3, 1, 1, 1, 1, 1, 3,
    3, 1, 1, 1, 1, 1, 1, 1, -1, -1,
    2, 2, -1, 2, 2, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 2, 1, 1,
    1, 1, 2, 1, 1, 1, 1, 1, 1);

// Whether T is a number with a fraction or exponent ("2.5", "-.5", "1E3").
function IsFloatText(const T: string): Boolean;
var
  D: Double;
  Code: Integer;
  S: string;
begin
  S := T;
  if (S <> '') and (S[1] = '.') then
    S := '0' + S
  else if (Length(S) > 1) and (S[1] in ['-', '+']) and (S[2] = '.') then
    S := S[1] + '0' + Copy(S, 2, MaxInt);
  Val(S, D, Code);
  Result := (Code = 0) and not IsInfinite(D) and not IsNan(D);
end;

// The BI_* id of a built-in function (and its argument counts), or 0.
// In QuickBASIC mode STR$ gives a leading space for numbers >= 0.
function FindBuiltin(const Name: string; QB: Boolean; out MinArgs, MaxArgs: Integer): Integer;
var
  U: string;
  I: Integer;
begin
  Result := 0;
  MinArgs := 0;
  MaxArgs := 0;
  U := UpperCase(Name);
  // QuickBASIC's file functions
  if QB and ((U = 'EOF') or (U = 'LOF')) then
  begin
    MinArgs := 1;
    MaxArgs := 1;
    if U = 'EOF' then Exit(BI_EOF) else Exit(BI_LOF);
  end;
  if QB and (U = 'FREEFILE') then
    Exit(BI_FREEFILE);
  if QB then
  begin
    MinArgs := 1;
    MaxArgs := 1;
    case U of
      'LOC': Exit(BI_FLOC);
      'SEEK': Exit(BI_FSEEKPOS);
      'MKI$': Exit(BI_MKI);
      'MKL$': Exit(BI_MKL);
      'MKS$': Exit(BI_MKS);
      'MKD$': Exit(BI_MKD);
      'CVI': Exit(BI_CVI);
      'CVL': Exit(BI_CVL);
      'CVS': Exit(BI_CVS);
      'CVD': Exit(BI_CVD);
      'INPUT$':
        begin
          MinArgs := 2; // INPUT$(count, #n)
          MaxArgs := 2;
          Exit(BI_FINPUTS);
        end;
    end;
    MinArgs := 0;
    MaxArgs := 0;
  end;
  if (Length(U) > 1) and (U[Length(U)] = '$') then
    SetLength(U, Length(U) - 1);
  for I := 0 to BuiltinCount - 1 do
    if BuiltinNames[I] = U then
    begin
      MinArgs := BuiltinMin[I];
      MaxArgs := BuiltinMax[I];
      Result := BuiltinIds[I];
      if QB and (U = 'STR') then
        Result := BI_QBSTR;
      Exit;
    end;
end;

{ TParser }

// Reads the whole program once before compiling it, for what code may use
// before (above) its definition: the top-level SUB / FUNCTION names, each
// CLASS's fields and methods, the names the program declares or assigns
// (to tell an array "a(1)" from a call to an undefined FUNCTION), and
// whether it has a TRY.
procedure TParser.Prescan;
var
  Toks: array of TToken;
  N, I, J, D, Count, NonEmpty: Integer;
  T: TToken;
  U, Prev, Acc, MName: string;
  Cls: TClassInfo;
  InMember, FieldLine, DimLine, ExpectName, InType: Boolean;
  TryLines: TStringList;

  function LineStart(K: Integer): Boolean;
  begin
    Result := (K = 0) or (Toks[K - 1].TokenType in [tkEndOfLine, tkColon]) or
      // after a QuickBASIC line number
      (FQB and (Toks[K - 1].TokenType = tkIntegerLiteral) and
       ((K = 1) or (Toks[K - 2].TokenType = tkEndOfLine)));
  end;

  // The base class's fields first, then the class's own; its methods,
  // then the class's own (which override them).
  procedure ResolveClass(C: TClassInfo);
  var
    K: Integer;
  begin
    if C.Resolved then
      Exit;
    if C.Resolving then
    begin
      C.Problem := 'CLASS ' + C.Name + ' inherits from itself';
      C.BaseName := '';
    end;
    C.Resolving := True;
    if C.BaseName <> '' then
    begin
      C.Base := ClassOf(C.BaseName);
      if C.Base = nil then
        C.Problem := 'CLASS ' + C.Name + ' inherits from "' + C.BaseName + '", which isn''t a CLASS'
      else
      begin
        ResolveClass(C.Base);
        if C.Base.IsA(C) then
        begin
          C.Problem := 'CLASS ' + C.Name + ' inherits from itself';
          C.Base := nil;
        end
        else
        begin
          C.Fields.Assign(C.Base.Fields);
          C.Methods.Assign(C.Base.Methods);
        end;
      end;
    end;
    for K := 0 to C.OwnFields.Count - 1 do
      if C.Fields.IndexOf(C.OwnFields[K]) >= 0 then
        C.Problem := Format('CLASS %s: field "%s" is already in its base class', [C.Name, C.OwnFields[K]])
      else
        C.Fields.Add(C.OwnFields[K]);
    for K := 0 to C.OwnMethods.Count - 1 do
      C.Methods.Values[C.OwnMethods.Names[K]] := C.OwnMethods.ValueFromIndex[K] + '@' + UpperCase(C.Name);
    C.Resolving := False;
    C.Resolved := True;
  end;

begin
  Toks := nil;
  N := 0;
  FLexer.Reset;
  try
    repeat
      T := FLexer.GetNextToken;
      if T.TokenType = tkComment then
        Continue;
      if N = Length(Toks) then
        SetLength(Toks, N * 2 + 256);
      Toks[N] := T;
      Inc(N);
    until T.TokenType = tkEndOfFile;
  except
    // Compiling reports whatever the lexer can't read.
  end;
  FLexer.Reset;
  FCurrentToken := FLexer.GetNextToken;
  FPreviousToken := FCurrentToken;

  Cls := nil;
  InType := False;
  TryLines := TStringList.Create;
  InMember := False;
  FieldLine := False;
  DimLine := False;
  ExpectName := False;
  D := 0;
  for I := 0 to N - 1 do
  begin
    T := Toks[I];
    U := UpperCase(T.Lexeme);
    if I > 0 then
      Prev := UpperCase(Toks[I - 1].Lexeme)
    else
      Prev := '';
    case T.TokenType of
      tkEndOfLine, tkColon:
        begin
          FieldLine := False;
          DimLine := False;
          D := 0;
          Continue;
        end;
      tkParenthesisOpen: Inc(D);
      tkParenthesisClose: Dec(D);
    end;

    // QuickBASIC TYPE ... END TYPE: a class with only fields.
    if FQB then
    begin
      if (U = 'TYPE') and (Prev = 'END') then
      begin
        Cls := nil;
        InType := False;
        Continue;
      end;
      if (U = 'TYPE') and LineStart(I) and (I + 2 < N) and (Toks[I + 1].TokenType = tkIdentifier) and
         (Toks[I + 2].TokenType in [tkEndOfLine, tkEndOfFile]) then
      begin
        if FClasses.IndexOf(UpperCase(Toks[I + 1].Lexeme)) < 0 then
        begin
          Cls := TClassInfo.Create(Toks[I + 1].Lexeme);
          Cls.IsType := True;
          FClasses.AddObject(UpperCase(Cls.Name), Cls);
          InType := True;
        end;
        Continue;
      end;
      if InType then
      begin
        if (T.TokenType = tkIdentifier) and LineStart(I) and (Cls.OwnFields.IndexOf(U) < 0) then
        begin
          Cls.OwnFields.Add(U);
          // its type, for GET / PUT: name AS type [* n]
          Acc := '';
          if (I + 2 < N) and SameText(Toks[I + 1].Lexeme, 'AS') then
          begin
            Acc := UpperCase(Toks[I + 2].Lexeme);
            if (I + 4 < N) and (Toks[I + 3].Lexeme = '*') then
              Acc := Acc + '*' + Toks[I + 4].Lexeme;
          end;
          Cls.FieldTypes.Add(Acc);
        end;
        Continue;
      end;
    end;

    // INHERITS Base - on the CLASS line or the next one
    if (Cls <> nil) and not InMember and (U = 'INHERITS') and (I + 1 < N) then
    begin
      Cls.BaseName := Toks[I + 1].Lexeme;
      Continue;
    end;

    // QuickBASIC DEF FNname: a FUNCTION
    if FQB and (U = 'DEF') and (Prev <> 'END') and (Prev <> 'EXIT') and (I + 1 < N) and
       (Toks[I + 1].TokenType = tkIdentifier) and (UpperCase(Copy(Toks[I + 1].Lexeme, 1, 2)) = 'FN') then
    begin
      FProcNames.Add(UpperCase(Toks[I + 1].Lexeme));
      Continue;
    end;
    if FQB and (U = 'ERROR') and (Prev = 'ON') then
      FUsesOnError := True;

    if T.TokenType = tkKeyword then
    begin
      if U = 'TRY' then
      begin
        FHasTry := True;
        if Prev = 'END' then
        begin
          if TryLines.Count > 0 then
            TryLines.Delete(TryLines.Count - 1);
        end
        else
          TryLines.Add(IntToStr(T.Line));
      end;
      if (U = 'FINALLY') and (TryLines.Count > 0) then
        FFinallyLines.Add(TryLines[TryLines.Count - 1]);
      if (U = 'CLASS') and (Prev = 'END') then
      begin
        Cls := nil;
        Continue;
      end;
      if (U = 'CLASS') and (I + 1 < N) and (Toks[I + 1].TokenType = tkIdentifier) then
      begin
        if FClasses.IndexOf(UpperCase(Toks[I + 1].Lexeme)) < 0 then
        begin
          Cls := TClassInfo.Create(Toks[I + 1].Lexeme);
          FClasses.AddObject(UpperCase(Cls.Name), Cls);
        end
        else
          Cls := nil; // reported when it's compiled
        InMember := False;
        Continue;
      end;
      if (U = 'SUB') or (U = 'FUNCTION') or (U = 'PROPERTY') then
      begin
        if Prev = 'END' then
        begin
          InMember := False;
          Continue;
        end;
        if Prev = 'EXIT' then
          Continue;
        // A header: [PROPERTY GET|LET|SET] name [(params)]
        J := I + 1;
        Acc := '';
        if U = 'PROPERTY' then
        begin
          if J < N then
            Acc := UpperCase(Toks[J].Lexeme);
          Inc(J);
        end;
        if J >= N then
          Continue;
        MName := UpperCase(Toks[J].Lexeme);
        Count := 0;
        if (J + 1 < N) and (Toks[J + 1].TokenType = tkParenthesisOpen) then
        begin
          J := J + 2;
          D := 1;
          NonEmpty := 0;
          while (J < N) and (D > 0) and (Toks[J].TokenType <> tkEndOfLine) do
          begin
            case Toks[J].TokenType of
              tkParenthesisOpen: Inc(D);
              tkParenthesisClose: Dec(D);
              tkComma: if D = 1 then Inc(Count);
            end;
            if (D > 0) and (Toks[J].TokenType <> tkComma) then
              NonEmpty := 1;
            Inc(J);
          end;
          Count := Count + NonEmpty;
          D := 0;
        end;
        if Cls = nil then
        begin
          if U <> 'PROPERTY' then
            FProcNames.Add(MName);
        end
        else if not InMember then
        begin
          if U = 'SUB' then
            Cls.OwnMethods.Add(MName + '=S' + IntToStr(Count))
          else if (U = 'FUNCTION') or (Acc = 'GET') then
            Cls.OwnMethods.Add(MName + '=F' + IntToStr(Count))
          else
            Cls.OwnMethods.Add(MName + '$LET=L' + IntToStr(Count));
        end;
        InMember := True;
        FieldLine := False;
        DimLine := False;
        Continue;
      end;
    end;

    // DIM / REDIM / PUBLIC / PRIVATE lines: the names they declare (and in
    // a CLASS, outside its methods, its fields).
    if LineStart(I) and ((U = 'DIM') or (U = 'REDIM') or (U = 'PUBLIC') or (U = 'PRIVATE') or
       (U = 'PROTECTED') or (U = 'FRIEND') or (U = 'CONST')) then
    begin
      DimLine := True;
      FieldLine := (Cls <> nil) and not InMember;
      ExpectName := True;
      Continue;
    end;
    if DimLine then
    begin
      case U of
        'OVERRIDES', 'OVERRIDABLE', 'OVERLOADS', 'SHADOWS', 'NOTOVERRIDABLE', 'MUSTOVERRIDE', 'READONLY',
        'WITHEVENTS':
          Continue; // modifiers
      end;
      if (T.TokenType = tkIdentifier) and ExpectName and (D = 0) and (U <> 'PRESERVE') and (U <> 'SHARED') then
      begin
        FKnownNames.Add(U);
        if FieldLine and (Cls.OwnFields.IndexOf(U) < 0) then
          Cls.OwnFields.Add(U);
        ExpectName := False;
      end
      else if (T.TokenType = tkComma) and (D = 0) then
        ExpectName := True;
    end;
    if T.TokenType = tkIdentifier then
      if (Prev = 'EACH') or (Prev = 'CATCH') or (Prev = 'TO') or (Prev = 'FOR') or (Prev = 'INPUT') or
         (Prev = 'READ') or (Prev = 'SWAP') or (Prev = 'SHARED') or (Prev = 'STATIC') or
         ((Prev = ',') and (I > 1) and (UpperCase(Toks[I - 2].Lexeme) = 'SWAP')) or
         ((I + 1 < N) and (Toks[I + 1].TokenType = tkOperator) and (Toks[I + 1].Lexeme = '=')) then
        FKnownNames.Add(U);
    if FQB and (U = 'DATA') then
      FUsesData := True;
  end;
  TryLines.Free;
  // ON ERROR is a TRY too: SUBs restore their locals as an error passes
  FHasTry := FHasTry or FUsesOnError;
  for I := 0 to FClasses.Count - 1 do
    ResolveClass(TClassInfo(FClasses.Objects[I]));
end;

function TParser.ClassOf(const Name: string): TClassInfo;
var
  Idx: Integer;
begin
  Idx := FClasses.IndexOf(UpperCase(Name));
  if Idx < 0 then
    Result := nil
  else
    Result := TClassInfo(FClasses.Objects[Idx]);
end;

// The class whose method is being compiled, or nil.
function TParser.CurrentClass: TClassInfo;
begin
  if (FProc <> '') and (FCurrentClassName <> '') then
    Result := ClassOf(FCurrentClassName)
  else
    Result := nil;
end;

// Whether Name is a local, or a variable the program declares or assigns.
function TParser.IsKnownVar(const Name: string): Boolean;
begin
  Result := ((FProc <> '') and (FLocals.IndexOf(UpperCase(Name)) >= 0)) or
    (FAssembler.CurrentProgram.VariableMap.IndexOf(Name) >= 0) or
    (FKnownNames.IndexOf(UpperCase(Name)) >= 0);
end;

function TParser.AtStatementEnd: Boolean;
begin
  Result := Check(tkEndOfLine) or Check(tkEndOfFile) or Check(tkComment) or Check(tkColon) or
    AtKeyword('ELSE');
end;

function TParser.AtAssign: Boolean;
begin
  Result := Check(tkOperator) and (FCurrentToken.Lexeme = '=');
end;

procedure TParser.EmitStr(const S: string);
begin
  FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral('"' + S + '"')]);
end;

procedure TParser.EmitInt(V: Int64);
begin
  FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(V)]);
end;

// A double: BC_LOAD_FLOAT with its 64 bits in two operands.
procedure TParser.EmitFloat(D: Double);
var
  Bits: QWord;
begin
  Move(D, Bits, SizeOf(Bits));
  FAssembler.Emit(BC_LOAD_FLOAT, [LongInt(LongWord(Bits and $FFFFFFFF)), LongInt(LongWord(Bits shr 32))]);
end;

// A number literal: whole if it is one (and fits), else a double.
procedure TParser.EmitNumber(const Text: string);
var
  I: Int64;
  D: Double;
  Code: Integer;
begin
  if TryStrToInt64(Text, I) then
    EmitInt(I)
  else
  begin
    if (Text <> '') and (Text[1] = '.') then
      Val('0' + Text, D, Code)
    else if (Length(Text) > 1) and (Text[1] in ['-', '+']) and (Text[2] = '.') then
      Val(Text[1] + '0' + Copy(Text, 2, MaxInt), D, Code)
    else
      Val(Text, D, Code);
    if (Code <> 0) or IsInfinite(D) or IsNan(D) then
      Error('The number ' + Text + ' is too large');
    EmitFloat(D);
  end;
end;

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;

  if ALexer = nil then
    raise Exception.Create('Lexer cannot be nil');

  FLexer := ALexer;
  FSuppressCodeGen := False;
  FStructs := TStructFieldMap.Create;
  FSubParams := TStringIntMap.Create;
  FLocals := TStringList.Create;
  FFunctions := TStringList.Create;
  FByRef := TStringList.Create;
  FLabels := TStringIntMap.Create;
  FProcNames := TStringList.Create;
  FClasses := TStringList.Create;
  FClasses.OwnsObjects := True;
  FKnownNames := TStringList.Create;
  FKnownNames.Sorted := True;
  FKnownNames.Duplicates := dupIgnore;
  FStructVars := TStringList.Create;
  FFinallyLines := TStringList.Create;
  FSharedNames := TStringList.Create;
  FProcShared := TStringList.Create;
  FProcStatic := TStringList.Create;
  FDataItems := TStringList.Create;
  FDataLabels := TStringIntMap.Create;
  FPrintFile := -1;
  FVarTypes := TStringList.Create;
  FCurrentToken := FLexer.GetNextToken;
  FPreviousToken := FCurrentToken;
  FAssembler := TAssembler.Create;
end;

destructor TParser.Destroy;
var
  I: Integer;
begin
  for I := 0 to FStructs.Count - 1 do
    FStructs.Data[I].Free;
  FStructs.Free;
  FSubParams.Free;
  FLocals.Free;
  FFunctions.Free;
  FByRef.Free;
  FLabels.Free;
  FProcNames.Free;
  FClasses.Free;
  FKnownNames.Free;
  FStructVars.Free;
  FFinallyLines.Free;
  FSharedNames.Free;
  FProcShared.Free;
  FProcStatic.Free;
  FDataItems.Free;
  FDataLabels.Free;
  FVarTypes.Free;
  FAssembler.Free;
  inherited Destroy;
end;

function TParser.Parse: TByteCodeProgram;
begin
  FAssembler.SetProgramTitle('Kayte Program');
  Prescan;
  // Replaced by a jump to the start-up code, if the program needs any
  // (see EmitStartup).
  FAssembler.Emit(BC_NOP, []);

  while FCurrentToken.TokenType = tkComment do
    Advance;

  while FCurrentToken.TokenType <> tkEndOfFile do
  begin
    try
      while (FCurrentToken.TokenType = tkEndOfLine) or
            (FCurrentToken.TokenType = tkComment) do
        Advance;

      if FCurrentToken.TokenType = tkEndOfFile then
        Break;

      // OPTION EXPLICIT ON/OFF is recognized by the lexer as its own
      // token type (not routed through the OPTION keyword statement),
      // so it needs handling here before falling through to DispatchStatement.
      if (FCurrentToken.TokenType = tkOptionExplicitOn) or
         (FCurrentToken.TokenType = tkOptionExplicitOff) then
      begin
        Advance;
        Continue;
      end;

      DispatchStatement;

      while Check(tkEndOfLine) do
        Advance;

    except
      on E: Exception do
      begin
        ReportError(E.Message);
        // Skip to the next line and keep going, so one compile reports
        // every error; Parse then fails instead of returning a program.
        try
          while (FCurrentToken.TokenType <> tkEndOfLine) and
                (FCurrentToken.TokenType <> tkEndOfFile) do
            Advance;
          if FCurrentToken.TokenType = tkEndOfLine then
            Advance;
        except
          // The lexer can't get past this point (e.g. a character it
          // doesn't know), so there's nothing more to check.
          on E2: Exception do
          begin
            ReportError(E2.Message);
            raise EKayteParseError.CreateFmt('%d error(s) - compilation stopped, nothing was written',
              [FErrorCount]);
          end;
        end;
      end;
    end;
  end;

  EmitErrorTrap;
  EmitStartup;
  ResolveGotos;
  ResolveCalls;
  // A program with errors would run with holes in it (skipped lines,
  // unresolved CALLs), so none is returned - and nothing gets written.
  if FErrorCount > 0 then
    raise EKayteParseError.CreateFmt('%d error(s) - compilation stopped, nothing was written',
      [FErrorCount]);
  Result := FAssembler.GetProgram;
end;

// Prints one compile error and counts it. Messages raised by the lexer,
// parser or assembler already carry their own "... Error:" prefix.
procedure TParser.ReportError(const Message: String);
begin
  Inc(FErrorCount);
  if Pos('Error:', Message) > 0 then
    Writeln(Message)
  else
    Writeln('Parser Error: ', Message);
end;

// Points every CALL at its SUB's entry address. Unknown SUBs and wrong
// argument counts are reported like other parser errors; an unresolved
// call keeps target -1, which the VM reports if it's ever executed.
procedure TParser.ResolveCalls;
var
  I, J, Idx: Integer;
  Call: TPendingCall;
  SubMap: TStringIntMap;
  Flags: string;
begin
  SubMap := FAssembler.CurrentProgram.SubroutineMap;
  for I := 0 to High(FPendingCalls) do
  begin
    Call := FPendingCalls[I];
    Idx := SubMap.IndexOf(Call.SubKey);
    if Idx < 0 then
    begin
      ReportError(Format('Call to undefined SUB or FUNCTION "%s" at line %d', [Call.SubKey, Call.Line + 1]));
      Continue;
    end;
    if Call.WantValue and (FFunctions.IndexOf(Call.SubKey) < 0) then
    begin
      ReportError(Format('%s is a SUB, so it has no value to use at line %d - make it a FUNCTION',
        [Call.SubKey, Call.Line + 1]));
      Continue;
    end;
    if FSubParams.KeyData[Call.SubKey] <> Call.ArgCount then
    begin
      // A method's count includes its hidden ME.
      J := Ord(Pos('.', Call.SubKey) > 0);
      ReportError(Format('%s takes %d argument(s), but the call at line %d passes %d',
        [Call.SubKey, FSubParams.KeyData[Call.SubKey] - J, Call.Line + 1, Call.ArgCount - J]));
    end;
    FAssembler.PatchJumpTarget(Call.InstrIndex, SubMap.Data[Idx]);
    Flags := FByRef.Values[Call.SubKey];
    for J := 0 to High(Call.CopyBacks) do
      if (Call.CopyBacks[J].ArgIndex < Length(Flags)) and (Flags[Call.CopyBacks[J].ArgIndex + 1] = '1') then
      begin
        FAssembler.ReplaceInstruction(Call.CopyBacks[J].InstrIndex, BC_LOAD_VAR,
          FAssembler.CurrentProgram.AddVariable(Format('__byref$%s$%d', [Call.SubKey, Call.CopyBacks[J].ArgIndex])));
        FAssembler.ReplaceInstruction(Call.CopyBacks[J].InstrIndex + 1, BC_STORE_VAR, Call.CopyBacks[J].Slot);
      end;
  end;
end;

procedure TParser.NextToken;
begin
  FCurrentToken := FLexer.GetNextToken;
end;

procedure TParser.Advance;
begin
  FPreviousToken := FCurrentToken;
  FCurrentToken := FLexer.GetNextToken;
end;

function TParser.PeekToken: TToken;
begin
  Result := FLexer.PeekNextToken;
end;

procedure TParser.Match(ExpectedType: TTokenType);
begin
  if FCurrentToken.TokenType = ExpectedType then
    Advance
  else
    raise Exception.CreateFmt('Parser Error: Expected %s but found %s ("%s") at %d:%d',
      [GetTokenTypeName(ExpectedType),
       GetTokenTypeName(FCurrentToken.TokenType),
       FCurrentToken.Lexeme,
       FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
end;

function TParser.Check(TokenType: TTokenType): Boolean;
begin
  Result := FCurrentToken.TokenType = TokenType;
end;

function TParser.MatchAny(const Types: array of TTokenType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Types) to High(Types) do
  begin
    if FCurrentToken.TokenType = Types[i] then
    begin
      Result := True;
      Break;
    end;
  end;
  if Result then Advance;
end;

procedure TParser.Error(const Message: String);
begin
  raise Exception.CreateFmt('Parser Error: %s at %d:%d (Token: "%s" Type: %s)',
    [Message, FCurrentToken.Line + 1, FCurrentToken.Column + 1, FCurrentToken.Lexeme,
     GetTokenTypeName(FCurrentToken.TokenType)]);
end;

// Helper functions for bytecode operand encoding
function TParser.Op_Variable(const VarName: string): Integer;
var
  U: string;
begin
  // QuickBASIC: a SUB's variables are its own unless SHARED (or DIM SHARED,
  // CONST at the top level); STATIC ones keep their value between calls.
  if FQB and (FProc <> '') and (FProcKind <> 'DEF') then // DEF FN sees the module's variables
  begin
    U := UpperCase(VarName);
    if FProcStatic.IndexOf(U) >= 0 then
      Exit(FAssembler.CurrentProgram.AddVariable(FProc + '$static$' + U));
    if (FLocals.IndexOf(U) < 0) and (FProcShared.IndexOf(U) < 0) and (FSharedNames.IndexOf(U) < 0) and
       (Copy(U, 1, 2) <> '__') and (Pos('.', U) = 0) then
      DeclareLocal(VarName);
  end;
  // Inside a SUB / FUNCTION its locals get their own variables, named
  // "<PROC>:<NAME>"; every other name is a global.
  if (FProc <> '') and (FLocals.IndexOf(UpperCase(VarName)) >= 0) then
    Exit(FAssembler.CurrentProgram.AddVariable(FProc + ':' + UpperCase(VarName)));
  // Uses CurrentProgram, not GetProgram: GetProgram transfers ownership and
  // nils out the assembler's program on first use, so it must only be
  // called once, after parsing has fully completed.
  Result := FAssembler.CurrentProgram.AddVariable(VarName);
end;

// Makes Name a local of the SUB / FUNCTION being compiled (no-op at the
// top level, where everything is global).
procedure TParser.DeclareLocal(const Name: string);
begin
  if (FProc <> '') and (FLocals.IndexOf(UpperCase(Name)) < 0) then
    FLocals.Add(UpperCase(Name));
end;

function TParser.Op_StringLiteral(const Literal: string): Integer;
begin
  Result := FAssembler.CurrentProgram.AddStringConstant(Literal);
end;

function TParser.Op_IntegerLiteral(const Value: Int64): Integer;
begin
  Result := FAssembler.CurrentProgram.AddIntegerLiteral(Value);
end;

//----------------------------------------------------------------------
// Parsing Rules
//----------------------------------------------------------------------

function TParser.Statement: TStatementNode;
begin
  Result := nil;
end;

// Dispatches a single keyword-based statement starting at FCurrentToken.
// Shared by the top-level parse loop and by any construct that embeds a
// single statement (single-line IF-THEN, WHILE/FOR loop bodies, and -
// via ParseBlockBody - WITH/SUB/FUNCTION/FORM bodies).
// With QuickBASIC ON ERROR in the program, each top-level statement
// records its id in __stmt as it starts, and its start / end addresses
// are kept, for RESUME (the statement again) and RESUME NEXT (the one
// after it) - see EmitErrorTrap.
procedure TParser.DispatchStatement;
var
  K: Integer;
begin
  if not (FUsesOnError and (FProc = '')) then
  begin
    DispatchStatementCore;
    Exit;
  end;
  K := Length(FStmtStarts);
  SetLength(FStmtStarts, K + 1);
  SetLength(FStmtEnds, K + 1);
  FStmtStarts[K] := FAssembler.CurrentInstructionIndex;
  FStmtEnds[K] := -1;
  EmitInt(K);
  FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable('__stmt')]);
  try
    DispatchStatementCore;
  finally
    FStmtEnds[K] := FAssembler.CurrentInstructionIndex;
  end;
end;

procedure TParser.DispatchStatementCore;
var
  Word: string;
begin
  Word := AnsiUpperCase(FCurrentToken.Lexeme);
  // MID$(s$, start[, length]) = value$ - replaces part of s$
  if ((Word = 'MID$') or (Word = 'MID')) and (PeekToken.TokenType = tkParenthesisOpen) then
  begin
    MidStatement;
    Exit;
  end;
  if FQB then
  begin
    if Check(tkIntegerLiteral) and not FInLineIf then
    begin
      LineNumberLabel; // 100 PRINT ...
      Exit;
    end;
    if QBStatement(Word) then
      Exit;
    if Check(tkIdentifier) then
      case Word of
        // Kayte's own statements: ordinary names in QuickBASIC
        'SHOW', 'HIDE', 'FORM', 'PROCESS', 'QT', 'QML', 'STRUCT', 'CLASS', 'PROPERTY', 'WITH', 'TRY',
        'CATCH', 'FINALLY', 'THROW', 'CONTINUE', 'MSGBOX', 'SET', 'PUBLIC', 'PRIVATE':
          Word := '';
      end;
  end;
  case Word of
    'CONST':
      ConstStatement;
    'LINE':
      if (PeekToken.TokenType = tkKeyword) and SameText(PeekToken.Lexeme, 'INPUT') then
        InputStatement
      else if FQB then
        Error('LINE (graphics) isn''t supported')
      else
        ChainExpr(True);
    'END':
      begin
        // A plain END stops the program; END <keyword> closes a block, and
        // reaching one here means there's no such block open.
        if PeekToken.TokenType = tkKeyword then
          Error('END ' + AnsiUpperCase(PeekToken.Lexeme) + ' without a matching ' +
            AnsiUpperCase(PeekToken.Lexeme));
        Advance;
        FAssembler.Emit(BC_HALT, []);
      end;
    'OPTION':
      OptionStatement;
    'PRINT':
      PrintStatement;
    'PROCESS':
      ProcessStatement;
    'QT':
      QtStatement;
    'QML':
      QmlStatement;
    'INPUT':
      InputStatement;
    'MSGBOX':
      MsgBoxStatement;
    'LET':
      begin
        Advance; // Consume LET
        AssignmentStatement;
      end;
    'SET':
      begin
        Advance; // Consume SET
        AssignmentStatement;
      end;
    'GOTO':
      GoToStatement;
    'GOSUB':
      GoSubStatement;
    'RETURN':
      ReturnStatement;
    'IF':
      IfStatement;
    'WHILE':
      WhileStatement;
    'DO':
      DoStatement;
    'SELECT':
      SelectStatement;
    'EXIT':
      ExitStatement;
    'CONTINUE':
      ContinueStatement;
    'FOR':
      ForStatement;
    'WITH':
      WithStatement;
    'SUB':
      SubDefinition;
    'FUNCTION':
      FunctionDefinition;
    'CLASS':
      ClassDefinition;
    'FORM':
      FormDefinition;
    'SHOW':
      ShowStatement;
    'HIDE':
      HideStatement;
    'DIM', 'PUBLIC', 'PRIVATE':
      DeclarationStatement;
    'REDIM':
      ReDimStatement;
    'CALL':
      CallStatement;
    'STRUCT':
      StructDefinition;
    'TRY':
      TryStatement;
    'THROW':
      ThrowStatement;
    'CATCH', 'FINALLY':
      Error(AnsiUpperCase(FCurrentToken.Lexeme) + ' without a matching TRY');
    'PROPERTY':
      Error('PROPERTY is only allowed inside a CLASS');
  else
    if FCurrentToken.TokenType = tkIdentifier then
    begin
      // A label, or an assignment / call (see ChainExpr):
      //   x = 1   a(i) = 2   p.X = 3   obj.Method 1, 2   Greet "Ada"
      if (PeekToken.TokenType = tkColon) and not FInLineIf then
        LabelDefinition // name:
      else
        ChainExpr(True);
    end
    else if Check(tkDot) then
      ChainExpr(True) // .Member inside WITH
    else if (FCurrentToken.TokenType = tkEndOfLine) or (FCurrentToken.TokenType = tkColon) then
      Advance // blank line, or ":" separating statements on one line
    else
      Error('Unexpected token: ' + FCurrentToken.Lexeme);
  end;
end;

// Parses statements via StatementHandler until the next token is the
// keyword "END" that closes the block, skipping blank lines and comments
// in between. Shared by every block construct (WITH/SUB/FUNCTION/FORM/
// CLASS) so each one only has to supply what "a statement" means inside
// it and what to do with the block's name - the surrounding "loop until
// END, bail on EOF, skip blank lines" plumbing lives here once.
// Does not consume the "END" token itself, nor the keyword that follows
// it (e.g. "SUB") - the caller matches those since only it knows what
// they should be.
procedure TParser.ParseBlockBody(const Context: string; StatementHandler: TStatementProc);
begin
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in ' + Context);
      Break;
    end;

    if Check(tkEndOfLine) or Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    StatementHandler;

    while Check(tkEndOfLine) do
      Advance;
  end;
end;

// Like ParseBlockBody, but the block ends at any of the given keywords
// (e.g. NEXT for FOR, or ELSE / ELSEIF / END / ENDIF inside a block IF).
// Nested blocks consume their own terminators, so a keyword seen here at
// statement level belongs to this block. Doesn't consume the terminator.
procedure TParser.ParseBlockUntil(const Context: string; const Terminators: array of string);
var
  T: string;
begin
  while True do
  begin
    if FCurrentToken.TokenType = tkEndOfFile then
      Error('Reached the end of the file inside ' + Context);
    if Check(tkEndOfLine) or Check(tkComment) then
    begin
      Advance;
      Continue;
    end;
    if Check(tkKeyword) then
      for T in Terminators do
        // END closes a block only as END <keyword>; a plain END is the
        // statement that stops the program.
        if (AnsiUpperCase(FCurrentToken.Lexeme) = T) and
           ((T <> 'END') or (PeekToken.TokenType = tkKeyword)) then
          Exit;
    // A statement's error is reported and parsing goes on with the next
    // line, so one compile lists every error and the enclosing block (a
    // SUB, a loop) stays open instead of unwinding.
    try
      // END <something> that isn't this block's own terminator closes the
      // wrong kind of block (e.g. END WHILE inside a FOR loop).
      if AtKeyword('END') and (PeekToken.TokenType = tkKeyword) then
        Error('END ' + AnsiUpperCase(PeekToken.Lexeme) + ' doesn''t close ' + Context);
      DispatchStatement;
    except
      on E: EKayteParseError do
        raise;
      on E: Exception do
      begin
        ReportError(E.Message);
        try
          while not (Check(tkEndOfLine) or Check(tkEndOfFile)) do
            Advance;
        except
          // The lexer can't get past this point: nothing more to check.
          on E2: Exception do
          begin
            ReportError(E2.Message);
            raise EKayteParseError.CreateFmt('%d error(s) - compilation stopped, nothing was written',
              [FErrorCount]);
          end;
        end;
      end;
    end;
    while Check(tkEndOfLine) do
      Advance;
  end;
end;

function TParser.AtKeyword(const Word: string): Boolean;
begin
  Result := Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = Word);
end;

// STRUCT Name
//   Field1 [AS Type]
//   Field2 [AS Type]
// END STRUCT
//
// Purely a compile-time declaration: it records the struct's field names
// so DIM can expand "DIM P AS Point" into one flat variable slot per
// field ("P.X", "P.Y", ...). There's no runtime struct value or type
// checking - fields are dynamically typed slots like any other variable.
procedure TParser.StructDefinition;
var
  StructName, FieldName: string;
  Fields: TStringList;
begin
  Match(tkKeyword); // Consume STRUCT

  if not Check(tkIdentifier) then
    Error('Expected struct name after STRUCT');
  StructName := FCurrentToken.Lexeme;
  Advance; // Consume struct name
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams

  if FStructs.IndexOf(StructName) >= 0 then
    Error('Struct "' + StructName + '" is already defined');

  Match(tkEndOfLine);

  Fields := TStringList.Create;

  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Fields.Free;
      Error('Unexpected end of file in STRUCT definition');
      Exit;
    end;

    if Check(tkEndOfLine) or Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    if not Check(tkIdentifier) then
    begin
      Fields.Free;
      Error('Expected field name in STRUCT body, found: ' + FCurrentToken.Lexeme);
      Exit;
    end;

    FieldName := FCurrentToken.Lexeme;
    Advance; // Consume field name

    // Optional "AS Type" - parsed but not enforced (see unit comment).
    if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
    begin
      Advance; // Consume AS
      if not Check(tkIdentifier) then
      begin
        Fields.Free;
        Error('Expected type name after AS');
        Exit;
      end;
      Advance; // Consume type name
      SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
    end;

    Fields.Add(FieldName);

    while Check(tkEndOfLine) do
      Advance;
  end;

  Match(tkKeyword); // END
  Match(tkKeyword); // STRUCT

  FStructs.Add(StructName, Fields);
end;

procedure TParser.OptionStatement;
begin
  Advance; // Consume OPTION

  // OPTION directives (e.g. "Explicit On", "Base 0") are compile-time only
  // and generate no bytecode, so the rest of the line is just discarded.
  while not Check(tkEndOfLine) and not Check(tkEndOfFile) do
    Advance;
end;

procedure TParser.IfStatement;
var
  ToNext, I: Integer;
  Context: string;
  EndJumps: array of Integer;

  procedure JumpToEnd;
  begin
    SetLength(EndJumps, Length(EndJumps) + 1);
    EndJumps[High(EndJumps)] := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
  end;

  procedure ExpectThen;
  begin
    if AtKeyword('THEN') then
      Advance
    else if not (FQB and AtKeyword('GOTO')) then // QuickBASIC: IF x GOTO 100
      Error('Expected THEN after IF condition');
  end;

  // The statements of one branch of a single-line IF: "a: b: c", up to
  // ELSE or the end of the line. QuickBASIC: a line number is a GOTO.
  procedure LineBranch;
  var
    SavedInLineIf: Boolean;
  begin
    if FQB and Check(tkIntegerLiteral) then
    begin
      EmitGoto(FCurrentToken.Lexeme, FCurrentToken.Line);
      Advance;
      Exit;
    end;
    SavedInLineIf := FInLineIf;
    FInLineIf := True;
    try
      repeat
        DispatchStatement;
        if not Check(tkColon) then
          Break;
        Advance; // :
      until Check(tkEndOfLine) or Check(tkEndOfFile) or Check(tkComment) or AtKeyword('ELSE');
    finally
      FInLineIf := SavedInLineIf;
    end;
  end;

  // The condition's value is on the stack: jump past the branch if false.
  function JumpIfFalse: Integer;
  begin
    Result := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  end;

begin
  Context := Format('the IF block that starts at line %d - is END IF missing?', [FCurrentToken.Line + 1]);
  Match(tkKeyword); // Consume IF
  Expression; // Leaves the condition's truth value on the stack
  ExpectThen;
  ToNext := JumpIfFalse;
  EndJumps := nil;

  // Single-line form: IF cond THEN statement [ELSE statement]
  if not (Check(tkEndOfLine) or Check(tkComment) or Check(tkEndOfFile)) then
  begin
    LineBranch;
    if AtKeyword('ELSE') then
    begin
      Advance;
      JumpToEnd;
      FAssembler.PatchJumpTarget(ToNext, FAssembler.CurrentInstructionIndex);
      if not (Check(tkEndOfLine) or Check(tkEndOfFile)) then
        LineBranch;
    end
    else
      FAssembler.PatchJumpTarget(ToNext, FAssembler.CurrentInstructionIndex);
    for I := 0 to High(EndJumps) do
      FAssembler.PatchJumpTarget(EndJumps[I], FAssembler.CurrentInstructionIndex);
    Exit;
  end;

  // Block form:
  //   IF cond THEN
  //     ...
  //   [ELSEIF cond THEN
  //     ...]...
  //   [ELSE
  //     ...]
  //   END IF            (or ENDIF)
  // Each branch ends with a jump past END IF; a false condition jumps to
  // the next ELSEIF / ELSE / END IF.
  ParseBlockUntil(Context, ['ELSEIF', 'ELSE', 'END', 'ENDIF']);
  while AtKeyword('ELSEIF') do
  begin
    Advance;
    JumpToEnd;
    FAssembler.PatchJumpTarget(ToNext, FAssembler.CurrentInstructionIndex);
    Expression;
    ExpectThen;
    if not (Check(tkEndOfLine) or Check(tkComment)) then
      Error('ELSEIF ... THEN must end the line (the branch goes on the lines below)');
    ToNext := JumpIfFalse;
    ParseBlockUntil(Context, ['ELSEIF', 'ELSE', 'END', 'ENDIF']);
  end;
  if AtKeyword('ELSE') then
  begin
    Advance;
    JumpToEnd;
    FAssembler.PatchJumpTarget(ToNext, FAssembler.CurrentInstructionIndex);
    ToNext := -1;
    ParseBlockUntil(Context, ['ELSEIF', 'ELSE', 'END', 'ENDIF']);
    if AtKeyword('ELSEIF') or AtKeyword('ELSE') then
      Error('ELSE must be the last branch of an IF block');
  end;
  if ToNext >= 0 then
    FAssembler.PatchJumpTarget(ToNext, FAssembler.CurrentInstructionIndex);

  if AtKeyword('ENDIF') then
    Advance
  else
  begin
    Advance; // END
    if not AtKeyword('IF') then
      Error('Expected END IF to close the IF block');
    Advance; // IF
  end;
  for I := 0 to High(EndJumps) do
    FAssembler.PatchJumpTarget(EndJumps[I], FAssembler.CurrentInstructionIndex);
end;

// DIM / PUBLIC / PRIVATE name [(bounds)] [AS [NEW] Type] [= value], ...
//
//   DIM a(10)          an array a(0) .. a(10), all 0
//   DIM m(3, 4)        an array of arrays, m(i, j)
//   DIM list()         an empty array (grow it with REDIM PRESERVE)
//   DIM p AS NEW Point(1, 2)   an object
//   DIM n AS Integer = 5
//   DIM a, b, c AS Pt  a STRUCT type applies to every name before it
// Types other than STRUCTs aren't checked. Arrays and objects are
// references: "b = a" makes b the same array as a.
procedure TParser.DeclarationStatement;
var
  Group: TStringList; // names the next AS applies to (Objects: dimension count)
  TypeName, Name, Spec: string;
  StructIdx, I, J, Dims: Integer;
  Fields: TStringList;
  HasDims, Shared: Boolean;
  Cls: TClassInfo;
begin
  Advance; // DIM / PUBLIC / PRIVATE
  Shared := FQB and Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'SHARED');
  if Shared then
    Advance; // DIM SHARED: seen in SUBs too
  // PUBLIC SUB / PRIVATE FUNCTION ...: the modifier changes nothing.
  if AtKeyword('SUB') or AtKeyword('FUNCTION') then
  begin
    ProcedureDefinition(AtKeyword('FUNCTION'));
    Exit;
  end;
  if AtKeyword('PROPERTY') then
    Error('PROPERTY is only allowed inside a CLASS');

  Group := TStringList.Create;
  try
    repeat
      if not Check(tkIdentifier) then
        Error('Expected variable name in declaration');
      Name := FCurrentToken.Lexeme;
      Advance;
      if Shared then
        FSharedNames.Add(UpperCase(Name));
      DeclareLocal(Name);
      HasDims := Check(tkParenthesisOpen);
      if HasDims then
      begin
        Dims := ParseDims;
        FAssembler.Emit(BC_BUILTIN, [BI_NEWARRAY, Dims]);
        FAssembler.Emit(BC_STORE_VAR, [Op_Variable(Name)]);
        if Name[Length(Name)] = '$' then
          FillArray(Op_Variable(Name), Dims, nil); // name$(n): strings
        Group.AddObject(Name, TObject(PtrInt(Dims)));
      end
      else
      begin
        Op_Variable(Name);
        Group.AddObject(Name, nil);
      end;

      if AtKeyword('AS') then
      begin
        Advance; // AS
        if AtKeyword('NEW') then
        begin
          if (Group.Count > 1) or HasDims then
            Error('AS NEW makes one object: declare each object variable on its own');
          NewExpression;
          FAssembler.Emit(BC_STORE_VAR, [Op_Variable(Name)]);
        end
        else
        begin
          if not Check(tkIdentifier) then
            Error('Expected type name after AS');
          TypeName := FCurrentToken.Lexeme;
          Advance;
          SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
          Spec := UpperCase(TypeName);
          if FQB and Check(tkOperator) and (FCurrentToken.Lexeme = '*') then
          begin
            // STRING * n: a fixed length (for GET / PUT)
            Advance;
            if not Check(tkIntegerLiteral) then
              Error('Expected a length after STRING *');
            Spec := Spec + '*' + FCurrentToken.Lexeme;
            Advance;
          end;
          if FQB then
            for I := 0 to Group.Count - 1 do
              FVarTypes.Values[UpperCase(Group[I])] := Spec;
          // AS String: "" (each element of an array); a QuickBASIC TYPE:
          // the object (each element's).
          Cls := ClassOf(TypeName);
          if (Cls <> nil) and not Cls.IsType then
            Cls := nil;
          if SameText(TypeName, 'String') or (Cls <> nil) then
            for I := 0 to Group.Count - 1 do
              if Group.Objects[I] <> nil then
                FillArray(Op_Variable(Group[I]), PtrInt(Group.Objects[I]), Cls)
              else if not AtAssign then
              begin
                if Cls <> nil then
                  EmitNewObject(Cls)
                else
                  EmitStr('');
                FAssembler.Emit(BC_STORE_VAR, [Op_Variable(Group[I])]);
              end;
          // Struct instance: one flat variable slot per field, named
          // "VarName.FieldName" (see the unit comment on StructDefinition).
          StructIdx := FStructs.IndexOf(TypeName);
          if StructIdx >= 0 then
            for I := 0 to Group.Count - 1 do
            begin
              if Group.Objects[I] <> nil then
                Error('Arrays of STRUCTs aren''t supported - use a CLASS');
              Fields := FStructs.Data[StructIdx];
              FStructVars.Add(UpperCase(Group[I]));
              for J := 0 to Fields.Count - 1 do
              begin
                DeclareLocal(Group[I] + '.' + Fields[J]);
                Op_Variable(Group[I] + '.' + Fields[J]);
              end;
            end;
        end;
        if AtAssign then
        begin
          if (Group.Count > 1) or HasDims then
            Error('Only a single, non-array variable can have an initial value');
          Advance;
          Expression;
          FAssembler.Emit(BC_STORE_VAR, [Op_Variable(Name)]);
        end;
        Group.Clear;
      end
      else if AtAssign then
      begin
        if (Group.Count > 1) or HasDims then
          Error('Only a single, non-array variable can have an initial value');
        Advance;
        Expression;
        FAssembler.Emit(BC_STORE_VAR, [Op_Variable(Name)]);
        Group.Clear;
      end;

      if not Check(tkComma) then
        Break;
      Advance;
    until False;
  finally
    Group.Free;
  end;
end;

// "(b1, b2, ...)" of DIM / REDIM: pushes each upper bound and returns how
// many; "()" is an empty array (upper bound -1). Arrays start at 0.
function TParser.ParseDims: Integer;
begin
  Match(tkParenthesisOpen);
  Result := 0;
  if Check(tkParenthesisClose) then
  begin
    EmitInt(-1);
    Result := 1;
  end
  else
    repeat
      Expression;
      if AtKeyword('TO') then
      begin
        if not FQB then
          Error('Arrays start at 0: write a(n) for a(0) .. a(n), not a(low TO high)');
        // QuickBASIC: a(1 TO 10) - indexes up to 10 work (0 does too).
        Advance;
        FAssembler.Emit(BC_POP, []);
        Expression;
      end;
      Inc(Result);
      if not Check(tkComma) then
        Break;
      Advance;
    until False;
  Match(tkParenthesisClose);
end;

// REDIM [PRESERVE] name(bounds) [, ...] - a new array of that size (all 0);
// with PRESERVE (one dimension only) the items that still fit are kept.
procedure TParser.ReDimStatement;
var
  Preserve: Boolean;
  Name: string;
  Count: Integer;
begin
  Advance; // REDIM
  Preserve := AtKeyword('PRESERVE');
  if Preserve then
    Advance;
  repeat
    if not Check(tkIdentifier) then
      Error('Expected an array name after REDIM');
    Name := FCurrentToken.Lexeme;
    Advance;
    if not Check(tkParenthesisOpen) then
      Error('Expected REDIM ' + Name + '(new upper bound)');
    if (CurrentClass = nil) or (CurrentClass.Fields.IndexOf(UpperCase(Name)) < 0) or
       (FLocals.IndexOf(UpperCase(Name)) >= 0) then
      DeclareLocal(Name);
    if Preserve then
    begin
      LoadName(Name);
      Count := ParseDims;
      if Count <> 1 then
        Error('REDIM PRESERVE works on one-dimensional arrays');
      FAssembler.Emit(BC_BUILTIN, [BI_RESIZE, 2]);
    end
    else
      FAssembler.Emit(BC_BUILTIN, [BI_NEWARRAY, ParseDims]);
    StoreToName(Name);
    if AtKeyword('AS') then
    begin
      Advance;
      Match(tkIdentifier);
    end;
    if not Check(tkComma) then
      Break;
    Advance;
  until False;
end;

// LET / SET name = value (also any ChainExpr target: a(i), obj.field ...).
procedure TParser.AssignmentStatement;
begin
  if not (Check(tkIdentifier) or Check(tkDot)) then
    Error('Expected a variable to assign to');
  ChainExpr(True);
end;

// PRINT a, b     the values, separated by spaces
// PRINT a; b     ";" joins them with nothing in between
// A "," or ";" at the end leaves the line open for the next PRINT.
procedure TParser.PrintStatement;
var
  Count, I: Integer;
  Join, Trailing: Boolean;
  Temps: TIntegerArray;
begin
  Advance; // Consume PRINT keyword

  // Handle empty PRINT (just prints a newline)
  if AtStatementEnd then
  begin
    FAssembler.Emit(BC_PRINT, [0]);
    Exit;
  end;

  Count := 0;
  Join := False;
  Trailing := False;
  repeat
    Expression; // Any expression: literal, variable, "a" & b, 1 + 2, ...
    if Join then
      FAssembler.Emit(BC_CONCAT, [])
    else
      Inc(Count);
    if not (Check(tkComma) or Check(tkSemicolon)) then
      Break;
    Join := Check(tkSemicolon);
    Advance;
    if AtStatementEnd then
      Trailing := True;
  until Trailing;

  if not Trailing then
  begin
    FAssembler.Emit(BC_PRINT, [Count]);
    Exit;
  end;
  // No line break: the parts, separated by spaces.
  SetLength(Temps, Count);
  for I := Count - 1 downto 0 do
  begin
    Temps[I] := HiddenVar('print');
    FAssembler.Emit(BC_STORE_VAR, [Temps[I]]);
  end;
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
    begin
      EmitStr(' ');
      FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
    end;
    FAssembler.Emit(BC_LOAD_VAR, [Temps[I]]);
    FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
  end;
end;

// QuickBASIC's PRINT (--qbs): numbers are printed with a space for their
// sign and one after (" 5 "), ";" puts nothing between items, "," moves to
// the next 14-column zone, TAB(n) / SPC(n) position the text, and a "," or
// ";" at the end leaves the line open. "?" is short for PRINT.
procedure TParser.QBPrintStatement;
var
  NeedNewline: Boolean;
  W: string;
begin
  Advance; // PRINT / ?
  FPrintFile := -1;
  try
    if AtHash then
    begin
      FPrintFile := ParseFileNumber(True); // PRINT #n, ...
      if not AtStatementEnd then
        Match(tkComma);
    end;
    if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'USING') then
    begin
      PrintUsing;
      Exit;
    end;
    NeedNewline := True;
    while not AtStatementEnd do
    begin
      if Check(tkSemicolon) then
      begin
        Advance;
        NeedNewline := False;
        Continue;
      end;
      if Check(tkComma) then
      begin
        Advance;
        EmitPiece(QP_COMMA, False);
        NeedNewline := False;
        Continue;
      end;
      W := AnsiUpperCase(FCurrentToken.Lexeme);
      if Check(tkIdentifier) and ((W = 'TAB') or (W = 'SPC')) and (PeekToken.TokenType = tkParenthesisOpen) then
      begin
        Advance;
        Match(tkParenthesisOpen);
        Expression;
        Match(tkParenthesisClose);
        if W = 'TAB' then
          EmitPiece(QP_TAB, True)
        else
          EmitPiece(QP_SPC, True);
      end
      else
      begin
        Expression;
        EmitPiece(QP_VALUE, True);
      end;
      NeedNewline := True;
    end;
    if NeedNewline then
      EmitPiece(QP_NEWLINE, False);
  finally
    FPrintFile := -1;
  end;
end;

// One piece of a QuickBASIC PRINT / WRITE (QP_*; the value, if any, is on
// the stack): to the screen, or to file FPrintFile.
procedure TParser.EmitPiece(Kind: Integer; HasValue: Boolean);
var
  T: Integer;
begin
  if FPrintFile < 0 then
  begin
    FAssembler.Emit(BC_QPRINT, [Kind]);
    Exit;
  end;
  if HasValue then
  begin
    T := HiddenVar('piece');
    FAssembler.Emit(BC_STORE_VAR, [T]);
  end;
  FAssembler.Emit(BC_LOAD_VAR, [FPrintFile]);
  EmitInt(Kind);
  if HasValue then
  begin
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    FAssembler.Emit(BC_BUILTIN, [BI_FPRINT, 3]);
  end
  else
    FAssembler.Emit(BC_BUILTIN, [BI_FPRINT, 2]);
  FAssembler.Emit(BC_POP, []);
end;

// PRINT [#n,] USING format$; item [{; | ,} item]... [; | ,]
// The format's fields: # digits (with . , + - $$ ** **$ ^^^^), ! the
// first character, & the whole string, \  \ a fixed width; _ makes the next
// character literal. It's reused when there are more items than fields.
procedure TParser.PrintUsing;
var
  Count: Integer;
  Trailing: Boolean;
begin
  Advance; // USING
  Expression; // the format
  if not (Check(tkSemicolon) or Check(tkComma)) then
    Error('Expected ";" after PRINT USING format');
  Advance;
  Count := 0;
  Trailing := False;
  while not AtStatementEnd do
  begin
    Expression;
    Inc(Count);
    Trailing := False;
    if Check(tkSemicolon) or Check(tkComma) then
    begin
      Advance;
      Trailing := True;
    end
    else
      Break;
  end;
  if Count = 0 then
    Error('PRINT USING needs something to print');
  FAssembler.Emit(BC_BUILTIN, [BI_USING, Count + 1]);
  EmitPiece(QP_TEXT, True);
  if not Trailing then
    EmitPiece(QP_NEWLINE, False);
end;

// OPEN file$ FOR {INPUT | OUTPUT | APPEND} AS [#]n
// OPEN mode$, [#]n, file$    (the older form: mode$ is "I", "O" or "A")
procedure TParser.OpenStatement;
var
  NameVar, Mode: Integer;
  W: string;
begin
  Advance; // OPEN
  Expression;
  NameVar := HiddenVar('open');
  FAssembler.Emit(BC_STORE_VAR, [NameVar]);
  if Check(tkComma) then
  begin
    // OPEN "O", #1, "file": the first expression was the mode
    Advance;
    Mode := ParseFileNumber(False);
    Match(tkComma);
    Expression; // the file name
    // mode letter -> 0 .. 4: INSTR("IOABR", UCASE(LEFT(m, 1))) - 1
    EmitStr('IOABR');
    FAssembler.Emit(BC_LOAD_VAR, [NameVar]);
    EmitInt(1);
    FAssembler.Emit(BC_BUILTIN, [BI_LEFT, 2]);
    FAssembler.Emit(BC_BUILTIN, [BI_UCASE, 1]);
    FAssembler.Emit(BC_BUILTIN, [BI_INSTR, 2]);
    EmitInt(1);
    FAssembler.Emit(BC_SUB, []);
    FAssembler.Emit(BC_LOAD_VAR, [Mode]);
    if Check(tkComma) then
    begin
      Advance;
      Expression; // the record length
    end
    else
      EmitInt(128);
    FAssembler.Emit(BC_BUILTIN, [BI_FOPEN, 4]);
    FAssembler.Emit(BC_POP, []);
    Exit;
  end;
  if AtKeyword('AS') then
    Mode := 4 // OPEN f$ AS #n: RANDOM
  else
  begin
    if not AtKeyword('FOR') then
      Error('Expected OPEN file$ FOR INPUT | OUTPUT | APPEND | BINARY | RANDOM AS #n');
    Advance; // FOR
    W := AnsiUpperCase(FCurrentToken.Lexeme);
    if W = 'INPUT' then Mode := 0
    else if W = 'OUTPUT' then Mode := 1
    else if W = 'APPEND' then Mode := 2
    else if W = 'BINARY' then Mode := 3
    else if W = 'RANDOM' then Mode := 4
    else
      Error('Expected INPUT, OUTPUT, APPEND, BINARY or RANDOM after OPEN ... FOR');
    Advance;
    // ACCESS READ WRITE, SHARED, LOCK ...: sharing between programs (not modeled)
    while not AtKeyword('AS') and not AtStatementEnd do
      Advance;
  end;
  if not AtKeyword('AS') then
    Error('Expected AS #n in OPEN');
  Advance; // AS
  FAssembler.Emit(BC_LOAD_VAR, [NameVar]);
  EmitInt(Mode);
  if Check(tkOperator) and (FCurrentToken.Lexeme = '#') then
    Advance;
  Expression;
  if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'LEN') then
  begin
    // LEN = n: the record length of a RANDOM file
    Advance;
    if not AtAssign then
      Error('Expected LEN = record length');
    Advance;
    Expression;
  end
  else
    EmitInt(128);
  FAssembler.Emit(BC_BUILTIN, [BI_FOPEN, 4]);
  FAssembler.Emit(BC_POP, []);
end;

// The GET / PUT layout (see klay in the native runtime) for a variable of
// QuickBASIC type Spec (INTEGER, LONG, SINGLE, DOUBLE, STRING, STRING*n, a
// TYPE), or by Name's suffix (% & ! # $), else "?" (by the value).
// The bytes a layout takes, or -1 if that depends on the value (V, ?).
function LayoutBytes(const Layout: string): Int64;
var
  I: Integer;
  N: Int64;
begin
  Result := 0;
  I := 1;
  while I <= Length(Layout) do
  begin
    case Layout[I] of
      'I', 'F', 'S':
        begin
          N := 0;
          Inc(I);
          while (I <= Length(Layout)) and (Layout[I] in ['0'..'9']) do
          begin
            N := N * 10 + Ord(Layout[I]) - Ord('0');
            Inc(I);
          end;
          Inc(Result, N);
          Continue;
        end;
      'V', '?': Exit(-1);
    end;
    Inc(I);
  end;
end;

function TParser.LayoutFor(const Spec, Name: string; Depth: Integer): string;
var
  Cls: TClassInfo;
  I: Integer;
  FieldSpec: string;
begin
  if Depth > 16 then
    Error('TYPE records nest too deeply');
  if Spec = 'INTEGER' then Exit('I2');
  if Spec = 'LONG' then Exit('I4');
  if Spec = 'SINGLE' then Exit('F4');
  if Spec = 'DOUBLE' then Exit('F8');
  if Spec = 'STRING' then Exit('V');
  if Copy(Spec, 1, 7) = 'STRING*' then Exit('S' + Copy(Spec, 8, MaxInt));
  if Spec <> '' then
  begin
    Cls := ClassOf(Spec);
    if (Cls <> nil) and Cls.IsType then
    begin
      Result := '{';
      for I := 0 to Cls.OwnFields.Count - 1 do
      begin
        FieldSpec := Cls.FieldTypes[I];
        if FieldSpec = 'STRING' then
          Error(Format('TYPE %s: field %s needs a fixed length (STRING * n) for GET / PUT',
            [Cls.Name, Cls.OwnFields[I]]));
        if FieldSpec = '' then
          FieldSpec := '?';
        if I > 0 then
          Result := Result + ',';
        Result := Result + LayoutFor(FieldSpec, Cls.OwnFields[I], Depth + 1);
      end;
      Exit(Result + '}');
    end;
    if Spec <> '?' then
      Error('GET / PUT: "' + Spec + '" isn''t a type a record can hold');
  end;
  case Name[Length(Name)] of
    '%': Result := 'I2';
    '&': Result := 'I4';
    '!': Result := 'F4';
    '#': Result := 'F8';
    '$': Result := 'V';
  else
    Result := '?';
  end;
end;

// GET [#]n, [position], variable / PUT [#]n, [position], variable - a
// record (RANDOM) or bytes (BINARY); without a position, the next record
// / the current position. The variable's type sets the bytes (see
// LayoutFor).
procedure TParser.GetPutStatement;
var
  IsPut: Boolean;
  FileSlot, T: Integer;
  Name: string;
  P: TPlace;
begin
  IsPut := AnsiUpperCase(FCurrentToken.Lexeme) = 'PUT';
  Advance; // GET / PUT
  FileSlot := ParseFileNumber(False);
  Match(tkComma);
  if Check(tkComma) then
    EmitInt(-1) // no position
  else
    Expression;
  T := HiddenVar('pos');
  FAssembler.Emit(BC_STORE_VAR, [T]);
  if not Check(tkComma) then
    Error('Expected GET / PUT #n, [position], variable (FIELD buffers aren''t supported: use a TYPE record)');
  Advance;
  Name := FCurrentToken.Lexeme;
  P := ParseTarget;
  Stabilize(P);
  FAssembler.Emit(BC_LOAD_VAR, [FileSlot]);
  FAssembler.Emit(BC_LOAD_VAR, [T]);
  LoadPlace(P);
  EmitStr(LayoutFor(FVarTypes.Values[UpperCase(Name)], Name, 0));
  if IsPut then
  begin
    FAssembler.Emit(BC_BUILTIN, [BI_FPUT, 4]);
    FAssembler.Emit(BC_POP, []);
  end
  else
  begin
    FAssembler.Emit(BC_BUILTIN, [BI_FGET, 4]);
    T := HiddenVar('get');
    FAssembler.Emit(BC_STORE_VAR, [T]);
    BeginStore(P);
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    EndStore(P);
  end;
end;

// CLOSE [[#]n [, [#]n]...] - with no numbers, every open file.
procedure TParser.CloseStatement;
begin
  Advance; // CLOSE
  if AtStatementEnd then
  begin
    EmitInt(0);
    FAssembler.Emit(BC_BUILTIN, [BI_FCLOSE, 1]);
    FAssembler.Emit(BC_POP, []);
    Exit;
  end;
  repeat
    if Check(tkOperator) and (FCurrentToken.Lexeme = '#') then
      Advance;
    Expression;
    FAssembler.Emit(BC_BUILTIN, [BI_FCLOSE, 1]);
    FAssembler.Emit(BC_POP, []);
    if not Check(tkComma) then
      Break;
    Advance;
  until False;
end;

// WRITE a, b (QuickBASIC): the values separated by commas, strings in
// quotes.
procedure TParser.WriteStatement;
var
  T, Skip, Done: Integer;
  First: Boolean;
begin
  Advance; // WRITE
  FPrintFile := -1;
  if AtHash then
  begin
    FPrintFile := ParseFileNumber(True); // WRITE #n, ...
    if not AtStatementEnd then
      Match(tkComma);
  end;
  First := True;
  while not AtStatementEnd do
  begin
    if not First then
    begin
      Match(tkComma);
      EmitStr(',');
      EmitPiece(QP_TEXT, True);
    end;
    First := False;
    Expression;
    T := HiddenVar('write');
    FAssembler.Emit(BC_STORE_VAR, [T]);
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    FAssembler.Emit(BC_BUILTIN, [BI_TYPENAME, 1]);
    EmitStr('String');
    FAssembler.Emit(BC_CMP_EQ, []);
    Skip := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    EmitInt(34);
    FAssembler.Emit(BC_BUILTIN, [BI_CHR, 1]);
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    FAssembler.Emit(BC_CONCAT, []);
    EmitInt(34);
    FAssembler.Emit(BC_BUILTIN, [BI_CHR, 1]);
    FAssembler.Emit(BC_CONCAT, []);
    Done := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
    FAssembler.PatchJumpTarget(Skip, FAssembler.CurrentInstructionIndex);
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    FAssembler.Emit(BC_BUILTIN, [BI_QBSTR, 1]); // numbers as QuickBASIC writes them: -.5
    FAssembler.Emit(BC_BUILTIN, [BI_LTRIM, 1]);
    FAssembler.PatchJumpTarget(Done, FAssembler.CurrentInstructionIndex);
    EmitPiece(QP_TEXT, True);
  end;
  EmitPiece(QP_NEWLINE, False);
  FPrintFile := -1;
end;

// INPUT ["prompt" {; | ,}] var [, var ...]
// LINE INPUT ["prompt" ;] var
// Reads a line. With several variables, its comma-separated parts go into
// them in turn. A value that's a whole number is stored as a number -
// except into a name$ - and anything else as a string. INPUT shows "? "
// after the prompt (none after "prompt,"); LINE INPUT stores the whole
// line as it is.
// The text on the stack, as INPUT stores it into variable Name: a number
// if it is one (nothing -> 0), else the text - but always the text for a
// name$.
procedure TParser.EmitInputConvert(const Name: string);
var
  C, J1, J2: Integer;
begin
  if Name[Length(Name)] = '$' then
    Exit;
  C := HiddenVar('input');
  FAssembler.Emit(BC_STORE_VAR, [C]);
  FAssembler.Emit(BC_LOAD_VAR, [C]);
  FAssembler.Emit(BC_BUILTIN, [BI_ISNUMERIC, 1]);
  FAssembler.Emit(BC_LOAD_VAR, [C]);
  EmitStr('');
  FAssembler.Emit(BC_CMP_EQ, []);
  FAssembler.Emit(BC_ADD, []); // numeric or empty
  J1 := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  FAssembler.Emit(BC_LOAD_VAR, [C]);
  FAssembler.Emit(BC_BUILTIN, [BI_VAL, 1]);
  J2 := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  FAssembler.PatchJumpTarget(J1, FAssembler.CurrentInstructionIndex);
  FAssembler.Emit(BC_LOAD_VAR, [C]);
  FAssembler.PatchJumpTarget(J2, FAssembler.CurrentInstructionIndex);
end;

// "#n" (the "#" is optional where QuickBASIC allows it): the file number,
// kept in a variable. Returns the variable.
function TParser.ParseFileNumber(HashRequired: Boolean): Integer;
begin
  if Check(tkOperator) and (FCurrentToken.Lexeme = '#') then
    Advance
  else if HashRequired then
    Error('Expected #filenumber');
  Expression;
  Result := HiddenVar('file');
  FAssembler.Emit(BC_STORE_VAR, [Result]);
end;

function TParser.AtHash: Boolean;
begin
  Result := FQB and Check(tkOperator) and (FCurrentToken.Lexeme = '#');
end;

procedure TParser.InputStatement;
var
  LineMode, Question, Multi: Boolean;
  LineVar, Parts, K, J1, J2, FileSlot: Integer;
  Name: string;
  P: TPlace;

  // The value for part K (or the whole line), converted for Name.
  procedure EmitValue;
  begin
    if Multi then
    begin
      FAssembler.Emit(BC_LOAD_VAR, [Parts]);
      FAssembler.Emit(BC_BUILTIN, [BI_LEN, 1]);
      EmitInt(K);
      FAssembler.Emit(BC_CMP_GT, []);
      J1 := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
      FAssembler.Emit(BC_LOAD_VAR, [Parts]);
      EmitInt(K);
      FAssembler.Emit(BC_INDEX_GET, []);
      J2 := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_JUMP, [-1]);
      FAssembler.PatchJumpTarget(J1, FAssembler.CurrentInstructionIndex);
      EmitStr('');
      FAssembler.PatchJumpTarget(J2, FAssembler.CurrentInstructionIndex);
    end
    else
      FAssembler.Emit(BC_LOAD_VAR, [LineVar]);
    if LineMode then
      Exit;
    FAssembler.Emit(BC_BUILTIN, [BI_TRIM, 1]);
    EmitInputConvert(Name);
  end;

begin
  LineMode := SameText(FCurrentToken.Lexeme, 'LINE');
  if LineMode then
    Advance; // LINE
  Advance; // INPUT
  if AtHash then
  begin
    // INPUT #n, a, b$ - the file's next comma- or line-separated fields;
    // LINE INPUT #n, a$ - its next line.
    FileSlot := ParseFileNumber(True);
    Match(tkComma);
    repeat
      Name := FCurrentToken.Lexeme;
      P := ParseTarget;
      BeginStore(P);
      FAssembler.Emit(BC_LOAD_VAR, [FileSlot]);
      if LineMode then
        FAssembler.Emit(BC_BUILTIN, [BI_FREADLINE, 1])
      else
      begin
        FAssembler.Emit(BC_BUILTIN, [BI_FREADFIELD, 1]);
        EmitInputConvert(Name);
      end;
      EndStore(P);
      if LineMode or not Check(tkComma) then
        Break;
      Advance;
    until False;
    Exit;
  end;
  if Check(tkSemicolon) then
    Advance; // INPUT; - keeps the cursor on the line (not modeled)
  Question := not LineMode;
  if Check(tkStringLiteral) then
  begin
    FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral(FCurrentToken.Lexeme)]);
    FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
    Advance;
    if Check(tkSemicolon) then
      Advance
    else if Check(tkComma) then
    begin
      Advance;
      Question := False;
    end
    else
      Question := False; // INPUT "Name: " name
  end;
  if Question then
  begin
    EmitStr('? ');
    FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
  end;
  FAssembler.Emit(BC_INPUT, []);
  LineVar := HiddenVar('line');
  FAssembler.Emit(BC_STORE_VAR, [LineVar]);

  Multi := False;
  Parts := -1;
  K := 0;
  repeat
    Name := FCurrentToken.Lexeme;
    P := ParseTarget;
    if (K = 0) and Check(tkComma) and not LineMode then
    begin
      Multi := True;
      FAssembler.Emit(BC_LOAD_VAR, [LineVar]);
      EmitStr(',');
      FAssembler.Emit(BC_BUILTIN, [BI_SPLIT, 2]);
      Parts := HiddenVar('input');
      FAssembler.Emit(BC_STORE_VAR, [Parts]);
    end;
    BeginStore(P);
    EmitValue;
    EndStore(P);
    Inc(K);
    if LineMode or not Check(tkComma) then
      Break;
    Advance;
  until False;
end;

procedure TParser.MsgBoxStatement;
var
  ArgCount: Integer;
begin
  Match(tkKeyword);
  // No real GUI available, so MSGBOX prints its arguments to the console
  // (like PRINT), tagged so it's clear where the output came from.
  FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral('"[MsgBox]"')]);
  ArgCount := 1;

  Expression;
  Inc(ArgCount);
  while Check(tkComma) do
  begin
    Advance;
    Expression;
    Inc(ArgCount);
  end;

  FAssembler.Emit(BC_PRINT, [ArgCount]);
end;

// CALL <name> [ ( <arg-expr> [, <arg-expr>]* ) ]
//
// Pushes each argument, then BC_CALL [target, argcount]; the target is
// patched in by ResolveCalls once every SUB is known.
procedure TParser.CallStatement;
var
  Name: string;
  Line: Integer;
begin
  Match(tkKeyword); // Consume CALL
  if not Check(tkIdentifier) then
    Error('Expected SUB name after CALL');
  Name := FCurrentToken.Lexeme;
  Line := FCurrentToken.Line;
  Advance;
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
  CallExpression(Name, Line, False);
end;

// Name[(args)] - pushes the arguments and calls the SUB / FUNCTION (resolved
// once every procedure is known, see ResolveCalls). With WantValue the
// FUNCTION's result is left on the stack.
procedure TParser.CallExpression(const Name: string; Line: Integer; WantValue: Boolean; BareArgs: Boolean);
var
  CopyBacks: TCopyBacks;
  ArgCount: Integer;
begin
  ArgCount := ParseArgs(BareArgs, CopyBacks);
  EmitCall(UpperCase(Name), ArgCount, Line, WantValue, CopyBacks);
end;

// The arguments of a call - "(a, b)", or with BareArgs the VB-style "a, b"
// up to the end of the statement - pushed left to right. Returns how many.
// An argument that compiles to a single variable load is a plain variable
// (or struct field), which a ByRef parameter can write back to: those are
// listed in CopyBacks.
function TParser.ParseArgs(BareArgs: Boolean; out CopyBacks: TCopyBacks): Integer;
var
  ArgCount: Integer;

  procedure Argument;
  var
    Before: Integer;
    Instrs: TBCInstructionArray;
  begin
    Before := FAssembler.CurrentInstructionIndex;
    Expression;
    if not FSuppressCodeGen and (FAssembler.CurrentInstructionIndex = Before + 1) then
    begin
      Instrs := FAssembler.CurrentProgram.Instructions;
      if Instrs[Before].OpCode = BC_LOAD_VAR then
      begin
        SetLength(CopyBacks, Length(CopyBacks) + 1);
        CopyBacks[High(CopyBacks)].ArgIndex := ArgCount;
        CopyBacks[High(CopyBacks)].Slot := Instrs[Before].Operand1;
      end;
    end;
    Inc(ArgCount);
  end;

begin
  ArgCount := 0;
  CopyBacks := nil;
  if Check(tkParenthesisOpen) then
  begin
    Advance;
    if not Check(tkParenthesisClose) then
      repeat
        Argument;
        if not Check(tkComma) then
          Break;
        Advance;
      until False;
    Match(tkParenthesisClose);
  end
  else if BareArgs and not AtStatementEnd then
    // VB-style statement call: Name arg1, arg2
    repeat
      Argument;
      if not Check(tkComma) then
        Break;
      Advance;
    until False;
  Result := ArgCount;
end;

// BC_CALL to the procedure Key (resolved once every procedure is known, see
// ResolveCalls), with room after it to copy ByRef parameters back into the
// variable arguments in CopyBacks. With WantValue the FUNCTION's result is
// left on the stack.
procedure TParser.EmitCall(const Key: string; ArgCount, Line: Integer; WantValue: Boolean;
  const CopyBacks: TCopyBacks);
var
  Call: TPendingCall;
  I: Integer;
begin
  if FSuppressCodeGen then
    Exit;
  Call.SubKey := Key;
  Call.Line := Line;
  Call.WantValue := WantValue;
  Call.ArgCount := ArgCount;
  Call.CopyBacks := Copy(CopyBacks); // each call fills in its own placeholders
  Call.InstrIndex := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_CALL, [-1, ArgCount]);
  // Filled in by ResolveCalls once it knows which parameters are ByRef.
  for I := 0 to High(Call.CopyBacks) do
  begin
    Call.CopyBacks[I].InstrIndex := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_NOP, []);
    FAssembler.Emit(BC_NOP, []);
  end;
  SetLength(FPendingCalls, Length(FPendingCalls) + 1);
  FPendingCalls[High(FPendingCalls)] := Call;
  if WantValue then
    FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable('__ret')]);
end;

// PROCESS <command-expr> [, <arg-expr>]* [TO <identifier>]
//
// Spawns an OS process: <command-expr> is the executable, each further
// comma-separated expression is passed as a separate argument (not
// shell-concatenated, so arguments containing spaces/quotes need no
// escaping). If a "TO <identifier>" clause is given, the process's
// captured output is stored in that variable; otherwise it's printed to
// the console directly, like MSGBOX/PRINT.
procedure TParser.ProcessStatement;
begin
  CommandStatement(BC_PROCESS);
end;

// QT <command-expr> [, <arg-expr>]* [TO <identifier>]
//
// Runs a Qt6 GUI command (see kayte_qt6.pas for the command list), e.g.
//   QT "window", "Hello", 400, 300 TO win
// Same shape as PROCESS: the command's result (a widget handle, text, or
// event id) is stored in the TO variable, or discarded if there is none.
procedure TParser.QtStatement;
begin
  CommandStatement(BC_QT, QT_STATEMENT_QT);
end;

// QML <command-expr> [, <arg-expr>]* [TO <identifier>]
//
// Builds a UI from QML (Qt Quick) and drives it, e.g.
//   QML "load", "ui.qml" TO win
//   QML "find", win, "okButton" TO ok
//   QML "set", label, "text", "Hello"
// Same shape and runtime as QT (it compiles to BC_QT, flagged in
// Operand3), but starts Qt by itself and has QML-specific command names;
// see source/qt6/README.md.
procedure TParser.QmlStatement;
begin
  CommandStatement(BC_QT, QT_STATEMENT_QML);
end;

// Shared by PROCESS, QT and QML: consumes the keyword, pushes each
// comma-separated expression, and emits Op with [ArgCount, DestVarIndex,
// Operand3] (DestVarIndex is -1 when there's no "TO <identifier>" clause).
procedure TParser.CommandStatement(Op: TByteCodeOp; Operand3: Integer);
var
  ArgCount: Integer;
  DestVarIndex: Integer;
begin
  Advance; // Consume PROCESS / QT / QML keyword

  ArgCount := 0;
  Expression; // Command
  Inc(ArgCount);

  while Check(tkComma) do
  begin
    Advance; // Consume ','
    Expression; // Argument
    Inc(ArgCount);
  end;

  DestVarIndex := -1;
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'TO') then
  begin
    Advance; // Consume TO
    if not Check(tkIdentifier) then
      Error('Expected variable name after TO');
    DestVarIndex := Op_Variable(FCurrentToken.Lexeme);
    Advance; // Consume variable name
  end;

  FAssembler.Emit(Op, [ArgCount, DestVarIndex, Operand3]);
end;

// GOTO label - jumps to "label:" in the same SUB / FUNCTION (or both at
// the top level). Resolved once the whole program is parsed, so it can
// jump forward or back. Values the program computes aren't kept on the
// evaluation stack between statements, so any statement is a safe target.
procedure TParser.GoToStatement;
var
  Pending: TPendingCall;
begin
  Advance; // GOTO
  if not (Check(tkIdentifier) or Check(tkIntegerLiteral)) then
    Error('Expected a label name after GOTO');
  EmitGoto(FCurrentToken.Lexeme, FCurrentToken.Line);
  Advance; // label name
end;

// A jump to a label (a name, or a QuickBASIC line number) of this SUB /
// FUNCTION (or the top level), resolved by ResolveGotos.
// A GOTO to a label of scope Scope ('' = the top level), with no TRY checks
// (for the error trap's own code).
procedure TParser.EmitGotoAt(const LabelName: string; Line: Integer; const Scope: string);
var
  Pending: TPendingCall;
begin
  Pending.SubKey := Scope + '|' + UpperCase(LabelName);
  Pending.Line := Line;
  Pending.InstrIndex := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  SetLength(FGotos, Length(FGotos) + 1);
  FGotos[High(FGotos)] := Pending;
end;

procedure TParser.EmitGoto(const LabelName: string; Line: Integer);
var
  Pending: TPendingCall;
begin
  if Length(FRegions) > 0 then
    Error('GOTO can''t be used inside a TRY block (EXIT a loop, or set a flag, instead)');
  if FInFinally > 0 then
    Error('GOTO can''t be used inside a FINALLY block');
  Pending.SubKey := FProc + '|' + UpperCase(LabelName);
  Pending.Line := Line;
  Pending.InstrIndex := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  SetLength(FGotos, Length(FGotos) + 1);
  FGotos[High(FGotos)] := Pending;
end;

// GOSUB label - runs the code at "label:" (in the same SUB / FUNCTION, or
// both at the top level) until a RETURN, then continues here. It's a
// BC_CALL to the label, so RETURN's BC_RETURN comes back.
//
// Inside a SUB / FUNCTION, RETURN also means "leave the procedure", so a
// hidden local counts the GOSUBs in progress: RETURN returns from a GOSUB
// while it's above 0, and leaves the procedure otherwise. Leaving the
// procedure (EXIT SUB, RETURN value...) while GOSUBs are in progress first
// unwinds them: the exit code sets __unwind and returns from the GOSUB,
// and the code after the GOSUB jumps straight back to the exit.
procedure TParser.GoSubStatement;
begin
  Advance; // GOSUB
  if not (Check(tkIdentifier) or Check(tkIntegerLiteral)) then
    Error('Expected a label name after GOSUB');
  EmitGosub(FCurrentToken.Lexeme, FCurrentToken.Line);
  Advance; // label name
end;

procedure TParser.EmitGosub(const LabelName: string; Line: Integer);
var
  Pending: TPendingCall;
  Counter, Unwind, Skip: Integer;
begin
  Pending.SubKey := FProc + '|' + UpperCase(LabelName);
  Pending.Line := Line;

  if FProc <> '' then
  begin
    FProcUsesGosub := True;
    DeclareLocal('__gosub');
    DeclareLocal('__unwind');
    Counter := Op_Variable('__gosub');
    FAssembler.Emit(BC_LOAD_VAR, [Counter]);
    FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]);
    FAssembler.Emit(BC_ADD, []);
    FAssembler.Emit(BC_STORE_VAR, [Counter]);
  end;

  Pending.InstrIndex := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_CALL, [-1, 0]);
  SetLength(FGotos, Length(FGotos) + 1);
  FGotos[High(FGotos)] := Pending;

  if FProc <> '' then
  begin
    FAssembler.Emit(BC_LOAD_VAR, [Counter]);
    FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]);
    FAssembler.Emit(BC_SUB, []);
    FAssembler.Emit(BC_STORE_VAR, [Counter]);
    // Unwinding on the way out of the procedure? Then keep going.
    Unwind := Op_Variable('__unwind');
    FAssembler.Emit(BC_LOAD_VAR, [Unwind]);
    Skip := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    SetLength(FGosubUnwinds, Length(FGosubUnwinds) + 1);
    FGosubUnwinds[High(FGosubUnwinds)] := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
    FAssembler.PatchJumpTarget(Skip, FAssembler.CurrentInstructionIndex);
  end;
end;

// name:   - a GOTO target, local to its SUB / FUNCTION. A statement may
// follow on the same line.
procedure TParser.LabelDefinition;
var
  Key: string;
begin
  Key := FProc + '|' + UpperCase(FCurrentToken.Lexeme);
  if (Length(FRegions) > 0) or (FInFinally > 0) then
    Error('Labels can''t be inside a TRY block');
  if FLabels.IndexOf(Key) >= 0 then
    Error('Label "' + FCurrentToken.Lexeme + '" is already defined here');
  FLabels.Add(Key, FAssembler.CurrentInstructionIndex);
  if FProc = '' then
    FDataLabels.Add(Key, FDataItems.Count); // for RESTORE label
  Advance; // name
  if Check(tkColon) then
    Advance; // : (a QuickBASIC line number has none)
end;

// A QuickBASIC line number at the start of a line: a label.
procedure TParser.LineNumberLabel;
var
  N: string;
begin
  N := FCurrentToken.Lexeme;
  LabelDefinition;
  if FUsesOnError and (FProc = '') then
  begin
    // for ERL: the last line number reached
    EmitNumber(N);
    FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable('__line')]);
  end;
end;

// QuickBASIC error trapping:
//   ON ERROR GOTO label    errors jump to label (ERR: the error number,
//                          ERL: the last line number reached)
//   ON ERROR GOTO 0        errors end the program again
//   ON ERROR RESUME NEXT   errors are skipped (the next statement runs)
//   RESUME [0]             back to the statement that failed
//   RESUME NEXT            on with the statement after it
//   RESUME label           on at label
//   ERROR n                raises error number n
// The trap is a TRY (BC_TRY) whose handler is EmitErrorTrap's code; an
// error removes it, and RESUME installs it again, as in QuickBASIC. Only
// top-level (module) code can set a trap; errors in SUBs reach it, and
// RESUME NEXT then goes on after the top-level statement that called them.
procedure TParser.OnErrorStatement;
var
  Pending: TPendingCall;
begin
  if FProc <> '' then
    Error('ON ERROR is only supported in top-level (module) code');
  Advance; // ERROR
  if AtKeyword('GOTO') then
  begin
    Advance;
    if not (Check(tkIdentifier) or Check(tkIntegerLiteral)) then
      Error('Expected ON ERROR GOTO label');
    if FCurrentToken.Lexeme = '0' then
    begin
      Advance;
      EmitTrap(False);
      Exit;
    end;
    Pending.SubKey := FCurrentToken.Lexeme;
    Pending.Line := FCurrentToken.Line;
    SetLength(FTrapLabels, Length(FTrapLabels) + 1);
    FTrapLabels[High(FTrapLabels)] := Pending;
    Advance;
    EmitTrap(False);
    EmitInt(High(FTrapLabels));
  end
  else if AtKeyword('RESUME') or (Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'RESUME')) then
  begin
    Advance;
    if not SameText(FCurrentToken.Lexeme, 'NEXT') then
      Error('Expected ON ERROR RESUME NEXT');
    Advance;
    EmitTrap(False);
    EmitInt(-1); // RESUME NEXT mode
  end
  else
    Error('Expected ON ERROR GOTO label or ON ERROR RESUME NEXT');
  FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable('__trapid')]);
  EmitTrap(True);
end;

// Installs the trap (BC_TRY to the handler), or removes it if there is one.
procedure TParser.EmitTrap(Install: Boolean);
var
  Trap, Skip: Integer;
begin
  Trap := FAssembler.CurrentProgram.AddVariable('__trap');
  if Install then
  begin
    SetLength(FTrapTries, Length(FTrapTries) + 1);
    FTrapTries[High(FTrapTries)] := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_TRY, [-1]);
    EmitInt(1);
  end
  else
  begin
    FAssembler.Emit(BC_LOAD_VAR, [Trap]);
    Skip := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    FAssembler.Emit(BC_TRY_END, []);
    FAssembler.PatchJumpTarget(Skip, FAssembler.CurrentInstructionIndex);
    EmitInt(0);
  end;
  FAssembler.Emit(BC_STORE_VAR, [Trap]);
end;

procedure TParser.ResumeStatement;
var
  InHandler, ToError: Integer;
  Name: string;
  Line: Integer;
begin
  Advance; // RESUME
  if not FUsesOnError then
    Error('RESUME without ON ERROR in the program');
  InHandler := FAssembler.CurrentProgram.AddVariable('__inhandler');
  FAssembler.Emit(BC_LOAD_VAR, [InHandler]);
  ToError := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  EmitInt(0);
  FAssembler.Emit(BC_STORE_VAR, [InHandler]);
  EmitTrap(True); // the trap is back on
  if AtStatementEnd or (Check(tkIntegerLiteral) and (FCurrentToken.Lexeme = '0')) then
  begin
    if not AtStatementEnd then
      Advance; // RESUME 0
    SetLength(FResumeJumps, Length(FResumeJumps) + 1);
    FResumeJumps[High(FResumeJumps)] := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
  end
  else if SameText(FCurrentToken.Lexeme, 'NEXT') then
  begin
    Advance;
    SetLength(FResumeNextJumps, Length(FResumeNextJumps) + 1);
    FResumeNextJumps[High(FResumeNextJumps)] := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
  end
  else
  begin
    Name := FCurrentToken.Lexeme;
    Line := FCurrentToken.Line;
    Advance;
    EmitGotoAt(Name, Line, '');
  end;
  // RESUME outside an error handler
  FAssembler.PatchJumpTarget(ToError, FAssembler.CurrentInstructionIndex);
  EmitInt(20);
  FAssembler.Emit(BC_BUILTIN, [BI_ERRMSG, 1]);
  FAssembler.Emit(BC_THROW, []);
end;

// The error trap's handler and RESUME's jump tables (after the program).
procedure TParser.EmitErrorTrap;
var
  Handler, I, Next, ResumeAt, ResumeNextAt: Integer;
  Pending: TIntegerArray;

  function V(const Name: string): Integer;
  begin
    Result := FAssembler.CurrentProgram.AddVariable(Name);
  end;

  // A table: __errstmt = k -> jump to Addrs[k].
  function Table(const Addrs: TIntegerArray): Integer;
  var
    K, Skip: Integer;
  begin
    Result := FAssembler.CurrentInstructionIndex;
    for K := 0 to High(Addrs) do
    begin
      FAssembler.Emit(BC_LOAD_VAR, [V('__errstmt')]);
      EmitInt(K);
      FAssembler.Emit(BC_CMP_EQ, []);
      Skip := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
      FAssembler.Emit(BC_JUMP, [Addrs[K]]);
      FAssembler.PatchJumpTarget(Skip, FAssembler.CurrentInstructionIndex);
    end;
    EmitStr('RESUME: no statement to go back to');
    FAssembler.Emit(BC_THROW, []);
  end;

begin
  if not FUsesOnError then
    Exit;
  FAssembler.Emit(BC_HALT, []); // the end of the program
  ResumeAt := Table(FStmtStarts);
  ResumeNextAt := Table(FStmtEnds);

  // The handler: the error message is on the stack, and the trap is gone.
  Handler := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_STORE_VAR, [V('__errmsg')]);
  EmitInt(0);
  FAssembler.Emit(BC_STORE_VAR, [V('__trap')]);
  FAssembler.Emit(BC_LOAD_VAR, [V('__errmsg')]);
  FAssembler.Emit(BC_BUILTIN, [BI_ERRCODE, 1]);
  FAssembler.Emit(BC_STORE_VAR, [V('__err')]);
  FAssembler.Emit(BC_LOAD_VAR, [V('__line')]);
  FAssembler.Emit(BC_STORE_VAR, [V('__erl')]);
  FAssembler.Emit(BC_LOAD_VAR, [V('__stmt')]);
  FAssembler.Emit(BC_STORE_VAR, [V('__errstmt')]);
  // ON ERROR RESUME NEXT: on with the next statement, trap still on
  FAssembler.Emit(BC_LOAD_VAR, [V('__trapid')]);
  EmitInt(-1);
  FAssembler.Emit(BC_CMP_EQ, []);
  Next := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  EmitTrap(True);
  FAssembler.Emit(BC_JUMP, [ResumeNextAt]);
  FAssembler.PatchJumpTarget(Next, FAssembler.CurrentInstructionIndex);
  // ON ERROR GOTO label: to the label of the trap that was on
  EmitInt(1);
  FAssembler.Emit(BC_STORE_VAR, [V('__inhandler')]);
  for I := 0 to High(FTrapLabels) do
  begin
    FAssembler.Emit(BC_LOAD_VAR, [V('__trapid')]);
    EmitInt(I);
    FAssembler.Emit(BC_CMP_EQ, []);
    Next := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    EmitGotoAt(FTrapLabels[I].SubKey, FTrapLabels[I].Line, '');
    FAssembler.PatchJumpTarget(Next, FAssembler.CurrentInstructionIndex);
  end;
  FAssembler.Emit(BC_LOAD_VAR, [V('__errmsg')]);
  FAssembler.Emit(BC_THROW, []);

  for I := 0 to High(FTrapTries) do
    FAssembler.PatchJumpTarget(FTrapTries[I], Handler);
  for I := 0 to High(FResumeJumps) do
    FAssembler.PatchJumpTarget(FResumeJumps[I], ResumeAt);
  for I := 0 to High(FResumeNextJumps) do
    FAssembler.PatchJumpTarget(FResumeNextJumps[I], ResumeNextAt);
  Pending := nil;
end;

// MID$(s$, start[, length]) = value$: the characters of s$ from start
// (at most length of them, and at most value$'s) become value$'s; s$ keeps
// its length.
procedure TParser.MidStatement;
var
  P: TPlace;
  St, Ln, Val_: Integer;
begin
  Advance; // MID$
  Match(tkParenthesisOpen);
  P := ParseTarget;
  Stabilize(P);
  Match(tkComma);
  Expression;
  St := HiddenVar('mid');
  FAssembler.Emit(BC_STORE_VAR, [St]);
  if Check(tkComma) then
  begin
    Advance;
    Expression;
  end
  else
    EmitInt(-1);
  Ln := HiddenVar('mid');
  FAssembler.Emit(BC_STORE_VAR, [Ln]);
  Match(tkParenthesisClose);
  if not AtAssign then
    Error('Expected MID$(variable, start[, length]) = value');
  Advance;
  Expression;
  Val_ := HiddenVar('mid');
  FAssembler.Emit(BC_STORE_VAR, [Val_]);
  BeginStore(P);
  LoadPlace(P);
  FAssembler.Emit(BC_LOAD_VAR, [St]);
  FAssembler.Emit(BC_LOAD_VAR, [Ln]);
  FAssembler.Emit(BC_LOAD_VAR, [Val_]);
  FAssembler.Emit(BC_BUILTIN, [BI_MIDSET, 4]);
  EndStore(P);
end;

procedure TParser.ResolveGotos;
var
  I, Idx: Integer;
  LabelName: string;
begin
  for I := 0 to High(FGotos) do
  begin
    Idx := FLabels.IndexOf(FGotos[I].SubKey);
    if Idx < 0 then
    begin
      LabelName := Copy(FGotos[I].SubKey, Pos('|', FGotos[I].SubKey) + 1, MaxInt);
      ReportError(Format('GOTO / GOSUB %s at line %d: there is no label "%s:" in the same SUB / FUNCTION (or top level)',
        [LabelName, FGotos[I].Line + 1, LabelName]));
      Continue;
    end;
    FAssembler.PatchJumpTarget(FGotos[I].InstrIndex, FLabels.Data[Idx]);
  end;
end;

// DO [WHILE cond | UNTIL cond]
//   ...
// LOOP [WHILE cond | UNTIL cond]
// The condition goes at the top (tested before each pass) or the bottom
// (after each pass, so the body runs at least once) - not both. With
// neither, the loop runs until EXIT DO (or EXIT SUB / GOTO).
procedure TParser.DoStatement;
var
  Top, ToExit, I: Integer;
  Context: string;
  TopTest: Boolean;

  // Condition on the stack -> jump when the loop should stop.
  function JumpWhenDone(IsUntil: Boolean): Integer;
  begin
    if IsUntil then
      FAssembler.Emit(BC_NOT, []); // UNTIL c stops when c is true
    Result := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  end;

begin
  Context := Format('the DO loop that starts at line %d - is LOOP missing?', [FCurrentToken.Line + 1]);
  Advance; // DO
  Top := FAssembler.CurrentInstructionIndex;
  ToExit := -1;
  TopTest := AtKeyword('WHILE') or AtKeyword('UNTIL');
  if TopTest then
  begin
    I := Ord(AtKeyword('UNTIL'));
    Advance;
    Expression;
    ToExit := JumpWhenDone(I = 1);
  end;
  if not (Check(tkEndOfLine) or Check(tkComment) or Check(tkColon)) then
    Error('Expected the end of the line after DO [WHILE | UNTIL condition]');

  PushLoop('DO');
  try
    ParseBlockUntil(Context, ['LOOP']);
    Advance; // LOOP
    // CONTINUE DO: re-test a bottom condition (it's compiled right here),
    // else go back to the top (which re-tests a top condition).
    if AtKeyword('WHILE') or AtKeyword('UNTIL') then
      PatchContinues(FAssembler.CurrentInstructionIndex)
    else
      PatchContinues(Top);
    if AtKeyword('WHILE') or AtKeyword('UNTIL') then
    begin
      if TopTest then
        Error('A DO loop has its condition at the top or at LOOP, not both');
      I := Ord(AtKeyword('UNTIL'));
      Advance;
      Expression;
      if I = 1 then
        // LOOP UNTIL c: go round again while c is false
        FAssembler.Emit(BC_JUMP_IF_FALSE, [Top])
      else
      begin
        // LOOP WHILE c: go round again while c is true
        ToExit := FAssembler.CurrentInstructionIndex;
        FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
        FAssembler.Emit(BC_JUMP, [Top]);
      end;
    end
    else
      FAssembler.Emit(BC_JUMP, [Top]);
    if ToExit >= 0 then
      FAssembler.PatchJumpTarget(ToExit, FAssembler.CurrentInstructionIndex);
  finally
    PopLoop; // EXIT DO jumps land here, past the loop
  end;
end;

// RETURN value - leaves the FUNCTION with that result.
// RETURN - at the top level, returns from a GOSUB (a runtime error without
// one). Inside a SUB / FUNCTION it returns from a GOSUB in progress, if
// any, and otherwise leaves the procedure.
procedure TParser.ReturnStatement;
var
  Counter, ToExit: Integer;
begin
  Match(tkKeyword);
  if FInFinally > 0 then
    Error('RETURN can''t leave a FINALLY block');
  if FProc = '' then
  begin
    LeaveRegions(0);
    FAssembler.Emit(BC_RETURN, []);
    Exit;
  end;
  if not (Check(tkEndOfLine) or Check(tkComment) or Check(tkEndOfFile) or Check(tkColon) or
          AtKeyword('ELSE')) then
  begin
    if not FProcIsFunction then
      Error('RETURN with a value is only allowed in a FUNCTION (a SUB has no result)');
    Expression;
    FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable(FProc + ':' + FLocals[0])]);
    LeaveRegions(0);
  end
  else
  begin
    LeaveRegions(0);
    // Plain RETURN: from a GOSUB in progress, if any.
    DeclareLocal('__gosub');
    Counter := Op_Variable('__gosub');
    FAssembler.Emit(BC_LOAD_VAR, [Counter]);
    FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
    FAssembler.Emit(BC_CMP_GT, []);
    ToExit := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    FAssembler.Emit(BC_RETURN, []);
    FAssembler.PatchJumpTarget(ToExit, FAssembler.CurrentInstructionIndex);
  end;
  SetLength(FReturnJumps, Length(FReturnJumps) + 1);
  FReturnJumps[High(FReturnJumps)] := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
end;

procedure TParser.PushLoop(const Kind: string);
begin
  SetLength(FLoops, Length(FLoops) + 1);
  FLoops[High(FLoops)].Kind := Kind;
  FLoops[High(FLoops)].TryDepth := Length(FRegions);
  FLoops[High(FLoops)].Jumps := nil;
  FLoops[High(FLoops)].ContinueJumps := nil;
end;

// Ends the innermost loop: its EXIT jumps go to the current address,
// just past the loop.
procedure TParser.PopLoop;
var
  I: Integer;
begin
  for I := 0 to High(FLoops[High(FLoops)].Jumps) do
    FAssembler.PatchJumpTarget(FLoops[High(FLoops)].Jumps[I], FAssembler.CurrentInstructionIndex);
  SetLength(FLoops, Length(FLoops) - 1);
end;

// CONTINUE FOR / CONTINUE WHILE / CONTINUE DO: skip the rest of the body
// and go on with the innermost loop of that kind's next iteration (FOR:
// its step, WHILE: its condition, DO: its condition, or its top).
procedure TParser.ContinueStatement;
var
  What: string;
  I: Integer;
begin
  Advance; // CONTINUE
  What := AnsiUpperCase(FCurrentToken.Lexeme);
  if not (Check(tkKeyword) and ((What = 'FOR') or (What = 'WHILE') or (What = 'DO'))) then
    Error('Expected CONTINUE FOR, CONTINUE WHILE or CONTINUE DO');
  for I := High(FLoops) downto 0 do
    if FLoops[I].Kind = What then
    begin
      Advance;
      if (FInFinally > 0) and (I < FFinallyLoopBase) then
        Error('CONTINUE can''t leave a FINALLY block');
      LeaveRegions(FLoops[I].TryDepth); // TRY blocks inside the loop
      SetLength(FLoops[I].ContinueJumps, Length(FLoops[I].ContinueJumps) + 1);
      FLoops[I].ContinueJumps[High(FLoops[I].ContinueJumps)] := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_JUMP, [-1]);
      Exit;
    end;
  Error('CONTINUE ' + What + ' is only allowed inside a ' + What + ' loop');
end;

// Points the innermost loop's CONTINUE jumps at Target.
procedure TParser.PatchContinues(Target: Integer);
var
  I: Integer;
begin
  for I := 0 to High(FLoops[High(FLoops)].ContinueJumps) do
    FAssembler.PatchJumpTarget(FLoops[High(FLoops)].ContinueJumps[I], Target);
end;

// EXIT FOR / EXIT WHILE / EXIT DO: leave the innermost loop of that kind (even from
// inside other loops nested in it). EXIT SUB: leave the SUB, like RETURN.
procedure TParser.ExitStatement;
var
  What: string;
  I: Integer;
begin
  Advance; // EXIT
  What := AnsiUpperCase(FCurrentToken.Lexeme);
  if ((What = 'SUB') or (What = 'FUNCTION') or (What = 'PROPERTY') or (What = 'DEF')) and Check(tkKeyword) then
  begin
    if (FProc = '') or (What <> FProcKind) then
      Error('EXIT ' + What + ' is only allowed inside a ' + What);
    if FInFinally > 0 then
      Error('EXIT ' + What + ' can''t leave a FINALLY block');
    Advance;
    LeaveRegions(0);
    SetLength(FReturnJumps, Length(FReturnJumps) + 1);
    FReturnJumps[High(FReturnJumps)] := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
    Exit;
  end;
  if not (Check(tkKeyword) and ((What = 'FOR') or (What = 'WHILE') or (What = 'DO'))) then
    Error('Expected EXIT FOR, EXIT WHILE, EXIT DO, EXIT SUB or EXIT FUNCTION');
  for I := High(FLoops) downto 0 do
    if FLoops[I].Kind = What then
    begin
      Advance;
      if (FInFinally > 0) and (I < FFinallyLoopBase) then
        Error('EXIT can''t leave a FINALLY block');
      LeaveRegions(FLoops[I].TryDepth); // TRY blocks inside the loop
      SetLength(FLoops[I].Jumps, Length(FLoops[I].Jumps) + 1);
      FLoops[I].Jumps[High(FLoops[I].Jumps)] := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_JUMP, [-1]);
      Exit;
    end;
  Error('EXIT ' + What + ' is only allowed inside a ' + What + ' loop');
end;

// SELECT CASE expr
//   CASE 1, 2, 3        - any of these values
//   CASE 5 TO 10        - a range (inclusive)
//   CASE IS > 20        - a comparison (IS optional: CASE > 20)
//   CASE ELSE           - none of the above
// END SELECT
// The expression is evaluated once. The first matching CASE runs; there's
// no fall-through.
procedure TParser.SelectStatement;
var
  Value, ToNextCase, I: Integer;
  EndJumps, BodyJumps, ItemFails: TIntegerArray;
  Context, Op: string;
  SeenElse: Boolean;

  procedure Add(var List: TIntegerArray; Index: Integer);
  begin
    SetLength(List, Length(List) + 1);
    List[High(List)] := Index;
  end;

  function JumpIfFalse: Integer;
  begin
    Result := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  end;

  function Jump: Integer;
  begin
    Result := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
  end;

  // value <op> expression; a false result fails this item.
  procedure TestCompare(CmpOp: TByteCodeOp);
  begin
    FAssembler.Emit(BC_LOAD_VAR, [Value]);
    Expression;
    FAssembler.Emit(CmpOp, []);
    Add(ItemFails, JumpIfFalse);
  end;

  function OpFor(const S: string): TByteCodeOp;
  begin
    Result := BC_CMP_EQ;
    if S = '=' then Result := BC_CMP_EQ
    else if S = '<>' then Result := BC_CMP_NEQ
    else if S = '<' then Result := BC_CMP_LT
    else if S = '>' then Result := BC_CMP_GT
    else if S = '<=' then Result := BC_CMP_LE
    else if S = '>=' then Result := BC_CMP_GE
    else
      Error('Expected a comparison (= <> < > <= >=) after IS');
  end;

  function AtComparison: Boolean;
  begin
    Result := (FCurrentToken.TokenType = tkOperator) and
      ((FCurrentToken.Lexeme = '<') or (FCurrentToken.Lexeme = '>') or (FCurrentToken.Lexeme = '<=') or
       (FCurrentToken.Lexeme = '>=') or (FCurrentToken.Lexeme = '<>') or (FCurrentToken.Lexeme = '='));
  end;

begin
  Context := Format('the SELECT CASE that starts at line %d - is END SELECT missing?', [FCurrentToken.Line + 1]);
  Advance; // SELECT
  if not AtKeyword('CASE') then
    Error('Expected SELECT CASE');
  Advance; // CASE
  Expression;
  Value := HiddenVar('select'); // evaluated once
  FAssembler.Emit(BC_STORE_VAR, [Value]);
  if not (Check(tkEndOfLine) or Check(tkComment)) then
    Error('Expected the end of the line after SELECT CASE ...');
  while Check(tkEndOfLine) or Check(tkComment) do
    Advance;
  if FCurrentToken.TokenType = tkEndOfFile then
    Error('Reached the end of the file inside ' + Context);
  if not (AtKeyword('CASE') or AtKeyword('END')) then
    Error('Expected CASE (only CASE branches can follow SELECT CASE)');

  EndJumps := nil;
  SeenElse := False;
  while AtKeyword('CASE') do
  begin
    if SeenElse then
      Error('CASE ELSE must be the last branch of a SELECT CASE');
    Advance; // CASE
    ToNextCase := -1;
    if AtKeyword('ELSE') then
    begin
      Advance;
      SeenElse := True;
    end
    else
    begin
      // Each item: its tests jump to the next item when they fail, and a
      // match jumps to the body. When every item fails, skip the body.
      BodyJumps := nil;
      repeat
        ItemFails := nil;
        if (FCurrentToken.TokenType = tkOperator) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'IS') then
        begin
          Advance;
          if not AtComparison then
            Error('Expected a comparison (= <> < > <= >=) after IS');
          Op := FCurrentToken.Lexeme;
          Advance;
          TestCompare(OpFor(Op));
        end
        else if AtComparison and (FCurrentToken.Lexeme <> '=') then
        begin
          Op := FCurrentToken.Lexeme; // CASE > 20, without IS
          Advance;
          TestCompare(OpFor(Op));
        end
        else
        begin
          FAssembler.Emit(BC_LOAD_VAR, [Value]);
          Expression;
          if AtKeyword('TO') then
          begin
            // CASE low TO high
            Advance;
            FAssembler.Emit(BC_CMP_GE, []);
            Add(ItemFails, JumpIfFalse);
            TestCompare(BC_CMP_LE);
          end
          else
          begin
            FAssembler.Emit(BC_CMP_EQ, []);
            Add(ItemFails, JumpIfFalse);
          end;
        end;
        Add(BodyJumps, Jump);
        for I := 0 to High(ItemFails) do
          FAssembler.PatchJumpTarget(ItemFails[I], FAssembler.CurrentInstructionIndex);
        if not Check(tkComma) then
          Break;
        Advance;
      until False;
      ToNextCase := Jump;
      for I := 0 to High(BodyJumps) do
        FAssembler.PatchJumpTarget(BodyJumps[I], FAssembler.CurrentInstructionIndex);
    end;
    if not (Check(tkEndOfLine) or Check(tkComment) or Check(tkColon)) then
      Error('Expected the end of the line after CASE ...');

    ParseBlockUntil(Context, ['CASE', 'END']);
    Add(EndJumps, Jump); // branch done: past END SELECT
    if ToNextCase >= 0 then
      FAssembler.PatchJumpTarget(ToNextCase, FAssembler.CurrentInstructionIndex);
  end;

  if not AtKeyword('END') then
    Error('Expected END SELECT');
  Advance; // END
  if not AtKeyword('SELECT') then
    Error('Expected END SELECT to close the SELECT CASE');
  Advance; // SELECT
  for I := 0 to High(EndJumps) do
    FAssembler.PatchJumpTarget(EndJumps[I], FAssembler.CurrentInstructionIndex);
end;

procedure TParser.WhileStatement;
var
  LoopStart, JumpToExit: Integer;
  Context: string;
begin
  Context := Format('the WHILE loop that starts at line %d - is END WHILE missing?', [FCurrentToken.Line + 1]);
  Match(tkKeyword); // Consume WHILE

  // The condition is re-evaluated every iteration, so its bytecode lives
  // at LoopStart and the back-edge below jumps to it, not to the body.
  LoopStart := FAssembler.CurrentInstructionIndex;
  Expression;
  if not (Check(tkEndOfLine) or Check(tkComment) or Check(tkColon)) then
    Error('Expected the end of the line after WHILE condition');

  JumpToExit := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);

  PushLoop('WHILE');
  try
    ParseBlockUntil(Context, ['END', 'WEND']);
    PatchContinues(LoopStart);
    FAssembler.Emit(BC_JUMP, [LoopStart]);
    FAssembler.PatchJumpTarget(JumpToExit, FAssembler.CurrentInstructionIndex);
  finally
    PopLoop; // EXIT WHILE jumps land here, past the loop
  end;

  if AtKeyword('WEND') then
    Advance
  else
  begin
    Advance; // END
    if not AtKeyword('WHILE') then
      Error('Expected END WHILE (or WEND) to close the WHILE loop');
    Advance; // WHILE
  end;
end;

procedure TParser.ForStatement;
var
  LoopVarName, EndVarName, StepVarName, Context: string;
  LoopStart, JumpToExit, JumpAscending, JumpToCheck: Integer;
begin
  Context := Format('the FOR loop that starts at line %d - is NEXT missing?', [FCurrentToken.Line + 1]);
  Match(tkKeyword); // Consume FOR
  if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'EACH') then
  begin
    ForEachStatement(Context);
    Exit;
  end;
  if not Check(tkIdentifier) then
    Error('Expected loop variable after FOR');
  LoopVarName := FCurrentToken.Lexeme;
  Advance; // Loop variable
  if not (Check(tkOperator) and (FCurrentToken.Lexeme = '=')) then
    Error('Expected = after FOR ' + LoopVarName);
  Advance; // =
  Expression; // Start value
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(LoopVarName)]); // loopvar := start
  if not AtKeyword('TO') then
    Error('Expected TO in FOR ' + LoopVarName + ' = ... TO ...');
  Advance; // TO

  // End and step are evaluated once, at loop entry (not re-evaluated each
  // iteration), matching classic BASIC FOR semantics. They're stashed in
  // hidden per-loop-variable slots since the bytecode has no scratch
  // registers of its own.
  EndVarName := '__for_end$' + LoopVarName;
  StepVarName := '__for_step$' + LoopVarName;
  DeclareLocal(EndVarName); // inside a SUB / FUNCTION, so recursion can't clobber them
  DeclareLocal(StepVarName);

  Expression; // End value
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(EndVarName)]);

  if AtKeyword('STEP') then
  begin
    Advance;
    Expression; // Step value
  end
  else
    FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]); // Default step: 1
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(StepVarName)]);

  if not (Check(tkEndOfLine) or Check(tkComment) or Check(tkColon)) then
    Error('Expected the end of the line after FOR ... TO ... [STEP ...]');

  // Keep looping while var <= end for a positive (or zero) step, and
  // var >= end for a negative one - decided at run time, so STEP s works
  // whatever the sign of s.
  LoopStart := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(StepVarName)]);
  FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
  FAssembler.Emit(BC_CMP_LT, []);
  JumpAscending := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(LoopVarName)]);
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(EndVarName)]);
  FAssembler.Emit(BC_CMP_GE, []);
  JumpToCheck := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  FAssembler.PatchJumpTarget(JumpAscending, FAssembler.CurrentInstructionIndex);
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(LoopVarName)]);
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(EndVarName)]);
  FAssembler.Emit(BC_CMP_LE, []);
  FAssembler.PatchJumpTarget(JumpToCheck, FAssembler.CurrentInstructionIndex);
  JumpToExit := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);

  PushLoop('FOR');
  try
    ParseBlockUntil(Context, ['NEXT']);
    PatchContinues(FAssembler.CurrentInstructionIndex); // the step code below

  // loopvar := loopvar + step
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(LoopVarName)]);
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(StepVarName)]);
  FAssembler.Emit(BC_ADD, []);
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(LoopVarName)]);
  FAssembler.Emit(BC_JUMP, [LoopStart]);
  FAssembler.PatchJumpTarget(JumpToExit, FAssembler.CurrentInstructionIndex);
  finally
    PopLoop; // EXIT FOR jumps land here, past the loop
  end;

  Advance; // NEXT
  // Optional loop variable after NEXT - it must be this loop's.
  if Check(tkIdentifier) then
  begin
    if not SameText(FCurrentToken.Lexeme, LoopVarName) then
      Error('NEXT ' + FCurrentToken.Lexeme + ' doesn''t match FOR ' + LoopVarName);
    Advance;
  end;
end;

// FOR EACH item IN array
//   ...
// NEXT [item]
// item is each element in turn (a copy of it: assigning to item doesn't
// change the array). EXIT FOR and CONTINUE FOR work as in FOR.
procedure TParser.ForEachStatement(const Context: string);
var
  VarName: string;
  Arr, Idx, LoopStart, JumpToExit: Integer;
begin
  Advance; // EACH
  if not Check(tkIdentifier) then
    Error('Expected a variable after FOR EACH');
  VarName := FCurrentToken.Lexeme;
  Advance;
  if not (Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'IN')) then
    Error('Expected IN in FOR EACH ' + VarName + ' IN ...');
  Advance; // IN
  Expression;
  Arr := HiddenVar('each');
  FAssembler.Emit(BC_STORE_VAR, [Arr]); // evaluated once
  Idx := HiddenVar('each');
  EmitInt(0);
  FAssembler.Emit(BC_STORE_VAR, [Idx]);
  if not (Check(tkEndOfLine) or Check(tkComment) or Check(tkColon)) then
    Error('Expected the end of the line after FOR EACH ... IN ...');

  LoopStart := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_LOAD_VAR, [Idx]);
  FAssembler.Emit(BC_LOAD_VAR, [Arr]);
  FAssembler.Emit(BC_BUILTIN, [BI_LEN, 1]);
  FAssembler.Emit(BC_CMP_LT, []);
  JumpToExit := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
  FAssembler.Emit(BC_LOAD_VAR, [Arr]);
  FAssembler.Emit(BC_LOAD_VAR, [Idx]);
  FAssembler.Emit(BC_INDEX_GET, []);
  FAssembler.Emit(BC_STORE_VAR, [Op_Variable(VarName)]);

  PushLoop('FOR');
  try
    ParseBlockUntil(Context, ['NEXT']);
    PatchContinues(FAssembler.CurrentInstructionIndex);
    FAssembler.Emit(BC_LOAD_VAR, [Idx]);
    EmitInt(1);
    FAssembler.Emit(BC_ADD, []);
    FAssembler.Emit(BC_STORE_VAR, [Idx]);
    FAssembler.Emit(BC_JUMP, [LoopStart]);
    FAssembler.PatchJumpTarget(JumpToExit, FAssembler.CurrentInstructionIndex);
  finally
    PopLoop;
  end;

  Advance; // NEXT
  if Check(tkIdentifier) then
  begin
    if not SameText(FCurrentToken.Lexeme, VarName) then
      Error('NEXT ' + FCurrentToken.Lexeme + ' doesn''t match FOR EACH ' + VarName);
    Advance;
  end;
end;

// WITH object
//   .Field = 1      .Method 2      PRINT .Field
// END WITH
procedure TParser.WithStatement;
var
  Context: string;
  Slot: Integer;
begin
  Context := Format('the WITH block that starts at line %d - is END WITH missing?', [FCurrentToken.Line + 1]);
  Advance; // WITH
  Expression;
  Slot := HiddenVar('with');
  FAssembler.Emit(BC_STORE_VAR, [Slot]);
  if not (Check(tkEndOfLine) or Check(tkComment)) then
    Error('Expected the end of the line after WITH ...');
  SetLength(FWithSlots, Length(FWithSlots) + 1);
  FWithSlots[High(FWithSlots)] := Slot;
  try
    ParseBlockUntil(Context, ['END']);
  finally
    SetLength(FWithSlots, Length(FWithSlots) - 1);
  end;
  Advance; // END
  if not AtKeyword('WITH') then
    Error('Expected END WITH to close the WITH block');
  Advance;
end;

// TRY
//   ...
// [CATCH [e [AS Exception]]
//   ...]           e is the error message (e.Message works too)
// [FINALLY
//   ...]           runs however the block is left
// END TRY
// A runtime error in the TRY block - also inside SUBs / FUNCTIONs it calls,
// or a THROW - jumps to CATCH. BC_TRY registers the CATCH address; the
// block's normal end unregisters it (BC_TRY_END) and skips the CATCH.
//
// FINALLY is compiled once, as a local subroutine (BC_CALL ... BC_RETURN,
// like GOSUB), called from every way out:
//   BC_TRY handler      body      BC_TRY_END, CALL fin, JUMP end
// handler: [CATCH: STORE e, BC_TRY handler2, catch body, BC_TRY_END,
//           CALL fin, JUMP end
// handler2:] STORE exc, CALL fin, LOAD exc, THROW    (the error goes on)
// fin:     finally body, BC_RETURN
// end:
// EXIT / CONTINUE / RETURN out of the TRY or CATCH part also end it and
// CALL fin first (see LeaveRegions).
procedure TParser.TryStatement;
var
  Context: string;
  TryAt, Handler2, Slot, Exc, I, SavedLoopBase: Integer;
  HasFinally, HasCatch: Boolean;
  Calls, EndJumps: TIntegerArray;
  R: TRegion;

  procedure Add(var List: TIntegerArray; Index: Integer);
  begin
    SetLength(List, Length(List) + 1);
    List[High(List)] := Index;
  end;

  procedure AddAll(const Region: TRegion);
  var
    K: Integer;
  begin
    for K := 0 to High(Region.Calls) do
      Add(Calls, Region.Calls[K]);
  end;

  procedure CallFinally;
  begin
    Add(Calls, FAssembler.CurrentInstructionIndex);
    FAssembler.Emit(BC_CALL, [-1, 0]);
  end;

  // The error on the stack goes on to the next TRY out, after FINALLY.
  procedure FinallyThenRethrow;
  begin
    if Exc < 0 then
      Exc := HiddenVar('exc');
    FAssembler.Emit(BC_STORE_VAR, [Exc]);
    CallFinally;
    FAssembler.Emit(BC_LOAD_VAR, [Exc]);
    FAssembler.Emit(BC_THROW, []);
  end;

begin
  Context := Format('the TRY block that starts at line %d - is END TRY missing?', [FCurrentToken.Line + 1]);
  HasFinally := FFinallyLines.IndexOf(IntToStr(FCurrentToken.Line)) >= 0;
  Calls := nil;
  EndJumps := nil;
  Exc := -1;
  Advance; // TRY
  if not (Check(tkEndOfLine) or Check(tkComment)) then
    Error('Expected the end of the line after TRY');
  TryAt := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_TRY, [-1]);
  PushRegion(HasFinally);
  try
    ParseBlockUntil(Context, ['CATCH', 'FINALLY', 'END']);
  finally
    R := PopRegion;
  end;
  AddAll(R);
  FAssembler.Emit(BC_TRY_END, []);
  if HasFinally then
    CallFinally;
  Add(EndJumps, FAssembler.CurrentInstructionIndex);
  FAssembler.Emit(BC_JUMP, [-1]);

  // The handler: the error message is on the stack.
  FAssembler.PatchJumpTarget(TryAt, FAssembler.CurrentInstructionIndex);
  HasCatch := AtKeyword('CATCH');
  if HasCatch then
  begin
    Advance; // CATCH
    if Check(tkIdentifier) then
    begin
      DeclareLocal(FCurrentToken.Lexeme);
      Slot := Op_Variable(FCurrentToken.Lexeme);
      Advance;
      if AtKeyword('AS') then
      begin
        Advance;
        Match(tkIdentifier); // the type isn't checked
      end;
    end
    else
      Slot := HiddenVar('exc');
    FAssembler.Emit(BC_STORE_VAR, [Slot]);
    Handler2 := -1;
    if HasFinally then
    begin
      // An error in the CATCH block still runs FINALLY.
      Handler2 := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_TRY, [-1]);
      PushRegion(True);
    end;
    SetLength(FCatchSlots, Length(FCatchSlots) + 1);
    FCatchSlots[High(FCatchSlots)] := Slot;
    SetLength(FCatchStack, Length(FCatchStack) + 1);
    FCatchStack[High(FCatchStack)] := Slot;
    try
      ParseBlockUntil(Context, ['CATCH', 'FINALLY', 'END']);
    finally
      SetLength(FCatchStack, Length(FCatchStack) - 1);
      if HasFinally then
        AddAll(PopRegion);
    end;
    if AtKeyword('CATCH') then
      Error('A TRY block can only have one CATCH');
    if HasFinally then
    begin
      FAssembler.Emit(BC_TRY_END, []);
      CallFinally;
      Add(EndJumps, FAssembler.CurrentInstructionIndex);
      FAssembler.Emit(BC_JUMP, [-1]);
      FAssembler.PatchJumpTarget(Handler2, FAssembler.CurrentInstructionIndex);
      FinallyThenRethrow;
    end;
  end
  else if HasFinally then
    FinallyThenRethrow
  else
    Error('A TRY block needs a CATCH or a FINALLY');

  if HasFinally then
  begin
    if not AtKeyword('FINALLY') then
      Error('Expected FINALLY');
    Advance; // FINALLY
    if not (Check(tkEndOfLine) or Check(tkComment)) then
      Error('Expected the end of the line after FINALLY');
    for I := 0 to High(Calls) do
      FAssembler.PatchJumpTarget(Calls[I], FAssembler.CurrentInstructionIndex);
    Inc(FInFinally);
    SavedLoopBase := FFinallyLoopBase;
    FFinallyLoopBase := Length(FLoops);
    try
      ParseBlockUntil(Context, ['CATCH', 'FINALLY', 'END']);
    finally
      Dec(FInFinally);
      FFinallyLoopBase := SavedLoopBase;
    end;
    FAssembler.Emit(BC_RETURN, []);
    if AtKeyword('CATCH') or AtKeyword('FINALLY') then
      Error('FINALLY must be the last part of a TRY block');
  end;
  Advance; // END
  if not AtKeyword('TRY') then
    Error('Expected END TRY to close the TRY block');
  Advance; // TRY
  for I := 0 to High(EndJumps) do
    FAssembler.PatchJumpTarget(EndJumps[I], FAssembler.CurrentInstructionIndex);
end;

procedure TParser.PushRegion(HasFinally: Boolean);
begin
  SetLength(FRegions, Length(FRegions) + 1);
  FRegions[High(FRegions)].HasFinally := HasFinally;
  FRegions[High(FRegions)].Calls := nil;
end;

function TParser.PopRegion: TRegion;
begin
  Result := FRegions[High(FRegions)];
  SetLength(FRegions, Length(FRegions) - 1);
end;

// Code to leave the TRY regions from Level up (innermost first): end each
// one's TRY, and run its FINALLY.
procedure TParser.LeaveRegions(Level: Integer);
var
  I: Integer;
begin
  for I := High(FRegions) downto Level do
  begin
    FAssembler.Emit(BC_TRY_END, []);
    if FRegions[I].HasFinally then
    begin
      SetLength(FRegions[I].Calls, Length(FRegions[I].Calls) + 1);
      FRegions[I].Calls[High(FRegions[I].Calls)] := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_CALL, [-1, 0]);
    end;
  end;
end;

// THROW message - raises a runtime error (caught by a TRY, else it ends the
// program). A plain THROW inside CATCH re-throws the error being handled.
procedure TParser.ThrowStatement;
begin
  Advance; // THROW
  if AtStatementEnd then
  begin
    if Length(FCatchStack) = 0 then
      Error('THROW needs a message (a plain THROW re-throws, inside CATCH)');
    FAssembler.Emit(BC_LOAD_VAR, [FCatchStack[High(FCatchStack)]]);
  end
  else
    Expression;
  FAssembler.Emit(BC_THROW, []);
end;

// SUB <name> [ ( [<param> [, <param>]*] ) ] ... END SUB
//
// The body is jumped over where it's defined and only runs via CALL (or
// as a QT "on" event handler). Layout:
//   BC_JUMP past-the-body
//   BC_ENTER paramcount            <- entry address (SubroutineMap)
//   BC_STORE_VAR param_n ... param_1  (arguments arrive pushed left to right)
//   <body>
//   BC_RETURN
// Variables are global in this VM, so parameters are too: there are no
// locals, and a recursive SUB overwrites its own parameters.
procedure TParser.SubDefinition;
begin
  ProcedureDefinition(False);
end;

procedure TParser.FunctionDefinition;
begin
  ProcedureDefinition(True);
end;

// SUB Name[(params)] ... END SUB
// FUNCTION Name[(params)] [AS Type] ... END FUNCTION
//
// A parameter is "[ByVal | ByRef] name [AS Type]". A ByRef parameter's
// final value is copied back into the variable the caller passed (copy-in /
// copy-out; an expression or literal argument just gets a temporary copy,
// as in VB). Parameters, DIMs inside the body
// and compiler-made variables are locals; a FUNCTION's result is the local
// named like it ("Name = value", or "RETURN value"). Any other name is a
// global, as before.
//
// The VM has no local storage, so each procedure saves its locals' current
// values on the evaluation stack on entry and restores them on the way out
// - which makes recursion work. The locals are only all known once the body
// is parsed, so the prologue is emitted after the body:
//   BC_JUMP past-it-all
//   BC_ENTER paramcount             <- entry address (SubroutineMap)
//   BC_JUMP prologue
// body:
//   <body>                          RETURN / EXIT jump to the epilogue
// epilogue:
//   [result -> __ret]               (FUNCTION: the caller reads __ret)
//   BC_STORE_VAR local_k .. local_1 (restore the caller's values)
//   BC_RETURN
// prologue:
//   BC_STORE_VAR arg temps          (arguments arrive pushed left to right)
//   BC_LOAD_VAR local_1 .. local_k  (save the caller's values)
//   params := args, other locals := 0
//   BC_JUMP body
//
// In a CLASS it's a method "CLASS.NAME" (a PROPERTY GET is a FUNCTION
// "CLASS.NAME", a PROPERTY LET / SET a SUB "CLASS.NAME$LET"), with a hidden
// last parameter ME: the object.
//
// When the program has a TRY, the body runs inside an implicit one: an
// error that leaves the procedure (to a CATCH further out) first restores
// the caller's locals, which the error skipped past, then re-throws.
procedure TParser.ProcedureDefinition(IsFunction: Boolean; const PropAccessor: string);
var
  Key, Kind, Name, Context: string;
  Params: TStringList;
  JumpOver, ToPrologue, BodyStart, Epilogue, I, ImplicitTry: Integer;
  SavedRegions: array of TRegion;
  Slot: Integer;
  ByRefFlags: string; // one '0' / '1' per parameter
  SingleLine: Boolean; // DEF FNname(params) = expression
begin
  SingleLine := False;
  if PropAccessor = 'DEF' then Kind := 'DEF' // QuickBASIC DEF FN (DEF consumed by the caller)
  else if PropAccessor <> '' then Kind := 'PROPERTY'
  else if IsFunction then Kind := 'FUNCTION'
  else Kind := 'SUB';
  Context := Format('the %s that starts at line %d - is END %s missing?', [Kind, FCurrentToken.Line + 1, Kind]);
  if PropAccessor = '' then
    Match(tkKeyword); // Consume SUB / FUNCTION (PROPERTY GET: consumed by PropertyDefinition)
  if FProc <> '' then
    Error(Kind + ' definitions cannot be nested');
  if not (Check(tkIdentifier) or ((FCurrentClassName <> '') and AtKeyword('NEW'))) then
    Error('Expected ' + Kind + ' name');
  Name := FCurrentToken.Lexeme;
  Key := UpperCase(Name);
  if FCurrentClassName <> '' then
    Key := UpperCase(FCurrentClassName) + '.' + Key; // methods don't clash with top-level procedures
  if (PropAccessor = 'LET') or (PropAccessor = 'SET') then
    Key := Key + '$LET';
  Advance; // name
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams

  if FAssembler.CurrentProgram.SubroutineMap.IndexOf(Key) >= 0 then
    Error(Kind + ' "' + Key + '" is already defined');

  Params := TStringList.Create;
  ByRefFlags := '';
  try
    if Check(tkParenthesisOpen) then
    begin
      Advance;
      if not Check(tkParenthesisClose) then
        repeat
          if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'BYREF') then
          begin
            Advance;
            ByRefFlags := ByRefFlags + '1';
          end
          else
          begin
            if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'BYVAL') then
              Advance;
            ByRefFlags := ByRefFlags + '0';
          end;
          if not Check(tkIdentifier) then
            Error('Expected parameter name');
          if Params.IndexOf(UpperCase(FCurrentToken.Lexeme)) >= 0 then
            Error('Parameter "' + FCurrentToken.Lexeme + '" is listed twice');
          Params.Add(UpperCase(FCurrentToken.Lexeme));
          Advance;
          if Check(tkParenthesisOpen) and (PeekToken.TokenType = tkParenthesisClose) then
          begin
            Advance; // arr() - an array parameter
            Advance;
          end;
          if AtKeyword('AS') then
          begin
            Advance;
            if FQB and Check(tkIdentifier) then
              FVarTypes.Values[Params[Params.Count - 1]] := UpperCase(FCurrentToken.Lexeme); // for GET / PUT
            Match(tkIdentifier); // type name (not checked yet)
            SkipGenericParams;
          end;
          if not Check(tkComma) then
            Break;
          Advance;
        until False;
      Match(tkParenthesisClose);
    end;
    if IsFunction and AtKeyword('AS') then
    begin
      Advance; // AS
      Match(tkIdentifier); // return type (not checked yet)
      SkipGenericParams;
    end;
    if FQB and Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'STATIC') then
      Advance; // SUB x STATIC: QuickBASIC's "all variables keep their values" (not modeled)
    SingleLine := (Kind = 'DEF') and AtAssign;
    if not SingleLine and not (Check(tkEndOfLine) or Check(tkComment)) then
      Error('Expected the end of the line after the ' + Kind + ' header');
    if (PropAccessor = 'GET') and (Params.Count > 0) then
      Error('PROPERTY GET can''t have parameters');
    if ((PropAccessor = 'LET') or (PropAccessor = 'SET')) and (Params.Count <> 1) then
      Error('PROPERTY ' + PropAccessor + ' takes one parameter: the new value');
    if FCurrentClassName <> '' then
    begin
      if Params.IndexOf('ME') >= 0 then
        Error('ME is the object itself: it can''t be a parameter name');
      Params.Add('ME');
      ByRefFlags := ByRefFlags + '0';
    end;

    JumpOver := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
    FAssembler.CurrentProgram.SubroutineMap.Add(Key, FAssembler.CurrentInstructionIndex);
    FSubParams.Add(Key, Params.Count);
    FByRef.Values[Key] := ByRefFlags;
    if IsFunction then
      FFunctions.Add(Key);
    FAssembler.Emit(BC_ENTER, [Params.Count]);
    ToPrologue := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP, [-1]);
    BodyStart := FAssembler.CurrentInstructionIndex;

    FProc := Key;
    FProcIsFunction := IsFunction;
    FProcKind := Kind;
    SavedRegions := FRegions;
    FRegions := nil;
    FProcShared.Clear;
    FProcStatic.Clear;
    ImplicitTry := -1;
    if FHasTry then
    begin
      ImplicitTry := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_TRY, [-1]);
    end;
    FLocals.Clear;
    FReturnJumps := nil;
    FProcUsesGosub := False;
    FGosubUnwinds := nil;
    if IsFunction then
      FLocals.Add(UpperCase(Name)); // the result: always FLocals[0]
    for I := 0 to Params.Count - 1 do
      if FLocals.IndexOf(Params[I]) < 0 then
        FLocals.Add(Params[I])
      else
        Error('A parameter can''t have the FUNCTION''s own name');

    try
      if SingleLine then
      begin
        Advance; // =
        Expression;
        FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable(Key + ':' + FLocals[0])]);
      end
      else
        ParseBlockUntil(Context, ['END']);
    finally
      // Epilogue: every way out of the body ends up here.
      Epilogue := FAssembler.CurrentInstructionIndex;
      for I := 0 to High(FReturnJumps) do
        FAssembler.PatchJumpTarget(FReturnJumps[I], Epilogue);
      for I := 0 to High(FGosubUnwinds) do
        FAssembler.PatchJumpTarget(FGosubUnwinds[I], Epilogue);
      if FProcUsesGosub then
      begin
        // Leaving while GOSUBs are in progress: return from each first
        // (with __unwind set, the code after the GOSUB comes straight back).
        FAssembler.Emit(BC_LOAD_VAR, [Op_Variable('__gosub')]);
        FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
        FAssembler.Emit(BC_CMP_GT, []);
        I := FAssembler.CurrentInstructionIndex;
        FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
        FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]);
        FAssembler.Emit(BC_STORE_VAR, [Op_Variable('__unwind')]);
        FAssembler.Emit(BC_RETURN, []);
        FAssembler.PatchJumpTarget(I, FAssembler.CurrentInstructionIndex);
      end;
      if IsFunction then
      begin
        FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable(Key + ':' + FLocals[0])]);
        FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable('__ret')]);
      end;
      // ByRef parameters: their final values, for the caller to copy back
      // into the variables it passed (before the locals are restored).
      for I := 0 to Params.Count - 1 do
        if ByRefFlags[I + 1] = '1' then
        begin
          FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable(Key + ':' + Params[I])]);
          FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable(Format('__byref$%s$%d', [Key, I]))]);
        end;
      for I := FLocals.Count - 1 downto 0 do
        FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable(Key + ':' + FLocals[I])]);
      FAssembler.Emit(BC_RETURN, []);

      // The implicit TRY's CATCH: the stack is back to the saved locals,
      // plus the message.
      if ImplicitTry >= 0 then
      begin
        FAssembler.PatchJumpTarget(ImplicitTry, FAssembler.CurrentInstructionIndex);
        Slot := FAssembler.CurrentProgram.AddVariable('__exc');
        FAssembler.Emit(BC_STORE_VAR, [Slot]);
        for I := FLocals.Count - 1 downto 0 do
          FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable(Key + ':' + FLocals[I])]);
        FAssembler.Emit(BC_LOAD_VAR, [Slot]);
        FAssembler.Emit(BC_THROW, []);
      end;

      // Prologue.
      FAssembler.PatchJumpTarget(ToPrologue, FAssembler.CurrentInstructionIndex);
      for I := Params.Count - 1 downto 0 do
        FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable(Format('__arg$%s$%d', [Key, I]))]);
      for I := 0 to FLocals.Count - 1 do
        FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable(Key + ':' + FLocals[I])]);
      for I := 0 to FLocals.Count - 1 do
      begin
        Slot := FAssembler.CurrentProgram.AddVariable(Key + ':' + FLocals[I]);
        if Params.IndexOf(FLocals[I]) >= 0 then
          FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable(
            Format('__arg$%s$%d', [Key, Params.IndexOf(FLocals[I])]))])
        else if FLocals[I][Length(FLocals[I])] = '$' then
          EmitStr('') // name$: a string
        else
          FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]); // fresh locals start at 0
        FAssembler.Emit(BC_STORE_VAR, [Slot]);
      end;
      FAssembler.Emit(BC_JUMP, [BodyStart]);
      FAssembler.PatchJumpTarget(JumpOver, FAssembler.CurrentInstructionIndex);

      FProc := '';
      FProcIsFunction := False;
      FProcKind := '';
      FRegions := SavedRegions;
      FLocals.Clear;
      FReturnJumps := nil;
      FProcUsesGosub := False;
      FGosubUnwinds := nil;
    end;
  finally
    Params.Free;
  end;

  if SingleLine then
    Exit;
  Advance; // END
  if not AtKeyword(Kind) then
    Error('Expected END ' + Kind + ' to close the ' + Kind);
  Advance; // SUB / FUNCTION
end;

procedure TParser.FormDefinition;
begin
  Match(tkKeyword); // Consume FORM
  Match(tkIdentifier); // Form name

  Match(tkEndOfLine);

  ParseBlockBody('FORM definition', @DispatchStatement);

  Match(tkKeyword); // END
  Match(tkKeyword); // FORM
end;

// PROPERTY GET Name [AS Type]        ... END PROPERTY   (Name = value, or RETURN value)
// PROPERTY LET | SET Name(value)      ... END PROPERTY
// Inside a CLASS: obj.Name reads through GET, obj.Name = v writes through
// LET / SET (see ProcedureDefinition for the names they get).
procedure TParser.PropertyDefinition;
var
  Acc: string;
begin
  Advance; // PROPERTY
  Acc := AnsiUpperCase(FCurrentToken.Lexeme);
  if not ((Acc = 'GET') or (Acc = 'LET') or (Acc = 'SET')) then
    Error('Expected PROPERTY GET, PROPERTY LET or PROPERTY SET');
  Advance;
  ProcedureDefinition(Acc = 'GET', Acc);
end;

// CLASS Name [INHERITS Base]
//   [INHERITS Base]
//   [PUBLIC | PRIVATE | DIM] field [(bounds)] [AS [NEW] Type] [= value], ...
//   [PUBLIC | PRIVATE] [OVERRIDABLE | OVERRIDES] SUB / FUNCTION / PROPERTY ...
//   (methods; SUB New is the constructor: NEW Name(args) calls it)
// END CLASS
//
// An object is an array of its fields tagged with the class name (see
// BI_NEWOBJECT). Field initial values (arrays, AS NEW objects, = values)
// are set by the hidden procedure "NAME.__INIT", which NEW calls with the
// new object and which leaves it on the stack. Its code is spread over the
// class body - one piece per initial value, each jumping to the next:
//   BC_JUMP over                    (at CLASS)
//   NAME.__INIT: BC_ENTER 1, BC_STORE_VAR self, [the base's __INIT],
//                BC_JUMP piece1
//   ... piece1: self.field := value, BC_JUMP piece2 ...
//   last: BC_LOAD_VAR self, BC_RETURN   (at END CLASS)
//
// INHERITS: the class starts with the base's fields and methods; a method
// of its own with the same name overrides the base's - also for calls in
// the base's own code (methods are virtual). MYBASE.Method calls the
// base's version. A class without SUB New uses its base's.
// Visibility (PUBLIC / PRIVATE / PROTECTED) isn't enforced.
procedure TParser.ClassDefinition;
var
  Cls: TClassInfo;
  ClsName, Context, U: string;
  SelfSlot: Integer;
  IsField: Boolean;

  function IsMethodModifier(const W: string): Boolean;
  begin
    case AnsiUpperCase(W) of
      'OVERRIDABLE', 'OVERRIDES', 'OVERLOADS', 'SHADOWS', 'NOTOVERRIDABLE', 'MUSTOVERRIDE':
        Result := True;
    else
      Result := False;
    end;
  end;

  function AtModifier: Boolean;
  begin
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'PUBLIC', 'PRIVATE', 'FRIEND', 'PROTECTED':
        Result := Check(tkIdentifier);
    else
      Result := Check(tkIdentifier) and IsMethodModifier(FCurrentToken.Lexeme);
    end;
  end;

begin
  Context := Format('the CLASS that starts at line %d - is END CLASS missing?', [FCurrentToken.Line + 1]);
  Match(tkKeyword); // CLASS
  if (FProc <> '') or (FCurrentClassName <> '') then
    Error('A CLASS can only be defined at the top level');
  if not Check(tkIdentifier) then
    Error('Expected class name after CLASS keyword');
  ClsName := FCurrentToken.Lexeme;
  Advance;
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
  U := UpperCase(ClsName);
  if (U = 'INTEGER') or (U = 'STRING') or (U = 'ARRAY') then
    Error('"' + ClsName + '" is the name of a built-in type');
  Cls := ClassOf(ClsName);
  if (Cls = nil) or (FAssembler.CurrentProgram.SubroutineMap.IndexOf(U + '.__INIT') >= 0) then
    Error('CLASS "' + ClsName + '" is already defined');
  if Cls.Problem <> '' then
    ReportError(Cls.Problem + Format(' at line %d', [FCurrentToken.Line + 1]));
  if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'INHERITS') then
  begin
    Advance; // INHERITS
    Advance; // the base (checked by the pre-scan)
  end;
  if not (Check(tkEndOfLine) or Check(tkComment)) then
    Error('Expected the end of the line after CLASS ' + ClsName);

  SelfSlot := BeginClassInit(Cls);
  FCurrentClassName := Cls.Name;
  try
    while not (AtKeyword('END') and (PeekToken.TokenType = tkKeyword)) do
    begin
      if Check(tkEndOfFile) then
        Error('Reached the end of the file inside ' + Context);
      if Check(tkEndOfLine) or Check(tkComment) or Check(tkColon) then
      begin
        Advance;
        Continue;
      end;
      try
        if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'INHERITS') then
        begin
          Advance;
          if SameText(FCurrentToken.Lexeme, Cls.BaseName) and not Cls.Resolving then
            Advance
          else
            Error('INHERITS: a class can only inherit from one class');
          Continue;
        end;
        // Modifiers before SUB / FUNCTION / PROPERTY change nothing;
        // "PUBLIC name" declares a field.
        IsField := False;
        while AtModifier do
        begin
          if (PeekToken.TokenType = tkIdentifier) and not IsMethodModifier(PeekToken.Lexeme) then
          begin
            IsField := True;
            Break;
          end;
          Advance;
        end;
        if IsField then
        begin
          FieldDeclaration(Cls);
          Continue;
        end;
        U := AnsiUpperCase(FCurrentToken.Lexeme);
        if U = 'DIM' then
          FieldDeclaration(Cls)
        else if U = 'SUB' then
          ProcedureDefinition(False)
        else if U = 'FUNCTION' then
          ProcedureDefinition(True)
        else if U = 'PROPERTY' then
          PropertyDefinition
        else
          Error('A CLASS can only contain fields (DIM / PUBLIC / PRIVATE), SUBs, FUNCTIONs and PROPERTYs');
      except
        on E: EKayteParseError do
          raise;
        on E: Exception do
        begin
          ReportError(E.Message);
          while not (Check(tkEndOfLine) or Check(tkEndOfFile)) do
            Advance;
        end;
      end;
    end;
    Advance; // END
    if not AtKeyword('CLASS') then
      Error('END ' + AnsiUpperCase(FCurrentToken.Lexeme) + ' doesn''t close ' + Context);
    Advance; // CLASS
  finally
    FCurrentClassName := '';
    EndClassInit(SelfSlot);
  end;
end;

// The start of a class's initializer "NAME.__INIT" (see ClassDefinition);
// returns the variable that holds the object while it runs.
function TParser.BeginClassInit(Cls: TClassInfo): Integer;
var
  Over: Integer;
begin
  Over := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  FAssembler.CurrentProgram.SubroutineMap.Add(UpperCase(Cls.Name) + '.__INIT', FAssembler.CurrentInstructionIndex);
  FSubParams.Add(UpperCase(Cls.Name) + '.__INIT', 1);
  FAssembler.Emit(BC_ENTER, [1]);
  Result := FAssembler.CurrentProgram.AddVariable('__self$' + UpperCase(Cls.Name));
  FAssembler.Emit(BC_STORE_VAR, [Result]);
  if Cls.Base <> nil then
  begin
    // The base class's fields first.
    FAssembler.Emit(BC_LOAD_VAR, [Result]);
    EmitCall(UpperCase(Cls.Base.Name) + '.__INIT', 1, FCurrentToken.Line, False, nil);
    FAssembler.Emit(BC_POP, []);
  end;
  FInitNext := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  FAssembler.PatchJumpTarget(Over, FAssembler.CurrentInstructionIndex);
end;

// The initializer's end: give the object back.
procedure TParser.EndClassInit(SelfSlot: Integer);
var
  Over: Integer;
begin
  Over := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  FAssembler.PatchJumpTarget(FInitNext, FAssembler.CurrentInstructionIndex);
  FAssembler.Emit(BC_LOAD_VAR, [SelfSlot]);
  FAssembler.Emit(BC_RETURN, []);
  FAssembler.PatchJumpTarget(Over, FAssembler.CurrentInstructionIndex);
end;

// One piece of an initializer: field Idx of the object := the value the
// caller emits between BeginInitPiece and EndInitPiece.
function TParser.BeginInitPiece(SelfSlot, Idx: Integer): Integer;
begin
  Result := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  FAssembler.PatchJumpTarget(FInitNext, FAssembler.CurrentInstructionIndex);
  FAssembler.Emit(BC_LOAD_VAR, [SelfSlot]);
  EmitInt(Idx);
end;

procedure TParser.EndInitPiece(Over: Integer);
begin
  FAssembler.Emit(BC_INDEX_SET, []);
  FInitNext := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP, [-1]);
  FAssembler.PatchJumpTarget(Over, FAssembler.CurrentInstructionIndex);
end;

// A field declaration line in a CLASS (see ClassDefinition). A field with
// an initial value adds a piece to the class's initializer.
procedure TParser.FieldDeclaration(Cls: TClassInfo);
var
  Name, TypeName: string;
  Idx, Over, SelfSlot: Integer;
begin
  SelfSlot := FAssembler.CurrentProgram.AddVariable('__self$' + UpperCase(Cls.Name));
  Advance; // DIM / PUBLIC / PRIVATE
  repeat
    if not Check(tkIdentifier) then
      Error('Expected a field name');
    Name := FCurrentToken.Lexeme;
    Idx := Cls.Fields.IndexOf(UpperCase(Name));
    if Idx < 0 then
      Error('Internal error: field "' + Name + '" was missed by the pre-scan');
    Advance;
    if Check(tkParenthesisOpen) then
    begin
      Over := BeginInitPiece(SelfSlot, Idx);
      FAssembler.Emit(BC_BUILTIN, [BI_NEWARRAY, ParseDims]);
      EndInitPiece(Over);
    end;
    if AtKeyword('AS') then
    begin
      Advance;
      if AtKeyword('NEW') then
      begin
        Over := BeginInitPiece(SelfSlot, Idx);
        NewExpression;
        EndInitPiece(Over);
      end
      else
      begin
        if not Check(tkIdentifier) then
          Error('Expected type name after AS');
        TypeName := FCurrentToken.Lexeme;
        Advance; // the type isn't checked
        SkipGenericParams;
        if SameText(TypeName, 'String') and not AtAssign then
        begin
          Over := BeginInitPiece(SelfSlot, Idx);
          EmitStr('');
          EndInitPiece(Over);
        end;
      end;
    end;
    if AtAssign then
    begin
      Advance;
      Over := BeginInitPiece(SelfSlot, Idx);
      Expression;
      EndInitPiece(Over);
    end;
    if not Check(tkComma) then
      Break;
    Advance;
  until False;
end;

// QuickBASIC (--qbs):
// TYPE Name
//   field AS type          (STRING * n: any length; another TYPE: nested)
// END TYPE
// Compiled as a CLASS with only fields; DIM p AS Name makes the object (so
// p.field works at once), and so does each element of DIM a(n) AS Name.
// Unlike QuickBASIC, "p = q" makes p the same object as q (no copy).
procedure TParser.TypeDefinition;
var
  Cls, FieldCls: TClassInfo;
  Name, TypeName, Context: string;
  SelfSlot, Idx, Over: Integer;
begin
  Context := Format('the TYPE that starts at line %d - is END TYPE missing?', [FCurrentToken.Line + 1]);
  Advance; // TYPE
  if (FProc <> '') then
    Error('A TYPE can only be defined at the top level');
  if not Check(tkIdentifier) then
    Error('Expected the TYPE''s name');
  Cls := ClassOf(FCurrentToken.Lexeme);
  if (Cls = nil) or (FAssembler.CurrentProgram.SubroutineMap.IndexOf(UpperCase(Cls.Name) + '.__INIT') >= 0) then
    Error('TYPE "' + FCurrentToken.Lexeme + '" is already defined');
  Advance;
  SelfSlot := BeginClassInit(Cls);
  try
    while not (AtKeyword('END') and SameText(PeekToken.Lexeme, 'TYPE')) do
    begin
      if Check(tkEndOfFile) then
        Error('Reached the end of the file inside ' + Context);
      if Check(tkEndOfLine) or Check(tkComment) or Check(tkColon) then
      begin
        Advance;
        Continue;
      end;
      if not Check(tkIdentifier) then
        Error('Expected a field: name AS type');
      Name := FCurrentToken.Lexeme;
      Idx := Cls.Fields.IndexOf(UpperCase(Name));
      Advance;
      if not AtKeyword('AS') then
        Error('Expected AS after the field name ' + Name);
      Advance;
      TypeName := FCurrentToken.Lexeme;
      Advance;
      if Check(tkOperator) and (FCurrentToken.Lexeme = '*') then
      begin
        Advance; // STRING * 20: the length isn't kept
        Advance;
      end;
      FieldCls := ClassOf(TypeName);
      if SameText(TypeName, 'String') then
      begin
        Over := BeginInitPiece(SelfSlot, Idx);
        EmitStr('');
        EndInitPiece(Over);
      end
      else if FieldCls <> nil then
      begin
        Over := BeginInitPiece(SelfSlot, Idx);
        EmitNewObject(FieldCls);
        EndInitPiece(Over);
      end;
    end;
    Advance; // END
    Advance; // TYPE
  finally
    EndClassInit(SelfSlot);
  end;
end;

//----------------------------------------------------------------------
// Names, array elements, object members and calls
//----------------------------------------------------------------------

// The field index of Name in ME, inside a method (unless a local hides it),
// or -1.
function TParser.MeFieldIndex(const Name: string): Integer;
var
  Cur: TClassInfo;
begin
  Result := -1;
  Cur := CurrentClass;
  if (Cur <> nil) and (FLocals.IndexOf(UpperCase(Name)) < 0) then
    Result := Cur.Fields.IndexOf(UpperCase(Name));
end;

// Pushes the variable Name (or ME's field Name, in a method).
procedure TParser.LoadName(const Name: string);
var
  Idx: Integer;
begin
  Idx := MeFieldIndex(Name);
  if Idx < 0 then
    FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(Name)])
  else
  begin
    FAssembler.Emit(BC_LOAD_VAR, [Op_Variable('ME')]);
    EmitInt(Idx);
    FAssembler.Emit(BC_INDEX_GET, []);
  end;
end;

// Pops a value into the variable Name (or ME's field Name, in a method).
procedure TParser.StoreToName(const Name: string);
var
  Idx, T: Integer;
begin
  Idx := MeFieldIndex(Name);
  if Idx < 0 then
    FAssembler.Emit(BC_STORE_VAR, [Op_Variable(Name)])
  else
  begin
    T := HiddenVar('value');
    FAssembler.Emit(BC_STORE_VAR, [T]);
    FAssembler.Emit(BC_LOAD_VAR, [Op_Variable('ME')]);
    EmitInt(Idx);
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    FAssembler.Emit(BC_INDEX_SET, []);
  end;
end;

// A name and what follows it - "(args)" (array index, or call) and
// ".member" - in an expression (IsStatement False: leaves the value on the
// stack) or as a statement (an assignment "... = value", or a call):
//   x   a(i)   m(i, j)   p.X   obj.Items(2).Name   Split(s, ",")(0)
//   obj.Method(1)   obj.Method 1, 2   Greet "Ada"   Me.Count
procedure TParser.ChainExpr(IsStatement: Boolean);
var
  P: TPlace;
begin
  if IsStatement then
    ChainParse(cmStatement, P)
  else
    ChainParse(cmExpr, P);
end;

// Something to store into - a variable, a(i), obj.field - for INPUT, READ,
// SWAP, FOR EACH ...: its code so far (an element's array and index) is
// emitted; store with BeginStore, the value, EndStore.
function TParser.ParseTarget: TPlace;
begin
  if not (Check(tkIdentifier) or Check(tkDot)) then
    Error('Expected a variable');
  ChainParse(cmTarget, Result);
end;

// See ChainExpr. What a name is, in order: a local; inside a method, a
// field or method of ME (MYBASE: of the base class); NOTHING (0); TYPEOF;
// a SUB / FUNCTION; a built-in function; a STRUCT variable's field; a call
// if it's no known variable; else a variable. Members are looked up in
// every CLASS (see FindMembers); ME's in its class and the ones derived
// from it.
procedure TParser.ChainParse(Mode: TChainMode; out Res: TPlace);
var
  P: TPlace;
  Name, U, MName: string;
  Line, MinA, MaxA, Id, Obj, I: Integer;
  IsLocal, IsCatchVar, IsStatement, IsTarget, Exact: Boolean;
  Cur, Static: TClassInfo;

  // After a call compiled as a statement (no value): if the chain goes on
  // (".x", "(i)"), its value is needed after all.
  procedure CallResult(HasValue: Boolean);
  begin
    if HasValue then
      P.Kind := pkValue
    else if Check(tkDot) or Check(tkParenthesisOpen) then
    begin
      FPendingCalls[High(FPendingCalls)].WantValue := True;
      FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable('__ret')]);
      P.Kind := pkValue;
    end
    else
      P.Kind := pkNone;
  end;

  function HasMethod(const Member: string; C: TClassInfo): Boolean;
  var
    K: Integer;
    Sg: string;
  begin
    Result := False;
    for K := 0 to FClasses.Count - 1 do
      if (C = nil) or (FClasses.Objects[K] = C) then
      begin
        Sg := TClassInfo(FClasses.Objects[K]).Sig(Member);
        if (Sg <> '') and (Sg[1] in ['S', 'F']) then
          Exit(True);
      end;
  end;

  procedure SetVar(Slot: Integer);
  begin
    P.Kind := pkVar;
    P.Slot := Slot;
  end;

  function QBNoArgBuiltin(const W: string): Integer;
  begin
    Result := 0;
    if not FQB then
      Exit;
    case W of
      'TIMER': Result := BI_TIMER;
      'DATE$': Result := BI_DATE;
      'TIME$': Result := BI_TIME;
      'INKEY$': Result := BI_INKEY;
      'FREEFILE': Result := BI_FREEFILE;
    end;
  end;

begin
  IsStatement := Mode = cmStatement;
  IsTarget := Mode = cmTarget;
  P.Kind := pkNone;
  P.Slot := -1;
  P.Index := -1;
  P.Cls := nil;
  P.Exact := False;
  Line := FCurrentToken.Line;
  Cur := CurrentClass;
  if Check(tkDot) then
  begin
    if Length(FWithSlots) = 0 then
      Error('".member" is only allowed inside a WITH block');
    SetVar(FWithSlots[High(FWithSlots)]);
  end
  else
  begin
    Name := FCurrentToken.Lexeme;
    U := UpperCase(Name);
    Advance;
    IsLocal := (FProc <> '') and (FLocals.IndexOf(U) >= 0);
    // A FUNCTION's own name inside it is its result - except in a call.
    if IsLocal and FProcIsFunction and (U = FLocals[0]) and Check(tkParenthesisOpen) then
      IsLocal := False;
    if IsLocal then
    begin
      SetVar(Op_Variable(Name));
      if U = 'ME' then
        P.Cls := Cur;
    end
    else if (U = 'MYBASE') and (Cur <> nil) then
    begin
      // The base class's own members, on ME.
      if Cur.Base = nil then
        Error('MYBASE: CLASS ' + Cur.Name + ' doesn''t inherit from another class');
      if not Check(tkDot) then
        Error('Expected MYBASE.member');
      SetVar(Op_Variable('ME'));
      P.Cls := Cur.Base;
      P.Exact := True;
    end
    else if (Cur <> nil) and (Cur.Fields.IndexOf(U) >= 0) then
    begin
      P.Kind := pkMeField;
      P.Index := Cur.Fields.IndexOf(U);
    end
    else if (Cur <> nil) and HasMethod(U, Cur) and not IsTarget and not (IsStatement and AtAssign) then
      MethodCall(Op_Variable('ME'), Cur, False, Name, Line, IsStatement, P)
    else if (U = 'NOTHING') and (Mode = cmExpr) then
    begin
      EmitInt(0);
      P.Kind := pkValue;
    end
    else if (U = 'TYPEOF') and (Mode = cmExpr) and not IsKnownVar(Name) then
    begin
      TypeOfExpr;
      P.Kind := pkValue;
    end
    else if (FProcNames.IndexOf(U) >= 0) and not IsTarget and not (IsStatement and AtAssign) then
    begin
      CallExpression(Name, Line, not IsStatement, IsStatement);
      CallResult(not IsStatement);
    end
    else if not IsTarget and Check(tkParenthesisOpen) and (FindBuiltin(Name, FQB, MinA, MaxA) > 0) then
    begin
      Id := FindBuiltin(Name, FQB, MinA, MaxA);
      if IsStatement then
        Error(Name + ' is a built-in function: use its value (x = ' + Name + '(...))');
      BuiltinCall(Id, MinA, MaxA, Name);
      P.Kind := pkValue;
    end
    else if FQB and ((U = 'ERR') or (U = 'ERL')) and (Mode = cmExpr) and not IsLocal then
    begin
      // ON ERROR: the error number / the line number reached
      if U = 'ERR' then
        FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable('__err')])
      else
        FAssembler.Emit(BC_LOAD_VAR, [FAssembler.CurrentProgram.AddVariable('__erl')]);
      P.Kind := pkValue;
    end
    else if (Mode = cmExpr) and (QBNoArgBuiltin(U) > 0) and not IsKnownVar(Name) then
    begin
      FAssembler.Emit(BC_BUILTIN, [QBNoArgBuiltin(U), 0]);
      P.Kind := pkValue;
    end
    else if (U = 'RND') and (Mode = cmExpr) and not Check(tkParenthesisOpen) and not IsKnownVar(Name) then
    begin
      FAssembler.Emit(BC_BUILTIN, [BI_RND, 0]); // RND: a fraction 0 <= x < 1
      P.Kind := pkValue;
    end
    else if (FStructVars.IndexOf(U) >= 0) and Check(tkDot) then
    begin
      // A STRUCT variable's field is a variable of its own: "P.X".
      Advance; // .
      if not Check(tkIdentifier) then
        Error('Expected field name after "."');
      SetVar(Op_Variable(Name + '.' + FCurrentToken.Lexeme));
      Advance;
    end
    else if not IsTarget and not IsKnownVar(Name) and
            (Check(tkParenthesisOpen) or (IsStatement and not AtAssign and not Check(tkDot))) then
    begin
      // Not a variable, so a call - of a procedure that doesn't exist, which
      // ResolveCalls reports.
      CallExpression(Name, Line, not IsStatement, IsStatement);
      CallResult(not IsStatement);
    end
    else
      SetVar(Op_Variable(Name));
  end;

  while Check(tkDot) or Check(tkParenthesisOpen) do
  begin
    if Check(tkParenthesisOpen) then
    begin
      // Array element: a(i), m(i, j) (= m(i)(j))
      Materialize(P);
      Advance; // (
      if Check(tkParenthesisClose) then
      begin
        Advance; // a(): the whole array, as in CALL Sort(a())
        Continue;
      end;
      repeat
        Expression;
        if not Check(tkComma) then
          Break;
        FAssembler.Emit(BC_INDEX_GET, []);
        Advance;
      until False;
      Match(tkParenthesisClose);
      P.Kind := pkIndex;
      P.Cls := nil;
      Continue;
    end;

    Advance; // .
    if not (Check(tkIdentifier) or Check(tkKeyword)) then
      Error('Expected a member name after "."');
    MName := FCurrentToken.Lexeme;
    Advance;
    // e.Message, for a CATCH variable e: the message itself.
    IsCatchVar := False;
    if P.Kind = pkVar then
      for I := 0 to High(FCatchSlots) do
        if FCatchSlots[I] = P.Slot then
          IsCatchVar := True;
    if IsCatchVar and SameText(MName, 'MESSAGE') then
      Continue;

    Static := P.Cls;
    Exact := P.Exact;
    if P.Kind = pkVar then
      Obj := P.Slot
    else
    begin
      Materialize(P);
      Obj := HiddenVar('obj');
      FAssembler.Emit(BC_STORE_VAR, [Obj]);
    end;
    if (not IsTarget or Check(tkParenthesisOpen)) and
       ((Check(tkParenthesisOpen) and HasMethod(UpperCase(MName), Static)) or
        (IsStatement and not Check(tkParenthesisOpen) and not AtAssign and not Check(tkDot))) then
      MethodCall(Obj, Static, Exact, MName, Line, IsStatement, P)
    else
    begin
      // A field or property; obj.list(i) indexes the field's array next.
      P.Kind := pkMember;
      P.Slot := Obj;
      P.Member := UpperCase(MName);
      P.MemberName := MName;
      P.Cls := Static;
      P.Exact := Exact;
    end;
  end;

  Res := P;
  case Mode of
    cmStatement:
      if AtAssign then
      begin
        Advance; // =
        AssignTo(P);
      end
      else if P.Kind <> pkNone then
        Error('Expected "=" (an assignment) or a SUB call');
    cmExpr:
      Materialize(P);
    cmTarget:
      if P.Kind in [pkNone, pkValue] then
        Error('Expected a variable, array element or field to store into');
  end;
end;

// TYPEOF x IS ClassName: 1 if x is an object of that class or of one
// derived from it, else 0.
procedure TParser.TypeOfExpr;
var
  T, I, Count: Integer;
  Target, C: TClassInfo;
begin
  Unary;
  if not (Check(tkOperator) and SameText(FCurrentToken.Lexeme, 'IS')) then
    Error('Expected TYPEOF value IS ClassName');
  Advance; // IS
  if not Check(tkIdentifier) then
    Error('Expected a class name after TYPEOF ... IS');
  Target := ClassOf(FCurrentToken.Lexeme);
  if Target = nil then
    Error('Unknown CLASS "' + FCurrentToken.Lexeme + '"');
  Advance;
  T := HiddenVar('typeof');
  FAssembler.Emit(BC_STORE_VAR, [T]);
  EmitInt(0);
  Count := 0;
  for I := 0 to FClasses.Count - 1 do
  begin
    C := TClassInfo(FClasses.Objects[I]);
    if not C.IsA(Target) then
      Continue;
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    FAssembler.Emit(BC_BUILTIN, [BI_TYPENAME, 1]);
    EmitStr(C.Name);
    FAssembler.Emit(BC_CMP_EQ, []);
    FAssembler.Emit(BC_ADD, []);
    Inc(Count);
  end;
  EmitInt(0);
  FAssembler.Emit(BC_CMP_GT, []);
end;

// Puts P's value on the stack.
procedure TParser.Materialize(var P: TPlace);
begin
  LoadPlace(P);
  P.Kind := pkValue;
  P.Cls := nil;
  P.Exact := False;
end;

// Pushes the value at P, which stays usable (for a pkIndex place, only
// once: its array and index are consumed - see Stabilize).
procedure TParser.LoadPlace(const P: TPlace);
var
  Static: Boolean;
  Acts: TMemberActions;
begin
  case P.Kind of
    pkNone:
      Error('A SUB has no value (make it a FUNCTION to use its result)');
    pkVar:
      FAssembler.Emit(BC_LOAD_VAR, [P.Slot]);
    pkMeField:
      begin
        FAssembler.Emit(BC_LOAD_VAR, [Op_Variable('ME')]);
        EmitInt(P.Index);
        FAssembler.Emit(BC_INDEX_GET, []);
      end;
    pkIndex:
      FAssembler.Emit(BC_INDEX_GET, []);
    pkIndexVars:
      begin
        FAssembler.Emit(BC_LOAD_VAR, [P.Slot]);
        FAssembler.Emit(BC_LOAD_VAR, [P.Index]);
        FAssembler.Emit(BC_INDEX_GET, []);
      end;
    pkMember:
      begin
        Acts := FindMembers(P.MemberName, P.Cls, P.Exact, mkGet, 0, True, Static);
        EmitDispatch(P.Slot, Acts, Static, P.MemberName, -1, 0, FCurrentToken.Line, True, nil);
      end;
  end;
end;

// An array element's array and index go into variables, so the element
// can be read and written (SWAP).
procedure TParser.Stabilize(var P: TPlace);
begin
  if P.Kind <> pkIndex then
    Exit;
  P.Index := HiddenVar('index');
  FAssembler.Emit(BC_STORE_VAR, [P.Index]);
  P.Slot := HiddenVar('array');
  FAssembler.Emit(BC_STORE_VAR, [P.Slot]);
  P.Kind := pkIndexVars;
end;

// Storing into P: BeginStore, then code that pushes the value, then EndStore.
procedure TParser.BeginStore(var P: TPlace);
begin
  case P.Kind of
    pkMeField:
      begin
        FAssembler.Emit(BC_LOAD_VAR, [Op_Variable('ME')]);
        EmitInt(P.Index);
      end;
    pkIndexVars:
      begin
        FAssembler.Emit(BC_LOAD_VAR, [P.Slot]);
        FAssembler.Emit(BC_LOAD_VAR, [P.Index]);
      end;
    pkVar, pkIndex, pkMember: ;
  else
    Error('Can''t assign to the result of a call');
  end;
end;

procedure TParser.EndStore(var P: TPlace);
var
  Acts: TMemberActions;
  V: Integer;
  Static: Boolean;
begin
  case P.Kind of
    pkVar:
      FAssembler.Emit(BC_ASSIGN, [P.Slot]);
    pkMeField, pkIndex, pkIndexVars:
      FAssembler.Emit(BC_INDEX_SET, []);
    pkMember:
      begin
        Acts := FindMembers(P.MemberName, P.Cls, P.Exact, mkSet, 0, False, Static);
        V := HiddenVar('value');
        FAssembler.Emit(BC_STORE_VAR, [V]);
        EmitDispatch(P.Slot, Acts, Static, P.MemberName, V, 0, FCurrentToken.Line, False, nil);
      end;
  end;
end;

// "... = value" into P (the "=" is consumed).
procedure TParser.AssignTo(var P: TPlace);
var
  Static: Boolean;
begin
  if P.Kind = pkMember then
    FindMembers(P.MemberName, P.Cls, P.Exact, mkSet, 0, False, Static); // errors before the value
  BeginStore(P);
  Expression;
  EndStore(P);
end;

// obj.Method(args) / obj.Method args (as a statement), for the object in
// variable ObjSlot: the arguments, then the object (the method's hidden
// last parameter ME), then the call.
procedure TParser.MethodCall(ObjSlot: Integer; Static: TClassInfo; Exact: Boolean; const MName: string;
  Line: Integer; IsStatement: Boolean; var P: TPlace);
var
  CopyBacks: TCopyBacks;
  ArgCount: Integer;
  WantValue, NoCheck: Boolean;
  Acts: TMemberActions;
begin
  ArgCount := ParseArgs(IsStatement, CopyBacks);
  WantValue := not IsStatement or Check(tkDot) or Check(tkParenthesisOpen);
  Acts := FindMembers(MName, Static, Exact, mkCall, ArgCount, WantValue, NoCheck);
  EmitDispatch(ObjSlot, Acts, NoCheck, MName, -1, ArgCount, Line, WantValue, CopyBacks);
  if WantValue then
    P.Kind := pkValue
  else
    P.Kind := pkNone;
  P.Cls := nil;
  P.Exact := False;
end;

// What each CLASS does for member MName: read it (a field, a PROPERTY GET
// or a FUNCTION without parameters), write it (a field or a PROPERTY LET /
// SET), or call it with ArgCount arguments. With Static, only that class
// and the ones derived from it (Exact: only that class). None at all is a
// compile error. NoCheck: one class only, so no run-time test is needed.
function TParser.FindMembers(const MName: string; Static: TClassInfo; Exact: Boolean; Kind: TMemberKind;
  ArgCount: Integer; WantValue: Boolean; out NoCheck: Boolean): TMemberActions;
var
  I, Idx, Included: Integer;
  C: TClassInfo;
  MU, Sg, Why, What: string;

  procedure Add(IsField: Boolean; Index: Integer; const Key: string);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Cls := C;
    Result[High(Result)].IsField := IsField;
    Result[High(Result)].Index := Index;
    Result[High(Result)].Key := Key;
  end;

begin
  Result := nil;
  Why := '';
  Included := 0;
  MU := UpperCase(MName);
  for I := 0 to FClasses.Count - 1 do
  begin
    C := TClassInfo(FClasses.Objects[I]);
    if Static <> nil then
      if (Exact and (C <> Static)) or not C.IsA(Static) then
        Continue;
    Inc(Included);
    Idx := C.Fields.IndexOf(MU);
    case Kind of
      mkGet:
        if Idx >= 0 then
          Add(True, Idx, '')
        else if C.Sig(MU) = 'F0' then
          Add(False, -1, C.KeyOf(MU));
      mkSet:
        if Idx >= 0 then
          Add(True, Idx, '')
        else if C.Sig(MU + '$LET') = 'L1' then
          Add(False, -1, C.KeyOf(MU + '$LET'));
      mkCall:
        begin
          Sg := C.Sig(MU);
          if (Sg = '') or not (Sg[1] in ['S', 'F']) then
            Continue;
          if StrToIntDef(Copy(Sg, 2, 9), -1) <> ArgCount then
            Why := Format('%s.%s takes %s argument(s), but this call passes %d', [C.Name, MName, Copy(Sg, 2, 9), ArgCount])
          else if WantValue and (Sg[1] = 'S') then
            Why := Format('%s.%s is a SUB, so it has no value - make it a FUNCTION', [C.Name, MName])
          else
            Add(False, -1, C.KeyOf(MU));
        end;
    end;
  end;
  NoCheck := (Static <> nil) and (Included = 1) and (Length(Result) = 1);
  if Result <> nil then
    Exit;
  if Why <> '' then
    Error(Why);
  case Kind of
    mkGet: What := 'a field, PROPERTY GET or FUNCTION without parameters';
    mkSet: What := 'a field or PROPERTY LET';
  else
    What := 'a SUB or FUNCTION';
  end;
  if Static <> nil then
    Error(Format('CLASS %s has no %s named "%s"', [Static.Name, What, MName]))
  else
    Error(Format('No CLASS has %s named "%s"', [What, MName]));
end;

// The code for a member access on the object in variable ObjSlot. Unless
// its class is known (Static: ME), the object's class is tested at run
// time against each CLASS that has the member - so any object with it
// works - and an object with none of them is a runtime error. ValueSlot
// >= 0: write that value; else read (WantValue) or call. A call's
// arguments are already on the stack.
procedure TParser.EmitDispatch(ObjSlot: Integer; const Acts: TMemberActions; Static: Boolean;
  const MName: string; ValueSlot, ArgCount, Line: Integer; WantValue: Boolean; const CopyBacks: TCopyBacks);
var
  I, Next: Integer;
  EndJumps: TIntegerArray;
begin
  EndJumps := nil;
  for I := 0 to High(Acts) do
  begin
    Next := -1;
    if not Static then
    begin
      FAssembler.Emit(BC_LOAD_VAR, [ObjSlot]);
      FAssembler.Emit(BC_BUILTIN, [BI_TYPENAME, 1]);
      EmitStr(Acts[I].Cls.Name);
      FAssembler.Emit(BC_CMP_EQ, []);
      Next := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    end;
    if Acts[I].IsField then
    begin
      FAssembler.Emit(BC_LOAD_VAR, [ObjSlot]);
      EmitInt(Acts[I].Index);
      if ValueSlot >= 0 then
      begin
        FAssembler.Emit(BC_LOAD_VAR, [ValueSlot]);
        FAssembler.Emit(BC_INDEX_SET, []);
      end
      else
        FAssembler.Emit(BC_INDEX_GET, []);
    end
    else
    begin
      if ValueSlot >= 0 then
        FAssembler.Emit(BC_LOAD_VAR, [ValueSlot]);
      FAssembler.Emit(BC_LOAD_VAR, [ObjSlot]); // ME
      EmitCall(Acts[I].Key, ArgCount + Ord(ValueSlot >= 0) + 1, Line, WantValue, CopyBacks);
    end;
    if not Static then
    begin
      SetLength(EndJumps, Length(EndJumps) + 1);
      EndJumps[High(EndJumps)] := FAssembler.CurrentInstructionIndex;
      FAssembler.Emit(BC_JUMP, [-1]);
      FAssembler.PatchJumpTarget(Next, FAssembler.CurrentInstructionIndex);
    end;
  end;
  if not Static then
  begin
    FAssembler.Emit(BC_LOAD_VAR, [ObjSlot]);
    FAssembler.Emit(BC_BUILTIN, [BI_TYPENAME, 1]);
    EmitStr(' has no member ''' + MName + '''');
    FAssembler.Emit(BC_CONCAT, []);
    FAssembler.Emit(BC_THROW, []);
    for I := 0 to High(EndJumps) do
      FAssembler.PatchJumpTarget(EndJumps[I], FAssembler.CurrentInstructionIndex);
  end;
end;

// Name(args) for a built-in function.
procedure TParser.BuiltinCall(Id, MinArgs, MaxArgs: Integer; const Name: string);
var
  Count: Integer;
  Range, Layout, VarName: string;
  Bytes: Int64;
begin
  Match(tkParenthesisOpen);
  // QuickBASIC: LEN of a TYPE record or a typed number is its size in bytes
  // (LEN(rec) for OPEN ... LEN = ...).
  if FQB and (Id = BI_LEN) and Check(tkIdentifier) and (PeekToken.TokenType = tkParenthesisClose) then
  begin
    VarName := FCurrentToken.Lexeme;
    if (FVarTypes.IndexOfName(UpperCase(VarName)) >= 0) or (VarName[Length(VarName)] in ['%', '&', '!', '#']) then
    begin
      Layout := LayoutFor(FVarTypes.Values[UpperCase(VarName)], VarName, 0);
      Bytes := LayoutBytes(Layout);
      if Bytes >= 0 then
      begin
        Advance; // the name
        Match(tkParenthesisClose);
        EmitInt(Bytes);
        Exit;
      end;
    end;
  end;
  Count := 0;
  if not Check(tkParenthesisClose) then
    repeat
      if AtHash then
        Advance; // a file number: INPUT$(5, #1)
      Expression;
      Inc(Count);
      if not Check(tkComma) then
        Break;
      Advance;
    until False;
  Match(tkParenthesisClose);
  if (Count < MinArgs) or ((MaxArgs >= 0) and (Count > MaxArgs)) then
  begin
    if MaxArgs = MinArgs then
      Range := IntToStr(MinArgs)
    else if MaxArgs < 0 then
      Range := 'at least ' + IntToStr(MinArgs)
    else
      Range := Format('%d or %d', [MinArgs, MaxArgs]);
    Error(Format('%s takes %s argument(s), not %d', [UpperCase(Name), Range, Count]));
  end;
  FAssembler.Emit(BC_BUILTIN, [Id, Count]);
end;

// NEW ClassName[(args)]: a new object (fields 0, then their initial
// values), passed to SUB New(args) if the class has one.
// NEW Exception(message) - as in THROW NEW Exception("...") - is just the
// message.
procedure TParser.NewExpression;
var
  Cls: TClassInfo;
  Name, Ctor, Sg: string;
  Line, Tmp, ArgCount, Need: Integer;
  CopyBacks: TCopyBacks;
begin
  Advance; // NEW
  if not Check(tkIdentifier) then
    Error('Expected class name after NEW');
  Name := FCurrentToken.Lexeme;
  Line := FCurrentToken.Line;
  Advance;
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
  Cls := ClassOf(Name);
  if Cls = nil then
  begin
    if SameText(Name, 'Exception') then
    begin
      if Check(tkParenthesisOpen) then
      begin
        Advance;
        if Check(tkParenthesisClose) then
          EmitStr('Exception')
        else
          Expression;
        Match(tkParenthesisClose);
      end
      else
        EmitStr('Exception');
      Exit;
    end;
    if FStructs.IndexOf(Name) >= 0 then
      Error('"' + Name + '" is a STRUCT, not a CLASS: DIM a variable AS ' + Name + ' instead');
    Error('Unknown CLASS "' + Name + '"');
  end;

  EmitNewObject(Cls);

  Ctor := 'NEW';
  Sg := Cls.Sig(Ctor);
  if Sg = '' then
  begin
    Ctor := 'CLASS_INITIALIZE'; // VB6's constructor
    Sg := Cls.Sig(Ctor);
  end;
  if Sg = '' then
  begin
    if Check(tkParenthesisOpen) then
    begin
      Advance;
      if not Check(tkParenthesisClose) then
        Error(Format('CLASS %s has no SUB New, so NEW %s takes no arguments', [Cls.Name, Cls.Name]));
      Advance;
    end;
    Exit;
  end;
  if Sg[1] <> 'S' then
    Error(Format('%s.%s (the constructor) must be a SUB', [Cls.Name, Ctor]));
  Tmp := HiddenVar('new');
  FAssembler.Emit(BC_STORE_VAR, [Tmp]);
  ArgCount := ParseArgs(False, CopyBacks);
  Need := StrToIntDef(Copy(Sg, 2, 9), 0);
  if ArgCount <> Need then
    Error(Format('NEW %s takes %d argument(s) (for its SUB %s), not %d', [Cls.Name, Need, Ctor, ArgCount]));
  FAssembler.Emit(BC_LOAD_VAR, [Tmp]);
  EmitCall(Cls.KeyOf(Ctor), ArgCount + 1, Line, False, CopyBacks); // maybe the base class's
  FAssembler.Emit(BC_LOAD_VAR, [Tmp]);
end;

// A new object of class Cls with its fields' initial values (no
// constructor call), on the stack.
procedure TParser.EmitNewObject(Cls: TClassInfo);
begin
  EmitStr(Cls.Name);
  EmitInt(Cls.Fields.Count);
  FAssembler.Emit(BC_BUILTIN, [BI_NEWOBJECT, 2]);
  EmitCall(UpperCase(Cls.Name) + '.__INIT', 1, FCurrentToken.Line, False, nil); // leaves the object on the stack
end;

//----------------------------------------------------------------------
// QuickBASIC compatibility (kayte --qbs)
//----------------------------------------------------------------------

// Code that runs before the program: global name$ variables start as "",
// and the DATA items go into the array READ takes them from. It goes at
// the end; instruction 0 jumps to it and it jumps back to 1.
procedure TParser.EmitStartup;
var
  I, Start, Data, Idx: Integer;
  Vars: TStringList;
  Item, Key: string;

  procedure Begin1;
  begin
    if Start >= 0 then
      Exit;
    FAssembler.Emit(BC_HALT, []); // the end of the program
    Start := FAssembler.CurrentInstructionIndex;
  end;

begin
  Start := -1;
  Vars := FAssembler.CurrentProgram.Variables;
  for I := 0 to Vars.Count - 1 do
    if (Vars[I] <> '') and (Vars[I][Length(Vars[I])] = '$') and (Pos(':', Vars[I]) = 0) and
       (Pos('$static$', Vars[I]) = 0) and (Copy(Vars[I], 1, 2) <> '__') then
    begin
      Begin1;
      EmitStr('');
      FAssembler.Emit(BC_STORE_VAR, [I]);
    end;
  if FUsesData then
  begin
    Begin1;
    Data := FAssembler.CurrentProgram.AddVariable('__data');
    EmitInt(FDataItems.Count - 1);
    FAssembler.Emit(BC_BUILTIN, [BI_NEWARRAY, 1]);
    FAssembler.Emit(BC_STORE_VAR, [Data]);
    for I := 0 to FDataItems.Count - 1 do
    begin
      Item := FDataItems[I];
      FAssembler.Emit(BC_LOAD_VAR, [Data]);
      EmitInt(I);
      if Item[1] = 'I' then
        EmitInt(StrToInt64(Copy(Item, 2, MaxInt)))
      else if Item[1] = 'N' then
        EmitNumber(Copy(Item, 2, MaxInt))
      else
        EmitStr(Copy(Item, 2, MaxInt));
      FAssembler.Emit(BC_INDEX_SET, []);
    end;
    EmitInt(0);
    FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable('__dataptr')]);
  end;
  for I := 0 to High(FRestores) do
  begin
    Key := FRestores[I].SubKey;
    Idx := FDataLabels.IndexOf(Key);
    if Idx < 0 then
      ReportError(Format('RESTORE at line %d: there is no label "%s" at the top level',
        [FRestores[I].Line + 1, Copy(Key, 2, MaxInt)]))
    else
      FAssembler.ReplaceInstruction(FRestores[I].InstrIndex, BC_LOAD_INT,
        Op_IntegerLiteral(FDataLabels.Data[Idx]));
  end;
  if Start < 0 then
    Exit;
  FAssembler.Emit(BC_JUMP, [1]);
  FAssembler.ReplaceInstruction(0, BC_JUMP, Start);
end;

// Skips the rest of the statement (up to ":" or the end of the line).
procedure TParser.SkipToLineEnd;
begin
  while not AtStatementEnd do
    Advance;
end;

// CONST name = value [, name = value] - a variable set once. QuickBASIC:
// CONSTs at the top level are seen in SUBs too.
procedure TParser.ConstStatement;
var
  Name: string;
begin
  Advance; // CONST
  repeat
    if not Check(tkIdentifier) then
      Error('Expected a name after CONST');
    Name := FCurrentToken.Lexeme;
    Advance;
    if FProc = '' then
      FSharedNames.Add(UpperCase(Name))
    else
      DeclareLocal(Name);
    if not AtAssign then
      Error('Expected CONST ' + Name + ' = value');
    Advance;
    Expression;
    FAssembler.Emit(BC_STORE_VAR, [Op_Variable(Name)]);
    if not Check(tkComma) then
      Break;
    Advance;
  until False;
end;

// Sets every element of the Dims-dimensional array in variable ArrSlot to
// "" (Cls = nil) or to a new object of class Cls.
procedure TParser.FillArray(ArrSlot, Dims: Integer; Cls: TClassInfo);

  procedure Fill(Arr, Level: Integer);
  var
    I, Top, ToExit, Sub: Integer;
  begin
    I := HiddenVar('fill');
    EmitInt(0);
    FAssembler.Emit(BC_STORE_VAR, [I]);
    Top := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_LOAD_VAR, [I]);
    FAssembler.Emit(BC_LOAD_VAR, [Arr]);
    FAssembler.Emit(BC_BUILTIN, [BI_LEN, 1]);
    FAssembler.Emit(BC_CMP_LT, []);
    ToExit := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    if Level = Dims then
    begin
      FAssembler.Emit(BC_LOAD_VAR, [Arr]);
      FAssembler.Emit(BC_LOAD_VAR, [I]);
      if Cls = nil then
        EmitStr('')
      else
        EmitNewObject(Cls);
      FAssembler.Emit(BC_INDEX_SET, []);
    end
    else
    begin
      Sub := HiddenVar('fill');
      FAssembler.Emit(BC_LOAD_VAR, [Arr]);
      FAssembler.Emit(BC_LOAD_VAR, [I]);
      FAssembler.Emit(BC_INDEX_GET, []);
      FAssembler.Emit(BC_STORE_VAR, [Sub]);
      Fill(Sub, Level + 1);
    end;
    FAssembler.Emit(BC_LOAD_VAR, [I]);
    EmitInt(1);
    FAssembler.Emit(BC_ADD, []);
    FAssembler.Emit(BC_STORE_VAR, [I]);
    FAssembler.Emit(BC_JUMP, [Top]);
    FAssembler.PatchJumpTarget(ToExit, FAssembler.CurrentInstructionIndex);
  end;

begin
  Fill(ArrSlot, 1);
end;

// Prints CHR(27) & Seq: an ANSI terminal control sequence (CLS, COLOR ...).
procedure TParser.EmitEsc(const Seq: string);
begin
  EmitInt(27);
  FAssembler.Emit(BC_BUILTIN, [BI_CHR, 1]);
  EmitStr(Seq);
  FAssembler.Emit(BC_CONCAT, []);
  FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
end;

// LOCATE [row] [, col] - moves the cursor (an ANSI sequence; 1-based).
procedure TParser.LocateStatement;
var
  HasRow: Boolean;
begin
  Advance; // LOCATE
  EmitInt(27);
  FAssembler.Emit(BC_BUILTIN, [BI_CHR, 1]);
  EmitStr('[');
  FAssembler.Emit(BC_CONCAT, []);
  HasRow := not (Check(tkComma) or AtStatementEnd);
  if HasRow then
  begin
    Expression;
    FAssembler.Emit(BC_CONCAT, []);
  end;
  if Check(tkComma) then
  begin
    Advance;
    if HasRow then
    begin
      EmitStr(';');
      FAssembler.Emit(BC_CONCAT, []);
    end;
    Expression;
    FAssembler.Emit(BC_CONCAT, []);
    if HasRow then
      EmitStr('H')
    else
      EmitStr('G'); // the column only
  end
  else
    EmitStr(';1H');
  FAssembler.Emit(BC_CONCAT, []);
  FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
  SkipToLineEnd; // cursor shape arguments
end;

// COLOR [foreground] [, background] - QuickBASIC's 16 colors as ANSI.
procedure TParser.ColorStatement;

  // ANSI code for the QuickBASIC color on the stack: Base + its RGB order,
  // + 60 for the bright ones (8-15).
  procedure Code(Base: Integer; Bright: Boolean);
  var
    T: Integer;
  begin
    T := HiddenVar('color');
    FAssembler.Emit(BC_STORE_VAR, [T]);
    EmitInt(0); EmitInt(4); EmitInt(2); EmitInt(6); EmitInt(1); EmitInt(5); EmitInt(3); EmitInt(7);
    FAssembler.Emit(BC_BUILTIN, [BI_ARRAY, 8]);
    // index: color MOD 8
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    EmitInt(8);
    FAssembler.Emit(BC_IDIV, []);
    EmitInt(8);
    FAssembler.Emit(BC_MUL, []);
    FAssembler.Emit(BC_SUB, []);
    FAssembler.Emit(BC_INDEX_GET, []);
    EmitInt(Base);
    FAssembler.Emit(BC_ADD, []);
    if Bright then
    begin
      // + 60 if color MOD 16 >= 8
      FAssembler.Emit(BC_LOAD_VAR, [T]);
      EmitInt(16);
      FAssembler.Emit(BC_IDIV, []);
      EmitInt(16);
      FAssembler.Emit(BC_MUL, []);
      FAssembler.Emit(BC_NEG, []);
      FAssembler.Emit(BC_LOAD_VAR, [T]);
      FAssembler.Emit(BC_ADD, []);
      EmitInt(7);
      FAssembler.Emit(BC_CMP_GT, []);
      EmitInt(60);
      FAssembler.Emit(BC_MUL, []);
      FAssembler.Emit(BC_ADD, []);
    end;
  end;

  procedure Sequence(Base: Integer; Bright: Boolean);
  begin
    EmitInt(27);
    FAssembler.Emit(BC_BUILTIN, [BI_CHR, 1]);
    EmitStr('[');
    FAssembler.Emit(BC_CONCAT, []);
    Expression;
    Code(Base, Bright);
    FAssembler.Emit(BC_CONCAT, []);
    EmitStr('m');
    FAssembler.Emit(BC_CONCAT, []);
    FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
  end;

begin
  Advance; // COLOR
  if not (Check(tkComma) or AtStatementEnd) then
    Sequence(30, True);
  if Check(tkComma) then
  begin
    Advance;
    if not (Check(tkComma) or AtStatementEnd) then
      Sequence(40, False);
  end;
  SkipToLineEnd; // border
end;

// DATA items - collected at compile time, in program order, for READ.
// Numbers are numbers; anything else (quoted or not) is a string.
procedure TParser.DataStatement;
var
  Raw, Item: string;
  I: Integer;
  InQuotes: Boolean;
  V: Int64;

  procedure AddItem;
  var
    T: string;
  begin
    T := Trim(Item);
    if (Length(T) >= 2) and (T[1] = '"') and (T[Length(T)] = '"') then
      FDataItems.Add('S' + Copy(T, 2, Length(T) - 2))
    else if TryStrToInt64(T, V) and (T <> '') and (T[1] in ['0'..'9', '-', '+']) then
      FDataItems.Add('I' + IntToStr(V))
    else if (T <> '') and (T[1] in ['0'..'9', '-', '+', '.']) and IsFloatText(T) then
      FDataItems.Add('N' + T)
    else
      FDataItems.Add('S' + T);
    Item := '';
  end;

begin
  Advance; // DATA
  Raw := '';
  if Check(tkComment) then
  begin
    Raw := FCurrentToken.Lexeme;
    Advance;
  end;
  if FProc <> '' then
    Error('DATA can only be at the top level');
  Item := '';
  InQuotes := False;
  for I := 1 to Length(Raw) do
    if Raw[I] = '"' then
    begin
      InQuotes := not InQuotes;
      Item := Item + Raw[I];
    end
    else if (Raw[I] = ',') and not InQuotes then
      AddItem
    else
      Item := Item + Raw[I];
  if (Trim(Raw) <> '') or (Pos(',', Raw) > 0) then
    AddItem;
end;

// READ var [, var ...] - the next DATA items.
procedure TParser.ReadStatement;
var
  P: TPlace;
  Data, Ptr, J1: Integer;
begin
  Advance; // READ
  FUsesData := True;
  Data := FAssembler.CurrentProgram.AddVariable('__data');
  Ptr := FAssembler.CurrentProgram.AddVariable('__dataptr');
  repeat
    FAssembler.Emit(BC_LOAD_VAR, [Ptr]);
    FAssembler.Emit(BC_LOAD_VAR, [Data]);
    FAssembler.Emit(BC_BUILTIN, [BI_LEN, 1]);
    FAssembler.Emit(BC_CMP_LT, []);
    J1 := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    P := ParseTarget;
    BeginStore(P);
    FAssembler.Emit(BC_LOAD_VAR, [Data]);
    FAssembler.Emit(BC_LOAD_VAR, [Ptr]);
    FAssembler.Emit(BC_INDEX_GET, []);
    EndStore(P);
    FAssembler.Emit(BC_LOAD_VAR, [Ptr]);
    EmitInt(1);
    FAssembler.Emit(BC_ADD, []);
    FAssembler.Emit(BC_STORE_VAR, [Ptr]);
    // Out of DATA: skip the store; jump over this error to the next item.
    FAssembler.Emit(BC_JUMP, [FAssembler.CurrentInstructionIndex + 3]);
    FAssembler.PatchJumpTarget(J1, FAssembler.CurrentInstructionIndex);
    EmitStr('Out of DATA');
    FAssembler.Emit(BC_THROW, []);
    if not Check(tkComma) then
      Break;
    Advance;
  until False;
end;

// RESTORE [label] - READ starts again from the first DATA (after label).
procedure TParser.RestoreStatement;
var
  Pending: TPendingCall;
begin
  Advance; // RESTORE
  FUsesData := True;
  if AtStatementEnd then
    EmitInt(0)
  else
  begin
    if not (Check(tkIdentifier) or Check(tkIntegerLiteral)) then
      Error('Expected a label after RESTORE');
    Pending.SubKey := '|' + UpperCase(FCurrentToken.Lexeme);
    Pending.Line := FCurrentToken.Line;
    Pending.InstrIndex := FAssembler.CurrentInstructionIndex;
    EmitInt(0); // the label's DATA position, filled in by Parse
    SetLength(FRestores, Length(FRestores) + 1);
    FRestores[High(FRestores)] := Pending;
    Advance;
  end;
  FAssembler.Emit(BC_STORE_VAR, [FAssembler.CurrentProgram.AddVariable('__dataptr')]);
end;

// ON n GOTO label1, label2, ... / ON n GOSUB ... - to the nth label (1 is
// the first); no jump if n is out of range.
procedure TParser.OnStatement;
var
  T, K, Skip: Integer;
  IsGosub: Boolean;
begin
  Advance; // ON
  if Check(tkIdentifier) and SameText(FCurrentToken.Lexeme, 'ERROR') then
  begin
    OnErrorStatement;
    Exit;
  end;
  Expression;
  T := HiddenVar('on');
  FAssembler.Emit(BC_STORE_VAR, [T]);
  IsGosub := AtKeyword('GOSUB');
  if not (IsGosub or AtKeyword('GOTO')) then
    Error('Expected ON ... GOTO or ON ... GOSUB');
  Advance;
  K := 1;
  repeat
    if not (Check(tkIdentifier) or Check(tkIntegerLiteral)) then
      Error('Expected a label');
    FAssembler.Emit(BC_LOAD_VAR, [T]);
    EmitInt(K);
    FAssembler.Emit(BC_CMP_EQ, []);
    Skip := FAssembler.CurrentInstructionIndex;
    FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);
    if IsGosub then
    begin
      EmitGosub(FCurrentToken.Lexeme, FCurrentToken.Line);
      FAssembler.Emit(BC_JUMP, [-1]); // after the GOSUB: past the ON statement
      SetLength(FGosubOnEnds, Length(FGosubOnEnds) + 1);
      FGosubOnEnds[High(FGosubOnEnds)] := FAssembler.CurrentInstructionIndex - 1;
    end
    else
      EmitGoto(FCurrentToken.Lexeme, FCurrentToken.Line);
    FAssembler.PatchJumpTarget(Skip, FAssembler.CurrentInstructionIndex);
    Advance;
    Inc(K);
    if not Check(tkComma) then
      Break;
    Advance;
  until False;
  for K := 0 to High(FGosubOnEnds) do
    FAssembler.PatchJumpTarget(FGosubOnEnds[K], FAssembler.CurrentInstructionIndex);
  FGosubOnEnds := nil;
end;

// SWAP a, b - exchanges two variables, elements or fields.
procedure TParser.SwapStatement;
var
  A, B: TPlace;
  T1, T2: Integer;
begin
  Advance; // SWAP
  A := ParseTarget;
  Stabilize(A);
  Match(tkComma);
  B := ParseTarget;
  Stabilize(B);
  T1 := HiddenVar('swap');
  T2 := HiddenVar('swap');
  LoadPlace(A);
  FAssembler.Emit(BC_STORE_VAR, [T1]);
  LoadPlace(B);
  FAssembler.Emit(BC_STORE_VAR, [T2]);
  BeginStore(A);
  FAssembler.Emit(BC_LOAD_VAR, [T2]);
  EndStore(A);
  BeginStore(B);
  FAssembler.Emit(BC_LOAD_VAR, [T1]);
  EndStore(B);
end;

// ERASE a [, b] - every element back to 0 (same size).
procedure TParser.EraseStatement;
var
  Name: string;
begin
  Advance; // ERASE
  repeat
    if not Check(tkIdentifier) then
      Error('Expected an array name after ERASE');
    Name := FCurrentToken.Lexeme;
    Advance;
    LoadName(Name);
    FAssembler.Emit(BC_BUILTIN, [BI_UBOUND, 1]);
    FAssembler.Emit(BC_BUILTIN, [BI_NEWARRAY, 1]);
    StoreToName(Name);
    if not Check(tkComma) then
      Break;
    Advance;
  until False;
end;

// The QuickBASIC statements Kayte's BASIC doesn't have. True if Word was
// one (and has been compiled).
function TParser.QBStatement(const Word: string): Boolean;
var
  T, V: Integer;
  P: TPlace;

  procedure Names(List: TStringList);
  begin
    Advance; // SHARED / STATIC / COMMON
    if SameText(FCurrentToken.Lexeme, 'SHARED') then
      Advance;
    repeat
      if not Check(tkIdentifier) then
        Error('Expected a variable name');
      List.Add(UpperCase(FCurrentToken.Lexeme));
      Advance;
      if Check(tkParenthesisOpen) then
      begin
        Advance;
        Match(tkParenthesisClose);
      end;
      if AtKeyword('AS') then
      begin
        Advance;
        Advance;
      end;
      if not Check(tkComma) then
        Break;
      Advance;
    until False;
  end;

begin
  Result := True;
  // "COLOR = 5" is an assignment to a variable.
  if Check(tkIdentifier) and (PeekToken.TokenType = tkOperator) and (PeekToken.Lexeme = '=') then
    Exit(False);
  case Word of
    'PRINT':
      QBPrintStatement;
    'CLS':
      begin
        Advance;
        EmitEsc('[2J');
        EmitEsc('[H');
      end;
    'LOCATE':
      LocateStatement;
    'COLOR':
      ColorStatement;
    'BEEP':
      begin
        Advance;
        EmitInt(7);
        FAssembler.Emit(BC_BUILTIN, [BI_CHR, 1]);
        FAssembler.Emit(BC_QPRINT, [QP_TEXT]);
      end;
    'SLEEP':
      begin
        Advance;
        if AtStatementEnd then
          FAssembler.Emit(BC_INPUT, []) // SLEEP: until a key (Enter) is pressed
        else
        begin
          Expression;
          FAssembler.Emit(BC_BUILTIN, [BI_SLEEP, 1]);
        end;
        FAssembler.Emit(BC_POP, []);
      end;
    'RANDOMIZE':
      begin
        // The generator is seeded at start-up already.
        Advance;
        if not AtStatementEnd then
        begin
          Expression;
          FAssembler.Emit(BC_POP, []);
        end;
      end;
    'WRITE':
      WriteStatement;
    'DATA':
      DataStatement;
    'READ':
      ReadStatement;
    'RESTORE':
      RestoreStatement;
    'ON':
      OnStatement;
    'SWAP':
      SwapStatement;
    'RESUME':
      ResumeStatement;
    'ERROR':
      begin
        // ERROR n: raises error number n (its QuickBASIC message)
        Advance;
        Expression;
        FAssembler.Emit(BC_BUILTIN, [BI_ERRMSG, 1]);
        FAssembler.Emit(BC_THROW, []);
      end;
    'LSET', 'RSET':
      begin
        // LSET / RSET a$ = value$: value$ left / right justified in a$'s length
        T := Ord(Word = 'LSET');
        Advance;
        P := ParseTarget;
        Stabilize(P);
        if not AtAssign then
          Error('Expected ' + Word + ' variable = value');
        Advance;
        Expression;
        V := HiddenVar('lset');
        FAssembler.Emit(BC_STORE_VAR, [V]);
        BeginStore(P);
        LoadPlace(P);
        FAssembler.Emit(BC_LOAD_VAR, [V]);
        if T = 1 then
          FAssembler.Emit(BC_BUILTIN, [BI_LSET, 2])
        else
          FAssembler.Emit(BC_BUILTIN, [BI_RSET, 2]);
        EndStore(P);
      end;
    'ERASE':
      EraseStatement;
    'TYPE':
      TypeDefinition;
    'SYSTEM', 'STOP':
      begin
        Advance;
        FAssembler.Emit(BC_HALT, []);
      end;
    'SHARED':
      begin
        if FProc = '' then
          Error('SHARED is only allowed inside a SUB or FUNCTION');
        Names(FProcShared);
      end;
    'STATIC':
      begin
        if FProc = '' then
          Error('STATIC is only allowed inside a SUB or FUNCTION');
        Names(FProcStatic);
      end;
    'COMMON':
      Names(FSharedNames);
    'DECLARE', 'DEFINT', 'DEFLNG', 'DEFSNG', 'DEFDBL', 'DEFSTR', 'WIDTH', 'KEY', 'VIEW':
      SkipToLineEnd; // nothing to do
    'SCREEN':
      begin
        Advance;
        if not (Check(tkIntegerLiteral) and (FCurrentToken.Lexeme = '0')) then
          Error('SCREEN graphics modes aren''t supported (only SCREEN 0, text)');
        SkipToLineEnd;
      end;
    'DEF':
      begin
        // DEF FNname[(params)] = expression, or a block up to END DEF
        Advance; // DEF
        if Check(tkIdentifier) and SameText(Copy(FCurrentToken.Lexeme, 1, 2), 'FN') then
          ProcedureDefinition(True, 'DEF')
        else if SameText(FCurrentToken.Lexeme, 'SEG') then
          SkipToLineEnd // DEF SEG: memory segments (nothing to do)
        else
          Error('Expected DEF FNname');
      end;
    'OPEN':
      OpenStatement;
    'CLOSE':
      CloseStatement;
    'KILL':
      begin
        Advance;
        Expression;
        FAssembler.Emit(BC_BUILTIN, [BI_KILL, 1]);
        FAssembler.Emit(BC_POP, []);
      end;
    'NAME':
      begin
        // NAME old$ AS new$
        Advance;
        Expression;
        if not AtKeyword('AS') then
          Error('Expected NAME old$ AS new$');
        Advance;
        Expression;
        FAssembler.Emit(BC_BUILTIN, [BI_NAME, 2]);
        FAssembler.Emit(BC_POP, []);
      end;
    'GET', 'PUT':
      GetPutStatement;
    'SEEK':
      begin
        // SEEK [#]n, position
        Advance;
        T := ParseFileNumber(False);
        Match(tkComma);
        FAssembler.Emit(BC_LOAD_VAR, [T]);
        Expression;
        FAssembler.Emit(BC_BUILTIN, [BI_FSEEK, 2]);
        FAssembler.Emit(BC_POP, []);
      end;
    'FIELD':
      Error('FIELD isn''t supported: GET / PUT a TYPE record instead (or use MKI$ / CVI ...)');
    'CHDIR', 'MKDIR', 'RMDIR', 'SHELL',
    'PSET', 'PRESET', 'CIRCLE', 'DRAW', 'PAINT', 'PALETTE', 'PLAY', 'SOUND', 'POKE', 'OUT':
      if Check(tkIdentifier) then
        Error(Word + ' isn''t supported in QuickBASIC mode yet')
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TParser.ShowStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
end;

procedure TParser.HideStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
end;

//----------------------------------------------------------------------
// Expression Parsing Methods
//
// These emit bytecode directly in postfix (stack-machine) order as they
// parse: each level leaves exactly one value on the VM's evaluation
// stack, built from whatever its operands pushed plus one opcode per
// operator - unless FSuppressCodeGen is set (see SkipExpression), in
// which case tokens are still consumed (so the parse stays in sync) but
// no bytecode is emitted and nothing is left on the stack.
//----------------------------------------------------------------------
procedure TParser.SkipExpression;
begin
  FSuppressCodeGen := True;
  try
    Expression;
  finally
    FSuppressCodeGen := False;
  end;
end;

// Kayte has no static type system (see TValueKind in VirtualMachine.pas),
// so type parameters can't be checked or specialized at compile time.
// Rather than reject generic-looking syntax outright, an optional
// "<T, U, ...>" list right after a name (STRUCT/CLASS/SUB/FUNCTION
// definitions, CALL and NEW instantiation sites, and "AS Type" type
// names) is parsed here and discarded: generic code runs exactly like
// its non-generic equivalent. Does not support nested type arguments
// (e.g. "List<List<T>>").
procedure TParser.SkipGenericParams;
begin
  if not (Check(tkOperator) and (FCurrentToken.Lexeme = '<')) then
    Exit;

  Advance; // Consume '<'

  if not Check(tkIdentifier) then
    Error('Expected type parameter name after "<"');
  Advance;

  while Check(tkComma) do
  begin
    Advance; // Consume ','
    if not Check(tkIdentifier) then
      Error('Expected type parameter name after ","');
    Advance;
  end;

  if not (Check(tkOperator) and (FCurrentToken.Lexeme = '>')) then
    Error('Expected ">" to close type parameter list');
  Advance; // Consume '>'
end;

// Precedence, lowest first (as in VB): XOR, OR, AND, NOT, = <>,
// < > <= >= IS, + - &, MOD, \, * /, unary - (and NOT inside an operand),
// primaries.
procedure TParser.Expression;
begin
  if FQB then
    QBImp
  else
    LogicalXor;
end;

procedure TParser.EmitQB(Id, Argc: Integer);
begin
  if not FSuppressCodeGen then
    FAssembler.Emit(BC_BUILTIN, [Id, Argc]);
end;

// QuickBASIC (--qbs): true is -1, and AND / OR / XOR / NOT / EQV / IMP
// work on the bits of whole numbers (both sides always evaluated), so they
// are logical for -1 / 0 and bit operations otherwise (x AND 4). From the
// loosest: IMP, EQV, XOR, OR, AND, NOT.
procedure TParser.QBImp;
begin
  QBEqv;
  while (FCurrentToken.TokenType = tkOperator) and SameText(FCurrentToken.Lexeme, 'IMP') do
  begin
    Advance;
    EmitQB(BI_BNOT, 1); // a IMP b = (NOT a) OR b
    QBEqv;
    EmitQB(BI_BOR, 2);
  end;
end;

procedure TParser.QBEqv;
begin
  LogicalXor;
  while (FCurrentToken.TokenType = tkOperator) and SameText(FCurrentToken.Lexeme, 'EQV') do
  begin
    Advance;
    LogicalXor;
    EmitQB(BI_BXOR, 2); // a EQV b = NOT (a XOR b)
    EmitQB(BI_BNOT, 1);
  end;
end;

// a XOR b: 1 if exactly one of them is true, else 0 (logical, like AND /
// OR). Both sides are always evaluated.
procedure TParser.LogicalXor;
begin
  if FQB then
  begin
    LogicalOr;
    while (FCurrentToken.TokenType = tkOperator) and SameText(FCurrentToken.Lexeme, 'XOR') do
    begin
      Advance;
      LogicalOr;
      EmitQB(BI_BXOR, 2);
    end;
    Exit;
  end;
  LogicalOr;
  while (FCurrentToken.TokenType = tkOperator) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'XOR') do
  begin
    Advance;
    // NOT NOT turns any value into 0 / 1, so the truth values compare.
    if not FSuppressCodeGen then
    begin
      FAssembler.Emit(BC_NOT, []);
      FAssembler.Emit(BC_NOT, []);
    end;
    LogicalOr;
    if not FSuppressCodeGen then
    begin
      FAssembler.Emit(BC_NOT, []);
      FAssembler.Emit(BC_NOT, []);
      FAssembler.Emit(BC_CMP_NEQ, []);
    end;
  end;
end;

// Code-generation helpers that respect FSuppressCodeGen (SkipExpression).
function TParser.EmitJumpAt(Op: TByteCodeOp): Integer;
begin
  Result := -1;
  if FSuppressCodeGen then
    Exit;
  Result := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(Op, [-1]);
end;

procedure TParser.PatchHere(InstrIndex: Integer);
begin
  if InstrIndex >= 0 then
    FAssembler.PatchJumpTarget(InstrIndex, FAssembler.CurrentInstructionIndex);
end;

// A variable only the compiler uses (a MOD operand, a SELECT CASE value).
function TParser.HiddenVar(const Purpose: string): Integer;
begin
  Inc(FHiddenCount);
  DeclareLocal(Format('__%s$%d', [Purpose, FHiddenCount]));
  Result := Op_Variable(Format('__%s$%d', [Purpose, FHiddenCount]));
end;

// a OR b: 1 if either is true, else 0. Like VB.NET's OrElse it
// short-circuits (b isn't evaluated when a is true), and it's logical,
// not bitwise - the VM has no bitwise operations.
procedure TParser.LogicalOr;
var
  TryRight, ToFalse, ToEnd1, ToEnd2: Integer;
begin
  if FQB then
  begin
    LogicalAnd;
    while (FCurrentToken.TokenType = tkOperator) and SameText(FCurrentToken.Lexeme, 'OR') do
    begin
      Advance;
      LogicalAnd;
      EmitQB(BI_BOR, 2);
    end;
    Exit;
  end;
  LogicalAnd;
  while (FCurrentToken.TokenType = tkOperator) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'OR') do
  begin
    Advance;
    TryRight := EmitJumpAt(BC_JUMP_IF_FALSE);
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]);
    ToEnd1 := EmitJumpAt(BC_JUMP);
    PatchHere(TryRight);
    LogicalAnd;
    ToFalse := EmitJumpAt(BC_JUMP_IF_FALSE);
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]);
    ToEnd2 := EmitJumpAt(BC_JUMP);
    PatchHere(ToFalse);
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
    PatchHere(ToEnd1);
    PatchHere(ToEnd2);
  end;
end;

// a AND b: 1 if both are true, else 0; short-circuits like AndAlso.
procedure TParser.LogicalAnd;
var
  False1, False2, ToEnd: Integer;
begin
  if FQB then
  begin
    LogicalNot;
    while (FCurrentToken.TokenType = tkOperator) and SameText(FCurrentToken.Lexeme, 'AND') do
    begin
      Advance;
      LogicalNot;
      EmitQB(BI_BAND, 2);
    end;
    Exit;
  end;
  LogicalNot;
  while (FCurrentToken.TokenType = tkOperator) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AND') do
  begin
    Advance;
    False1 := EmitJumpAt(BC_JUMP_IF_FALSE);
    LogicalNot;
    False2 := EmitJumpAt(BC_JUMP_IF_FALSE);
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]);
    ToEnd := EmitJumpAt(BC_JUMP);
    PatchHere(False1);
    PatchHere(False2);
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
    PatchHere(ToEnd);
  end;
end;

// NOT binds looser than comparisons, as in VB: NOT x = 5 is NOT (x = 5).
procedure TParser.LogicalNot;
begin
  if (FCurrentToken.TokenType = tkOperator) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'NOT') then
  begin
    Advance;
    LogicalNot;
    if FQB then
      EmitQB(BI_BNOT, 1) // bitwise: NOT 0 is -1, NOT -1 is 0
    else if not FSuppressCodeGen then
      FAssembler.Emit(BC_NOT, []);
  end
  else
    Equality;
end;

procedure TParser.Equality;
var
  OperatorToken: TToken;
begin
  Comparison;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '=') or (FCurrentToken.Lexeme = '<>')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Comparison;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '=' then
        FAssembler.Emit(BC_CMP_EQ, [])
      else
        FAssembler.Emit(BC_CMP_NEQ, []);
      if FQB then
        FAssembler.Emit(BC_NEG, []); // QuickBASIC: true is -1
    end;
  end;
end;

procedure TParser.Comparison;
var
  OperatorToken: TToken;
  Op: string;
begin
  Term;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '>') or (FCurrentToken.Lexeme = '<') or (FCurrentToken.Lexeme = '>=') or (FCurrentToken.Lexeme = '<=') or (FCurrentToken.Lexeme.ToUpper = 'IS')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Term;
    if not FSuppressCodeGen then
    begin
      Op := OperatorToken.Lexeme;
      if Op = '>' then
        FAssembler.Emit(BC_CMP_GT, [])
      else if Op = '<' then
        FAssembler.Emit(BC_CMP_LT, [])
      else if Op = '>=' then
        FAssembler.Emit(BC_CMP_GE, [])
      else if Op = '<=' then
        FAssembler.Emit(BC_CMP_LE, [])
      else
        // 'IS': no distinct object-identity model yet, treat as equality.
        FAssembler.Emit(BC_CMP_EQ, []);
      if FQB then
        FAssembler.Emit(BC_NEG, []); // QuickBASIC: true is -1
    end;
  end;
end;

procedure TParser.Term;
var
  OperatorToken: TToken;
begin
  ModExpr;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '+') or (FCurrentToken.Lexeme = '-') or (FCurrentToken.Lexeme = '&')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    ModExpr;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '+' then
        FAssembler.Emit(BC_ADD, [])
      else if OperatorToken.Lexeme = '-' then
        FAssembler.Emit(BC_SUB, [])
      else
        FAssembler.Emit(BC_CONCAT, []);
    end;
  end;
end;

// a MOD b: the remainder, with the sign of a (as in VB): 17 MOD 5 is 2,
// -17 MOD 5 is -2, 5.5 MOD 2 is 1.5.
procedure TParser.ModExpr;
begin
  IntDivExpr;
  while (FCurrentToken.TokenType = tkOperator) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'MOD') do
  begin
    Advance;
    IntDivExpr;
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_MOD, []);
  end;
end;

// a \ b: whole-number division (VB precedence: below * /, above MOD): the
// operands are rounded to whole numbers and the result truncated, so
// 17 \ 5 is 3 and 7.8 \ 2 is 4. (a / b divides exactly: 17 / 5 is 3.4.)
procedure TParser.IntDivExpr;
begin
  Factor;
  while (FCurrentToken.TokenType = tkOperator) and (FCurrentToken.Lexeme = '\') do
  begin
    Advance;
    Factor;
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_IDIV, []);
  end;
end;

procedure TParser.Factor;
var
  OperatorToken: TToken;
begin
  Unary;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '*') or (FCurrentToken.Lexeme = '/')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Unary;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '*' then
        FAssembler.Emit(BC_MUL, [])
      else
        FAssembler.Emit(BC_DIV, []);
    end;
  end;
end;

procedure TParser.Unary;
var
  OperatorToken: TToken;
begin
  if (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '-') or (FCurrentToken.Lexeme.ToUpper = 'NOT')) then
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Unary;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '-' then
        FAssembler.Emit(BC_NEG, [])
      else if FQB then
        FAssembler.Emit(BC_BUILTIN, [BI_BNOT, 1])
      else
        FAssembler.Emit(BC_NOT, []);
    end;
  end
  else
    PowerExpr;
end;

// a ^ b: a whole-number power, binding tighter than unary minus (-2 ^ 2
// is -4) and left to right (2 ^ 3 ^ 2 is 64), as in QuickBASIC.
procedure TParser.PowerExpr;
begin
  Primary;
  while Check(tkOperator) and (FCurrentToken.Lexeme = '^') do
  begin
    Advance;
    if Check(tkOperator) and (FCurrentToken.Lexeme = '-') then
    begin
      Advance;
      Primary;
      if not FSuppressCodeGen then
        FAssembler.Emit(BC_NEG, []);
    end
    else
      Primary;
    if not FSuppressCodeGen then
      FAssembler.Emit(BC_BUILTIN, [BI_POW, 2]);
  end;
end;

procedure TParser.Primary;
var
  Token: TToken;
begin
  Token := FCurrentToken;
  case Token.TokenType of
    tkIntegerLiteral, tkFloatLiteral:
      begin
        Advance;
        if not FSuppressCodeGen then
          EmitNumber(Token.Lexeme);
      end;
    tkStringLiteral:
      begin
        Advance;
        if not FSuppressCodeGen then
          FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral(Token.Lexeme)]);
      end;
    tkBooleanLiteral:
      begin
        Advance;
        if not FSuppressCodeGen then
        begin
          if AnsiUpperCase(Token.Lexeme) = 'TRUE' then
            FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)])
          else
            FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
        end;
      end;
    tkIdentifier, tkDot:
      ChainExpr(False);
    tkParenthesisOpen:
      begin
        Advance; // Consume '('
        Expression;
        Match(tkParenthesisClose); // Consume ')'
      end;
    tkKeyword:
      begin
        if AnsiUpperCase(Token.Lexeme) = 'NEW' then
          NewExpression
        else
          Error('Expected expression, found keyword: ' + FCurrentToken.Lexeme);
      end;
    else
      Error('Expected expression, found ' + FCurrentToken.Lexeme);
  end;
end;

end.
