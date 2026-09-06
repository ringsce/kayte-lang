unit dhtml;

interface
uses
  Classes, SysUtils, contnrs,
  JSBindings; // QuickJS/Duktape bindings unit - not implemented yet

type
  // opaque handles; layout is defined by whichever JS engine backs JSBindings
  JSContextHandle = Pointer;
  JSValueHandle = Pointer;

  THTMLElement = class
    TagName: String;
    Attributes: TStringList;
    Children: TObjectList;
    InnerText: String;
    JSObjectRef: JSValueHandle; // the JS-side object mirroring this element, so JS can read/write it
    procedure AddChild(Element: THTMLElement);
    constructor Create(ATag: String);
    destructor Destroy; override;
  end;

  THTMLDocument = class
  private
    FRoot: THTMLElement;
    FJSContext: JSContextHandle;
    procedure ExposeElementToJS(Element: THTMLElement; JSContext: JSContextHandle; ParentJSObj: JSValueHandle);
  public
    function LoadFromFile(const Filename: String): Boolean;
    function LoadFromString(const HTML: String): Boolean;
    function SaveToFile(const Filename: String): Boolean;
    function FindElementById(const ID: String): THTMLElement;
    property Root: THTMLElement read FRoot;

    constructor Create;
    destructor Destroy; override;
    procedure ExecuteScript(const Script: String);
    procedure ExposeObjectToJS(const Name: String; Obj: TObject);
    function GetJSDocumentObject: JSValueHandle;
  end;

  { Basic JS event callback }
  TDOMEventCallback = procedure(Sender: TObject) of object;

  { Form input simulation }
  TFormElement = class(THTMLElement)
    Value: String;
    OnChange: TDOMEventCallback;
    procedure TriggerChange;
    // subclasses could override how JSObjectRef gets created for form-specific behavior
  end;

implementation

{ THTMLElement }

constructor THTMLElement.Create(ATag: String);
begin
  TagName := ATag;
  Attributes := TStringList.Create;
  Children := TObjectList.Create(True);
  JSObjectRef := nil;
end;

destructor THTMLElement.Destroy;
begin
  // JSObjectRef isn't released here - freeing it is the JS engine's job, and
  // that hook isn't wired up yet (JSBindings.ReleaseJSValue)
  Attributes.Free;
  Children.Free;
  inherited Destroy;
end;

procedure THTMLElement.AddChild(Element: THTMLElement);
begin
  Children.Add(Element);
  // doesn't expose the new child to the JS DOM automatically - that happens
  // separately via THTMLDocument.ExposeElementToJS
end;

{ TFormElement }

procedure TFormElement.TriggerChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
  // should also fire a JS-side 'change' event via JSBindings once event
  // listener lookup exists there
end;

{ THTMLDocument }

constructor THTMLDocument.Create;
begin
  inherited Create;
  FJSContext := JSBindings.CreateJSContext;
  FRoot := THTMLElement.Create('html');
  JSBindings.ExposeGlobalObject(FJSContext, 'document', GetJSDocumentObject);
  JSBindings.DefineJSFunction(FJSContext, GetJSDocumentObject, 'getElementById', @JS_getElementById_callback);
end;

destructor THTMLDocument.Destroy;
begin
  JSBindings.ReleaseJSContext(FJSContext);
  FRoot.Free;
  inherited Destroy;
end;

function THTMLDocument.LoadFromFile(const Filename: String): Boolean;
var
  HTML: TStringList;
begin
  Result := False;
  HTML := TStringList.Create;
  try
    HTML.LoadFromFile(Filename);
    Result := LoadFromString(HTML.Text);
  finally
    HTML.Free;
  end;
end;

function THTMLDocument.LoadFromString(const HTML: String): Boolean;
begin
  // TODO: no real HTML parser yet - should recursively build THTMLElement
  // nodes from the markup and then expose them to the JS context
  FRoot.Destroy;
  FRoot := THTMLElement.Create('html'); // placeholder root until parsing exists

  ExposeElementToJS(FRoot, FJSContext, JSBindings.GetGlobalJSObject(FJSContext));

  Result := True; // always reports success since there's no parser to fail yet
end;

procedure THTMLDocument.ExposeElementToJS(Element: THTMLElement; JSContext: JSContextHandle; ParentJSObj: JSValueHandle);
var
  // minimal mirror: tag name, inner text, and attributes only - no methods or events yet
  JSObj: JSValueHandle;
  Attr: String;
  ChildElement: THTMLElement;
  i: Integer;
begin
  JSObj := JSBindings.CreateJSObject(JSContext);
  JSBindings.SetJSObjectProperty(JSContext, JSObj, 'tagName', JSBindings.CreateJSString(JSContext, Element.TagName));
  JSBindings.SetJSObjectProperty(JSContext, JSObj, 'innerText', JSBindings.CreateJSString(JSContext, Element.InnerText));

  for Attr in Element.Attributes do
    JSBindings.SetJSObjectProperty(JSContext, JSObj, Attr, JSBindings.CreateJSString(JSContext, Element.Attributes.Values[Attr]));

  JSBindings.SetNativePascalObject(JSContext, JSObj, Element);

  Element.JSObjectRef := JSObj;

  // no real 'children' array on the JS side yet - each child just links back
  // to its parent's JS object rather than being added to a proper collection
  if Assigned(ParentJSObj) then
    JSBindings.AddJSChildElement(JSContext, ParentJSObj, JSObj);

  for i := 0 to Element.Children.Count - 1 do
  begin
    ChildElement := THTMLElement(Element.Children[i]);
    ExposeElementToJS(ChildElement, JSContext, JSObj);
  end;
end;

function THTMLDocument.SaveToFile(const Filename: String): Boolean;
var
  F: TextFile;
begin
  AssignFile(F, Filename);
  Rewrite(F);
  // TODO: serialize the FRoot tree back to HTML instead of this stub
  WriteLn(F, '<html><body></body></html>');
  CloseFile(F);
  Result := True;
end;

function THTMLDocument.FindElementById(const ID: String): THTMLElement;
  function FindRecursive(Elem: THTMLElement): THTMLElement;
  var
    i: Integer;
    Child: THTMLElement;
  begin
    if Elem.Attributes.Values['id'] = ID then
      Exit(Elem);
    for i := 0 to Elem.Children.Count - 1 do
    begin
      Child := THTMLElement(Elem.Children[i]);
      Result := FindRecursive(Child);
      if Assigned(Result) then Exit;
    end;
    Result := nil;
  end;
begin
  Result := FindRecursive(FRoot);
end;

function THTMLDocument.GetJSDocumentObject: JSValueHandle;
begin
  // returns the JS object created for 'document' in the constructor
  Result := JSBindings.GetGlobalProperty(FJSContext, 'document');
end;

procedure THTMLDocument.ExecuteScript(const Script: String);
begin
  JSBindings.EvaluateJSCode(FJSContext, Script);
end;

procedure THTMLDocument.ExposeObjectToJS(const Name: String; Obj: TObject);
begin
  // wraps Obj as a JS object and binds it to the global scope under Name
  // (e.g. exposing 'console' or another host object)
  JSBindings.ExposePascalObject(FJSContext, Name, Obj);
end;

// --- callbacks the JS engine invokes when script calls into exposed Pascal functions ---

// backs document.getElementById(id) from JS
procedure JS_getElementById_callback(JSContext: JSContextHandle; JSThis: JSValueHandle;
  JSArgs: array of JSValueHandle; ArgCount: Integer; var JSResult: JSValueHandle); cdecl;
var
  Doc: THTMLDocument;
  ElementID: String;
  FoundElement: THTMLElement;
begin
  JSResult := JSBindings.CreateJSUndefined(JSContext);
  // JSThis isn't actually used to resolve the document instance yet - this
  // just grabs it from MainForm, which assumes a single global document
  Doc := MainForm.HTMLDoc;

  if (Doc <> nil) and (ArgCount >= 1) and JSBindings.IsJSString(JSArgs[0]) then
  begin
    ElementID := JSBindings.JSValueToString(JSContext, JSArgs[0]);
    FoundElement := Doc.FindElementById(ElementID);
    if Assigned(FoundElement) then
      JSResult := FoundElement.JSObjectRef;
  end;
end;

end.
