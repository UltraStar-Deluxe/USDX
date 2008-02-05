(**
===============================================================================================
Name    : LibXmlParser
===============================================================================================
Project : All Projects
===============================================================================================
Subject : Progressive XML Parser for all types of XML Files
===============================================================================================
Author  : Stefan Heymann
          Eschenweg 3
          72076 Tübingen
          GERMANY

E-Mail:   stefan@destructor.de
URL:      www.destructor.de
===============================================================================================
Source, Legals ("Licence")
--------------------------
The official site to get this parser is http://www.destructor.de/

Usage and Distribution of this Source Code is ruled by the
"Destructor.de Source code Licence" (DSL) which comes with this file or
can be downloaded at http://www.destructor.de/

IN SHORT: Usage and distribution of this source code is free.
          You use it completely on your own risk.

Postcardware
------------
If you like this code, please send a postcard of your city to my above address.
===============================================================================================
!!!  All parts of this code which are not finished or not conforming exactly to
     the XmlSpec are marked with three exclamation marks

-!-  Parts where the parser may be able to detect errors in the document's syntax are
     marked with the dash-exlamation mark-dash sequence.
===============================================================================================
Terminology:
------------
- Start:   Start of a buffer part
- Final:   End (last character) of a buffer part
- DTD:     Document Type Definition
- DTDc:    Document Type Declaration
- XMLSpec: The current W3C XML Recommendation (version 1.0 as of 1998-02-10), Chapter No.
- Cur*:    Fields concerning the "Current" part passed back by the "Scan" method
===============================================================================================
Scanning the XML document
-------------------------
- Create TXmlParser Instance                     MyXml := TXmlParser.Create;
- Load XML Document                              MyXml.LoadFromFile (Filename);
- Start Scanning                                 MyXml.StartScan;
- Scan Loop                                      WHILE MyXml.Scan DO
- Test for Part Type                               CASE MyXml.CurPartType OF
- Handle Parts                                       ... : ;;;
- Handle Parts                                       ... : ;;;
- Handle Parts                                       ... : ;;;
                                                     END;
- Destroy                                        MyXml.Free;
===============================================================================================
Loading the XML document
------------------------
You can load the XML document from a file with the "LoadFromFile" method.
It is beyond the scope of this parser to perform HTTP or FTP accesses. If you want your
application to handle such requests (URLs), you can load the XML via HTTP or FTP or whatever
protocol and hand over the data buffer using the "LoadFromBuffer" or "SetBuffer" method.
"LoadFromBuffer" loads the internal buffer of TXmlParser with the given null-terminated
string, thereby creating a copy of that buffer.
"SetBuffer" just takes the pointer to another buffer, which means that the given
buffer pointer must be valid while the document is accessed via TXmlParser.
===============================================================================================
Encodings:
----------
This XML parser kind of "understands" the following encodings:
- UTF-8
- ISO-8859-1
- Windows-1252

Any flavor of multi-byte characters (and this includes UTF-16) is not supported. Sorry.

Every string which has to be passed to the application passes the virtual method
"TranslateEncoding" which translates the string from the current encoding (stored in
"CurEncoding") into the encoding the application wishes to receive.
The "TranslateEncoding" method that is built into TXmlParser assumes that the application
wants to receive Windows ANSI (Windows-1252, about the same as ISO-8859-1) and is able
to convert UTF-8 and ISO-8859-1 encodings.
For other source and target encodings, you will have to override "TranslateEncoding".
===============================================================================================
Buffer Handling
---------------
- The document must be loaded completely into a piece of RAM
- All character positions are referenced by PChar pointers
- The TXmlParser instance can either "own" the buffer itself (then, FBufferSize is > 0)
  or reference the buffer of another instance or object (then, FBuffersize is 0 and
  FBuffer is not NIL)
- The Property DocBuffer passes back a pointer to the first byte of the document. If there
  is no document stored (FBuffer is NIL), the DocBuffer returns a pointer to a NULL character.
===============================================================================================
Whitespace Handling
-------------------
The TXmlParser property "PackSpaces" determines how Whitespace is returned in Text Content:
While PackSpaces is true, all leading and trailing whitespace characters are trimmed of, all
Whitespace is converted to Space #x20 characters and contiguous Whitespace characters are
compressed to one.
If the "Scan" method reports a ptContent part, the application can get the original text
with all whitespace characters by extracting the characters from "CurStart" to "CurFinal".
If the application detects an xml:space attribute, it can set "PackSpaces" accordingly or
use CurStart/CurFinal.
Please note that TXmlParser does _not_ normalize Line Breaks to single LineFeed characters
as the XmlSpec requires (XmlSpec 2.11).
The xml:space attribute is not handled by TXmlParser. This is on behalf of the application.
===============================================================================================
Non-XML-Conforming
------------------
TXmlParser does not conform 100 % exactly to the XmlSpec:
- UTF-16 is not supported (XmlSpec 2.2)
  (Workaround: Convert UTF-16 to UTF-8 and hand the buffer over to TXmlParser)
- As the parser only works with single byte strings, all Unicode characters > 255
  can currently not be handled correctly.
- Line breaks are not normalized to single Linefeed #x0A characters (XmlSpec 2.11)
  (Workaround: The Application can access the text contents on its own [CurStart, CurFinal],
  thereby applying every normalization it wishes to)
- The attribute value normalization does not work exactly as defined in the
  Second Edition of the XML 1.0 specification.
- See also the code parts marked with three consecutive exclamation marks. These are
  parts which are not finished in the current code release.

This list may be incomplete, so it may grow if I get to know any other points.
As work on the parser proceeds, this list may also shrink.
===============================================================================================
Things Todo
-----------
- Introduce a new event/callback which is called when there is an unresolvable
  entity or character reference
- Support Unicode
- Use Streams instead of reading the whole XML into memory
===============================================================================================
Change History, Version numbers
-------------------------------
The Date is given in ISO Year-Month-Day (YYYY-MM-DD) order.
Versions are counted from 1.0.0 beginning with the version from 2000-03-16.
Unreleased versions don't get a version number.

Date        Author Version Changes
-----------------------------------------------------------------------------------------------
2000-03-16  HeySt  1.0.0   Start
2000-03-28  HeySt  1.0.1   Initial Publishing of TXmlParser on the destructor.de Web Site
2000-03-30  HeySt  1.0.2   TXmlParser.AnalyzeCData: Call "TranslateEncoding" for CurContent
2000-03-31  HeySt  1.0.3   Deleted the StrPosE function (was not needed anyway)
2000-04-04  HeySt  1.0.4   TDtdElementRec modified: Start/Final for all Elements;
                           Should be backwards compatible.
                           AnalyzeDtdc: Set CurPartType to ptDtdc
2000-04-23  HeySt  1.0.5   New class TObjectList. Eliminated reference to the Delphi 5
                           "Contnrs" unit so LibXmlParser is Delphi 4 compatible.
2000-07-03  HeySt  1.0.6   TNvpNode: Added Constructor
2000-07-11  HeySt  1.0.7   Removed "Windows" from USES clause
                           Added three-exclamation-mark comments for Utf8ToAnsi/AnsiToUtf8
                           Added three-exclamation-mark comments for CHR function calls
2000-07-23  HeySt  1.0.8   TXmlParser.Clear: CurAttr.Clear; EntityStack.Clear;
                           (This was not a bug; just defensive programming)
2000-07-29  HeySt  1.0.9   TNvpList: Added methods: Node(Index), Value(Index), Name(Index);
2000-10-07  HeySt          Introduced Conditional Defines
                           Uses Contnrs unit and its TObjectList class again for
                           Delphi 5 and newer versions
2001-01-30  HeySt          Introduced Version Numbering
                           Made LoadFromFile and LoadFromBuffer BOOLEAN functions
                           Introduced FileMode parameter for LoadFromFile
                           BugFix: TAttrList.Analyze: Must add CWhitespace to ExtractName call
                           Comments worked over
2001-02-28  HeySt  1.0.10  Completely worked over and tested the UTF-8 functions
                           Fixed a bug in TXmlParser.Scan which caused it to start over when it
                           was called after the end of scanning, resulting in an endless loop
                           TEntityStack is now a TObjectList instead of TList
2001-07-03  HeySt  1.0.11  Updated Compiler Version IFDEFs for Kylix
2001-07-11  HeySt  1.0.12  New TCustomXmlScanner component (taken over from LibXmlComps.pas)
2001-07-14  HeySt  1.0.13  Bugfix TCustomXmlScanner.FOnTranslateEncoding
2001-10-22  HeySt          Don't clear CurName anymore when the parser finds a CDATA section.
2001-12-03  HeySt  1.0.14  TObjectList.Clear: Make call to INHERITED method (fixes a memory leak)
2001-12-05  HeySt  1.0.15  TObjectList.Clear: removed call to INHERITED method
                           TObjectList.Destroy: Inserted SetCapacity call.
                           Reduces need for frequent re-allocation of pointer buffer
                           Dedicated to my father, Theodor Heymann
2002-06-26  HeySt  1.0.16  TXmlParser.Scan: Fixed a bug with PIs whose name is beginning
                           with 'xml'. Thanks to Uwe Kamm for submitting this bug.
                           The CurEncoding property is now always in uppercase letters (the XML
                           spec wants it to be treated case independently so when it's uppercase
                           comparisons are faster)
2002-03-04  HeySt  1.0.17  Included an IFDEF for Delphi 7 (VER150) and Kylix
                           There is a new symbol HAS_CONTNRS_UNIT which is used now to
                           distinguish between IDEs which come with the Contnrs unit and
                           those that don't.
*)

UNIT libxmlparser;

{$I jedi-sdl.inc}

{$ALIGN ON}

INTERFACE

USES
  SysUtils, Classes,
  (*$IFDEF HAS_CONTNRS_UNIT *)  // The Contnrs Unit was introduced in Delphi 5
  Contnrs,
  (*$ENDIF*)
  Math;

CONST
  CVersion = '1.0.17';  // This variable will be updated for every release
                        // (I hope, I won't forget to do it everytime ...)

TYPE
  TPartType    = // --- Document Part Types
                 (ptNone,            // Nothing
                  ptXmlProlog,       // XML Prolog                  XmlSpec 2.8 / 4.3.1
                  ptComment,         // Comment                     XmlSpec 2.5
                  ptPI,              // Processing Instruction      XmlSpec 2.6
                  ptDtdc,            // Document Type Declaration   XmlSpec 2.8
                  ptStartTag,        // Start Tag                   XmlSpec 3.1
                  ptEmptyTag,        // Empty-Element Tag           XmlSpec 3.1
                  ptEndTag,          // End Tag                     XmlSpec 3.1
                  ptContent,         // Text Content between Tags
                  ptCData);          // CDATA Section               XmlSpec 2.7

  TDtdElemType = // --- DTD Elements
                 (deElement,         // !ELEMENT declaration
                  deAttList,         // !ATTLIST declaration
                  deEntity,          // !ENTITY declaration
                  deNotation,        // !NOTATION declaration
                  dePI,              // PI in DTD
                  deComment,         // Comment in DTD
                  deError);          // Error found in the DTD

TYPE
  TAttrList    = CLASS;
  TEntityStack = CLASS;
  TNvpList     = CLASS;
  TElemDef     = CLASS;
  TElemList    = CLASS;
  TEntityDef   = CLASS;
  TNotationDef = CLASS;

  TDtdElementRec = RECORD    // --- This Record is returned by the DTD parser callback function
                     Start, Final : PChar;                             // Start/End of the Element's Declaration
                     CASE ElementType : TDtdElemType OF                // Type of the Element
                       deElement,                                      // <!ELEMENT>
                       deAttList  : (ElemDef      : TElemDef);         // <!ATTLIST>
                       deEntity   : (EntityDef    : TEntityDef);       // <!ENTITY>
                       deNotation : (NotationDef  : TNotationDef);     // <!NOTATION>
                       dePI       : (Target       : PChar;             // <?PI ?>
                                     Content      : PChar;
                                     AttrList     : TAttrList);
                       deError    : (Pos          : PChar);            // Error
                       // deComment : ((No additional fields here));   // <!-- Comment -->
                   END;

  TXmlParser = CLASS                             // --- Internal Properties and Methods
               PROTECTED
                 FBuffer      : PChar;           // NIL if there is no buffer available
                 FBufferSize  : INTEGER;         // 0 if the buffer is not owned by the Document instance
                 FSource      : STRING;          // Name of Source of document. Filename for Documents loaded with LoadFromFile

                 FXmlVersion  : STRING;          // XML version from Document header. Default is '1.0'
                 FEncoding    : STRING;          // Encoding from Document header. Default is 'UTF-8'
                 FStandalone  : BOOLEAN;         // Standalone declaration from Document header. Default is 'yes'
                 FRootName    : STRING;          // Name of the Root Element (= DTD name)
                 FDtdcFinal   : PChar;           // Pointer to the '>' character terminating the DTD declaration

                 FNormalize   : BOOLEAN;         // If true: Pack Whitespace and don't return empty contents
                 EntityStack  : TEntityStack;    // Entity Stack for Parameter and General Entities
                 FCurEncoding : STRING;          // Current Encoding during parsing (always uppercase)

                 PROCEDURE AnalyzeProlog;                                         // Analyze XML Prolog or Text Declaration
                 PROCEDURE AnalyzeComment (Start : PChar; VAR Final : PChar);     // Analyze Comments
                 PROCEDURE AnalyzePI      (Start : PChar; VAR Final : PChar);     // Analyze Processing Instructions (PI)
                 PROCEDURE AnalyzeDtdc;                                           // Analyze Document Type Declaration
                 PROCEDURE AnalyzeDtdElements (Start : PChar; VAR Final : PChar); // Analyze DTD declarations
                 PROCEDURE AnalyzeTag;                                            // Analyze Start/End/Empty-Element Tags
                 PROCEDURE AnalyzeCData;                                          // Analyze CDATA Sections
                 PROCEDURE AnalyzeText (VAR IsDone : BOOLEAN);                    // Analyze Text Content between Tags
                 PROCEDURE AnalyzeElementDecl  (Start : PChar; VAR Final : PChar);
                 PROCEDURE AnalyzeAttListDecl  (Start : PChar; VAR Final : PChar);
                 PROCEDURE AnalyzeEntityDecl   (Start : PChar; VAR Final : PChar);
                 PROCEDURE AnalyzeNotationDecl (Start : PChar; VAR Final : PChar);

                 PROCEDURE PushPE (VAR Start : PChar);
                 PROCEDURE ReplaceCharacterEntities (VAR Str : STRING);
                 PROCEDURE ReplaceParameterEntities (VAR Str : STRING);
                 PROCEDURE ReplaceGeneralEntities   (VAR Str : STRING);

                 FUNCTION GetDocBuffer : PChar;  // Returns FBuffer or a pointer to a NUL char if Buffer is empty

               PUBLIC                         // --- Document Properties
                 PROPERTY XmlVersion : STRING  READ FXmlVersion;                 // XML version from the Document Prolog
                 PROPERTY Encoding   : STRING  READ FEncoding;                   // Document Encoding from Prolog
                 PROPERTY Standalone : BOOLEAN READ FStandalone;                 // Standalone Declaration from Prolog
                 PROPERTY RootName   : STRING  READ FRootName;                   // Name of the Root Element
                 PROPERTY Normalize  : BOOLEAN READ FNormalize WRITE FNormalize; // True if Content is to be normalized
                 PROPERTY Source     : STRING  READ FSource;                     // Name of Document Source (Filename)
                 PROPERTY DocBuffer  : PChar   READ GetDocBuffer;                // Returns document buffer
               PUBLIC                         // --- DTD Objects
                 Elements    : TElemList;     // Elements: List of TElemDef (contains Attribute Definitions)
                 Entities    : TNvpList;      // General Entities: List of TEntityDef
                 ParEntities : TNvpList;      // Parameter Entities: List of TEntityDef
                 Notations   : TNvpList;      // Notations: List of TNotationDef
               PUBLIC
                 CONSTRUCTOR Create;
                 DESTRUCTOR Destroy;                                      OVERRIDE;

                 // --- Document Handling
                 FUNCTION  LoadFromFile   (Filename : STRING;
                                           FileMode : INTEGER = fmOpenRead OR fmShareDenyNone) : BOOLEAN;
                                                                          // Loads Document from given file
                 FUNCTION  LoadFromBuffer (Buffer : PChar) : BOOLEAN;     // Loads Document from another buffer
                 PROCEDURE SetBuffer      (Buffer : PChar);               // References another buffer
                 PROCEDURE Clear;                                         // Clear Document

               PUBLIC
                 // --- Scanning through the document
                 CurPartType : TPartType;                         // Current Type
                 CurName     : STRING;                            // Current Name
                 CurContent  : STRING;                            // Current Normalized Content
                 CurStart    : PChar;                             // Current First character
                 CurFinal    : PChar;                             // Current Last character
                 CurAttr     : TAttrList;                         // Current Attribute List
                 PROPERTY CurEncoding : STRING READ FCurEncoding; // Current Encoding
                 PROCEDURE StartScan;
                 FUNCTION  Scan : BOOLEAN;

                 // --- Events / Callbacks
                 FUNCTION  LoadExternalEntity (SystemId, PublicId,
                                               Notation : STRING) : TXmlParser;     VIRTUAL;
                 FUNCTION  TranslateEncoding  (CONST Source : STRING) : STRING;     VIRTUAL;
                 PROCEDURE DtdElementFound (DtdElementRec : TDtdElementRec);        VIRTUAL;
               END;

  TValueType   = // --- Attribute Value Type
                 (vtNormal,       // Normal specified Attribute
                  vtImplied,      // #IMPLIED attribute value
                  vtFixed,        // #FIXED attribute value
                  vtDefault);     // Attribute value from default value in !ATTLIST declaration

  TAttrDefault = // --- Attribute Default Type
                 (adDefault,      // Normal default value
                  adRequired,     // #REQUIRED attribute
                  adImplied,      // #IMPLIED attribute
                  adFixed);       // #FIXED attribute

  TAttrType    = // --- Type of attribute
                 (atUnknown,      // Unknown type
                  atCData,        // Character data only
                  atID,           // ID
                  atIdRef,        // ID Reference
                  atIdRefs,       // Several ID References, separated by Whitespace
                  atEntity,       // Name of an unparsed Entity
                  atEntities,     // Several unparsed Entity names, separated by Whitespace
                  atNmToken,      // Name Token
                  atNmTokens,     // Several Name Tokens, separated by Whitespace
                  atNotation,     // A selection of Notation names (Unparsed Entity)
                  atEnumeration); // Enumeration

  TElemType    = // --- Element content type
                 (etEmpty,        // Element is always empty
                  etAny,          // Element can have any mixture of PCDATA and any elements
                  etChildren,     // Element must contain only elements
                  etMixed);       // Mixed PCDATA and elements

  (*$IFDEF HAS_CONTNRS_UNIT *)
  TObjectList = Contnrs.TObjectList;    // Re-Export this identifier
  (*$ELSE *)
  TObjectList = CLASS (TList)
                  DESTRUCTOR Destroy; OVERRIDE;
                  PROCEDURE Delete (Index : INTEGER);
                  PROCEDURE Clear; OVERRIDE;
                END;
  (*$ENDIF *)

  TNvpNode  = CLASS                     // Name-Value Pair Node
                 Name  : STRING;
                 Value : STRING;
                 CONSTRUCTOR Create (TheName : STRING = ''; TheValue : STRING = '');
              END;

  TNvpList  = CLASS (TObjectList)       // Name-Value Pair List
                PROCEDURE Add   (Node  : TNvpNode);
                FUNCTION  Node  (Name  : STRING)  : TNvpNode;  OVERLOAD;
                FUNCTION  Node  (Index : INTEGER) : TNvpNode;  OVERLOAD;
                FUNCTION  Value (Name  : STRING)  : STRING;    OVERLOAD;
                FUNCTION  Value (Index : INTEGER) : STRING;    OVERLOAD;
                FUNCTION  Name  (Index : INTEGER) : STRING;
              END;

  TAttr     = CLASS (TNvpNode)          // Attribute of a Start-Tag or Empty-Element-Tag
                 ValueType : TValueType;
                 AttrType  : TAttrType;
               END;

  TAttrList = CLASS (TNvpList)          // List of Attributes
                PROCEDURE Analyze (Start : PChar; VAR Final : PChar);
              END;

  TEntityStack = CLASS (TObjectList)    // Stack where current position is stored before parsing entities
                 PROTECTED
                   Owner : TXmlParser;
                 PUBLIC
                   CONSTRUCTOR Create (TheOwner : TXmlParser);
                   PROCEDURE Push (LastPos : PChar);                      OVERLOAD;
                   PROCEDURE Push (Instance : TObject; LastPos : PChar);  OVERLOAD;
                   FUNCTION  Pop : PChar;         // Returns next char or NIL if EOF is reached. Frees Instance.
                 END;

  TAttrDef    = CLASS (TNvpNode)        // Represents a <!ATTLIST Definition. "Value" is the default value
                  TypeDef     : STRING;           // Type definition from the DTD
                  Notations   : STRING;           // Notation List, separated by pipe symbols '|'
                  AttrType    : TAttrType;        // Attribute Type
                  DefaultType : TAttrDefault;     // Default Type
                END;

  TElemDef    = CLASS (TNvpList)       // Represents a <!ELEMENT Definition. Is a list of TAttrDef-Nodes
                  Name       : STRING;            // Element name
                  ElemType   : TElemType;         // Element type
                  Definition : STRING;            // Element definition from DTD
                END;

  TElemList   = CLASS (TObjectList)    // List of TElemDef nodes
                  FUNCTION  Node (Name : STRING) : TElemDef;
                  PROCEDURE Add (Node : TElemDef);
                END;

  TEntityDef  = CLASS (TNvpNode)       // Represents a <!ENTITY Definition.
                  SystemId     : STRING;
                  PublicId     : STRING;
                  NotationName : STRING;
                END;

  TNotationDef = CLASS (TNvpNode)      // Represents a <!NOTATION Definition. Value is the System ID
                   PublicId : STRING;
                 END;

  TCharset = SET OF CHAR;


CONST
  CWhitespace   = [#32, #9, #13, #10];                // Whitespace characters (XmlSpec 2.3)
  CLetter       = [#$41..#$5A, #$61..#$7A, #$C0..#$D6, #$D8..#$F6, #$F8..#$FF];
  CDigit        = [#$30..#$39];
  CNameChar     = CLetter + CDigit + ['.', '-', '_', ':', #$B7];
  CNameStart    = CLetter + ['_', ':'];
  CQuoteChar    = ['"', ''''];
  CPubidChar    = [#32, ^M, ^J, #9, 'a'..'z', 'A'..'Z', '0'..'9',
                   '-', '''', '(', ')', '+', ',', '.', '/', ':',
                   '=', '?', ';', '!', '*', '#', '@', '$', '_', '%'];

  CDStart       = '<![CDATA[';
  CDEnd         = ']]>';

  // --- Name Constants for the above enumeration types
  CPartType_Name : ARRAY [TPartType] OF STRING =
                   ('', 'XML Prolog', 'Comment', 'PI',
                    'DTD Declaration', 'Start Tag', 'Empty Tag', 'End Tag',
                    'Text', 'CDATA');
  CValueType_Name   : ARRAY [TValueType]    OF STRING = ('Normal', 'Implied', 'Fixed', 'Default');
  CAttrDefault_Name : ARRAY [TAttrDefault]  OF STRING = ('Default', 'Required', 'Implied', 'Fixed');
  CElemType_Name    : ARRAY [TElemType]     OF STRING = ('Empty', 'Any', 'Childs only', 'Mixed');
  CAttrType_Name    : ARRAY [TAttrType]     OF STRING = ('Unknown', 'CDATA',
                                                         'ID', 'IDREF', 'IDREFS',
                                                         'ENTITY', 'ENTITIES',
                                                         'NMTOKEN', 'NMTOKENS',
                                                         'Notation', 'Enumeration');

FUNCTION  ConvertWs   (Source: STRING; PackWs: BOOLEAN) : STRING;          // Convert WS to spaces #x20
PROCEDURE SetStringSF (VAR S : STRING; BufferStart, BufferFinal : PChar);  // SetString by Start/Final of buffer
FUNCTION  StrSFPas    (Start, Finish : PChar) : STRING;                    // Convert buffer part to Pascal string
FUNCTION  TrimWs      (Source : STRING) : STRING;                          // Trim Whitespace

FUNCTION  AnsiToUtf8  (Source : ANSISTRING) : STRING;                            // Convert Win-1252 to UTF-8
FUNCTION  Utf8ToAnsi  (Source : STRING; UnknownChar : CHAR = '¿') : ANSISTRING;  // Convert UTF-8 to Win-1252


(*
===============================================================================================
TCustomXmlScanner event based component wrapper for TXmlParser
===============================================================================================
*)

TYPE
  TCustomXmlScanner = CLASS;
  TXmlPrologEvent   = PROCEDURE (Sender : TObject; XmlVersion, Encoding: STRING; Standalone : BOOLEAN) OF OBJECT;
  TCommentEvent     = PROCEDURE (Sender : TObject; Comment : STRING)                                   OF OBJECT;
  TPIEvent          = PROCEDURE (Sender : TObject; Target, Content: STRING; Attributes : TAttrList)    OF OBJECT;
  TDtdEvent         = PROCEDURE (Sender : TObject; RootElementName : STRING)                           OF OBJECT;
  TStartTagEvent    = PROCEDURE (Sender : TObject; TagName : STRING; Attributes : TAttrList)           OF OBJECT;
  TEndTagEvent      = PROCEDURE (Sender : TObject; TagName : STRING)                                   OF OBJECT;
  TContentEvent     = PROCEDURE (Sender : TObject; Content : STRING)                                   OF OBJECT;
  TElementEvent     = PROCEDURE (Sender : TObject; ElemDef : TElemDef)                                 OF OBJECT;
  TEntityEvent      = PROCEDURE (Sender : TObject; EntityDef : TEntityDef)                             OF OBJECT;
  TNotationEvent    = PROCEDURE (Sender : TObject; NotationDef : TNotationDef)                         OF OBJECT;
  TErrorEvent       = PROCEDURE (Sender : TObject; ErrorPos : PChar)                                   OF OBJECT;
  TExternalEvent    = PROCEDURE (Sender : TObject; SystemId, PublicId, NotationId : STRING;
                                 VAR Result : TXmlParser)                                              OF OBJECT;
  TEncodingEvent    = FUNCTION  (Sender : TObject; CurrentEncoding, Source : STRING) : STRING          OF OBJECT;


  TCustomXmlScanner = CLASS (TComponent)
    PROTECTED
      FXmlParser           : TXmlParser;
      FOnXmlProlog         : TXmlPrologEvent;
      FOnComment           : TCommentEvent;
      FOnPI                : TPIEvent;
      FOnDtdRead           : TDtdEvent;
      FOnStartTag          : TStartTagEvent;
      FOnEmptyTag          : TStartTagEvent;
      FOnEndTag            : TEndTagEvent;
      FOnContent           : TContentEvent;
      FOnCData             : TContentEvent;
      FOnElement           : TElementEvent;
      FOnAttList           : TElementEvent;
      FOnEntity            : TEntityEvent;
      FOnNotation          : TNotationEvent;
      FOnDtdError          : TErrorEvent;
      FOnLoadExternal      : TExternalEvent;
      FOnTranslateEncoding : TEncodingEvent;
      FStopParser          : BOOLEAN;
      FUNCTION  GetNormalize : BOOLEAN;
      PROCEDURE SetNormalize (Value : BOOLEAN);

      PROCEDURE WhenXmlProlog(XmlVersion, Encoding: STRING; Standalone : BOOLEAN); VIRTUAL;
      PROCEDURE WhenComment  (Comment : STRING);                                   VIRTUAL;
      PROCEDURE WhenPI       (Target, Content: STRING; Attributes : TAttrList);    VIRTUAL;
      PROCEDURE WhenDtdRead  (RootElementName : STRING);                           VIRTUAL;
      PROCEDURE WhenStartTag (TagName : STRING; Attributes : TAttrList);           VIRTUAL;
      PROCEDURE WhenEmptyTag (TagName : STRING; Attributes : TAttrList);           VIRTUAL;
      PROCEDURE WhenEndTag   (TagName : STRING);                                   VIRTUAL;
      PROCEDURE WhenContent  (Content : STRING);                                   VIRTUAL;
      PROCEDURE WhenCData    (Content : STRING);                                   VIRTUAL;
      PROCEDURE WhenElement  (ElemDef : TElemDef);                                 VIRTUAL;
      PROCEDURE WhenAttList  (ElemDef : TElemDef);                                 VIRTUAL;
      PROCEDURE WhenEntity   (EntityDef : TEntityDef);                             VIRTUAL;
      PROCEDURE WhenNotation (NotationDef : TNotationDef);                         VIRTUAL;
      PROCEDURE WhenDtdError (ErrorPos : PChar);                                   VIRTUAL;

    PUBLIC
      CONSTRUCTOR Create (AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy;                      OVERRIDE;

      PROCEDURE LoadFromFile   (Filename : TFilename);   // Load XML Document from file
      PROCEDURE LoadFromBuffer (Buffer : PChar);         // Load XML Document from buffer
      PROCEDURE SetBuffer      (Buffer : PChar);         // Refer to Buffer
      FUNCTION  GetFilename : TFilename;

      PROCEDURE Execute;                                 // Perform scanning

    PROTECTED
      PROPERTY XmlParser           : TXmlParser        READ FXmlParser;
      PROPERTY StopParser          : BOOLEAN           READ FStopParser          WRITE FStopParser;
      PROPERTY Filename            : TFilename         READ GetFilename          WRITE LoadFromFile;
      PROPERTY Normalize           : BOOLEAN           READ GetNormalize         WRITE SetNormalize;
      PROPERTY OnXmlProlog         : TXmlPrologEvent   READ FOnXmlProlog         WRITE FOnXmlProlog;
      PROPERTY OnComment           : TCommentEvent     READ FOnComment           WRITE FOnComment;
      PROPERTY OnPI                : TPIEvent          READ FOnPI                WRITE FOnPI;
      PROPERTY OnDtdRead           : TDtdEvent         READ FOnDtdRead           WRITE FOnDtdRead;
      PROPERTY OnStartTag          : TStartTagEvent    READ FOnStartTag          WRITE FOnStartTag;
      PROPERTY OnEmptyTag          : TStartTagEvent    READ FOnEmptyTag          WRITE FOnEmptyTag;
      PROPERTY OnEndTag            : TEndTagEvent      READ FOnEndTag            WRITE FOnEndTag;
      PROPERTY OnContent           : TContentEvent     READ FOnContent           WRITE FOnContent;
      PROPERTY OnCData             : TContentEvent     READ FOnCData             WRITE FOnCData;
      PROPERTY OnElement           : TElementEvent     READ FOnElement           WRITE FOnElement;
      PROPERTY OnAttList           : TElementEvent     READ FOnAttList           WRITE FOnAttList;
      PROPERTY OnEntity            : TEntityEvent      READ FOnEntity            WRITE FOnEntity;
      PROPERTY OnNotation          : TNotationEvent    READ FOnNotation          WRITE FOnNotation;
      PROPERTY OnDtdError          : TErrorEvent       READ FOnDtdError          WRITE FOnDtdError;
      PROPERTY OnLoadExternal      : TExternalEvent    READ FOnLoadExternal      WRITE FOnLoadExternal;
      PROPERTY OnTranslateEncoding : TEncodingEvent    READ FOnTranslateEncoding WRITE FOnTranslateEncoding;
    END;

(*
===============================================================================================
IMPLEMENTATION
===============================================================================================
*)

IMPLEMENTATION


(*
===============================================================================================
Unicode and UTF-8 stuff
===============================================================================================
*)

CONST
  // --- Character Translation Table for Unicode <-> Win-1252
  WIN1252_UNICODE : ARRAY [$00..$FF] OF WORD = (
                    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009,
                    $000A, $000B, $000C, $000D, $000E, $000F, $0010, $0011, $0012, $0013,
                    $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D,
                    $001E, $001F, $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
                    $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F, $0030, $0031,
                    $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B,
                    $003C, $003D, $003E, $003F, $0040, $0041, $0042, $0043, $0044, $0045,
                    $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
                    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059,
                    $005A, $005B, $005C, $005D, $005E, $005F, $0060, $0061, $0062, $0063,
                    $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D,
                    $006E, $006F, $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
                    $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,

                    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030,
                    $0160, $2039, $0152, $008D, $017D, $008F, $0090, $2018, $2019, $201C,
                    $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, $009D,
                    $017E, $0178, $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
                    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF, $00B0, $00B1,
                    $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB,
                    $00BC, $00BD, $00BE, $00BF, $00C0, $00C1, $00C2, $00C3, $00C4, $00C5,
                    $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
                    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9,
                    $00DA, $00DB, $00DC, $00DD, $00DE, $00DF, $00E0, $00E1, $00E2, $00E3,
                    $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED,
                    $00EE, $00EF, $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
                    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF);

(* UTF-8  (somewhat simplified)
   -----
   Character Range    Byte sequence
   ---------------    --------------------------     (x=Bits from original character)
   $0000..$007F       0xxxxxxx
   $0080..$07FF       110xxxxx 10xxxxxx
   $8000..$FFFF       1110xxxx 10xxxxxx 10xxxxxx

   Example
   --------
   Transforming the Unicode character U+00E4 LATIN SMALL LETTER A WITH DIAERESIS  ("ä"):

         ISO-8859-1,           Decimal  228
         Win1252,              Hex      $E4
         ANSI                  Bin      1110 0100
                                        abcd efgh

         UTF-8                 Binary   1100xxab 10cdefgh
                               Binary   11000011 10100100
                               Hex      $C3      $A4
                               Decimal  195      164
                               ANSI     Ã        ¤         *)


FUNCTION  AnsiToUtf8 (Source : ANSISTRING) : STRING;
          (* Converts the given Windows ANSI (Win1252) String to UTF-8. *)
VAR
  I   : INTEGER;  // Loop counter
  U   : WORD;     // Current Unicode value
  Len : INTEGER;  // Current real length of "Result" string
BEGIN
  SetLength (Result, Length (Source) * 3);   // Worst case
  Len := 0;
  FOR I := 1 TO Length (Source) DO BEGIN
    U := WIN1252_UNICODE [ORD (Source [I])];
    CASE U OF
      $0000..$007F : BEGIN
                       INC (Len);
                       Result [Len] := CHR (U);
                     END;
      $0080..$07FF : BEGIN
                       INC (Len);
                       Result [Len] := CHR ($C0 OR (U SHR 6));
                       INC (Len);
                       Result [Len] := CHR ($80 OR (U AND $3F));
                     END;
      $0800..$FFFF : BEGIN
                       INC (Len);
                       Result [Len] := CHR ($E0 OR (U SHR 12));
                       INC (Len);
                       Result [Len] := CHR ($80 OR ((U SHR 6) AND $3F));
                       INC (Len);
                       Result [Len] := CHR ($80 OR (U AND $3F));
                     END;
      END;
    END;
  SetLength (Result, Len);
END;


FUNCTION  Utf8ToAnsi (Source : STRING; UnknownChar : CHAR = '¿') : ANSISTRING;
          (* Converts the given UTF-8 String to Windows ANSI (Win-1252).
             If a character can not be converted, the "UnknownChar" is inserted. *)
VAR
  SourceLen : INTEGER;  // Length of Source string
  I, K      : INTEGER;
  A         : BYTE;     // Current ANSI character value
  U         : WORD;
  Ch        : CHAR;     // Dest char
  Len       : INTEGER;  // Current real length of "Result" string
BEGIN
  SourceLen := Length (Source);
  SetLength (Result, SourceLen);   // Enough room to live
  Len := 0;
  I   := 1;
  WHILE I <= SourceLen DO BEGIN
    A := ORD (Source [I]);
    IF A < $80 THEN BEGIN                                               // Range $0000..$007F
      INC (Len);
      Result [Len] := Source [I];
      INC (I);
      END
    ELSE BEGIN                                                          // Determine U, Inc I
      IF (A AND $E0 = $C0) AND (I < SourceLen) THEN BEGIN               // Range $0080..$07FF
        U := (WORD (A AND $1F) SHL 6) OR (ORD (Source [I+1]) AND $3F);
        INC (I, 2);
        END
      ELSE IF (A AND $F0 = $E0) AND (I < SourceLen-1) THEN BEGIN        // Range $0800..$FFFF
        U := (WORD (A AND $0F) SHL 12) OR
             (WORD (ORD (Source [I+1]) AND $3F) SHL 6) OR
             (      ORD (Source [I+2]) AND $3F);
        INC (I, 3);
        END
      ELSE BEGIN                                                        // Unknown/unsupported
        INC (I);
        FOR K := 7 DOWNTO 0 DO
          IF A AND (1 SHL K) = 0 THEN BEGIN
            INC (I, (A SHR (K+1))-1);
            BREAK;
            END;
        U := WIN1252_UNICODE [ORD (UnknownChar)];
        END;
      Ch := UnknownChar;                                                // Retrieve ANSI char
      FOR A := $00 TO $FF DO
        IF WIN1252_UNICODE [A] = U THEN BEGIN
          Ch := CHR (A);
          BREAK;
          END;
      INC (Len);
      Result [Len] := Ch;
      END;
    END;
  SetLength (Result, Len);
END;


(*
===============================================================================================
"Special" Helper Functions

Don't ask me why. But including these functions makes the parser *DRAMATICALLY* faster
on my K6-233 machine. You can test it yourself just by commenting them out.
They do exactly the same as the Assembler routines defined in SysUtils.
(This is where you can see how great the Delphi compiler really is. The compiled code is
faster than hand-coded assembler!)
===============================================================================================
--> Just move this line below the StrScan function -->  *)


FUNCTION StrPos (CONST Str, SearchStr : PChar) : PChar;
         // Same functionality as SysUtils.StrPos
VAR
  First : CHAR;
  Len   : INTEGER;
BEGIN
  First  := SearchStr^;
  Len    := StrLen (SearchStr);
  Result := Str;
  REPEAT
    IF Result^ = First THEN
      IF StrLComp (Result, SearchStr, Len) = 0 THEN BREAK;
    IF Result^ = #0 THEN BEGIN
      Result := NIL;
      BREAK;
      END;
    INC (Result);
  UNTIL FALSE;
END;


FUNCTION StrScan (CONST Start : PChar; CONST Ch : CHAR) : PChar;
         // Same functionality as SysUtils.StrScan
BEGIN
  Result := Start;
  WHILE Result^ <> Ch DO BEGIN
    IF Result^ = #0 THEN BEGIN
      Result := NIL;
      EXIT;
      END;
    INC (Result);
    END;
END;


(*
===============================================================================================
Helper Functions
===============================================================================================
*)

FUNCTION  DelChars (Source : STRING; CharsToDelete : TCharset) : STRING;
          // Delete all "CharsToDelete" from the string
VAR
  I : INTEGER;
BEGIN
  Result := Source;
  FOR I := Length (Result) DOWNTO 1 DO
    IF Result [I] IN CharsToDelete THEN
      Delete (Result, I, 1);
END;


FUNCTION  TrimWs (Source : STRING) : STRING;
          // Trimms off Whitespace characters from both ends of the string
VAR
  I : INTEGER;
BEGIN
  // --- Trim Left
  I := 1;
  WHILE (I <= Length (Source)) AND (Source [I] IN CWhitespace) DO
    INC (I);
  Result := Copy (Source, I, MaxInt);

  // --- Trim Right
  I := Length (Result);
  WHILE (I > 1) AND (Result [I] IN CWhitespace) DO
    DEC (I);
  Delete (Result, I+1, Length (Result)-I);
END;


FUNCTION  ConvertWs (Source: STRING; PackWs: BOOLEAN) : STRING;
          // Converts all Whitespace characters to the Space #x20 character
          // If "PackWs" is true, contiguous Whitespace characters are packed to one
VAR
  I : INTEGER;
BEGIN
  Result := Source;
  FOR I := Length (Result) DOWNTO 1 DO
    IF (Result [I] IN CWhitespace) THEN
      IF PackWs AND (I > 1) AND (Result [I-1] IN CWhitespace)
        THEN Delete (Result, I, 1)
        ELSE Result [I] := #32;
END;


PROCEDURE SetStringSF (VAR S : STRING; BufferStart, BufferFinal : PChar);
BEGIN
  SetString (S, BufferStart, BufferFinal-BufferStart+1);
END;


FUNCTION  StrLPas  (Start : PChar; Len : INTEGER) : STRING;
BEGIN
  SetString (Result, Start, Len);
END;


FUNCTION  StrSFPas (Start, Finish : PChar) : STRING;
BEGIN
  SetString (Result, Start, Finish-Start+1);
END;


FUNCTION  StrScanE (CONST Source : PChar; CONST CharToScanFor : CHAR) : PChar;
          // If "CharToScanFor" is not found, StrScanE returns the last char of the
          // buffer instead of NIL
BEGIN
  Result := StrScan (Source, CharToScanFor);
  IF Result = NIL THEN
    Result := StrEnd (Source)-1;
END;


PROCEDURE ExtractName (Start : PChar; Terminators : TCharset; VAR Final : PChar);
          (* Extracts the complete Name beginning at "Start".
             It is assumed that the name is contained in Markup, so the '>' character is
             always a Termination.
             Start:       IN  Pointer to first char of name. Is always considered to be valid
             Terminators: IN  Characters which terminate the name
             Final:       OUT Pointer to last char of name *)
BEGIN
  Final := Start+1;
  Include (Terminators, #0);
  Include (Terminators, '>');
  WHILE NOT (Final^ IN Terminators) DO
    INC (Final);
  DEC (Final);
END;


PROCEDURE ExtractQuote (Start : PChar; VAR Content : STRING; VAR Final : PChar);
          (* Extract a string which is contained in single or double Quotes.
             Start:    IN   Pointer to opening quote
             Content:  OUT  The quoted string
             Final:    OUT  Pointer to closing quote *)
BEGIN
  Final := StrScan (Start+1, Start^);
  IF Final = NIL THEN BEGIN
    Final := StrEnd (Start+1)-1;
    SetString (Content, Start+1, Final-Start);
    END
  ELSE
    SetString (Content, Start+1, Final-1-Start);
END;


(*
===============================================================================================
TEntityStackNode
This Node is pushed to the "Entity Stack" whenever the parser parses entity replacement text.
The "Instance" field holds the Instance pointer of an External Entity buffer. When it is
popped, the Instance is freed.
The "Encoding" field holds the name of the Encoding. External Parsed Entities may have
another encoding as the document entity (XmlSpec 4.3.3). So when there is an "<?xml" PI
found in the stream (= Text Declaration at the beginning of external parsed entities), the
Encoding found there is used for the External Entity (is assigned to TXmlParser.CurEncoding)
Default Encoding is for the Document Entity is UTF-8. It is assumed that External Entities
have the same Encoding as the Document Entity, unless they carry a Text Declaration.
===============================================================================================
*)

TYPE
  TEntityStackNode = CLASS
                       Instance : TObject;
                       Encoding : STRING;
                       LastPos  : PChar;
                     END;

(*
===============================================================================================
TEntityStack
For nesting of Entities.
When there is an entity reference found in the data stream, the corresponding entity
definition is searched and the current position is pushed to this stack.
From then on, the program scans the entitiy replacement text as if it were normal content.
When the parser reaches the end of an entity, the current position is popped off the
stack again.
===============================================================================================
*)

CONSTRUCTOR TEntityStack.Create (TheOwner : TXmlParser);
BEGIN
  INHERITED Create;
  Owner := TheOwner;
END;


PROCEDURE TEntityStack.Push (LastPos : PChar);
BEGIN
  Push (NIL, LastPos);
END;


PROCEDURE TEntityStack.Push (Instance : TObject; LastPos : PChar);
VAR
  ESN : TEntityStackNode;
BEGIN
  ESN := TEntityStackNode.Create;
  ESN.Instance := Instance;
  ESN.Encoding := Owner.FCurEncoding;  // Save current Encoding
  ESN.LastPos  := LastPos;
  Add (ESN);
END;


FUNCTION  TEntityStack.Pop : PChar;
VAR
  ESN : TEntityStackNode;
BEGIN
  IF Count > 0 THEN BEGIN
    ESN := TEntityStackNode (Items [Count-1]);
    Result := ESN.LastPos;
    IF ESN.Instance <> NIL THEN
      ESN.Instance.Free;
    IF ESN.Encoding <> '' THEN
      Owner.FCurEncoding := ESN.Encoding;   // Restore current Encoding
    Delete (Count-1);
    END
  ELSE
    Result := NIL;
END;


(*
===============================================================================================
TExternalID
-----------
XmlSpec 4.2.2:  ExternalID ::= 'SYSTEM' S SystemLiteral |
                               'PUBLIC' S PubidLiteral S SystemLiteral
XmlSpec 4.7:    PublicID   ::= 'PUBLIC' S PubidLiteral
SystemLiteral and PubidLiteral are quoted
===============================================================================================
*)

TYPE
  TExternalID = CLASS
                  PublicId : STRING;
                  SystemId : STRING;
                  Final    : PChar;
                  CONSTRUCTOR Create (Start : PChar);
                END;

CONSTRUCTOR TExternalID.Create (Start : PChar);
BEGIN
  INHERITED Create;
  Final := Start;
  IF StrLComp (Start, 'SYSTEM', 6) = 0 THEN BEGIN
    WHILE NOT (Final^ IN (CQuoteChar + [#0, '>', '['])) DO INC (Final);
    IF NOT (Final^ IN CQuoteChar) THEN EXIT;
    ExtractQuote (Final, SystemID, Final);
    END
  ELSE IF StrLComp (Start, 'PUBLIC', 6) = 0 THEN BEGIN
    WHILE NOT (Final^ IN (CQuoteChar + [#0, '>', '['])) DO INC (Final);
    IF NOT (Final^ IN CQuoteChar) THEN EXIT;
    ExtractQuote (Final, PublicID, Final);
    INC (Final);
    WHILE NOT (Final^ IN (CQuoteChar + [#0, '>', '['])) DO INC (Final);
    IF NOT (Final^ IN CQuoteChar) THEN EXIT;
    ExtractQuote (Final, SystemID, Final);
    END;
END;


(*
===============================================================================================
TXmlParser
===============================================================================================
*)

CONSTRUCTOR TXmlParser.Create;
BEGIN
  INHERITED Create;
  FBuffer     := NIL;
  FBufferSize := 0;
  Elements    := TElemList.Create;
  Entities    := TNvpList.Create;
  ParEntities := TNvpList.Create;
  Notations   := TNvpList.Create;
  CurAttr     := TAttrList.Create;
  EntityStack := TEntityStack.Create (Self);
  Clear;
END;


DESTRUCTOR TXmlParser.Destroy;
BEGIN
  Clear;
  Elements.Free;
  Entities.Free;
  ParEntities.Free;
  Notations.Free;
  CurAttr.Free;
  EntityStack.Free;
  INHERITED Destroy;
END;


PROCEDURE TXmlParser.Clear;
          // Free Buffer and clear all object attributes
BEGIN
  IF (FBufferSize > 0) AND (FBuffer <> NIL) THEN
    FreeMem (FBuffer);
  FBuffer     := NIL;
  FBufferSize := 0;
  FSource     := '';
  FXmlVersion := '';
  FEncoding   := '';
  FStandalone := FALSE;
  FRootName   := '';
  FDtdcFinal  := NIL;
  FNormalize  := TRUE;
  Elements.Clear;
  Entities.Clear;
  ParEntities.Clear;
  Notations.Clear;
  CurAttr.Clear;
  EntityStack.Clear;
END;


FUNCTION  TXmlParser.LoadFromFile (Filename : STRING; FileMode : INTEGER = fmOpenRead OR fmShareDenyNone) : BOOLEAN;
          // Loads Document from given file
          // Returns TRUE if successful
VAR
  f           : FILE;
  ReadIn      : INTEGER;
  OldFileMode : INTEGER;
BEGIN
  Result := FALSE;
  Clear;

  // --- Open File
  OldFileMode := SYSTEM.FileMode;
  TRY
    SYSTEM.FileMode := FileMode;
    TRY
      AssignFile (f, Filename);
      Reset (f, 1);
    EXCEPT
      EXIT;
      END;

    TRY
      // --- Allocate Memory
      TRY
        FBufferSize := Filesize (f) + 1;
        GetMem (FBuffer, FBufferSize);
      EXCEPT
        Clear;
        EXIT;
        END;

      // --- Read File
      TRY
        BlockRead (f, FBuffer^, FBufferSize, ReadIn);
        (FBuffer+ReadIn)^ := #0;  // NULL termination
      EXCEPT
        Clear;
        EXIT;
        END;
    FINALLY
      CloseFile (f);
      END;

    FSource := Filename;
    Result  := TRUE;

  FINALLY
    SYSTEM.FileMode := OldFileMode;
    END;
END;


FUNCTION  TXmlParser.LoadFromBuffer (Buffer : PChar) : BOOLEAN;
          // Loads Document from another buffer
          // Returns TRUE if successful
          // The "Source" property becomes '<MEM>' if successful
BEGIN
  Result := FALSE;
  Clear;
  FBufferSize := StrLen (Buffer) + 1;
  TRY
    GetMem (FBuffer, FBufferSize);
  EXCEPT
    Clear;
    EXIT;
    END;
  StrCopy (FBuffer, Buffer);
  FSource := '<MEM>';
  Result := TRUE;
END;


PROCEDURE TXmlParser.SetBuffer (Buffer : PChar);      // References another buffer
BEGIN
  Clear;
  FBuffer     := Buffer;
  FBufferSize := 0;
  FSource := '<REFERENCE>';
END;


//-----------------------------------------------------------------------------------------------
// Scanning through the document
//-----------------------------------------------------------------------------------------------

PROCEDURE TXmlParser.StartScan;
BEGIN
  CurPartType := ptNone;
  CurName     := '';
  CurContent  := '';
  CurStart    := NIL;
  CurFinal    := NIL;
  CurAttr.Clear;
  EntityStack.Clear;
END;


FUNCTION  TXmlParser.Scan : BOOLEAN;
          // Scans the next Part
          // Returns TRUE if a part could be found, FALSE if there is no part any more
          //
          // "IsDone" can be set to FALSE by AnalyzeText in order to go to the next part
          // if there is no Content due to normalization
VAR
  IsDone : BOOLEAN;
BEGIN
  REPEAT
    IsDone := TRUE;

    // --- Start of next Part
    IF CurStart = NIL
      THEN CurStart := DocBuffer
      ELSE CurStart := CurFinal+1;
    CurFinal := CurStart;

    // --- End of Document of Pop off a new part from the Entity stack?
    IF CurStart^ = #0 THEN
      CurStart := EntityStack.Pop;

    // --- No Document or End Of Document: Terminate Scan
    IF (CurStart = NIL) OR (CurStart^ = #0) THEN BEGIN
      CurStart := StrEnd (DocBuffer);
      CurFinal := CurStart-1;
      EntityStack.Clear;
      Result   := FALSE;
      EXIT;
      END;

    IF (StrLComp (CurStart, '<?xml', 5) = 0) AND
       ((CurStart+5)^ IN CWhitespace) THEN AnalyzeProlog                                      // XML Declaration, Text Declaration
    ELSE IF StrLComp (CurStart, '<?',        2) = 0 THEN AnalyzePI (CurStart, CurFinal)       // PI
    ELSE IF StrLComp (CurStart, '<!--',      4) = 0 THEN AnalyzeComment (CurStart, CurFinal)  // Comment
    ELSE IF StrLComp (CurStart, '<!DOCTYPE', 9) = 0 THEN AnalyzeDtdc                          // DTDc
    ELSE IF StrLComp (CurStart, CDStart, Length (CDStart)) = 0 THEN AnalyzeCdata              // CDATA Section
    ELSE IF StrLComp (CurStart, '<',         1) = 0 THEN AnalyzeTag                           // Start-Tag, End-Tag, Empty-Element-Tag
    ELSE AnalyzeText (IsDone);                                                                // Text Content
  UNTIL IsDone;
  Result := TRUE;
END;


PROCEDURE TXmlParser.AnalyzeProlog;
          // Analyze XML Prolog or Text Declaration
VAR
  F : PChar;
BEGIN
  CurAttr.Analyze (CurStart+5, F);
  IF EntityStack.Count = 0 THEN BEGIN
    FXmlVersion := CurAttr.Value ('version');
    FEncoding   := CurAttr.Value ('encoding');
    FStandalone := CurAttr.Value ('standalone') = 'yes';
    END;
  CurFinal := StrPos (F, '?>');
  IF CurFinal <> NIL
    THEN INC (CurFinal)
    ELSE CurFinal := StrEnd (CurStart)-1;
  FCurEncoding := AnsiUpperCase (CurAttr.Value ('encoding'));
  IF FCurEncoding = '' THEN
    FCurEncoding := 'UTF-8';   // Default XML Encoding is UTF-8
  CurPartType  := ptXmlProlog;
  CurName      := '';
  CurContent   := '';
END;


PROCEDURE TXmlParser.AnalyzeComment (Start : PChar; VAR Final : PChar);
          // Analyze Comments
BEGIN
  Final := StrPos (Start+4, '-->');
  IF Final = NIL
    THEN Final := StrEnd (Start)-1
    ELSE INC (Final, 2);
  CurPartType := ptComment;
END;


PROCEDURE TXmlParser.AnalyzePI (Start : PChar; VAR Final : PChar);
          // Analyze Processing Instructions (PI)
          // This is also called for Character
VAR
  F : PChar;
BEGIN
  CurPartType := ptPI;
  Final := StrPos (Start+2, '?>');
  IF Final = NIL
    THEN Final := StrEnd (Start)-1
    ELSE INC (Final);
  ExtractName (Start+2, CWhitespace + ['?', '>'], F);
  SetStringSF (CurName, Start+2, F);
  SetStringSF (CurContent, F+1, Final-2);
  CurAttr.Analyze (F+1, F);
END;


PROCEDURE TXmlParser.AnalyzeDtdc;
          (* Analyze Document Type Declaration
                 doctypedecl  ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
                 markupdecl   ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
                 PEReference  ::= '%' Name ';'

                 elementdecl  ::= '<!ELEMENT' S Name S contentspec S?                    '>'
                 AttlistDecl  ::= '<!ATTLIST' S Name AttDef* S?                          '>'
                 EntityDecl   ::= '<!ENTITY' S Name S EntityDef S?                       '>' |
                                  '<!ENTITY' S '%' S Name S PEDef S?                     '>'
                 NotationDecl ::= '<!NOTATION' S Name S (ExternalID |  PublicID) S?      '>'
                 PI           ::=  '<?' PITarget (S (Char* - (Char* '?>' Char* )))?     '?>'
                 Comment      ::= '<!--' ((Char - '-') | ('-' (Char - '-')))*          '-->'  *)
TYPE
  TPhase = (phName, phDtd, phInternal, phFinishing);
VAR
  Phase       : TPhase;
  F           : PChar;
  ExternalID  : TExternalID;
  ExternalDTD : TXmlParser;
  DER         : TDtdElementRec;
BEGIN
  DER.Start := CurStart;
  EntityStack.Clear;    // Clear stack for Parameter Entities
  CurPartType := ptDtdc;

  // --- Don't read DTDc twice
  IF FDtdcFinal <> NIL THEN BEGIN
    CurFinal := FDtdcFinal;
    EXIT;
    END;

  // --- Scan DTDc
  CurFinal := CurStart + 9;    // First char after '<!DOCTYPE'
  Phase    := phName;
  REPEAT
    CASE CurFinal^ OF
      '%' : BEGIN
              PushPE (CurFinal);
              CONTINUE;
            END;
      #0  : IF EntityStack.Count = 0 THEN
              BREAK
            ELSE BEGIN
              CurFinal := EntityStack.Pop;
              CONTINUE;
              END;
      '[' : BEGIN
              Phase := phInternal;
              AnalyzeDtdElements (CurFinal+1, CurFinal);
              CONTINUE;
            END;
      ']' : Phase := phFinishing;
      '>' : BREAK;
      ELSE  IF NOT (CurFinal^ IN CWhitespace) THEN BEGIN
              CASE Phase OF
                phName : IF (CurFinal^ IN CNameStart)  THEN BEGIN
                           ExtractName (CurFinal, CWhitespace + ['[', '>'], F);
                           SetStringSF (FRootName, CurFinal, F);
                           CurFinal := F;
                           Phase := phDtd;
                           END;
                phDtd  : IF (StrLComp (CurFinal, 'SYSTEM', 6) = 0) OR
                            (StrLComp (CurFinal, 'PUBLIC', 6) = 0) THEN BEGIN
                           ExternalID  := TExternalID.Create (CurFinal);
                           ExternalDTD := LoadExternalEntity (ExternalId.SystemId, ExternalID.PublicId, '');
                           F := StrPos (ExternalDtd.DocBuffer, '<!');
                           IF F <> NIL THEN
                             AnalyzeDtdElements (F, F);
                           ExternalDTD.Free;
                           CurFinal := ExternalID.Final;
                           ExternalID.Free;
                           END;
                ELSE     BEGIN
                           DER.ElementType := deError;
                           DER.Pos         := CurFinal;
                           DER.Final       := CurFinal;
                           DtdElementFound (DER);
                         END;
                END;

              END;
      END;
    INC (CurFinal);
  UNTIL FALSE;

  CurPartType := ptDtdc;
  CurName     := '';
  CurContent  := '';

  // It is an error in the document if "EntityStack" is not empty now
  IF EntityStack.Count > 0 THEN BEGIN
    DER.ElementType := deError;
    DER.Final       := CurFinal;
    DER.Pos         := CurFinal;
    DtdElementFound (DER);
    END;

  EntityStack.Clear;    // Clear stack for General Entities
  FDtdcFinal := CurFinal;
END;


PROCEDURE TXmlParser.AnalyzeDtdElements (Start : PChar; VAR Final : PChar);
          // Analyze the "Elements" of a DTD contained in the external or
          // internal DTD subset.
VAR
  DER : TDtdElementRec;
BEGIN
  Final := Start;
  REPEAT
    CASE Final^ OF
      '%' : BEGIN
              PushPE (Final);
              CONTINUE;
            END;
      #0  : IF EntityStack.Count = 0 THEN
              BREAK
            ELSE BEGIN
              CurFinal := EntityStack.Pop;
              CONTINUE;
              END;
      ']',
      '>' : BREAK;
      '<' : IF      StrLComp (Final, '<!ELEMENT',   9) = 0 THEN AnalyzeElementDecl  (Final, Final)
            ELSE IF StrLComp (Final, '<!ATTLIST',   9) = 0 THEN AnalyzeAttListDecl  (Final, Final)
            ELSE IF StrLComp (Final, '<!ENTITY',    8) = 0 THEN AnalyzeEntityDecl   (Final, Final)
            ELSE IF StrLComp (Final, '<!NOTATION', 10) = 0 THEN AnalyzeNotationDecl (Final, Final)
            ELSE IF StrLComp (Final, '<?',          2) = 0 THEN BEGIN   // PI in DTD
              DER.ElementType := dePI;
              DER.Start       := Final;
              AnalyzePI (Final, Final);
              DER.Target      := PChar (CurName);
              DER.Content     := PChar (CurContent);
              DER.AttrList    := CurAttr;
              DER.Final       := Final;
              DtdElementFound (DER);
              END
            ELSE IF StrLComp (Final, '<!--', 4) = 0 THEN BEGIN   // Comment in DTD
              DER.ElementType := deComment;
              DER.Start       := Final;
              AnalyzeComment  (Final, Final);
              DER.Final       := Final;
              DtdElementFound (DER);
              END
            ELSE BEGIN
              DER.ElementType := deError;
              DER.Start       := Final;
              DER.Pos         := Final;
              DER.Final       := Final;
              DtdElementFound (DER);
              END;

      END;
    INC (Final);
  UNTIL FALSE;
END;


PROCEDURE TXmlParser.AnalyzeTag;
          // Analyze Tags
VAR
  S, F    : PChar;
  Attr    : TAttr;
  ElemDef : TElemDef;
  AttrDef : TAttrDef;
  I       : INTEGER;
BEGIN
  CurPartType := ptStartTag;
  S := CurStart+1;
  IF S^ = '/' THEN BEGIN
    CurPartType := ptEndTag;
    INC (S);
    END;
  ExtractName (S, CWhitespace + ['/'], F);
  SetStringSF (CurName, S, F);
  CurAttr.Analyze (F+1, CurFinal);
  IF CurFinal^ = '/' THEN BEGIN
    CurPartType := ptEmptyTag;
    END;
  CurFinal := StrScanE (CurFinal, '>');

  // --- Set Default Attribute values for nonexistent attributes
  IF (CurPartType = ptStartTag) OR (CurPartType = ptEmptyTag) THEN BEGIN
    ElemDef := Elements.Node (CurName);
    IF ElemDef <> NIL THEN BEGIN
      FOR I := 0 TO ElemDef.Count-1 DO BEGIN
        AttrDef := TAttrDef (ElemDef [I]);
        Attr := TAttr (CurAttr.Node (AttrDef.Name));
        IF (Attr = NIL) AND (AttrDef.Value <> '') THEN BEGIN
          Attr           := TAttr.Create (AttrDef.Name, AttrDef.Value);
          Attr.ValueType := vtDefault;
          CurAttr.Add (Attr);
          END;
        IF Attr <> NIL THEN BEGIN
          CASE AttrDef.DefaultType OF
            adDefault  : ;
            adRequired : ; // -!- It is an error in the document if "Attr.Value" is an empty string
            adImplied  : Attr.ValueType := vtImplied;
            adFixed    : BEGIN
                           Attr.ValueType := vtFixed;
                           Attr.Value     := AttrDef.Value;
                         END;
            END;
          Attr.AttrType := AttrDef.AttrType;
          END;
        END;
      END;

    // --- Normalize Attribute Values. XmlSpec:
           // - a character reference is processed by appending the referenced character to the attribute value
           // - an entity reference is processed by recursively processing the replacement text of the entity
           // - a whitespace character (#x20, #xD, #xA, #x9) is processed by appending #x20 to the normalized value,
           //   except that only a single #x20 is appended for a "#xD#xA" sequence that is part of an external
           //   parsed entity or the literal entity value of an internal parsed entity
           // - other characters are processed by appending them to the normalized value
           // If the declared value is not CDATA, then the XML processor must further process the
           // normalized attribute value by discarding any leading and trailing space (#x20) characters,
           // and by replacing sequences of space (#x20) characters by a single space (#x20) character.
           // All attributes for which no declaration has been read should be treated by a
           // non-validating parser as if declared CDATA.
           // !!! The XML 1.0 SE specification is somewhat different here
           //     This code does not conform exactly to this specification
    FOR I := 0 TO CurAttr.Count-1 DO
      WITH TAttr (CurAttr [I]) DO BEGIN
        ReplaceGeneralEntities   (Value);
        ReplaceCharacterEntities (Value);
        IF (AttrType <> atCData) AND (AttrType <> atUnknown)
          THEN Value := TranslateEncoding (TrimWs (ConvertWs (Value, TRUE)))
          ELSE Value := TranslateEncoding (ConvertWs (Value, FALSE));
        END;
    END;
END;


PROCEDURE TXmlParser.AnalyzeCData;
          // Analyze CDATA Sections
BEGIN
  CurPartType := ptCData;
  CurFinal := StrPos (CurStart, CDEnd);
  IF CurFinal = NIL THEN BEGIN
    CurFinal   := StrEnd (CurStart)-1;
    CurContent := TranslateEncoding (StrPas (CurStart+Length (CDStart)));
    END
  ELSE BEGIN
    SetStringSF (CurContent, CurStart+Length (CDStart), CurFinal-1);
    INC (CurFinal, Length (CDEnd)-1);
    CurContent := TranslateEncoding (CurContent);
    END;
END;


PROCEDURE TXmlParser.AnalyzeText (VAR IsDone : BOOLEAN);
          (* Analyzes Text Content between Tags. CurFinal will point to the last content character.
             Content ends at a '<' character or at the end of the document.
             Entity References and Character Entity references are resolved.
             If PackSpaces is TRUE, contiguous Whitespace Characters will be compressed to
             one Space #x20 character, Whitespace at the beginning and end of content will
             be trimmed off and content which is or becomes empty is not returned to
             the application (in this case, "IsDone" is set to FALSE which causes the
             Scan method to proceed directly to the next part. *)

  PROCEDURE ProcessEntity;
            (* Is called if there is an ampsersand '&' character found in the document.
               IN  "CurFinal" points to the ampersand
               OUT "CurFinal" points to the first character after the semi-colon ';' *)
  VAR
    P              : PChar;
    Name           : STRING;
    EntityDef      : TEntityDef;
    ExternalEntity : TXmlParser;
  BEGIN
    P := StrScan (CurFinal , ';');
    IF P <> NIL THEN BEGIN
      SetStringSF (Name, CurFinal+1, P-1);

      // Is it a Character Entity?
      IF (CurFinal+1)^ = '#' THEN BEGIN
        IF UpCase ((CurFinal+2)^) = 'X'       // !!! Can't use "CHR" for Unicode characters > 255:
          THEN CurContent := CurContent + CHR (StrToIntDef ('$'+Copy (Name, 3, MaxInt), 32))
          ELSE CurContent := CurContent + CHR (StrToIntDef (Copy (Name, 2, MaxInt), 32));
        CurFinal := P+1;
        EXIT;
        END

      // Is it a Predefined Entity?
      ELSE IF Name = 'lt'   THEN BEGIN CurContent := CurContent + '<';  CurFinal := P+1; EXIT; END
      ELSE IF Name = 'gt'   THEN BEGIN CurContent := CurContent + '>';  CurFinal := P+1; EXIT; END
      ELSE IF Name = 'amp'  THEN BEGIN CurContent := CurContent + '&';  CurFinal := P+1; EXIT; END
      ELSE IF Name = 'apos' THEN BEGIN CurContent := CurContent + ''''; CurFinal := P+1; EXIT; END
      ELSE IF Name = 'quot' THEN BEGIN CurContent := CurContent + '"';  CurFinal := P+1; EXIT; END;

      // Replace with Entity from DTD
      EntityDef := TEntityDef (Entities.Node (Name));
      IF EntityDef <> NIL THEN BEGIN
        IF EntityDef.Value <> '' THEN BEGIN
          EntityStack.Push (P+1);
          CurFinal := PChar (EntityDef.Value);
          END
        ELSE BEGIN
          ExternalEntity := LoadExternalEntity (EntityDef.SystemId, EntityDef.PublicId, EntityDef.NotationName);
          EntityStack.Push (ExternalEntity, P+1);
          CurFinal := ExternalEntity.DocBuffer;
          END;
        END
      ELSE BEGIN
        CurContent := CurContent + Name;
        CurFinal   := P+1;
        END;
      END
    ELSE BEGIN
      INC (CurFinal);
      END;
  END;

VAR
  C  : INTEGER;
BEGIN
  CurFinal    := CurStart;
  CurPartType := ptContent;
  CurContent  := '';
  C           := 0;
  REPEAT
    CASE CurFinal^ OF
      '&' : BEGIN
              CurContent := CurContent + TranslateEncoding (StrLPas (CurFinal-C, C));
              C := 0;
              ProcessEntity;
              CONTINUE;
            END;
      #0  : BEGIN
              IF EntityStack.Count = 0 THEN
                BREAK
              ELSE BEGIN
                CurContent := CurContent + TranslateEncoding (StrLPas (CurFinal-C, C));
                C := 0;
                CurFinal := EntityStack.Pop;
                CONTINUE;
                END;
            END;
      '<' : BREAK;
      ELSE INC (C);
      END;
    INC (CurFinal);
  UNTIL FALSE;
  CurContent := CurContent + TranslateEncoding (StrLPas (CurFinal-C, C));
  DEC (CurFinal);

  IF FNormalize THEN BEGIN
    CurContent := ConvertWs (TrimWs (CurContent), TRUE);
    IsDone     := CurContent <> '';    // IsDone will only get FALSE if PackSpaces is TRUE
    END;
END;


PROCEDURE TXmlParser.AnalyzeElementDecl  (Start : PChar; VAR Final : PChar);
          (* Parse <!ELEMENT declaration starting at "Start"
             Final must point to the terminating '>' character
             XmlSpec 3.2:
                 elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
                 contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
                 Mixed       ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'   |
                                 '(' S? '#PCDATA' S? ')'
                 children    ::= (choice | seq) ('?' | '*' | '+')?
                 choice      ::= '(' S? cp ( S? '|' S? cp )* S? ')'
                 cp          ::= (Name | choice | seq) ('?' | '*' | '+')?
                 seq         ::= '(' S? cp ( S? ',' S? cp )* S? ')'

             More simply:
                 contentspec ::= EMPTY
                                 ANY
                                 '(#PCDATA)'
                                 '(#PCDATA | A | B)*'
                                 '(A, B, C)'
                                 '(A | B | C)'
                                 '(A?, B*, C+),
                                 '(A, (B | C | D)* )'                       *)
VAR
  Element : TElemDef;
  Elem2   : TElemDef;
  F       : PChar;
  DER     : TDtdElementRec;
BEGIN
  Element   := TElemDef.Create;
  Final     := Start + 9;
  DER.Start := Start;
  REPEAT
    IF Final^ = '>' THEN BREAK;
    IF (Final^ IN CNameStart) AND (Element.Name = '') THEN BEGIN
      ExtractName (Final, CWhitespace, F);
      SetStringSF (Element.Name, Final, F);
      Final := F;
      F := StrScan (Final+1, '>');
      IF F = NIL THEN BEGIN
        Element.Definition := STRING (Final);
        Final := StrEnd (Final);
        BREAK;
        END
      ELSE BEGIN
        SetStringSF (Element.Definition, Final+1, F-1);
        Final := F;
        BREAK;
        END;
      END;
    INC (Final);
  UNTIL FALSE;
  Element.Definition := DelChars (Element.Definition, CWhitespace);
  ReplaceParameterEntities (Element.Definition);
  IF      Element.Definition = 'EMPTY' THEN Element.ElemType := etEmpty
  ELSE IF Element.Definition = 'ANY'   THEN Element.ElemType := etAny
  ELSE IF Copy (Element.Definition, 1, 8) = '(#PCDATA' THEN Element.ElemType := etMixed
  ELSE IF Copy (Element.Definition, 1, 1) = '('        THEN Element.ElemType := etChildren
  ELSE Element.ElemType := etAny;

  Elem2 := Elements.Node (Element.Name);
  IF Elem2 <> NIL THEN
    Elements.Delete (Elements.IndexOf (Elem2));
  Elements.Add (Element);
  Final := StrScanE (Final, '>');
  DER.ElementType := deElement;
  DER.ElemDef  := Element;
  DER.Final    := Final;
  DtdElementFound (DER);
END;


PROCEDURE TXmlParser.AnalyzeAttListDecl  (Start : PChar; VAR Final : PChar);
          (* Parse <!ATTLIST declaration starting at "Start"
             Final must point to the terminating '>' character
             XmlSpec 3.3:
                 AttlistDecl    ::= '<!ATTLIST' S Name AttDef* S? '>'
                 AttDef         ::= S Name S AttType S DefaultDecl
                 AttType        ::= StringType | TokenizedType | EnumeratedType
                 StringType     ::= 'CDATA'
                 TokenizedType  ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
                 EnumeratedType ::= NotationType | Enumeration
                 NotationType   ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
                 Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
                 DefaultDecl    ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
                 AttValue       ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
            Examples:
                 <!ATTLIST address
                           A1 CDATA "Default"
                           A2 ID    #REQUIRED
                           A3 IDREF #IMPLIED
                           A4 IDREFS #IMPLIED
                           A5 ENTITY #FIXED "&at;&#252;"
                           A6 ENTITIES #REQUIRED
                           A7 NOTATION (WMF | DXF) "WMF"
                           A8 (A | B | C) #REQUIRED>                *)
TYPE
  TPhase = (phElementName, phName, phType, phNotationContent, phDefault);
VAR
  Phase       : TPhase;
  F           : PChar;
  ElementName : STRING;
  ElemDef     : TElemDef;
  AttrDef     : TAttrDef;
  AttrDef2    : TAttrDef;
  Strg        : STRING;
  DER         : TDtdElementRec;
BEGIN
  Final     := Start + 9;   // The character after <!ATTLIST
  Phase     := phElementName;
  DER.Start := Start;
  AttrDef   := NIL;
  ElemDef   := NIL;
  REPEAT
    IF NOT (Final^ IN CWhitespace) THEN
      CASE Final^ OF
        '%' : BEGIN
                PushPE (Final);
                CONTINUE;
              END;
        #0  : IF EntityStack.Count = 0 THEN
                BREAK
              ELSE BEGIN
                Final := EntityStack.Pop;
                CONTINUE;
                END;
        '>' : BREAK;
        ELSE  CASE Phase OF
                phElementName     : BEGIN
                                      ExtractName (Final, CWhitespace + CQuoteChar + ['#'], F);
                                      SetStringSF (ElementName, Final, F);
                                      Final := F;
                                      ElemDef := Elements.Node (ElementName);
                                      IF ElemDef = NIL THEN BEGIN
                                        ElemDef := TElemDef.Create;
                                        ElemDef.Name       := ElementName;
                                        ElemDef.Definition := 'ANY';
                                        ElemDef.ElemType   := etAny;
                                        Elements.Add (ElemDef);
                                        END;
                                      Phase := phName;
                                    END;
                phName            : BEGIN
                                      AttrDef := TAttrDef.Create;
                                      ExtractName (Final, CWhitespace + CQuoteChar + ['#'], F);
                                      SetStringSF (AttrDef.Name, Final, F);
                                      Final := F;
                                      AttrDef2 := TAttrDef (ElemDef.Node (AttrDef.Name));
                                      IF AttrDef2 <> NIL THEN
                                        ElemDef.Delete (ElemDef.IndexOf (AttrDef2));
                                      ElemDef.Add (AttrDef);
                                      Phase := phType;
                                    END;
                phType            : BEGIN
                                      IF Final^ = '(' THEN BEGIN
                                        F := StrScan (Final+1, ')');
                                        IF F <> NIL
                                          THEN SetStringSF (AttrDef.TypeDef, Final+1, F-1)
                                          ELSE AttrDef.TypeDef := STRING (Final+1);
                                        AttrDef.TypeDef := DelChars (AttrDef.TypeDef, CWhitespace);
                                        AttrDef.AttrType := atEnumeration;
                                        ReplaceParameterEntities (AttrDef.TypeDef);
                                        ReplaceCharacterEntities (AttrDef.TypeDef);
                                        Phase := phDefault;
                                        END
                                      ELSE IF StrLComp (Final, 'NOTATION', 8) = 0 THEN BEGIN
                                        INC (Final, 8);
                                        AttrDef.AttrType := atNotation;
                                        Phase := phNotationContent;
                                        END
                                      ELSE BEGIN
                                        ExtractName (Final, CWhitespace+CQuoteChar+['#'], F);
                                        SetStringSF (AttrDef.TypeDef, Final, F);
                                        IF      AttrDef.TypeDef = 'CDATA'    THEN AttrDef.AttrType := atCData
                                        ELSE IF AttrDef.TypeDef = 'ID'       THEN AttrDef.AttrType := atId
                                        ELSE IF AttrDef.TypeDef = 'IDREF'    THEN AttrDef.AttrType := atIdRef
                                        ELSE IF AttrDef.TypeDef = 'IDREFS'   THEN AttrDef.AttrType := atIdRefs
                                        ELSE IF AttrDef.TypeDef = 'ENTITY'   THEN AttrDef.AttrType := atEntity
                                        ELSE IF AttrDef.TypeDef = 'ENTITIES' THEN AttrDef.AttrType := atEntities
                                        ELSE IF AttrDef.TypeDef = 'NMTOKEN'  THEN AttrDef.AttrType := atNmToken
                                        ELSE IF AttrDef.TypeDef = 'NMTOKENS' THEN AttrDef.AttrType := atNmTokens;
                                        Phase := phDefault;
                                        END
                                    END;
                phNotationContent : BEGIN
                                      F := StrScan (Final, ')');
                                      IF F <> NIL THEN
                                        SetStringSF (AttrDef.Notations, Final+1, F-1)
                                      ELSE BEGIN
                                        AttrDef.Notations := STRING (Final+1);
                                        Final := StrEnd (Final);
                                        END;
                                      ReplaceParameterEntities (AttrDef.Notations);
                                      AttrDef.Notations := DelChars (AttrDef.Notations, CWhitespace);
                                      Phase := phDefault;
                                    END;
                phDefault :         BEGIN
                                      IF Final^ = '#' THEN BEGIN
                                        ExtractName (Final, CWhiteSpace + CQuoteChar, F);
                                        SetStringSF (Strg, Final, F);
                                        Final := F;
                                        ReplaceParameterEntities (Strg);
                                        IF      Strg = '#REQUIRED' THEN BEGIN AttrDef.DefaultType := adRequired; Phase := phName; END
                                        ELSE IF Strg = '#IMPLIED'  THEN BEGIN AttrDef.DefaultType := adImplied;  Phase := phName; END
                                        ELSE IF Strg = '#FIXED'    THEN       AttrDef.DefaultType := adFixed;
                                        END
                                      ELSE IF (Final^ IN CQuoteChar) THEN BEGIN
                                        ExtractQuote (Final, AttrDef.Value, Final);
                                        ReplaceParameterEntities (AttrDef.Value);
                                        ReplaceCharacterEntities (AttrDef.Value);
                                        Phase := phName;
                                        END;
                                      IF Phase = phName THEN BEGIN
                                        AttrDef := NIL;
                                        END;
                                    END;

                END;
        END;
    INC (Final);
  UNTIL FALSE;

  Final := StrScan (Final, '>');

  DER.ElementType := deAttList;
  DER.ElemDef  := ElemDef;
  DER.Final    := Final;
  DtdElementFound (DER);
END;


PROCEDURE TXmlParser.AnalyzeEntityDecl   (Start : PChar; VAR Final : PChar);
          (* Parse <!ENTITY declaration starting at "Start"
             Final must point to the terminating '>' character
             XmlSpec 4.2:
                 EntityDecl  ::= '<!ENTITY' S Name S EntityDef S? '>'   |
                                 '<!ENTITY' S '%' S Name S PEDef S? '>'
                 EntityDef   ::= EntityValue | (ExternalID NDataDecl?)
                 PEDef       ::= EntityValue | ExternalID
                 NDataDecl   ::= S 'NDATA' S Name
                 EntityValue ::= '"' ([^%&"] | PEReference | EntityRef | CharRef)* '"'   |
                                 "'" ([^%&'] | PEReference | EntityRef | CharRef)* "'"
                 PEReference ::= '%' Name ';'

             Examples
                 <!ENTITY test1 "Stefan Heymann">                   <!-- Internal, general, parsed              -->
                 <!ENTITY test2 SYSTEM "ent2.xml">                  <!-- External, general, parsed              -->
                 <!ENTITY test2 SYSTEM "ent3.gif" NDATA gif>        <!-- External, general, unparsed            -->
                 <!ENTITY % test3 "<!ELEMENT q ANY>">               <!-- Internal, parameter                    -->
                 <!ENTITY % test6 SYSTEM "ent6.xml">                <!-- External, parameter                    -->
                 <!ENTITY test4 "&test1; ist lieb">                 <!-- IGP, Replacement text <> literal value -->
                 <!ENTITY test5 "<p>Dies ist ein Test-Absatz</p>">  <!-- IGP, See XmlSpec 2.4                   -->
          *)
TYPE
  TPhase = (phName, phContent, phNData, phNotationName, phFinalGT);
VAR
  Phase         : TPhase;
  IsParamEntity : BOOLEAN;
  F             : PChar;
  ExternalID    : TExternalID;
  EntityDef     : TEntityDef;
  EntityDef2    : TEntityDef;
  DER           : TDtdElementRec;
BEGIN
  Final         := Start + 8;   // First char after <!ENTITY
  DER.Start     := Start;
  Phase         := phName;
  IsParamEntity := FALSE;
  EntityDef     := TEntityDef.Create;
  REPEAT
    IF NOT (Final^ IN CWhitespace) THEN
      CASE Final^ OF
        '%' : IsParamEntity := TRUE;
        '>' : BREAK;
        ELSE  CASE Phase OF
                phName         : IF Final^ IN CNameStart THEN BEGIN
                                   ExtractName (Final, CWhitespace + CQuoteChar, F);
                                   SetStringSF (EntityDef.Name, Final, F);
                                   Final := F;
                                   Phase := phContent;
                                   END;
                phContent      : IF Final^ IN CQuoteChar THEN BEGIN
                                   ExtractQuote (Final, EntityDef.Value, Final);
                                   Phase := phFinalGT;
                                   END
                                 ELSE IF (StrLComp (Final, 'SYSTEM', 6) = 0) OR
                                         (StrLComp (Final, 'PUBLIC', 6) = 0) THEN BEGIN
                                   ExternalID := TExternalID.Create (Final);
                                   EntityDef.SystemId := ExternalID.SystemId;
                                   EntityDef.PublicId := ExternalID.PublicId;
                                   Final      := ExternalID.Final;
                                   Phase      := phNData;
                                   ExternalID.Free;
                                   END;
                phNData        : IF StrLComp (Final, 'NDATA', 5) = 0 THEN BEGIN
                                   INC (Final, 4);
                                   Phase := phNotationName;
                                   END;
                phNotationName : IF Final^ IN CNameStart THEN BEGIN
                                   ExtractName (Final, CWhitespace + ['>'], F);
                                   SetStringSF (EntityDef.NotationName, Final, F);
                                   Final := F;
                                   Phase := phFinalGT;
                                   END;
                phFinalGT      : ; // -!- There is an error in the document if this branch is called
                END;
        END;
    INC (Final);
  UNTIL FALSE;
  IF IsParamEntity THEN BEGIN
    EntityDef2 := TEntityDef (ParEntities.Node (EntityDef.Name));
    IF EntityDef2 <> NIL THEN
      ParEntities.Delete (ParEntities.IndexOf (EntityDef2));
    ParEntities.Add (EntityDef);
    ReplaceCharacterEntities (EntityDef.Value);
    END
  ELSE BEGIN
    EntityDef2 := TEntityDef (Entities.Node (EntityDef.Name));
    IF EntityDef2 <> NIL THEN
      Entities.Delete (Entities.IndexOf (EntityDef2));
    Entities.Add (EntityDef);
    ReplaceParameterEntities (EntityDef.Value);  //  Create replacement texts (see XmlSpec 4.5)
    ReplaceCharacterEntities (EntityDef.Value);
    END;
  Final := StrScanE (Final, '>');

  DER.ElementType := deEntity;
  DER.EntityDef   := EntityDef;
  DER.Final       := Final;
  DtdElementFound (DER);
END;


PROCEDURE TXmlParser.AnalyzeNotationDecl (Start : PChar; VAR Final : PChar);
          // Parse <!NOTATION declaration starting at "Start"
          // Final must point to the terminating '>' character
          // XmlSpec 4.7: NotationDecl ::=  '<!NOTATION' S Name S (ExternalID |  PublicID) S? '>'
TYPE
  TPhase = (phName, phExtId, phEnd);
VAR
  ExternalID  : TExternalID;
  Phase       : TPhase;
  F           : PChar;
  NotationDef : TNotationDef;
  DER         : TDtdElementRec;
BEGIN
  Final       := Start + 10;   // Character after <!NOTATION
  DER.Start   := Start;
  Phase       := phName;
  NotationDef := TNotationDef.Create;
  REPEAT
    IF NOT (Final^ IN CWhitespace) THEN
      CASE Final^ OF
        '>',
        #0   : BREAK;
        ELSE   CASE Phase OF
                 phName  : BEGIN
                             ExtractName (Final, CWhitespace + ['>'], F);
                             SetStringSF (NotationDef.Name, Final, F);
                             Final := F;
                             Phase := phExtId;
                           END;
                 phExtId : BEGIN
                             ExternalID := TExternalID.Create (Final);
                             NotationDef.Value    := ExternalID.SystemId;
                             NotationDef.PublicId := ExternalID.PublicId;
                             Final := ExternalId.Final;
                             ExternalId.Free;
                             Phase := phEnd;
                           END;
                 phEnd   : ;   // -!- There is an error in the document if this branch is called
                 END;
        END;
    INC (Final);
  UNTIL FALSE;
  Notations.Add (NotationDef);
  Final := StrScanE (Final, '>');

  DER.ElementType := deNotation;
  DER.NotationDef := NotationDef;
  DER.Final       := Final;
  DtdElementFound (DER);
END;


PROCEDURE TXmlParser.PushPE (VAR Start : PChar);
          (* If there is a parameter entity reference found in the data stream,
             the current position will be pushed to the entity stack.
             Start:  IN  Pointer to the '%' character starting the PE reference
                     OUT Pointer to first character of PE replacement text *)
VAR
  P         : PChar;
  EntityDef : TEntityDef;
BEGIN
  P := StrScan (Start, ';');
  IF P <> NIL THEN BEGIN
    EntityDef := TEntityDef (ParEntities.Node (StrSFPas (Start+1, P-1)));
    IF EntityDef <> NIL THEN BEGIN
      EntityStack.Push (P+1);
      Start := PChar (EntityDef.Value);
      END
    ELSE
      Start := P+1;
    END;
END;


PROCEDURE TXmlParser.ReplaceCharacterEntities (VAR Str : STRING);
          // Replaces all Character Entity References in the String
VAR
  Start  : INTEGER;
  PAmp   : PChar;
  PSemi  : PChar;
  PosAmp : INTEGER;
  Len    : INTEGER;    // Length of Entity Reference
BEGIN
  IF Str = '' THEN EXIT;
  Start := 1;
  REPEAT
    PAmp := StrPos (PChar (Str) + Start-1, '&#');
    IF PAmp = NIL THEN BREAK;
    PSemi := StrScan (PAmp+2, ';');
    IF PSemi = NIL THEN BREAK;
    PosAmp := PAmp - PChar (Str) + 1;
    Len    := PSemi-PAmp+1;
    IF CompareText (Str [PosAmp+2], 'x') = 0          // !!! Can't use "CHR" for Unicode characters > 255
      THEN Str [PosAmp] := CHR (StrToIntDef ('$'+Copy (Str, PosAmp+3, Len-4), 0))
      ELSE Str [PosAmp] := CHR (StrToIntDef (Copy (Str, PosAmp+2, Len-3), 32));
    Delete (Str, PosAmp+1, Len-1);  
    Start := PosAmp + 1;
  UNTIL FALSE;
END;


PROCEDURE TXmlParser.ReplaceParameterEntities (VAR Str : STRING);
          // Recursively replaces all Parameter Entity References in the String
  PROCEDURE ReplaceEntities (VAR Str : STRING);
  VAR
    Start   : INTEGER;
    PAmp    : PChar;
    PSemi   : PChar;
    PosAmp  : INTEGER;
    Len     : INTEGER;
    Entity  : TEntityDef;
    Repl    : STRING;        // Replacement
  BEGIN
    IF Str = '' THEN EXIT;
    Start := 1;
    REPEAT
      PAmp := StrPos (PChar (Str)+Start-1, '%');
      IF PAmp = NIL THEN BREAK;
      PSemi := StrScan (PAmp+2, ';');
      IF PSemi = NIL THEN BREAK;
      PosAmp := PAmp - PChar (Str) + 1;
      Len    := PSemi-PAmp+1;
      Entity := TEntityDef (ParEntities.Node (Copy (Str, PosAmp+1, Len-2)));
      IF Entity <> NIL THEN BEGIN
        Repl := Entity.Value;
        ReplaceEntities (Repl);    // Recursion
        END
      ELSE
        Repl := Copy (Str, PosAmp, Len);
      Delete (Str, PosAmp, Len);  
      Insert (Repl, Str, PosAmp);
      Start := PosAmp + Length (Repl);
    UNTIL FALSE;
  END;
BEGIN
  ReplaceEntities (Str);
END;


PROCEDURE TXmlParser.ReplaceGeneralEntities (VAR Str : STRING);
          // Recursively replaces General Entity References in the String
  PROCEDURE ReplaceEntities (VAR Str : STRING);
  VAR
    Start          : INTEGER;
    PAmp           : PChar;
    PSemi          : PChar;
    PosAmp         : INTEGER;
    Len            : INTEGER;
    EntityDef      : TEntityDef;
    EntName        : STRING;
    Repl           : STRING;        // Replacement
    ExternalEntity : TXmlParser;
  BEGIN
    IF Str = '' THEN EXIT;
    Start := 1;
    REPEAT
      PAmp := StrPos (PChar (Str)+Start-1, '&');
      IF PAmp = NIL THEN BREAK;
      PSemi := StrScan (PAmp+2, ';');
      IF PSemi = NIL THEN BREAK;
      PosAmp := PAmp - PChar (Str) + 1;
      Len    := PSemi-PAmp+1;
      EntName := Copy (Str, PosAmp+1, Len-2);
      IF      EntName = 'lt'   THEN Repl := '<'
      ELSE IF EntName = 'gt'   THEN Repl := '>'
      ELSE IF EntName = 'amp'  THEN Repl := '&'
      ELSE IF EntName = 'apos' THEN Repl := ''''
      ELSE IF EntName = 'quot' THEN Repl := '"'
      ELSE BEGIN
        EntityDef := TEntityDef (Entities.Node (EntName));
        IF EntityDef <> NIL THEN BEGIN
          IF EntityDef.Value <> '' THEN   // Internal Entity
            Repl := EntityDef.Value
          ELSE BEGIN                      // External Entity
            ExternalEntity := LoadExternalEntity (EntityDef.SystemId, EntityDef.PublicId, EntityDef.NotationName);
            Repl := StrPas (ExternalEntity.DocBuffer);  // !!! What if it contains a Text Declaration?
            ExternalEntity.Free;
            END;
          ReplaceEntities (Repl);    // Recursion
          END
        ELSE
          Repl := Copy (Str, PosAmp, Len);
        END;
      Delete (Str, PosAmp, Len);  
      Insert (Repl, Str, PosAmp);
      Start := PosAmp + Length (Repl);
    UNTIL FALSE;
  END;
BEGIN
  ReplaceEntities (Str);
END;


FUNCTION  TXmlParser.LoadExternalEntity (SystemId, PublicId, Notation : STRING) : TXmlParser;
          // This will be called whenever there is a Parsed External Entity or
          // the DTD External Subset to be parsed.
          // It has to create a TXmlParser instance and load the desired Entity.
          // This instance of LoadExternalEntity assumes that "SystemId" is a valid
          // file name (relative to the Document source) and loads this file using
          // the LoadFromFile method.
VAR
  Filename : STRING;
BEGIN
  // --- Convert System ID to complete filename
  Filename := StringReplace (SystemId, '/', '\', [rfReplaceAll]);
  IF Copy (FSource, 1, 1) <> '<' THEN
    IF (Copy (Filename, 1, 2) = '\\') OR (Copy (Filename, 2, 1) = ':') THEN
      // Already has an absolute Path
    ELSE BEGIN
      Filename := ExtractFilePath (FSource) + Filename;
      END;

  // --- Load the File
  Result := TXmlParser.Create;
  Result.LoadFromFile (Filename);
END;


FUNCTION  TXmlParser.TranslateEncoding  (CONST Source : STRING) : STRING;
          // The member variable "CurEncoding" always holds the name of the current
          // encoding, e.g. 'UTF-8' or 'ISO-8859-1'.
          // This virtual method "TranslateEncoding" is responsible for translating
          // the content passed in the "Source" parameter to the Encoding which
          // is expected by the application.
          // This instance of "TranlateEncoding" assumes that the Application expects
          // Windows ANSI (Win1252) strings. It is able to transform UTF-8 or ISO-8859-1
          // encodings.
          // If you want your application to understand or create other encodings, you
          // override this function.
BEGIN
  IF CurEncoding = 'UTF-8'
    THEN Result := Utf8ToAnsi (Source)
    ELSE Result := Source;
END;


PROCEDURE TXmlParser.DtdElementFound (DtdElementRec : TDtdElementRec);
          // This method is called for every element which is found in the DTD
          // declaration. The variant record TDtdElementRec is passed which
          // holds informations about the element.
          // You can override this function to handle DTD declarations.
          // Note that when you parse the same Document instance a second time,
          // the DTD will not get parsed again.
BEGIN
END;


FUNCTION TXmlParser.GetDocBuffer: PChar;
         // Returns FBuffer or a pointer to a NUL char if Buffer is empty
BEGIN
  IF FBuffer = NIL
    THEN Result := #0
    ELSE Result := FBuffer;
END;


(*$IFNDEF HAS_CONTNRS_UNIT
===============================================================================================
TObjectList
===============================================================================================
*)

DESTRUCTOR TObjectList.Destroy;
BEGIN
  Clear;
  SetCapacity(0);
  INHERITED Destroy;
END;


PROCEDURE TObjectList.Delete (Index : INTEGER);
BEGIN
  IF (Index < 0) OR (Index >= Count) THEN EXIT;
  TObject (Items [Index]).Free;
  INHERITED Delete (Index);
END;


PROCEDURE TObjectList.Clear;
BEGIN
  WHILE Count > 0 DO
    Delete (Count-1);
END;

(*$ENDIF *)

(*
===============================================================================================
TNvpNode
--------
Node base class for the TNvpList
===============================================================================================
*)

CONSTRUCTOR TNvpNode.Create (TheName, TheValue : STRING);
BEGIN
  INHERITED Create;
  Name  := TheName;
  Value := TheValue;
END;


(*
===============================================================================================
TNvpList
--------
A generic List of Name-Value Pairs, based on the TObjectList introduced in Delphi 5
===============================================================================================
*)

PROCEDURE TNvpList.Add (Node : TNvpNode);
VAR
  I : INTEGER;
BEGIN
  FOR I := Count-1 DOWNTO 0 DO
    IF Node.Name > TNvpNode (Items [I]).Name THEN BEGIN
      Insert (I+1, Node);
      EXIT;
      END;
  Insert (0, Node);
END;



FUNCTION  TNvpList.Node (Name : STRING) : TNvpNode;
          // Binary search for Node
VAR
  L, H : INTEGER;    // Low, High Limit
  T, C : INTEGER;    // Test Index, Comparison result
  Last : INTEGER;    // Last Test Index
BEGIN
  IF Count=0 THEN BEGIN
    Result := NIL;
    EXIT;
    END;

  L    := 0;
  H    := Count;
  Last := -1;
  REPEAT
    T := (L+H) DIV 2;
    IF T=Last THEN BREAK;
    Result := TNvpNode (Items [T]);
    C := CompareStr (Result.Name, Name);
    IF      C = 0 THEN EXIT
    ELSE IF C < 0 THEN L := T
    ELSE               H := T;
    Last := T;
  UNTIL FALSE;
  Result := NIL;
END;


FUNCTION  TNvpList.Node (Index : INTEGER) : TNvpNode;
BEGIN
  IF (Index < 0) OR (Index >= Count)
    THEN Result := NIL
    ELSE Result := TNvpNode (Items [Index]);
END;


FUNCTION  TNvpList.Value (Name : STRING) : STRING;
VAR
  Nvp : TNvpNode;
BEGIN
  Nvp := TNvpNode (Node (Name));
  IF Nvp <> NIL
    THEN Result := Nvp.Value
    ELSE Result := '';
END;


FUNCTION  TNvpList.Value (Index : INTEGER) : STRING;
BEGIN
  IF (Index < 0) OR (Index >= Count)
    THEN Result := ''
    ELSE Result := TNvpNode (Items [Index]).Value;
END;


FUNCTION  TNvpList.Name (Index : INTEGER) : STRING;
BEGIN
  IF (Index < 0) OR (Index >= Count)
    THEN Result := ''
    ELSE Result := TNvpNode (Items [Index]).Name;
END;


(*
===============================================================================================
TAttrList
List of Attributes. The "Analyze" method extracts the Attributes from the given Buffer.
Is used for extraction of Attributes in Start-Tags, Empty-Element Tags and the "pseudo"
attributes in XML Prologs, Text Declarations and PIs.
===============================================================================================
*)

PROCEDURE TAttrList.Analyze (Start : PChar; VAR Final : PChar);
          // Analyze the Buffer for Attribute=Name pairs.
          // Terminates when there is a character which is not IN CNameStart
          // (e.g. '?>' or '>' or '/>')
TYPE
  TPhase = (phName, phEq, phValue);
VAR
  Phase : TPhase;
  F     : PChar;
  Name  : STRING;
  Value : STRING;
  Attr  : TAttr;
BEGIN
  Clear;
  Phase := phName;
  Final := Start;
  REPEAT
    IF (Final^ = #0) OR (Final^ = '>') THEN BREAK;
    IF NOT (Final^ IN CWhitespace) THEN
      CASE Phase OF
        phName  : BEGIN
                    IF NOT (Final^ IN CNameStart) THEN EXIT;
                    ExtractName (Final, CWhitespace + ['=', '/'], F);
                    SetStringSF (Name, Final, F);
                    Final := F;
                    Phase := phEq;
                  END;
        phEq    : BEGIN
                    IF Final^ = '=' THEN
                      Phase := phValue
                  END;
        phValue : BEGIN
                    IF Final^ IN CQuoteChar THEN BEGIN
                      ExtractQuote (Final, Value, F);
                      Attr := TAttr.Create;
                      Attr.Name      := Name;
                      Attr.Value     := Value;
                      Attr.ValueType := vtNormal;
                      Add (Attr);
                      Final := F;
                      Phase := phName;
                      END;
                  END;
        END;
    INC (Final);
  UNTIL FALSE;
END;


(*
===============================================================================================
TElemList
List of TElemDef nodes.
===============================================================================================
*)

FUNCTION  TElemList.Node (Name : STRING) : TElemDef;
          // Binary search for the Node with the given Name
VAR
  L, H : INTEGER;    // Low, High Limit
  T, C : INTEGER;    // Test Index, Comparison result
  Last : INTEGER;    // Last Test Index
BEGIN
  IF Count=0 THEN BEGIN
    Result := NIL;
    EXIT;
    END;

  L    := 0;
  H    := Count;
  Last := -1;
  REPEAT
    T := (L+H) DIV 2;
    IF T=Last THEN BREAK;
    Result := TElemDef (Items [T]);
    C := CompareStr (Result.Name, Name);
    IF C = 0 THEN EXIT
    ELSE IF C < 0 THEN L := T
    ELSE               H := T;
    Last := T;
  UNTIL FALSE;
  Result := NIL;
END;


PROCEDURE TElemList.Add (Node : TElemDef);
VAR
  I : INTEGER;
BEGIN
  FOR I := Count-1 DOWNTO 0 DO
    IF Node.Name > TElemDef (Items [I]).Name THEN BEGIN
      Insert (I+1, Node);
      EXIT;
      END;
  Insert (0, Node);
END;


(*
===============================================================================================
TScannerXmlParser
A TXmlParser descendant for the TCustomXmlScanner component
===============================================================================================
*)

TYPE
  TScannerXmlParser = CLASS (TXmlParser)
                       Scanner : TCustomXmlScanner;
                       CONSTRUCTOR Create (TheScanner : TCustomXmlScanner);
                       FUNCTION  LoadExternalEntity (SystemId, PublicId,
                                                     Notation : STRING) : TXmlParser;  OVERRIDE;
                       FUNCTION  TranslateEncoding  (CONST Source : STRING) : STRING;  OVERRIDE;
                       PROCEDURE DtdElementFound (DtdElementRec : TDtdElementRec);     OVERRIDE;
                      END;

CONSTRUCTOR TScannerXmlParser.Create (TheScanner : TCustomXmlScanner);
BEGIN
  INHERITED Create;
  Scanner := TheScanner;
END;


FUNCTION  TScannerXmlParser.LoadExternalEntity (SystemId, PublicId, Notation : STRING) : TXmlParser;
BEGIN
  IF Assigned (Scanner.FOnLoadExternal)
    THEN Scanner.FOnLoadExternal (Scanner, SystemId, PublicId, Notation, Result)
    ELSE Result :=  INHERITED LoadExternalEntity (SystemId, PublicId, Notation);
END;


FUNCTION  TScannerXmlParser.TranslateEncoding  (CONST Source : STRING) : STRING;
BEGIN
  IF Assigned (Scanner.FOnTranslateEncoding)
    THEN Result := Scanner.FOnTranslateEncoding (Scanner, CurEncoding, Source)
    ELSE Result := INHERITED TranslateEncoding (Source);
END;


PROCEDURE TScannerXmlParser.DtdElementFound (DtdElementRec : TDtdElementRec);
BEGIN
  WITH DtdElementRec DO
    CASE ElementType OF
      deElement  : Scanner.WhenElement  (ElemDef);
      deAttList  : Scanner.WhenAttList  (ElemDef);
      deEntity   : Scanner.WhenEntity   (EntityDef);
      deNotation : Scanner.WhenNotation (NotationDef);
      dePI       : Scanner.WhenPI       (STRING (Target), STRING (Content), AttrList);
      deComment  : Scanner.WhenComment  (StrSFPas (Start, Final));
      deError    : Scanner.WhenDtdError (Pos);
      END;
END;


(*
===============================================================================================
TCustomXmlScanner
===============================================================================================
*)

CONSTRUCTOR TCustomXmlScanner.Create (AOwner: TComponent);
BEGIN
  INHERITED;
  FXmlParser := TScannerXmlParser.Create (Self);
END;


DESTRUCTOR TCustomXmlScanner.Destroy;
BEGIN
  FXmlParser.Free;
  INHERITED;
END;


PROCEDURE TCustomXmlScanner.LoadFromFile (Filename : TFilename);
          // Load XML Document from file
BEGIN
  FXmlParser.LoadFromFile (Filename);
END;


PROCEDURE TCustomXmlScanner.LoadFromBuffer (Buffer : PChar);
          // Load XML Document from buffer
BEGIN
  FXmlParser.LoadFromBuffer (Buffer);
END;


PROCEDURE TCustomXmlScanner.SetBuffer (Buffer : PChar);
          // Refer to Buffer
BEGIN
  FXmlParser.SetBuffer (Buffer);
END;


FUNCTION  TCustomXmlScanner.GetFilename : TFilename;
BEGIN
  Result := FXmlParser.Source;
END;


FUNCTION  TCustomXmlScanner.GetNormalize : BOOLEAN;
BEGIN
  Result := FXmlParser.Normalize;
END;


PROCEDURE TCustomXmlScanner.SetNormalize (Value : BOOLEAN);
BEGIN
  FXmlParser.Normalize := Value;
END;


PROCEDURE TCustomXmlScanner.WhenXmlProlog(XmlVersion, Encoding: STRING; Standalone : BOOLEAN);
          // Is called when the parser has parsed the <? xml ?> declaration of the prolog
BEGIN
  IF Assigned (FOnXmlProlog) THEN FOnXmlProlog (Self, XmlVersion, Encoding, Standalone);
END;


PROCEDURE TCustomXmlScanner.WhenComment  (Comment : STRING);
          // Is called when the parser has parsed a <!-- comment -->
BEGIN
  IF Assigned (FOnComment) THEN FOnComment (Self, Comment);
END;


PROCEDURE TCustomXmlScanner.WhenPI (Target, Content: STRING; Attributes : TAttrList);
          // Is called when the parser has parsed a <?processing instruction ?>
BEGIN
  IF Assigned (FOnPI) THEN FOnPI (Self, Target, Content, Attributes);
END;


PROCEDURE TCustomXmlScanner.WhenDtdRead (RootElementName : STRING);
          // Is called when the parser has completely parsed the DTD
BEGIN
  IF Assigned (FOnDtdRead) THEN FOnDtdRead (Self, RootElementName);
END;


PROCEDURE TCustomXmlScanner.WhenStartTag (TagName : STRING; Attributes : TAttrList);
          // Is called when the parser has parsed a start tag like <p>
BEGIN
  IF Assigned (FOnStartTag) THEN FOnStartTag (Self, TagName, Attributes);
END;


PROCEDURE TCustomXmlScanner.WhenEmptyTag (TagName : STRING; Attributes : TAttrList);
          // Is called when the parser has parsed an Empty Element Tag like <br/>
BEGIN
  IF Assigned (FOnEmptyTag) THEN FOnEmptyTag (Self, TagName, Attributes);
END;


PROCEDURE TCustomXmlScanner.WhenEndTag (TagName : STRING);
          // Is called when the parser has parsed an End Tag like </p>
BEGIN
  IF Assigned (FOnEndTag) THEN FOnEndTag (Self, TagName);
END;


PROCEDURE TCustomXmlScanner.WhenContent (Content : STRING);
          // Is called when the parser has parsed an element's text content
BEGIN
  IF Assigned (FOnContent) THEN FOnContent (Self, Content);
END;


PROCEDURE TCustomXmlScanner.WhenCData (Content : STRING);
          // Is called when the parser has parsed a CDATA section
BEGIN
  IF Assigned (FOnCData) THEN FOnCData (Self, Content);
END;


PROCEDURE TCustomXmlScanner.WhenElement (ElemDef : TElemDef);
          // Is called when the parser has parsed an <!ELEMENT> definition
          // inside the DTD
BEGIN
  IF Assigned (FOnElement) THEN FOnElement (Self, ElemDef);
END;


PROCEDURE TCustomXmlScanner.WhenAttList (ElemDef : TElemDef);
          // Is called when the parser has parsed an <!ATTLIST> definition
          // inside the DTD
BEGIN
  IF Assigned (FOnAttList) THEN FOnAttList (Self, ElemDef);
END;


PROCEDURE TCustomXmlScanner.WhenEntity   (EntityDef : TEntityDef);
          // Is called when the parser has parsed an <!ENTITY> definition
          // inside the DTD
BEGIN
  IF Assigned (FOnEntity) THEN FOnEntity (Self, EntityDef);
END;


PROCEDURE TCustomXmlScanner.WhenNotation (NotationDef : TNotationDef);
          // Is called when the parser has parsed a <!NOTATION> definition
          // inside the DTD
BEGIN
  IF Assigned (FOnNotation) THEN FOnNotation (Self, NotationDef);
END;


PROCEDURE TCustomXmlScanner.WhenDtdError (ErrorPos : PChar);
          // Is called when the parser has found an Error in the DTD
BEGIN
  IF Assigned (FOnDtdError) THEN FOnDtdError (Self, ErrorPos);
END;


PROCEDURE TCustomXmlScanner.Execute;
          // Perform scanning
          // Scanning is done synchronously, i.e. you can expect events to be triggered
          // in the order of the XML data stream. Execute will finish when the whole XML
          // document has been scanned or when the StopParser property has been set to TRUE.
BEGIN
  FStopParser := FALSE;
  FXmlParser.StartScan;
  WHILE FXmlParser.Scan AND (NOT FStopParser) DO
    CASE FXmlParser.CurPartType OF
      ptNone      : ;
      ptXmlProlog : WhenXmlProlog (FXmlParser.XmlVersion, FXmlParser.Encoding, FXmlParser.Standalone);
      ptComment   : WhenComment   (StrSFPas (FXmlParser.CurStart, FXmlParser.CurFinal));
      ptPI        : WhenPI        (FXmlParser.CurName, FXmlParser.CurContent, FXmlParser.CurAttr);
      ptDtdc      : WhenDtdRead   (FXmlParser.RootName);
      ptStartTag  : WhenStartTag  (FXmlParser.CurName, FXmlParser.CurAttr);
      ptEmptyTag  : WhenEmptyTag  (FXmlParser.CurName, FXmlParser.CurAttr);
      ptEndTag    : WhenEndTag    (FXmlParser.CurName);
      ptContent   : WhenContent   (FXmlParser.CurContent);
      ptCData     : WhenCData     (FXmlParser.CurContent);
      END;
END;


END.
