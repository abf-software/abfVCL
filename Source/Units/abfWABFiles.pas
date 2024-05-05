{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfWABFiles;

{$I abf.inc}

interface

uses
  Windows, Classes, Types, Contnrs, WideStrings,
// ABF VCL
  abfClasses;

type
//==============================================================================
// Forward declaration
//==============================================================================

//  TabfWABFile = class;


//==============================================================================
// EabfWABException
//==============================================================================

  EabfWABException = class(EabfException);


//==============================================================================
// EabfWABFileException
//==============================================================================

  EabfWABFileException = class(EabfWABException);


//==============================================================================
// TabfWABFileByteArray
//==============================================================================

  TabfWABFileByteArray = class
  private
    FValue: AnsiString;
    function GetByte(AIndex: Integer): Byte;
    function GetSize: Integer;
  public

    function GetValue(var AValue): Boolean; virtual;
    function SetValue(const AValue; AValueSize: Integer): Boolean; virtual;

    property Size: Integer read GetSize;
    property Value[AIndex: Integer]: Byte read GetByte; default;
  end;


//==============================================================================
// TabfWABFileCustomField
//==============================================================================

  TabfWABFileCustomField = class
  private
    FTag: DWORD;
    function GetID: Word;
  protected
    function GetIsEmpty: Boolean; virtual;
    function GetType: Word; virtual; abstract;
    function LoadFromStream(AStream: TStream): Boolean; virtual; abstract;
    function SaveToStream(AStream: TStream): Boolean; virtual; abstract;
  public
    constructor Create(ATag: DWORD); virtual;

    property ID: Word read GetID;
    property IsEmpty: Boolean read GetIsEmpty;
    property Tag: DWORD read FTag;
    property Type_: Word read GetType;
  end;


//==============================================================================
// TabfWABFileField
//==============================================================================

  TabfWABFileField = class(TabfWABFileCustomField)
  public
    constructor Create(ATag: DWORD); override;
  end;


//==============================================================================
// TabfWABFileUnknownField
//==============================================================================

  TabfWABFileUnknownField = class(TabfWABFileCustomField)
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
  end;


//==============================================================================
// TabfWABFileUnknownListField
//==============================================================================

  TabfWABFileUnknownListField = class(TabfWABFileCustomField)
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
  end;


//==============================================================================
// TabfWABFileBooleanField
//==============================================================================

  TabfWABFileBooleanField = class(TabfWABFileField)
  private
    FValue: WordBool;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: WordBool read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileBooleanListField
//==============================================================================

  TabfWABFileBooleanListField = class(TabfWABFileField)
  private
    FValue: TList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): WordBool;
    procedure SetValue(AIndex: Integer; AValue: WordBool);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: WordBool): Integer;
    procedure Insert(AIndex: Integer; AValue: WordBool);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: WordBool read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileIntegerField
//==============================================================================

  TabfWABFileIntegerField = class(TabfWABFileField)
  private
    FValue: Integer;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: Integer read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileIntegerListField
//==============================================================================

  TabfWABFileIntegerListField = class(TabfWABFileField)
  private
    FValue: TList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): Integer;
    procedure SetValue(AIndex: Integer; AValue: Integer);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: Integer): Integer;
    procedure Insert(AIndex: Integer; AValue: Integer);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: Integer read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileSmallIntField
//==============================================================================

  TabfWABFileSmallIntField = class(TabfWABFileField)
  private
    FValue: SmallInt;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: SmallInt read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileSmallIntListField
//==============================================================================

  TabfWABFileSmallIntListField = class(TabfWABFileField)
  private
    FValue: TList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): SmallInt;
    procedure SetValue(AIndex: Integer; AValue: SmallInt);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: SmallInt): Integer;
    procedure Insert(AIndex: Integer; AValue: SmallInt);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: SmallInt read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileInt64Field
//==============================================================================

  TabfWABFileInt64Field = class(TabfWABFileField)
  private
    FValue: Int64;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: Int64 read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileInt64ListField
//==============================================================================

  TabfWABFileInt64ListField = class(TabfWABFileField)
  private
    FValue: TObjectList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): Int64;
    procedure SetValue(AIndex: Integer; AValue: Int64);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: Int64): Integer;
    procedure Insert(AIndex: Integer; AValue: Int64);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: Int64 read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileCurrencyField
//==============================================================================

  TabfWABFileCurrencyField = class(TabfWABFileField)
  private
    FValue: Currency;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: Currency read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileCurrencyListField
//==============================================================================

  TabfWABFileCurrencyListField = class(TabfWABFileField)
  private
    FValue: TObjectList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): Currency;
    procedure SetValue(AIndex: Integer; AValue: Currency);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: Currency): Integer;
    procedure Insert(AIndex: Integer; AValue: Currency);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: Currency read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileSingleField
//==============================================================================

  TabfWABFileSingleField = class(TabfWABFileField)
  private
    FValue: Single;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: Single read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileSingleListField
//==============================================================================

  TabfWABFileSingleListField = class(TabfWABFileField)
  private
    FValue: TList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): Single;
    procedure SetValue(AIndex: Integer; AValue: Single);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: Single): Integer;
    procedure Insert(AIndex: Integer; AValue: Single);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: Single read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileDoubleField
//==============================================================================

  TabfWABFileDoubleField = class(TabfWABFileField)
  private
    FValue: Double;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: Double read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileDoubleListField
//==============================================================================

  TabfWABFileDoubleListField = class(TabfWABFileField)
  private
    FValue: TObjectList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): Double;
    procedure SetValue(AIndex: Integer; AValue: Double);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: Double): Integer;
    procedure Insert(AIndex: Integer; AValue: Double);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: Double read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileAppTimeField
//==============================================================================

  TabfWABFileAppTimeField = class(TabfWABFileField)
  private
    FValue: Double;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: Double read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileAppTimeListField
//==============================================================================

  TabfWABFileAppTimeListField = class(TabfWABFileField)
  private
    FValue: TObjectList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): Double;
    procedure SetValue(AIndex: Integer; AValue: Double);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: Double): Integer;
    procedure Insert(AIndex: Integer; AValue: Double);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: Double read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileSysTimeField
//==============================================================================

  TabfWABFileSysTimeField = class(TabfWABFileField)
  private
    FValue: TFileTime;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: TFileTime read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileSysTimeListField
//==============================================================================

  TabfWABFileSysTimeListField = class(TabfWABFileField)
  private
    FValue: TObjectList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): TFileTime;
    procedure SetValue(AIndex: Integer; AValue: TFileTime);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: TFileTime): Integer;
    procedure Insert(AIndex: Integer; AValue: TFileTime);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: TFileTime read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileAnsiStringField
//==============================================================================

  TabfWABFileAnsiStringField = class(TabfWABFileField)
  private
    FValue: AnsiString;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: AnsiString read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileAnsiStringListField
//==============================================================================

  TabfWABFileAnsiStringListField = class(TabfWABFileField)
  private
    FValue: TStringList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): AnsiString;
    procedure SetValue(AIndex: Integer; const AValue: AnsiString);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(const AValue: AnsiString): Integer;
    procedure Insert(AIndex: Integer; const AValue: AnsiString);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: AnsiString read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileUnicodeStringField
//==============================================================================

  TabfWABFileUnicodeStringField = class(TabfWABFileField)
  private
    FValue: WideString;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: WideString read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileUnicodeStringListField
//==============================================================================

  TabfWABFileUnicodeStringListField = class(TabfWABFileField)
  private
    FValue: TWideStringList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): WideString;
    procedure SetValue(AIndex: Integer; const AValue: WideString);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(const AValue: WideString): Integer;
    procedure Insert(AIndex: Integer; const AValue: WideString);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: WideString read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileCLSIDField
//==============================================================================

  TabfWABFileCLSIDField = class(TabfWABFileField)
  private
    FValue: TGUID;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    property Value: TGUID read FValue write FValue;
  end;


//==============================================================================
// TabfWABFileCLSIDListField
//==============================================================================

  TabfWABFileCLSIDListField = class(TabfWABFileField)
  private
    FValue: TObjectList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): TGUID;
    procedure SetValue(AIndex: Integer; AValue: TGUID);
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(AValue: TGUID): Integer;
    procedure Insert(AIndex: Integer; AValue: TGUID);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: TGUID read GetValue
      write SetValue; default;
  end;


//==============================================================================
// TabfWABFileByteArrayField
//==============================================================================

  TabfWABFileByteArrayField = class(TabfWABFileField)
  private
    FValue: TabfWABFileByteArray;
  protected
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    property Value: TabfWABFileByteArray read FValue;
  end;


//==============================================================================
// TabfWABFileByteArrayListField
//==============================================================================

  TabfWABFileByteArrayListField = class(TabfWABFileField)
  private
    FValue: TObjectList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): TabfWABFileByteArray;
  protected
    function GetIsEmpty: Boolean; override;
    function GetType: Word; override;
    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;
  public
    constructor Create(ATag: DWORD); override;
    destructor Destroy; override;

    function Add(const AValue; AValueSize: Integer): Integer;
    procedure Insert(AIndex: Integer; const AValue; AValueSize: Integer);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: TabfWABFileByteArray
      read GetValue; default;
  end;


//==============================================================================
// TabfWABFileDataRecord
//==============================================================================

  TabfWABFileDataRecord = class
  private
    FIsValid: DWORD;   // seems to be 0x1 and becomes 0x0 when the record is deleted
    FType: DWORD;      // seems to always be 0x1 for Contact, 0x2 for Group, 0x3 for Folder
    FID: DWORD;        // global ID

    FFields: TObjectList;
    function GetCount: Integer;
    function GetField(AIndex: Integer): TabfWABFileField;
    function GetIsEmpty: Boolean;
  protected
    function LoadFromStream(AStream: TStream): Boolean; virtual;
    function SaveToStream(AStream: TStream): Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(AField: TabfWABFileField): Integer;
    procedure Insert(AIndex: Integer; AField: TabfWABFileField);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Fields[AIndex: Integer]: TabfWABFileField read GetField; default;

    property IsValid: DWORD read FIsValid write FIsValid;
    property Type_: DWORD read FType write FType;
    property ID: DWORD read FID write FID;
    property IsEmpty: Boolean read GetIsEmpty;
  end;


//==============================================================================
// TabfWABFileDataTable
//==============================================================================

  TabfWABFileDataTable = class
  private
    FRecords: TObjectList;
    function GetCount: Integer;
    function GetRecord(AIndex: Integer): TabfWABFileDataRecord;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(ARecord: TabfWABFileDataRecord): Integer;
    procedure Insert(AIndex: Integer; ARecord: TabfWABFileDataRecord);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Records[AIndex: Integer]: TabfWABFileDataRecord
      read GetRecord; default;
  end;


//==============================================================================
// TabfWABFileSearchRecord
//==============================================================================

  TabfWABFileSearchRecord = class
  private
    FID: DWORD;
    FValue: WideString;
    function GetIsEmpty: Boolean;
    procedure SetValue(const AValue: WideString);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property ID: DWORD read FID write FID;
    property IsEmpty: Boolean read GetIsEmpty;
    property Value: WideString read FValue write SetValue;
  end;


//==============================================================================
// TabfWABFileSearchTable
//==============================================================================

  TabfWABFileSearchTable = class
  private
    FRecords: TObjectList;
    function GetCount: Integer;
    function GetRecord(AIndex: Integer): TabfWABFileSearchRecord;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(ARecord: TabfWABFileSearchRecord): Integer;
    procedure Insert(AIndex: Integer; ARecord: TabfWABFileSearchRecord);
    procedure Clear;
    procedure Delete(AIndex: Integer);

    property Count: Integer read GetCount;
    property Records[AIndex: Integer]: TabfWABFileSearchRecord
      read GetRecord; default;
  end;


//==============================================================================
// TabfWABFileTables
//==============================================================================

  TabfWABFileTables = class
  private
    FDataTable: TabfWABFileDataTable;
    FDisplayNameTable: TabfWABFileSearchTable;
    FFirstNameTable: TabfWABFileSearchTable;
    FLastNameTable: TabfWABFileSearchTable;
    FEmailTable: TabfWABFileSearchTable;
    FNicknameTable: TabfWABFileSearchTable;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;

    property DataTable: TabfWABFileDataTable read FDataTable;
    property DisplayNameTable: TabfWABFileSearchTable read FDisplayNameTable;
    property FirstNameTable: TabfWABFileSearchTable read FFirstNameTable;
    property LastNameTable: TabfWABFileSearchTable read FLastNameTable;
    property EmailTable: TabfWABFileSearchTable read FEmailTable;
    property NicknameTable: TabfWABFileSearchTable read FNicknameTable;
  end;


//==============================================================================
// TabfWABFile
//==============================================================================

  TabfWABFileType = (wftUnknown, wftAnsi, wftUnicode);

  TabfWABFile = class
  private
    FFileType: TabfWABFileType;
    FLastRecordID: DWORD;
    FTables: TabfWABFileTables;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    function GetNextRecordID: DWORD; virtual;

    function LoadFromStream(AStream: TStream): Boolean; virtual;
    function SaveToStream(AStream: TStream): Boolean; virtual;
    function LoadFromFile(const AFileName: WideString): Boolean; virtual;
    function SaveToFile(const AFileName: WideString): Boolean; virtual;

    property FileType: TabfWABFileType read FFileType write FFileType;
    property Tables: TabfWABFileTables read FTables;
  end;


{******************************************************************************}
implementation
{******************************************************************************}

uses
  SysUtils,
// 3rd party
  WabDefs,
// ABF VCL
  abfSysUtils;

//==============================================================================
// Private consts
//==============================================================================
const
  PT_MV_BOOLEAN = PT_BOOLEAN or MV_FLAG;

// Exceptions
  SabfArrayIndexError      = 'Array index out of bounds (%d)';
  SabfListIndexError       = 'List index out of bounds (%d)';
  SabfTagTypeMismatchError = 'Tag type (0x%x) and class type (%s) mismatched';
  SabfInvalidBinary        = 'Invalid binary value';

// Misc
  SabfFileHeaderMagicA: MAPIUID = (
    ab: ($81, $32, $84, $C1, $85, $05, $D0, $11,
         $B2, $90, $00, $AA, $00, $3C, $F6, $76));

  SabfFileHeaderMagicW: MAPIUID = (
    ab: ($9C, $CB, $CB, $8D, $13, $75, $D2, $11,
         $91, $58, $00, $C0, $4F, $79, $56, $A4));

  CabfSearchValueSize           = 32;


//==============================================================================
// Private types
//==============================================================================
type
  TabfWABFileIndexTableType = (fitDataTable, fitDisplayNameTable,
    fitLastNameTable, fitFirstNameTable, fitEmailTable, fitNicknameTable);


  TabfWABFileIndexTableDesc = record
	  MaxSize: DWORD;  // in bytes
  	Size: DWORD;     // in bytes
  	Offset: DWORD;
  	Count: DWORD;    // in records
  end;


  TabfWABFileHeader = record
	  Magic: MAPIUID;
  	DeletedRecordCount: DWORD;   // deleted records
  	LastRecordID: DWORD;         // last got ID for record
  	Tables: array[TabfWABFileIndexTableType] of TabfWABFileIndexTableDesc;
    RecordCount: DWORD;          // record count
    RecordCapacity: DWORD;       // record capacity
    Unknown3: DWORD;             // 00
    Unknown4: DWORD;             // 800 size of the second block?
    Unknown5: DWORD;             // 00
    Unknown6: DWORD;             // a4  offset of the second block?
    Unknown7: array[0..4] of DWORD;
  end;


  TabfWABFileDataRecordHeader = record
    RecordExists: DWORD;    // seems to be 0x1 and becomes 0x0 when the record is deleted
	  RecordType: DWORD;      // seems to always be 0x1 for Contact, 0x2 for Group, 0x3 for Folder
  	RecordID: DWORD;        // global ID
	  FieldCount: DWORD;      // total fields in record
  	Mystery4: DWORD;        // always seems to be 0x20
	  OpCountCRC1: DWORD;     // always seems to be FieldCount shl 2
	  OpCountCRC2: DWORD;     // always seems to be FieldCount shl 2
	  DataSize: DWORD;
  end;


  TabfWABFileDataIndex = record
  	ID: DWORD;
	  Offset: DWORD;
  end;

  
  TabfWABFileSearchIndexA = record
	  Value: array[0..CabfSearchValueSize - 1] of AnsiChar;
    ID: DWORD;
  end;


  TabfWABFileSearchIndexW = record
	  Value: array[0..CabfSearchValueSize - 1] of WideChar;
    ID: DWORD;
  end;


//==============================================================================
// Forward declarations
//==============================================================================

  TabfWABFileIndexTables = class;


//==============================================================================
// TabfWABFileIndexTable
//==============================================================================

  TabfWABFileIndexTable = class
  private
    FIndexes: TObjectList;
    FTables: TabfWABFileIndexTables;
    function GetCount: Integer;
    function GetMaxSize: DWORD;
    function GetSize: DWORD;
    function GetOffset: DWORD;
    procedure SetCount(AValue: Integer);
  protected
    function AddData(const AData): Integer;
    function GetDataSize: DWORD; virtual; abstract;
    procedure GetData(AIndex: Integer; var AData);
    procedure InsertData(AIndex: Integer; const AData);
    procedure SetData(AIndex: Integer; const AData);
  public
    constructor Create(ATables: TabfWABFileIndexTables); virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(AIndex: Integer);

    function LoadFromStream(AStream: TStream): Boolean; virtual; abstract;
    function SaveToStream(AStream: TStream): Boolean; virtual; abstract;

    property Count: Integer read GetCount write SetCount;
    property MaxSize: DWORD read GetMaxSize;
    property Offset: DWORD read GetOffset;
    property Size: DWORD read GetSize;
  end;


//==============================================================================
// TabfWABFileDataIndexTable
//==============================================================================

  TabfWABFileDataIndexTable = class(TabfWABFileIndexTable)
  private
    function GetIndex(AIndex: Integer): TabfWABFileDataIndex;
  protected
    function GetDataSize: DWORD; override;
  public
    function Add(ADataIndex: TabfWABFileDataIndex): Integer;
    procedure Insert(AIndex: Integer; ADataIndex: TabfWABFileDataIndex);

    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;

    property Indexes[AIndex: Integer]: TabfWABFileDataIndex
      read GetIndex; default;
  end;


//==============================================================================
// TabfWABFileSearchIndexTable
//==============================================================================

  TabfWABFileSearchIndexTable = class(TabfWABFileIndexTable);
  TabfWABFileSearchIndexTableClass = class of TabfWABFileSearchIndexTable;


//==============================================================================
// TabfWABFileSearchIndexTableA
//==============================================================================

  TabfWABFileSearchIndexTableA = class(TabfWABFileSearchIndexTable)
  private
    function GetIndex(AIndex: Integer): TabfWABFileSearchIndexA;
  protected
    function GetDataSize: DWORD; override;
  public
    function Add(ASearchIndex: TabfWABFileSearchIndexA): Integer;
    procedure Insert(AIndex: Integer; ASearchIndex: TabfWABFileSearchIndexA);

    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;

    property Indexes[AIndex: Integer]: TabfWABFileSearchIndexA
      read GetIndex; default;
  end;


//==============================================================================
// TabfWABFileSearchIndexTableW
//==============================================================================

  TabfWABFileSearchIndexTableW = class(TabfWABFileSearchIndexTable)
  private
    function GetIndex(AIndex: Integer): TabfWABFileSearchIndexW;
  protected
    function GetDataSize: DWORD; override;
  public
    function Add(ASearchIndex: TabfWABFileSearchIndexW): Integer;
    procedure Insert(AIndex: Integer; ASearchIndex: TabfWABFileSearchIndexW);

    function LoadFromStream(AStream: TStream): Boolean; override;
    function SaveToStream(AStream: TStream): Boolean; override;

    property Indexes[AIndex: Integer]: TabfWABFileSearchIndexW
      read GetIndex; default;
  end;


//==============================================================================
// TabfWABFileIndexTables
//==============================================================================

  TabfWABFileIndexTables = class
  private
    FDataTable: TabfWABFileDataIndexTable;
    FDisplayNameTable: TabfWABFileSearchIndexTable;
    FLastNameTable: TabfWABFileSearchIndexTable;
    FFirstNameTable: TabfWABFileSearchIndexTable;
    FEmailTable: TabfWABFileSearchIndexTable;
    FNicknameTable: TabfWABFileSearchIndexTable;
    FOffset: Int64;
    FRecordCapacity: DWORD;
    FRecordGrowthDelta: DWORD;
    function GetRecordCapacity: DWORD;
    function GetRecordCount: DWORD;
    function GetSize: Int64;
    procedure SetRecordCapacity(AValue: DWORD);
  protected
    function GetSearchIndexTableClass: TabfWABFileSearchIndexTableClass; virtual; abstract;
    function GetTableOffest(ATable: TabfWABFileIndexTable): Int64; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LoadFromStream(AStream: TStream): Boolean; virtual;
    function SaveToStream(AStream: TStream): Boolean; virtual;

    property Offset: Int64 read FOffset write FOffset;
    property Size: Int64 read GetSize;
    property RecordCapacity: DWORD read GetRecordCapacity
      write SetRecordCapacity;
    property RecordCount: DWORD read GetRecordCount;
    property RecordGrowthDelta: DWORD read FRecordGrowthDelta
      write FRecordGrowthDelta;
    property DataTable: TabfWABFileDataIndexTable read FDataTable;
    property DisplayNameTable: TabfWABFileSearchIndexTable
      read FDisplayNameTable;
    property LastNameTable: TabfWABFileSearchIndexTable read FLastNameTable;
    property FirstNameTable: TabfWABFileSearchIndexTable read FFirstNameTable;
    property EmailTable: TabfWABFileSearchIndexTable read FEmailTable;
    property NicknameTable: TabfWABFileSearchIndexTable read FNicknameTable;
  end;


//==============================================================================
// TabfWABFileIndexTablesA
//==============================================================================

  TabfWABFileIndexTablesA = class(TabfWABFileIndexTables)
  private
    function GetDisplayNameTable: TabfWABFileSearchIndexTableA;
    function GetLastNameTable: TabfWABFileSearchIndexTableA;
    function GetFirstNameTable: TabfWABFileSearchIndexTableA;
    function GetEmailTable: TabfWABFileSearchIndexTableA;
    function GetNicknameTable: TabfWABFileSearchIndexTableA;
  protected
    function GetSearchIndexTableClass: TabfWABFileSearchIndexTableClass; override;
  public
    property DataTable: TabfWABFileDataIndexTable read FDataTable;
    property DisplayNameTable: TabfWABFileSearchIndexTableA
      read GetDisplayNameTable;
    property LastNameTable: TabfWABFileSearchIndexTableA read GetLastNameTable;
    property FirstNameTable: TabfWABFileSearchIndexTableA read GetFirstNameTable;
    property EmailTable: TabfWABFileSearchIndexTableA read GetEmailTable;
    property NicknameTable: TabfWABFileSearchIndexTableA read GetNicknameTable;
  end;


//==============================================================================
// TabfWABFileIndexTablesW
//==============================================================================

  TabfWABFileIndexTablesW = class(TabfWABFileIndexTables)
  private
    function GetDisplayNameTable: TabfWABFileSearchIndexTableW;
    function GetLastNameTable: TabfWABFileSearchIndexTableW;
    function GetFirstNameTable: TabfWABFileSearchIndexTableW;
    function GetEmailTable: TabfWABFileSearchIndexTableW;
    function GetNicknameTable: TabfWABFileSearchIndexTableW;
  protected
    function GetSearchIndexTableClass: TabfWABFileSearchIndexTableClass; override;
  public
    property DataTable: TabfWABFileDataIndexTable read FDataTable;
    property DisplayNameTable: TabfWABFileSearchIndexTableW
      read GetDisplayNameTable;
    property LastNameTable: TabfWABFileSearchIndexTableW read GetLastNameTable;
    property FirstNameTable: TabfWABFileSearchIndexTableW read GetFirstNameTable;
    property EmailTable: TabfWABFileSearchIndexTableW read GetEmailTable;
    property NicknameTable: TabfWABFileSearchIndexTableW read GetNicknameTable;
  end;


//==============================================================================
// TabfWABFileIndexTable
//==============================================================================

constructor TabfWABFileIndexTable.Create(ATables: TabfWABFileIndexTables);
begin
  inherited Create;

  FIndexes := TObjectList.Create;
  FTables := ATables;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileIndexTable.Destroy;
begin
  FreeAndNil(FIndexes);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTable.GetCount: Integer;
begin
  Result := FIndexes.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTable.GetMaxSize: DWORD;
begin
  Result := GetDataSize * DWORD(FTables.RecordCapacity);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTable.GetSize: DWORD;
begin
  Result := GetDataSize * DWORD(Count);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTable.GetOffset: DWORD;
begin
  Result := FTables.GetTableOffest(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIndexTable.SetCount(AValue: Integer);
var
  TempBuf: AnsiString;
  i: Integer;
begin
  if AValue > FIndexes.Count then
  begin
    SetLength(TempBuf, GetDataSize);
    FillChar(TempBuf[1], GetDataSize, 0);

    for i := FIndexes.Count to AValue - 1 do
      AddData(TempBuf[1]);
  end else
    FIndexes.Count := AValue;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTable.AddData(const AData): Integer;
begin
  Result := Count;
  InsertData(Result, AData);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIndexTable.GetData(AIndex: Integer; var AData);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  if not TabfWABFileByteArray(FIndexes[AIndex]).GetValue(AData) then
    raise EabfWABFileException.Create(SabfInvalidBinary);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIndexTable.InsertData(AIndex: Integer;
  const AData);
var
  TempData: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempData := TabfWABFileByteArray.Create;
  try
    if not TempData.SetValue(AData, GetDataSize) then
      raise EabfWABFileException.Create(SabfInvalidBinary);

    FIndexes.Insert(AIndex, TempData);
  except
    TempData.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIndexTable.SetData(AIndex: Integer; const AData);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  if not TabfWABFileByteArray(FIndexes[AIndex]).SetValue(AData, GetDataSize) then
    raise EabfWABFileException.Create(SabfInvalidBinary);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIndexTable.Clear;
begin
  FIndexes.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIndexTable.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FIndexes.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileDataIndexTable
//==============================================================================

function TabfWABFileDataIndexTable.GetIndex(AIndex: Integer): TabfWABFileDataIndex;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  GetData(AIndex, Result);
end;

//------------------------------------------------------------------------------

function TabfWABFileDataIndexTable.GetDataSize: DWORD;
begin
  Result := SizeOf(TabfWABFileDataIndex);
end;

//------------------------------------------------------------------------------

function TabfWABFileDataIndexTable.Add(ADataIndex: TabfWABFileDataIndex): Integer;
begin
  Result := AddData(ADataIndex);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDataIndexTable.Insert(AIndex: Integer;
  ADataIndex: TabfWABFileDataIndex);
begin
  InsertData(AIndex, ADataIndex);
end;

//------------------------------------------------------------------------------

function TabfWABFileDataIndexTable.LoadFromStream(AStream: TStream): Boolean;
var
  TempIndex: TabfWABFileDataIndex;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    for i := 0 to Count - 1 do
    begin
      if AStream.Read(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;
      TempIndex.Offset := TempIndex.Offset - FTables.Offset - FTables.Size;
      SetData(i, TempIndex);
    end;

    for i := Count to FTables.RecordCapacity - 1 do
      if AStream.Read(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileDataIndexTable.SaveToStream(AStream: TStream): Boolean;
var
  TempIndex: TabfWABFileDataIndex;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    for i := 0 to Count - 1 do
    begin
      GetData(i, TempIndex);
      TempIndex.Offset := TempIndex.Offset + FTables.Offset + FTables.Size;
      if AStream.Write(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;
    end;

    FillChar(TempIndex, GetDataSize, 0);

    for i := Count to FTables.RecordCapacity - 1 do
      if AStream.Write(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;

    Result := True;
  except
  end;
end;


//==============================================================================
// TabfWABFileSearchIndexTableA
//==============================================================================

function TabfWABFileSearchIndexTableA.GetIndex(AIndex: Integer): TabfWABFileSearchIndexA;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  GetData(AIndex, Result);
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableA.GetDataSize: DWORD;
begin
  Result := SizeOf(TabfWABFileSearchIndexA);
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableA.Add(ASearchIndex: TabfWABFileSearchIndexA): Integer;
begin
  Result := AddData(ASearchIndex);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSearchIndexTableA.Insert(AIndex: Integer;
  ASearchIndex: TabfWABFileSearchIndexA);
begin
  InsertData(AIndex, ASearchIndex);
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableA.LoadFromStream(AStream: TStream): Boolean;
var
  TempIndex: TabfWABFileSearchIndexA;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    for i := 0 to Count - 1 do
    begin
      if AStream.Read(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;
      SetData(i, TempIndex);
    end;

    for i := Count to FTables.RecordCapacity - 1 do
      if AStream.Read(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableA.SaveToStream(AStream: TStream): Boolean;
var
  TempIndex: TabfWABFileSearchIndexA;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    for i := 0 to Count - 1 do
    begin
      GetData(i, TempIndex);
      if AStream.Write(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;
    end;

    FillChar(TempIndex, GetDataSize, 0);

    for i := Count to FTables.RecordCapacity - 1 do
      if AStream.Write(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;

    Result := True;
  except
  end;
end;


//==============================================================================
// TabfWABFileSearchIndexTableW
//==============================================================================

function TabfWABFileSearchIndexTableW.GetIndex(AIndex: Integer): TabfWABFileSearchIndexW;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  GetData(AIndex, Result);
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableW.GetDataSize: DWORD;
begin
  Result := SizeOf(TabfWABFileSearchIndexW);
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableW.Add(ASearchIndex: TabfWABFileSearchIndexW): Integer;
begin
  Result := AddData(ASearchIndex);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSearchIndexTableW.Insert(AIndex: Integer;
  ASearchIndex: TabfWABFileSearchIndexW);
begin
  InsertData(AIndex, ASearchIndex);
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableW.LoadFromStream(AStream: TStream): Boolean;
var
  TempIndex: TabfWABFileSearchIndexW;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    for i := 0 to Count - 1 do
    begin
      if AStream.Read(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;
      SetData(i, TempIndex);
    end;

    for i := Count to FTables.RecordCapacity - 1 do
      if AStream.Read(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchIndexTableW.SaveToStream(AStream: TStream): Boolean;
var
  TempIndex: TabfWABFileSearchIndexW;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    for i := 0 to Count - 1 do
    begin
      GetData(i, TempIndex);
      if AStream.Write(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;
    end;

    FillChar(TempIndex, GetDataSize, 0);

    for i := Count to FTables.RecordCapacity - 1 do
      if AStream.Write(TempIndex, GetDataSize) <> Int64(GetDataSize) then Abort;

    Result := True;
  except
  end;
end;


//==============================================================================
// TabfWABFileIndexTables
//==============================================================================

constructor TabfWABFileIndexTables.Create;
begin
  inherited Create;
  FDataTable := TabfWABFileDataIndexTable.Create(Self);
  FDisplayNameTable := GetSearchIndexTableClass.Create(Self);
  FLastNameTable := GetSearchIndexTableClass.Create(Self);
  FFirstNameTable := GetSearchIndexTableClass.Create(Self);
  FEmailTable := GetSearchIndexTableClass.Create(Self);
  FNicknameTable := GetSearchIndexTableClass.Create(Self);

  FOffset := 0;
  FRecordGrowthDelta := 1;
  FRecordCapacity := 0;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileIndexTables.Destroy;
begin
  FreeAndNil(FNicknameTable);
  FreeAndNil(FEmailTable);
  FreeAndNil(FFirstNameTable);
  FreeAndNil(FLastNameTable);
  FreeAndNil(FDisplayNameTable);
  FreeAndNil(FDataTable);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTables.GetRecordCapacity: DWORD;
begin
  Result := FDataTable.Count;
  if DWORD(FDisplayNameTable.Count) > Result then
    Result := FDisplayNameTable.Count;
  if DWORD(FLastNameTable.Count) > Result then
    Result := FLastNameTable.Count;
  if DWORD(FFirstNameTable.Count) > Result then
    Result := FFirstNameTable.Count;
  if DWORD(FEmailTable.Count) > Result then
    Result := FEmailTable.Count;
  if DWORD(FNicknameTable.Count) > Result then
    Result := FNicknameTable.Count;

  Result := ((Result div RecordGrowthDelta) + 1) * RecordGrowthDelta;

  if FRecordCapacity > Result then
    Result := FRecordCapacity;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTables.GetRecordCount: DWORD;
begin
  Result := FDataTable.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTables.GetSize: Int64;
begin
  Result := FNicknameTable.Offset + FNicknameTable.MaxSize - Offset;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIndexTables.SetRecordCapacity(AValue: DWORD);
begin
  FRecordCapacity := AValue;
  if RecordCapacity > AValue then
    FRecordCapacity := 0;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTables.GetTableOffest(ATable: TabfWABFileIndexTable): Int64;
begin
  Result := FOffset;
  if ATable = FDataTable then Exit;

  Result := Result + FDataTable.MaxSize;
  if ATable = FDisplayNameTable then Exit;

  Result := Result + FDisplayNameTable.MaxSize;
  if ATable = FLastNameTable then Exit;

  Result := Result + FLastNameTable.MaxSize;
  if ATable = FFirstNameTable then Exit;

  Result := Result + FFirstNameTable.MaxSize;
  if ATable = FEmailTable then Exit;

  Result := Result + FEmailTable.MaxSize;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTables.LoadFromStream(AStream: TStream): Boolean;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  if not FDataTable.LoadFromStream(AStream) then Exit;
  if not FDisplayNameTable.LoadFromStream(AStream) then Exit;
  if not FLastNameTable.LoadFromStream(AStream) then Exit;
  if not FFirstNameTable.LoadFromStream(AStream) then Exit;
  if not FEmailTable.LoadFromStream(AStream) then Exit;
  if not FNicknameTable.LoadFromStream(AStream) then Exit;

  Result := True;
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTables.SaveToStream(AStream: TStream): Boolean;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  if not FDataTable.SaveToStream(AStream) then Exit;
  if not FDisplayNameTable.SaveToStream(AStream) then Exit;
  if not FLastNameTable.SaveToStream(AStream) then Exit;
  if not FFirstNameTable.SaveToStream(AStream) then Exit;
  if not FEmailTable.SaveToStream(AStream) then Exit;
  if not FNicknameTable.SaveToStream(AStream) then Exit;

  Result := True;
end;

//==============================================================================
// TabfWABFileIndexTablesA
//==============================================================================

function TabfWABFileIndexTablesA.GetDisplayNameTable: TabfWABFileSearchIndexTableA;
begin
  Result := TabfWABFileSearchIndexTableA(FDisplayNameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesA.GetLastNameTable: TabfWABFileSearchIndexTableA;
begin
  Result := TabfWABFileSearchIndexTableA(FLastNameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesA.GetFirstNameTable: TabfWABFileSearchIndexTableA;
begin
  Result := TabfWABFileSearchIndexTableA(FFirstNameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesA.GetEmailTable: TabfWABFileSearchIndexTableA;
begin
  Result := TabfWABFileSearchIndexTableA(FEmailTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesA.GetNicknameTable: TabfWABFileSearchIndexTableA;
begin
  Result := TabfWABFileSearchIndexTableA(FNicknameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesA.GetSearchIndexTableClass: TabfWABFileSearchIndexTableClass;
begin
  Result := TabfWABFileSearchIndexTableA;
end;


//==============================================================================
// TabfWABFileIndexTablesW
//==============================================================================

function TabfWABFileIndexTablesW.GetDisplayNameTable: TabfWABFileSearchIndexTableW;
begin
  Result := TabfWABFileSearchIndexTableW(FDisplayNameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesW.GetLastNameTable: TabfWABFileSearchIndexTableW;
begin
  Result := TabfWABFileSearchIndexTableW(FLastNameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesW.GetFirstNameTable: TabfWABFileSearchIndexTableW;
begin
  Result := TabfWABFileSearchIndexTableW(FFirstNameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesW.GetEmailTable: TabfWABFileSearchIndexTableW;
begin
  Result := TabfWABFileSearchIndexTableW(FEmailTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesW.GetNicknameTable: TabfWABFileSearchIndexTableW;
begin
  Result := TabfWABFileSearchIndexTableW(FNicknameTable);
end;

//------------------------------------------------------------------------------

function TabfWABFileIndexTablesW.GetSearchIndexTableClass: TabfWABFileSearchIndexTableClass;
begin
  Result := TabfWABFileSearchIndexTableW;
end;


//==============================================================================
// TabfWABFileByteArray
//==============================================================================

function TabfWABFileByteArray.GetByte(AIndex: Integer): Byte;
begin
  if (AIndex < 0) or (AIndex >= Size) then
    raise EabfWABFileException.CreateFmt(SabfArrayIndexError, [AIndex]);

  Result := Byte(FValue[AIndex + 1]);
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArray.GetSize: Integer;
begin
  Result := Length(FValue);
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArray.GetValue(var AValue): Boolean;
begin
  Result := False;

  try
    Move(FValue[1], AValue, Size);

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArray.SetValue(const AValue; AValueSize: Integer): Boolean;
begin
  Result := False;
  try
    SetLength(FValue, AValueSize);

    Move(AValue, FValue[1], AValueSize);

    Result := True;
  except
  end;
end;


//==============================================================================
// TabfWABFileCustomField
//==============================================================================

constructor TabfWABFileCustomField.Create(ATag: DWORD);
begin
  inherited Create;

  FTag := ATag;
end;

//------------------------------------------------------------------------------

function TabfWABFileCustomField.GetID: Word;
begin
  Result := PROP_ID(FTag);
end;

//------------------------------------------------------------------------------

function TabfWABFileCustomField.GetIsEmpty: Boolean;
begin
  Result := (Tag = 0) or (Type_ = 0);
end;


//==============================================================================
// TabfWABFileField
//==============================================================================

constructor TabfWABFileField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  if PROP_TYPE(ATag) <> Type_ then
    raise EabfWABFileException.CreateFmt(SabfTagTypeMismatchError,
      [PROP_TYPE(ATag), ClassName]);
end;


//==============================================================================
// TabfWABFileUnknownField
//==============================================================================

constructor TabfWABFileUnknownField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  if (PROP_TYPE(ATag) and MV_FLAG) = MV_FLAG then
    raise EabfWABFileException.CreateFmt(SabfTagTypeMismatchError,
      [PROP_TYPE(ATag), ClassName]);
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownField.GetIsEmpty: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownField.GetType: Word;
begin
  Result := PROP_TYPE(Tag);
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if (AStream.Position + TempSize) > AStream.Size then
    begin
      AStream.Seek(0, soFromEnd);
      Abort;
    end;

    AStream.Position := AStream.Position + TempSize;
    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownField.SaveToStream(AStream: TStream): Boolean;
begin
  Result := False;
end;


//==============================================================================
// TabfWABFileUnknownListField
//==============================================================================

constructor TabfWABFileUnknownListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  if (PROP_TYPE(ATag) and MV_FLAG) <> MV_FLAG then
    raise EabfWABFileException.CreateFmt(SabfTagTypeMismatchError,
      [PROP_TYPE(ATag), ClassName]);
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownListField.GetIsEmpty: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownListField.GetType: Word;
begin
  Result := PROP_TYPE(Tag);
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    if (AStream.Position + TempSize) > AStream.Size then
    begin
      AStream.Seek(0, soFromEnd);
      Abort;
    end;

    AStream.Position := AStream.Position + TempSize;
    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnknownListField.SaveToStream(AStream: TStream): Boolean;
begin
  Result := False;
end;


//==============================================================================
// TabfWABFileBooleanField
//==============================================================================

function TabfWABFileBooleanField.GetType: Word;
begin
  Result := PT_BOOLEAN;
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileBooleanListField
//==============================================================================

constructor TabfWABFileBooleanListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileBooleanListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanListField.GetValue(AIndex: Integer): WordBool;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := WordBool(FValue[AIndex]);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileBooleanListField.SetValue(AIndex: Integer; AValue: WordBool);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue[AIndex] := Pointer(AValue);
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanListField.GetType: Word;
begin
  Result := PT_MV_BOOLEAN
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: WordBool;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: WordBool;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileBooleanListField.Add(AValue: WordBool): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileBooleanListField.Insert(AIndex: Integer; AValue: WordBool);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Insert(AIndex, Pointer(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileBooleanListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileBooleanListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileIntegerField
//==============================================================================

function TabfWABFileIntegerField.GetType: Word;
begin
  Result := PT_LONG;
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileIntegerListField
//==============================================================================

constructor TabfWABFileIntegerListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileIntegerListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerListField.GetValue(AIndex: Integer): Integer;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := Integer(FValue[AIndex]);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIntegerListField.SetValue(AIndex: Integer; AValue: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue[AIndex] := Pointer(AValue);
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerListField.GetType: Word;
begin
  Result := PT_MV_LONG;
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Integer;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);

      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Integer;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileIntegerListField.Add(AValue: Integer): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIntegerListField.Insert(AIndex: Integer; AValue: Integer);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Insert(AIndex, Pointer(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIntegerListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileIntegerListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileSmallIntField
//==============================================================================

function TabfWABFileSmallIntField.GetType: Word;
begin
  Result := PT_I2;
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileSmallIntListField
//==============================================================================

constructor TabfWABFileSmallIntListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileSmallIntListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntListField.GetValue(AIndex: Integer): SmallInt;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := ShortInt(FValue[AIndex]);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSmallIntListField.SetValue(AIndex: Integer; AValue: SmallInt);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue[AIndex] := Pointer(AValue);
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntListField.GetType: Word;
begin
  Result := PT_MV_I2;
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: SmallInt;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: SmallInt;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSmallIntListField.Add(AValue: SmallInt): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSmallIntListField.Insert(AIndex: Integer; AValue: SmallInt);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Insert(AIndex, Pointer(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSmallIntListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSmallIntListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileInt64Field
//==============================================================================

function TabfWABFileInt64Field.GetType: Word;
begin
  Result := PT_I8;
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64Field.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64Field.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileInt64ListField
//==============================================================================

constructor TabfWABFileInt64ListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileInt64ListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64ListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64ListField.GetValue(AIndex: Integer): Int64;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).GetValue(Result);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileInt64ListField.SetValue(AIndex: Integer; AValue: Int64);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64ListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64ListField.GetType: Word;
begin
  Result := PT_MV_I8;
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64ListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Int64;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64ListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Int64;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileInt64ListField.Add(AValue: Int64): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileInt64ListField.Insert(AIndex: Integer; AValue: Int64);
var
  TempItem: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempItem := TabfWABFileByteArray.Create;
  try
    FValue.Insert(AIndex, TempItem);
  except
    TempItem.Free;
    raise;
  end;

  TempItem.SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileInt64ListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileInt64ListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileCurrencyField
//==============================================================================

function TabfWABFileCurrencyField.GetType: Word;
begin
  Result := PT_CURRENCY;
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
  TempInt64: Int64;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(TempInt64) then Abort;

    Result := AStream.Read(TempInt64, TempSize) = Int64(TempSize);
    FValue := TempInt64 / 10000;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
  TempInt64: Int64;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempInt64 := Trunc(FValue * 10000);
    TempSize := SizeOf(TempInt64);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(TempInt64, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileCurrencyListField
//==============================================================================

constructor TabfWABFileCurrencyListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileCurrencyListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyListField.GetValue(AIndex: Integer): Currency;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).GetValue(Result);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCurrencyListField.SetValue(AIndex: Integer; AValue: Currency);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyListField.GetType: Word;
begin
  Result := PT_MV_CURRENCY;
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Currency;
  TempInt64: Int64;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempInt64) then Abort;

        if Read(TempInt64, TempSize) <> Int64(TempSize) then Abort;

        TempValue := TempInt64 / 10000;
        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempInt64: Int64;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempInt64);

      for i := 0 to Count - 1 do
      begin
        TempInt64 := Trunc(Values[i] * 10000);
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempInt64, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileCurrencyListField.Add(AValue: Currency): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCurrencyListField.Insert(AIndex: Integer; AValue: Currency);
var
  TempItem: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempItem := TabfWABFileByteArray.Create;
  try
    FValue.Insert(AIndex, TempItem);
  except
    TempItem.Free;
    raise;
  end;

  TempItem.SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCurrencyListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCurrencyListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileSingleField
//==============================================================================

function TabfWABFileSingleField.GetType: Word;
begin
  Result := PT_R4;
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileSingleListField
//==============================================================================

constructor TabfWABFileSingleListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileSingleListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleListField.GetValue(AIndex: Integer): Single;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := Single(FValue[AIndex]);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSingleListField.SetValue(AIndex: Integer; AValue: Single);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue[AIndex] := Pointer(AValue);
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleListField.GetType: Word;
begin
  Result := PT_MV_R4;
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Single;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Single;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSingleListField.Add(AValue: Single): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSingleListField.Insert(AIndex: Integer; AValue: Single);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Insert(AIndex, Pointer(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSingleListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSingleListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileDoubleField
//==============================================================================

function TabfWABFileDoubleField.GetType: Word;
begin
  Result := PT_DOUBLE;
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileDoubleListField
//==============================================================================

constructor TabfWABFileDoubleListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileDoubleListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleListField.GetValue(AIndex: Integer): Double;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).GetValue(Result);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDoubleListField.SetValue(AIndex: Integer; AValue: Double);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleListField.GetType: Word;
begin
  Result := PT_MV_DOUBLE;
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Double;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Double;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileDoubleListField.Add(AValue: Double): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDoubleListField.Insert(AIndex: Integer; AValue: Double);
var
  TempItem: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempItem := TabfWABFileByteArray.Create;
  try
    FValue.Insert(AIndex, TempItem);
  except
    TempItem.Free;
    raise;
  end;

  TempItem.SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDoubleListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDoubleListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileAppTimeField
//==============================================================================

function TabfWABFileAppTimeField.GetType: Word;
begin
  Result := PT_APPTIME;
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileAppTimeListField
//==============================================================================

constructor TabfWABFileAppTimeListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileAppTimeListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeListField.GetValue(AIndex: Integer): Double;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).GetValue(Result);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAppTimeListField.SetValue(AIndex: Integer; AValue: Double);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeListField.GetType: Word;
begin
  Result := PT_MV_APPTIME;
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Double;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: Double;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileAppTimeListField.Add(AValue: Double): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAppTimeListField.Insert(AIndex: Integer; AValue: Double);
var
  TempItem: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempItem := TabfWABFileByteArray.Create;
  try
    FValue.Insert(AIndex, TempItem);
  except
    TempItem.Free;
    raise;
  end;

  TempItem.SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAppTimeListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAppTimeListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileSysTimeField
//==============================================================================

function TabfWABFileSysTimeField.GetType: Word;
begin
  Result := PT_SYSTIME;
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileSysTimeListField
//==============================================================================

constructor TabfWABFileSysTimeListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileSysTimeListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeListField.GetValue(AIndex: Integer): TFileTime;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).GetValue(Result);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSysTimeListField.SetValue(AIndex: Integer; AValue: TFileTime);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeListField.GetType: Word;
begin
  Result := PT_MV_SYSTIME;
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: TFileTime;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: TFileTime;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileSysTimeListField.Add(AValue: TFileTime): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSysTimeListField.Insert(AIndex: Integer; AValue: TFileTime);
var
  TempItem: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempItem := TabfWABFileByteArray.Create;
  try
    FValue.Insert(AIndex, TempItem);
  except
    TempItem.Free;
    raise;
  end;

  TempItem.SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSysTimeListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSysTimeListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileAnsiStringField
//==============================================================================

function TabfWABFileAnsiStringField.GetType: Word;
begin
  Result := PT_STRING8;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
  TempStr: AnsiString;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    SetLength(TempStr, TempSize);
    if AStream.Read(TempStr[1], TempSize) <> Int64(TempSize) then Abort;

    FValue := PAnsiChar(TempStr);

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := Length(FValue) + 1;
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue[1], TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileAnsiStringListField
//==============================================================================

constructor TabfWABFileAnsiStringListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileAnsiStringListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringListField.GetValue(AIndex: Integer): AnsiString;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := FValue[AIndex];
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAnsiStringListField.SetValue(AIndex: Integer;
  const AValue: AnsiString);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue[AIndex] := AValue;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringListField.GetType: Word;
begin
  Result := PT_MV_STRING8;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: AnsiString;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then      
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;

        SetLength(TempValue, TempSize);
        if Read(TempValue[1], TempSize) <> Int64(TempSize) then Abort;

        Add(PAnsiChar(TempValue));
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: AnsiString;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        TempSize := Length(TempValue) + 1;        
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue[1], TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileAnsiStringListField.Add(const AValue: AnsiString): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAnsiStringListField.Insert(AIndex: Integer;
  const AValue: AnsiString);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Insert(AIndex, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAnsiStringListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileAnsiStringListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileUnicodeStringField
//==============================================================================

function TabfWABFileUnicodeStringField.GetType: Word;
begin
  Result := PT_UNICODE;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
  TempStr: WideString;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    SetLength(TempStr, (TempSize + 1) div 2);
    if AStream.Read(TempStr[1], TempSize) <> Int64(TempSize) then Abort;

    FValue := PWideChar(TempStr);

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := (Length(FValue) + 1) * 2;
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue[1], TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileUnicodeStringListField
//==============================================================================

constructor TabfWABFileUnicodeStringListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TWideStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileUnicodeStringListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringListField.GetValue(AIndex: Integer): WideString;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := FValue[AIndex];
end;

//------------------------------------------------------------------------------

procedure TabfWABFileUnicodeStringListField.SetValue(AIndex: Integer;
  const AValue: WideString);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue[AIndex] := AValue;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringListField.GetType: Word;
begin
  Result := PT_MV_UNICODE;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: WideString;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;

        SetLength(TempValue, (TempSize + 1) div 2);
        if Read(TempValue[1], TempSize) <> Int64(TempSize) then Abort;

        Add(PWideChar(TempValue));
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: WideString;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        TempSize := (Length(TempValue) + 1) * 2;        
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue[1], TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileUnicodeStringListField.Add(const AValue: WideString): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileUnicodeStringListField.Insert(AIndex: Integer;
  const AValue: WideString);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Insert(AIndex, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileUnicodeStringListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileUnicodeStringListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileCLSIDField
//==============================================================================

function TabfWABFileCLSIDField.GetType: Word;
begin
  Result := PT_CLSID;
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
    if TempSize <> SizeOf(FValue) then Abort;

    Result := AStream.Read(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    TempSize := SizeOf(FValue);
    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(FValue, TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileCLSIDListField
//==============================================================================

constructor TabfWABFileCLSIDListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileCLSIDListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDListField.GetValue(AIndex: Integer): TGUID;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).GetValue(Result);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCLSIDListField.SetValue(AIndex: Integer; AValue: TGUID);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TabfWABFileByteArray(FValue[AIndex]).SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDListField.GetType: Word;
begin
  Result := PT_MV_CLSID;
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: TGUID;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempSize <> 0 then
        CopyFrom(AStream, TempSize);
        
      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

        if TempSize <= 0 then Continue;
        if TempSize <> SizeOf(TempValue) then Abort;

        if Read(TempValue, TempSize) <> Int64(TempSize) then Abort;

        Add(TempValue);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: TGUID;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      TempSize := SizeOf(TempValue);

      for i := 0 to Count - 1 do
      begin
        TempValue := Values[i];
        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue, TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;
      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileCLSIDListField.Add(AValue: TGUID): Integer;
begin
  Result := Count;
  Insert(Result, AValue);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCLSIDListField.Insert(AIndex: Integer; AValue: TGUID);
var
  TempItem: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempItem := TabfWABFileByteArray.Create;
  try
    FValue.Insert(AIndex, TempItem);
  except
    TempItem.Free;
    raise;
  end;

  TempItem.SetValue(AValue, SizeOf(AValue));
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCLSIDListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileCLSIDListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileBinaryField
//==============================================================================

constructor TabfWABFileByteArrayField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TabfWABFileByteArray.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileByteArrayField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayField.GetType: Word;
begin
  Result := PT_BINARY;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayField.LoadFromStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
  TempStr: AnsiString;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    SetLength(TempStr, TempSize);
    if AStream.Read(TempStr[1], TempSize) <> Int64(TempSize) then Abort;

    FValue.SetValue(TempStr[1], TempSize);

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayField.SaveToStream(AStream: TStream): Boolean;
var
  TempSize: DWORD;
  TempStr: AnsiString;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempSize := FValue.Size;
    SetLength(TempStr, TempSize);
    FValue.GetValue(TempStr[1]);

    if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    Result := AStream.Write(TempStr[1], TempSize) = Int64(TempSize);
  except
  end;
end;


//==============================================================================
// TabfWABFileByteArrayListField
//==============================================================================

constructor TabfWABFileByteArrayListField.Create(ATag: DWORD);
begin
  inherited Create(ATag);

  FValue := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileByteArrayListField.Destroy;
begin
  FreeAndNil(FValue);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayListField.GetCount: Integer;
begin
  Result := FValue.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayListField.GetValue(AIndex: Integer): TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := TabfWABFileByteArray(FValue[AIndex]);
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayListField.GetIsEmpty: Boolean;
begin
  Result := (inherited GetIsEmpty) or (Count = 0);
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayListField.GetType: Word;
begin
  Result := PT_MV_BINARY;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayListField.LoadFromStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: AnsiString;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    if AStream.Read(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;
    if AStream.Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if (TempSize <> 0) then
        CopyFrom(AStream, TempSize);

      Position := 0;

      for i := 0 to TempCount - 1 do
      begin
        if Read(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

//        if TempSize <= 0 then Continue;

        SetLength(TempValue, TempSize);
        if Read(TempValue[1], TempSize) <> Int64(TempSize) then Abort;
        Add(TempValue[1], TempSize);
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayListField.SaveToStream(AStream: TStream): Boolean;
var
  TempCount: DWORD;
  TempSize: DWORD;
  TempStream: TMemoryStream;
  TempValue: AnsiString;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;
  if not Assigned(FValue) then Exit;

  try
    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      for i := 0 to Count - 1 do
      begin
        TempSize := Values[i].Size;

        SetLength(TempValue, TempSize);
        Values[i].GetValue(TempValue[1]);

        if Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;
        if Write(TempValue[1], TempSize) <> Int64(TempSize) then Abort;
      end;

      TempCount := Count;

//      if TempCount = 0 then TempCount := 1;

      if AStream.Write(TempCount, SizeOf(TempCount)) <> SizeOf(TempCount) then Abort;

      TempSize := TempStream.Size;
      if AStream.Write(TempSize, SizeOf(TempSize)) <> SizeOf(TempSize) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileByteArrayListField.Add(const AValue;
  AValueSize: Integer): Integer;
begin
  Result := Count;
  Insert(Result, AValue, AValueSize);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileByteArrayListField.Insert(AIndex: Integer;
  const AValue; AValueSize: Integer);
var
  TempItem: TabfWABFileByteArray;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  TempItem := TabfWABFileByteArray.Create;
  try
    FValue.Insert(AIndex, TempItem);
  except
    TempItem.Free;
    raise;
  end;

  TempItem.SetValue(AValue, AValueSize);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileByteArrayListField.Clear;
begin
  FValue.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileByteArrayListField.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FValue.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileDataRecord
//==============================================================================

constructor TabfWABFileDataRecord.Create;
begin
  inherited Create;

  FFields := TObjectList.Create;

  FIsValid := 1;
  FType := 0;
  FID := 0;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileDataRecord.Destroy;
begin
  FreeAndNil(FFields);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileDataRecord.GetCount: Integer;
begin
  Result := FFields.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileDataRecord.GetField(AIndex: Integer): TabfWABFileField;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := TabfWABFileField(FFields[AIndex]);
end;

//------------------------------------------------------------------------------

function TabfWABFileDataRecord.GetIsEmpty: Boolean;
var
  i: Integer;
begin
  Result := (ID = 0) or (Count = 0);
  if Result then Exit;

  for i := 0 to Count - 1 do
    if not TabfWABFileField(FFields[i]).IsEmpty then Exit;

  Result := True;  
end;

//------------------------------------------------------------------------------

function TabfWABFileDataRecord.LoadFromStream(AStream: TStream): Boolean;
var
  TempHeader: TabfWABFileDataRecordHeader;
  TempStream: TMemoryStream;
  TempFieldList: array of DWORD;
  TempFieldTag: DWORD;
  TempField: TabfWABFileCustomField;
  i: Integer;
begin
  Result := False;
  Clear;
  if not Assigned(AStream) then Exit;

  try
    if AStream.Read(TempHeader, SizeOf(TempHeader)) <> SizeOf(TempHeader) then Abort;

    //always seems to be 0x20
    if TempHeader.Mystery4 <> $20 then Abort;

    if ((TempHeader.FieldCount shl 2) <> TempHeader.OpCountCRC1) or
      (TempHeader.OpCountCRC1 <> TempHeader.OpCountCRC2) then Abort;

    FIsValid := TempHeader.RecordExists;
    FType := TempHeader.RecordType;
    FID := TempHeader.RecordID;

    SetLength(TempFieldList, TempHeader.FieldCount);

    if AStream.Read(TempFieldList[0],
      Length(TempFieldList) * SizeOf(TempFieldList[0])) <>
      Length(TempFieldList) * SizeOf(TempFieldList[0]) then Abort;

    TempStream := TMemoryStream.Create;
    with TempStream do
    try
      if TempHeader.DataSize <> 0 then
        CopyFrom(AStream, TempHeader.DataSize);
        
      Position := 0;

      for i := 0 to TempHeader.FieldCount - 1 do
      begin
        if Read(TempFieldTag, SizeOf(TempFieldTag)) <> SizeOf(TempFieldTag) then Abort;

        case PROP_TYPE(TempFieldTag) of
          PT_BOOLEAN:     TempField := TabfWABFileBooleanField.Create(TempFieldList[i]);
          PT_MV_BOOLEAN:  TempField := TabfWABFileBooleanListField.Create(TempFieldList[i]);
          PT_LONG:        TempField := TabfWABFileIntegerField.Create(TempFieldList[i]);
          PT_MV_LONG:     TempField := TabfWABFileIntegerListField.Create(TempFieldList[i]);
          PT_I2:          TempField := TabfWABFileSmallIntField.Create(TempFieldList[i]);
          PT_MV_I2:       TempField := TabfWABFileSmallIntListField.Create(TempFieldList[i]);
          PT_I8:          TempField := TabfWABFileInt64Field.Create(TempFieldList[i]);
          PT_MV_I8:       TempField := TabfWABFileInt64ListField.Create(TempFieldList[i]);
          PT_CURRENCY:    TempField := TabfWABFileCurrencyField.Create(TempFieldList[i]);
          PT_MV_CURRENCY: TempField := TabfWABFileCurrencyListField.Create(TempFieldList[i]);
          PT_R4:          TempField := TabfWABFileSingleField.Create(TempFieldList[i]);
          PT_MV_R4:       TempField := TabfWABFileSingleListField.Create(TempFieldList[i]);
          PT_DOUBLE:      TempField := TabfWABFileDoubleField.Create(TempFieldList[i]);
          PT_MV_DOUBLE:   TempField := TabfWABFileDoubleListField.Create(TempFieldList[i]);
          PT_APPTIME:     TempField := TabfWABFileAppTimeField.Create(TempFieldList[i]);
          PT_MV_APPTIME:  TempField := TabfWABFileAppTimeListField.Create(TempFieldList[i]);
          PT_SYSTIME:     TempField := TabfWABFileSysTimeField.Create(TempFieldList[i]);
          PT_MV_SYSTIME:  TempField := TabfWABFileSysTimeListField.Create(TempFieldList[i]);
          PT_STRING8:     TempField := TabfWABFileAnsiStringField.Create(TempFieldList[i]);
          PT_MV_STRING8:  TempField := TabfWABFileAnsiStringListField.Create(TempFieldList[i]);
          PT_UNICODE:     TempField := TabfWABFileUnicodeStringField.Create(TempFieldList[i]);
          PT_MV_UNICODE:  TempField := TabfWABFileUnicodeStringListField.Create(TempFieldList[i]);
          PT_CLSID:       TempField := TabfWABFileCLSIDField.Create(TempFieldList[i]);
          PT_MV_CLSID:    TempField := TabfWABFileCLSIDListField.Create(TempFieldList[i]);
          PT_BINARY:      TempField := TabfWABFileByteArrayField.Create(TempFieldList[i]);
          PT_MV_BINARY:   TempField := TabfWABFileByteArrayListField.Create(TempFieldList[i]);
        else
          begin
            if (PROP_TYPE(TempFieldTag) and MV_FLAG) = MV_FLAG then
              TempField := TabfWABFileUnknownListField.Create(TempFieldList[i])
            else
              TempField := TabfWABFileUnknownField.Create(TempFieldList[i]);
          end;
        end;

        if not Assigned(TempField) then Abort;
        try
          if not TempField.LoadFromStream(TempStream) then Abort;

          if TempField is TabfWABFileField then
            Add(TabfWABFileField(TempField))
          else
            Abort;  
        except
          FreeAndNil(TempField);
        end;
      end;
    finally
      TempStream.Free;
    end;

    Result := True;
  except
    Clear;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileDataRecord.SaveToStream(AStream: TStream): Boolean;
var
  TempHeader: TabfWABFileDataRecordHeader;
  TempStream: TMemoryStream;
  TempFieldList: array of DWORD;
  i: Integer;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  if {(FIsValid = 0) or {}IsEmpty then
  begin
    Result := True;
    Exit;
  end;

  with TempHeader do
  try
    TempStream := TMemoryStream.Create;
    try
      FillChar(TempHeader, SizeOf(TempHeader), 0);
      SetLength(TempFieldList, FFields.Count);

      for i := 0 to FFields.Count - 1 do
        with TabfWABFileField(FFields[i]) do
        begin
          if IsEmpty then Continue;

          TempFieldList[FieldCount] := Tag;

          if TempStream.Write(Tag, SizeOf(Tag)) <> SizeOf(Tag) then Abort;
          if not SaveToStream(TempStream) then Abort;

          Inc(FieldCount);
        end;

      SetLength(TempFieldList, FieldCount);

      RecordExists := FIsValid;
      RecordType := FType;
      RecordID := FID;

      //always seems to be 0x20
      Mystery4 := $20;
      //always seems to be FieldCount shl 2
      OpCountCRC1 := FieldCount shl 2;
      //always seems to be FieldCount shl 2
      OpCountCRC2 := FieldCount shl 2;
      DataSize := TempStream.Size;

      if AStream.Write(TempHeader,
        SizeOf(TempHeader)) <> SizeOf(TempHeader) then Abort;

      if AStream.Write(TempFieldList[0],
        Length(TempFieldList) * SizeOf(TempFieldList[0])) <>
        Length(TempFieldList) * SizeOf(TempFieldList[0]) then Abort;

      AStream.CopyFrom(TempStream, 0);
    finally
      TempStream.Free;
    end;

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFileDataRecord.Add(AField: TabfWABFileField): Integer;
begin
  Result := Count;
  Insert(Result, AField);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDataRecord.Insert(AIndex: Integer;
  AField: TabfWABFileField);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FFields.Insert(AIndex, AField);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDataRecord.Clear;
begin
  FFields.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDataRecord.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FFields.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileDataTable
//==============================================================================

constructor TabfWABFileDataTable.Create;
begin
  inherited Create;

  FRecords := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileDataTable.Destroy;
begin
  FreeAndNil(FRecords);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileDataTable.GetCount: Integer;
begin
  Result := FRecords.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileDataTable.GetRecord(AIndex: Integer): TabfWABFileDataRecord;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := TabfWABFileDataRecord(FRecords[AIndex]);
end;

//------------------------------------------------------------------------------

function TabfWABFileDataTable.Add(ARecord: TabfWABFileDataRecord): Integer;
begin
  Result := Count;
  Insert(Result, ARecord);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDataTable.Insert(AIndex: Integer;
  ARecord: TabfWABFileDataRecord);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FRecords.Insert(AIndex, ARecord);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDataTable.Clear;
begin
  FRecords.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileDataTable.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FRecords.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileSearchRecord
//==============================================================================

constructor TabfWABFileSearchRecord.Create;
begin
  inherited Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileSearchRecord.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchRecord.GetIsEmpty: Boolean;
begin
  Result := (ID = 0) or (Value = '');
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSearchRecord.SetValue(const AValue: WideString);
begin
  FValue := Copy(AValue, 1, CabfSearchValueSize - 1);
end;


//==============================================================================
// TabfWABFileSearchTable
//==============================================================================

constructor TabfWABFileSearchTable.Create;
begin
  inherited Create;

  FRecords := TObjectList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileSearchTable.Destroy;
begin
  FreeAndNil(FRecords);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchTable.GetCount: Integer;
begin
  Result := FRecords.Count;
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchTable.GetRecord(AIndex: Integer): TabfWABFileSearchRecord;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  Result := TabfWABFileSearchRecord(FRecords[AIndex]);
end;

//------------------------------------------------------------------------------

function TabfWABFileSearchTable.Add(ARecord: TabfWABFileSearchRecord): Integer;
begin
  Result := Count;
  Insert(Result, ARecord);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSearchTable.Insert(AIndex: Integer;
  ARecord: TabfWABFileSearchRecord);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FRecords.Insert(AIndex, ARecord);
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSearchTable.Clear;
begin
  FRecords.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileSearchTable.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EabfWABFileException.CreateFmt(SabfListIndexError, [AIndex]);

  FRecords.Delete(AIndex);
end;


//==============================================================================
// TabfWABFileTables
//==============================================================================

constructor TabfWABFileTables.Create;
begin
  inherited Create;

  FDataTable := TabfWABFileDataTable.Create;
  FDisplayNameTable := TabfWABFileSearchTable.Create;
  FFirstNameTable := TabfWABFileSearchTable.Create;
  FLastNameTable := TabfWABFileSearchTable.Create;
  FEmailTable := TabfWABFileSearchTable.Create;
  FNicknameTable := TabfWABFileSearchTable.Create;
end;

//------------------------------------------------------------------------------

destructor TabfWABFileTables.Destroy;
begin
  FreeAndNil(FNicknameTable);
  FreeAndNil(FEmailTable);
  FreeAndNil(FLastNameTable);
  FreeAndNil(FFirstNameTable);
  FreeAndNil(FDisplayNameTable);
  FreeAndNil(FDataTable);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfWABFileTables.Clear;
begin
  if Assigned(FDataTable) then
    FDataTable.Clear;
  if Assigned(FDisplayNameTable) then
    FDisplayNameTable.Clear;
  if Assigned(FFirstNameTable) then
    FFirstNameTable.Clear;
  if Assigned(FLastNameTable) then
    FLastNameTable.Clear;
  if Assigned(FEmailTable) then
    FEmailTable.Clear;
  if Assigned(FNicknameTable) then
    FNicknameTable.Clear;
end;


//==============================================================================
// TabfWABFile
//==============================================================================

constructor TabfWABFile.Create;
begin
  inherited Create;

  FTables := TabfWABFileTables.Create;

  Clear;
end;

//------------------------------------------------------------------------------

destructor TabfWABFile.Destroy;
begin
  FreeAndNil(FTables);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfWABFile.Clear;
begin
  if Assigned(FTables) then
    FTables.Clear;

  FLastRecordID := 0;
  FFileType := wftUnknown;
end;

//------------------------------------------------------------------------------

function TabfWABFile.GetNextRecordID: DWORD;
begin
  Inc(FLastRecordID);
  Result := FLastRecordID;
end;

//------------------------------------------------------------------------------

function TabfWABFile.LoadFromStream(AStream: TStream): Boolean;
var
  TempFileHeader: TabfWABFileHeader;
  TempIndexTables: TabfWABFileIndexTables;

  //-------------------------------------

  function _IsIndexTableValid(ATableType: TabfWABFileIndexTableType): Boolean;
  var
    TempSize: Cardinal;
  begin
    Result := False;
    with TempFileHeader do
      if ATableType = fitDataTable then
        Result := (Tables[ATableType].Count * SizeOf(TabfWABFileDataIndex)) =
          Tables[ATableType].Size
      else
        begin
          if FFileType = wftAnsi then
            TempSize := SizeOf(TabfWABFileSearchIndexA)
          else if FFileType = wftUnicode then
            TempSize := SizeOf(TabfWABFileSearchIndexW)
          else
            Exit;

          Result := (Tables[ATableType].Count * TempSize) =
            Tables[ATableType].Size;
        end;
  end;

  //-------------------------------------

  function _LoadDataTable(ADataIndexTable: TabfWABFileDataIndexTable;
    ATableType: TabfWABFileIndexTableType;
    ATable: TabfWABFileDataTable): Boolean;
  var
    TempDataRecord: TabfWABFileDataRecord;
    i: Integer;
  begin
    Result := False;
    if not Assigned(ATable) then Exit;

    try
      if not _IsIndexTableValid(ATableType) then Exit;

      with AStream, TempFileHeader do
      begin
        for i := 0 to ADataIndexTable.Count - 1 do
        begin
          Position := ADataIndexTable[i].Offset + TempIndexTables.Offset + TempIndexTables.Size;

          TempDataRecord := TabfWABFileDataRecord.Create;
          try
            if not TempDataRecord.LoadFromStream(AStream) then Abort;
            if TempDataRecord.ID <> ADataIndexTable[i].ID then Abort;

            ATable.Add(TempDataRecord);
          except
            TempDataRecord.Free;
          end;
        end;
      end;

      Result := True;
    except
    end;
  end;

  //-------------------------------------

  function _LoadSearchTable(ASearchIndexTable: TabfWABFileSearchIndexTable;
    ATableType: TabfWABFileIndexTableType;
    ATable: TabfWABFileSearchTable): Boolean;
  var
    TempSearchRecord: TabfWABFileSearchRecord;
    i: Integer;
  begin
    Result := False;
    if not Assigned(ATable) then Exit;

    try
      if not _IsIndexTableValid(ATableType) then Exit;

      with AStream, TempFileHeader do
      begin
        for i := 0 to ASearchIndexTable.Count - 1 do
        begin
          TempSearchRecord := TabfWABFileSearchRecord.Create;
          try
            if FFileType = wftAnsi then
              with TabfWABFileSearchIndexTableA(ASearchIndexTable)[i] do
              begin
                TempSearchRecord.ID := ID;
                TempSearchRecord.Value := AnsiString(PAnsiChar(@Value[0]));
              end
            else if FFileType = wftUnicode then
              with TabfWABFileSearchIndexTableW(ASearchIndexTable)[i] do
              begin
                TempSearchRecord.ID := ID;
                TempSearchRecord.Value := WideString(PWideChar(@Value[0]));
              end
            else
              Abort;

            ATable.Add(TempSearchRecord);
          except
            TempSearchRecord.Free;
          end;
        end;
      end;

      Result := True;
    except
    end;
  end;

  //-------------------------------------

begin
  Result := False;
  Clear;
  if not Assigned(AStream) then Exit;
  try
      if AStream.Read(TempFileHeader,
        SizeOf(TempFileHeader)) <> SizeOf(TempFileHeader) then Abort;

      if IsEqualMAPIUID(TempFileHeader.Magic, SabfFileHeaderMagicA) then
        FFileType := wftAnsi
      else if IsEqualMAPIUID(TempFileHeader.Magic, SabfFileHeaderMagicW) then
        FFileType := wftUnicode;

      if FFileType = wftUnknown then Abort;

      FLastRecordID := TempFileHeader.LastRecordID;

      if FFileType = wftAnsi then
        TempIndexTables := TabfWABFileIndexTablesA.Create
      else if FFileType = wftUnicode then
        TempIndexTables := TabfWABFileIndexTablesW.Create
      else
        Abort;
      try
        AStream.Position := $8A4;
        TempIndexTables.Offset := $8A4;
        TempIndexTables.RecordCapacity := TempFileHeader.RecordCapacity;

        TempIndexTables.DataTable.Count := TempFileHeader.Tables[fitDataTable].Count;
        TempIndexTables.DisplayNameTable.Count := TempFileHeader.Tables[fitDisplayNameTable].Count;
        TempIndexTables.LastNameTable.Count := TempFileHeader.Tables[fitLastNameTable].Count;
        TempIndexTables.FirstNameTable.Count := TempFileHeader.Tables[fitFirstNameTable].Count;
        TempIndexTables.EmailTable.Count := TempFileHeader.Tables[fitEmailTable].Count;
        TempIndexTables.NicknameTable.Count := TempFileHeader.Tables[fitNicknameTable].Count;

        if not TempIndexTables.LoadFromStream(AStream) then Abort;

        if not _LoadDataTable(TempIndexTables.DataTable,
          fitDataTable, FTables.DataTable) then Abort;
        if not _LoadSearchTable(TempIndexTables.DisplayNameTable,
          fitDisplayNameTable, FTables.DisplayNameTable) then Abort;
        if not _LoadSearchTable(TempIndexTables.LastNameTable,
          fitLastNameTable, FTables.LastNameTable) then Abort;
        if not _LoadSearchTable(TempIndexTables.FirstNameTable,
          fitFirstNameTable, FTables.FirstNameTable) then Abort;
        if not _LoadSearchTable(TempIndexTables.EmailTable,
          fitEmailTable, FTables.EmailTable) then Abort;
        if not _LoadSearchTable(TempIndexTables.NicknameTable,
          fitNicknameTable, FTables.NicknameTable) then Abort;

          Result := True;
      finally
        TempIndexTables.Free;
      end;
  except
    Clear;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFile.SaveToStream(AStream: TStream): Boolean;
var
  TempFileHeader: TabfWABFileHeader;
  TempIndexTables: TabfWABFileIndexTables;
  TempDataRecords: TMemoryStream;

  //-------------------------------------

  function _PrepareDataTable(ADataIndexTable: TabfWABFileDataIndexTable;
    ATableType: TabfWABFileIndexTableType;
    ATable: TabfWABFileDataTable): Boolean;
  var
    TempDataIndex: TabfWABFileDataIndex;
    i: Integer;
  begin
    Result := False;
    if not Assigned(ADataIndexTable) then Exit;
    if ATableType <> fitDataTable then Exit;
    if not Assigned(ATable) then Exit;

    try

      for i := 0 to ATable.Count - 1 do
      try
        if ATable.Records[i].IsEmpty then Continue;

        TempDataIndex.Offset := TempDataRecords.Position;
        TempDataIndex.ID := ATable.Records[i].ID;

        if not ATable.Records[i].SaveToStream(TempDataRecords) then Abort;

        ADataIndexTable.Add(TempDataIndex);
      except
        TempDataRecords.Size := TempDataIndex.Offset;
      end;

      Result := True;
    except
    end;
  end;

  //-------------------------------------

  function _PrepareSearchTable(ASearchIndexTable: TabfWABFileSearchIndexTable;
    ATableType: TabfWABFileIndexTableType;
    ATable: TabfWABFileSearchTable): Boolean;
  var
    TempSearchIndexA: TabfWABFileSearchIndexA;
    TempSearchIndexW: TabfWABFileSearchIndexW;
    TempS: AnsiString;
    i: Integer;
  begin
    Result := False;
    if not Assigned(ASearchIndexTable) then Exit;
    if ATableType = fitDataTable then Exit;
    if not Assigned(ATable) then Exit;

    try
      for i := 0 to ATable.Count - 1 do
      try
        if ATable.Records[i].IsEmpty then Continue;

        if FFileType = wftAnsi then
          with TabfWABFileSearchIndexTableA(ASearchIndexTable) do
          begin
            FillChar(TempSearchIndexA, SizeOf(TempSearchIndexA), 0);

            TempSearchIndexA.ID := ATable.Records[i].ID;
            TempS := ATable.Records[i].Value;

            Move(TempS[1], TempSearchIndexA.Value[0],
              Min(SizeOf(TempSearchIndexA.Value) - 1, Length(TempS)));

            Add(TempSearchIndexA);
          end
        else if FFileType = wftUnicode then
          with TabfWABFileSearchIndexTableW(ASearchIndexTable) do
          begin
            FillChar(TempSearchIndexW, SizeOf(TempSearchIndexW), 0);

            TempSearchIndexW.ID := ATable.Records[i].ID;
            Move(ATable.Records[i].Value[1], TempSearchIndexW.Value[0],
              Min(SizeOf(TempSearchIndexW.Value) - 2,
                Length(ATable.Records[i].Value) * 2));

            Add(TempSearchIndexW);
          end
        else
          Abort;

      except
      end;

      Result := True;
    except
    end;
  end;

  //-------------------------------------

  function _PrepareFileHeader: Boolean;
  begin
    Result := False;

    try
      FillChar(TempFileHeader, SizeOf(TempFileHeader), 0);

      with TempFileHeader.Tables[fitDataTable] do
      begin
        Count   := TempIndexTables.DataTable.Count;
        MaxSize := TempIndexTables.DataTable.MaxSize;
        Offset  := TempIndexTables.DataTable.Offset;
        Size    := TempIndexTables.DataTable.Size;
      end;

      with TempFileHeader.Tables[fitDisplayNameTable] do
      begin
        Count   := TempIndexTables.DisplayNameTable.Count;
        MaxSize := TempIndexTables.DisplayNameTable.MaxSize;
        Offset  := TempIndexTables.DisplayNameTable.Offset;
        Size    := TempIndexTables.DisplayNameTable.Size;
      end;

      with TempFileHeader.Tables[fitLastNameTable] do
      begin
        Count   := TempIndexTables.LastNameTable.Count;
        MaxSize := TempIndexTables.LastNameTable.MaxSize;
        Offset  := TempIndexTables.LastNameTable.Offset;
        Size    := TempIndexTables.LastNameTable.Size;
      end;

      with TempFileHeader.Tables[fitFirstNameTable] do
      begin
        Count   := TempIndexTables.FirstNameTable.Count;
        MaxSize := TempIndexTables.FirstNameTable.MaxSize;
        Offset  := TempIndexTables.FirstNameTable.Offset;
        Size    := TempIndexTables.FirstNameTable.Size;
      end;

      with TempFileHeader.Tables[fitEmailTable] do
      begin
        Count   := TempIndexTables.EmailTable.Count;
        MaxSize := TempIndexTables.EmailTable.MaxSize;
        Offset  := TempIndexTables.EmailTable.Offset;
        Size    := TempIndexTables.EmailTable.Size;
      end;

      with TempFileHeader.Tables[fitNicknameTable] do
      begin
        Count   := TempIndexTables.NicknameTable.Count;
        MaxSize := TempIndexTables.NicknameTable.MaxSize;
        Offset  := TempIndexTables.NicknameTable.Offset;
        Size    := TempIndexTables.NicknameTable.Size;
      end;

      with TempFileHeader do
      begin
        if FFileType = wftAnsi then
          Move(SabfFileHeaderMagicA, Magic, SizeOf(Magic))
        else if FFileType = wftUnicode then
          Move(SabfFileHeaderMagicW, Magic, SizeOf(Magic))
        else
          Abort;

        LastRecordID   := FLastRecordID;
        RecordCount    := TempIndexTables.RecordCount;
        RecordCapacity := TempIndexTables.RecordCapacity;
//        Unknown3       := 0;
        Unknown4       := $800;
//        Unknown5       := 0;
        Unknown6       := $A4; //SizeOf(TempFileHeader) ???
//        Unknown7[0]    := 3;
      end;

      Result := True;
    except
    end;
  end;

  //-------------------------------------

begin
  Result := False;
  if not Assigned(AStream) then Exit;

  try
    with AStream, TempFileHeader do
    begin
      if FFileType = wftAnsi then
        TempIndexTables := TabfWABFileIndexTablesA.Create
      else if FFileType = wftUnicode then
        TempIndexTables := TabfWABFileIndexTablesW.Create
      else
        Abort;
      try
        TempDataRecords := TMemoryStream.Create;
        try
          TempIndexTables.RecordGrowthDelta := 500;
          TempIndexTables.Offset := $8A4;

          if not _PrepareDataTable(TempIndexTables.DataTable,
            fitDataTable, FTables.DataTable) then Abort;
          if not _PrepareSearchTable(TempIndexTables.DisplayNameTable,
            fitDisplayNameTable, FTables.DisplayNameTable) then Abort;
          if not _PrepareSearchTable(TempIndexTables.LastNameTable,
            fitLastNameTable, FTables.LastNameTable) then Abort;
          if not _PrepareSearchTable(TempIndexTables.FirstNameTable,
            fitFirstNameTable, FTables.FirstNameTable) then Abort;
          if not _PrepareSearchTable(TempIndexTables.EmailTable,
            fitEmailTable, FTables.EmailTable) then Abort;
          if not _PrepareSearchTable(TempIndexTables.NicknameTable,
            fitNicknameTable, FTables.NicknameTable) then Abort;

          if not _PrepareFileHeader then Abort;

          if Write(TempFileHeader,
            SizeOf(TempFileHeader)) <> SizeOf(TempFileHeader) then Abort;

          AStream.Size := AStream.Size + $800;
          AStream.Position := AStream.Size;

          if not TempIndexTables.SaveToStream(AStream) then Abort;

          CopyFrom(TempDataRecords, 0);

          Result := True;
        finally
          TempDataRecords.Free;
        end;
      finally
        TempIndexTables.Free;
      end;
    end;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFile.LoadFromFile(const AFileName: WideString): Boolean;
var
  TempStream: TabfFileStream;
begin
  Result := False;

  try
    TempStream := TabfFileStream.Create(AFileName,
      fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(TempStream);
    finally
      TempStream.Free;
    end;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFile.SaveToFile(const AFileName: WideString): Boolean;
var
  TempStream: TabfFileStream;
begin
  Result := False;

  try
    TempStream := TabfFileStream.Create(AFileName, fmCreate);
    try
      Result := SaveToStream(TempStream);
    finally
      TempStream.Free;
    end;
  except
  end;
end;


end.
