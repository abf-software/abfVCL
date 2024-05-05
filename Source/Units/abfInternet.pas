{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfInternet;

{$I abf.inc}

interface

uses
  Forms,
  Windows, WinInet, WinSock, Classes, SysUtils,
// ABF VCL
  abfConsts, abfSysUtils, abfClasses;

const
  SabfInet_AgentName = 'ABF Internet Agent (www.abf-software.com)';

  SabfInet_Protocol_Unknown   = 'unknown';
  SabfInet_Protocol_Ftp       = 'ftp';
  SabfInet_Protocol_Gopher    = 'gopher';
  SabfInet_Protocol_Http      = 'http';
  SabfInet_Protocol_Https     = 'https';

  SabfInet_Http_Get  = 'GET';
  SabfInet_Http_Post = 'POST';
  SabfInet_Http_Query = 'Content-Type: application/x-www-form-urlencoded';


  SabfInet_Delimiter_Protocol  = '://';
  SabfInet_Delimiter_Slash     = '/';
  SabfInet_Delimiter_Port      = ':';
  SabfInet_Delimiter_UserName  = '@';
  SabfInet_Delimiter_Password  = ':';
  SabfInet_Delimiter_ExtraInfo = '?';

  SabfInet_CopyBufferSize = 4096;

type

//==============================================================================
// TabfCustomInternetCopyThread
//==============================================================================
// Prototype of all internet oriented threads that provides a "Copy" operation
// asynchronously to the main VCL thread.

  TabfCustomInternetCopyThread = class(TabfCustomCopyThread)
  private
    FAgentName: string;
    FProxy: string;
    FProxyBypass: string;
    // Src part
    FSrcHostName: string;
    FSrcPassword: string;
    FSrcUserName: string;
    FSrcProtocol: string;
    FSrcFileName: string;
    FSrcExtraInfo: string;
    FSrcPort: Word;
    // Dst part
    FDstHostName: string;
    FDstPassword: string;
    FDstUserName: string;
    FDstProtocol: string;
    FDstFileName: string;
    FDstExtraInfo: string;
    FDstPort: Word;
  // Properties Get/Set
    // Src part
    procedure SetSrcProtocol(const A: string);
    procedure SetSrcHostName(const A: string);
    procedure SetSrcFileName(const A: string);
    procedure SetSrcUserName(const A: string);
    procedure SetSrcPassword(const A: string);
    procedure SetSrcExtraInfo(const A: string);
    procedure SetSrcPort(A: Word);
    // Dst part
    procedure SetDstProtocol(const A: string);
    procedure SetDstHostName(const A: string);
    procedure SetDstFileName(const A: string);
    procedure SetDstUserName(const A: string);
    procedure SetDstPassword(const A: string);
    procedure SetDstExtraInfo(const A: string);
    procedure SetDstPort(A: Word);
  protected
    FUseOwnSession: Boolean;
    FSession: HINTERNET;
    FConnect: HINTERNET;
    FRequest: HINTERNET;
    FUseCache: Boolean;
  // Main routines
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Copy; override;
  // Update routines
    procedure UpdateSrcName; virtual;
    procedure UpdateDstName; virtual;
  // Properties Get/Set
    function GetErrorMessage: string; override;
    procedure SetSrcName(const A: WideString); override;
    procedure SetDstName(const A: WideString); override;
  public
    constructor Create(const ASrcName, ADstName: WideString; CreateSuspended,
      AFreeOnTerminate: Boolean; AOnError: TabfCopyThreadErrorEvent;
      AOnProgress: TabfCopyThreadProgressEvent; AOnFinish: TNotifyEvent;
      const ExtraData: Pointer); override;
    constructor CreateForInternet(const SessionHandle: HINTERNET; const SrcURL,
      DstURL, AgentName, Proxy, ProxyBypass: string; UseCache, CreateSuspended,
      AFreeOnTerminate: Boolean; AOnError: TabfCopyThreadErrorEvent;
      AOnProgress: TabfCopyThreadProgressEvent; AOnFinish: TNotifyEvent;
      const ExtraData: Pointer); virtual;
    constructor CreateForInternetAllParams(const SessionHandle: HINTERNET;
      const SrcProtocol, SrcHostName, SrcFileName, SrcUserName, SrcPassword,
      SrcExtraInfo: string; SrcPort: Word;
      const DstProtocol, DstHostName, DstFileName, DstUserName, DstPassword,
      DstExtraInfo: string; DstPort: Word;
      const AgentName, Proxy, ProxyBypass: string; UseCache, CreateSuspended,
      AFreeOnTerminate: Boolean; AOnError: TabfCopyThreadErrorEvent;
      AOnProgress: TabfCopyThreadProgressEvent; AOnFinish: TNotifyEvent;
      const ExtraData: Pointer); virtual;
    destructor Destroy; override;
  // Main routines
    procedure Execute; override;
  // Properties
    property AgentName: string read FAgentName write FAgentName;
    property Proxy: string read FProxy write FProxy;
    property ProxyBypass: string read FProxyBypass write FProxyBypass;
    // Src part
    property SrcProtocol: string read FSrcProtocol write SetSrcProtocol;
    property SrcHostName: string read FSrcHostName write SetSrcHostName;
    property SrcFileName: string read FSrcFileName write SetSrcFileName;
    property SrcUserName: string read FSrcUserName write SetSrcUserName;
    property SrcPassword: string read FSrcPassword write SetSrcPassword;
    property SrcExtraInfo: string read FSrcExtraInfo write SetSrcExtraInfo;
    property SrcPort: Word read FSrcPort write SetSrcPort;
    // Dst part 
    property DstProtocol: string read FDstProtocol write SetDstProtocol;
    property DstHostName: string read FDstHostName write SetDstHostName;
    property DstFileName: string read FDstFileName write SetDstFileName;
    property DstUserName: string read FDstUserName write SetDstUserName;
    property DstPassword: string read FDstPassword write SetDstPassword;
    property DstExtraInfo: string read FDstExtraInfo write SetDstExtraInfo;
    property DstPort: Word read FDstPort write SetDstPort;
  end;{TabfCustomInternetCopyThread = class(TabfCustomCopyThread)}


//==============================================================================
// TabfHttpCopyThread
//==============================================================================

  TabfHttpCopyThread = class(TabfCustomInternetCopyThread)
  private
    function GetFlags: DWORD;
    function GetMethod: string;
    function GetAcceptTypes: string;
  protected
  // Main routines
    procedure Connect; override;
    procedure CalcTotalSize; override;
  // Properties Get/Set
    function GetSrcSecured: Boolean; virtual;
    procedure SetSrcSecured(A: Boolean); virtual;
  public
  // Properties
    property SrcSecured: Boolean read GetSrcSecured
      write SetSrcSecured stored False;
    // TabfCustomCopyThread 
    property DstName;
    property SrcName;
    property TotalSize;
    property CopiedSize;
    property ErrorCode;
    property ErrorMessage;
    // TabfCustomInternetCopyThread
    property AgentName;
    property Proxy;
    property ProxyBypass;
    property SrcProtocol;
    property SrcHostName;
    property SrcFileName;
    property SrcUserName;
    property SrcPassword;
    property SrcExtraInfo;
    property SrcPort;
    property DstProtocol;
    property DstHostName;
    property DstFileName;
    property DstUserName;
    property DstPassword;
    property DstExtraInfo;
    property DstPort;
  // Events 
    property OnError;
    property OnProgress;
  end;{TabfHttpCopyThread = class(TabfCustomInternetCopyThread)}


//==============================================================================
// TabfFtpCopyThread
//==============================================================================

  TabfFtpCopyThread = class(TabfCustomInternetCopyThread)
  private
    function GetFlags: DWORD;
  protected
    FSrcPassiveMode: Boolean;
  // Main routines
    procedure Connect; override;
    procedure CalcTotalSize; override;
    procedure Copy; override;
  public
    property SrcPassiveMode: Boolean read FSrcPassiveMode write FSrcPassiveMode;
    // TabfCustomCopyThread
    property DstName;
    property SrcName;
    property TotalSize;
    property CopiedSize;
    property ErrorCode;
    property ErrorMessage;
    // TabfCustomInternetCopyThread
    property AgentName;
    property Proxy;
    property ProxyBypass;
    property SrcProtocol;
    property SrcHostName;
    property SrcFileName;
    property SrcUserName;
    property SrcPassword;
    property SrcExtraInfo;
    property SrcPort;
    property DstProtocol;
    property DstHostName;
    property DstFileName;
    property DstUserName;
    property DstPassword;
    property DstExtraInfo;
    property DstPort;
  // Events 
    property OnError;
    property OnProgress;
  end;{TabfFtpCopyThread = class(TabfCustomInternetCopyThread)}


//==============================================================================
//  URL routines
//==============================================================================

//------------------------------------------------------------------------------
// Parses given URL to url parts: Protocol, HostName, FileName, UserName,
// Password, ExtraInfo and Port. Uses WinInet Api.
// URL = Protocol://[UserName[:Password]@]HostName[:Port]/FileName?[ExtraInfo]
function abfParseURL(const URL: string; var Protocol, HostName, FileName,
  UserName, Password, ExtraInfo: string; var Port: Word): Boolean;

//------------------------------------------------------------------------------
// Creates the URL by concatenating of url parts: Protocol, HostName,
// FileName, UserName, Password, ExtraInfo and Port. Uses WinInet Api.
// URL = Protocol://[UserName[:Password]@]HostName[:Port]/FileName?[ExtraInfo]
function abfCreateURL(const Protocol, HostName, FileName, UserName,
  Password, ExtraInfo: string; Port: Word): string;

//------------------------------------------------------------------------------
// Parses given URL to url parts: Protocol, HostName, FileName, UserName,
// Password, ExtraInfo and Port.
// URL = Protocol://[UserName[:Password]@]HostName[:Port]/FileName[[?]ExtraInfo]
function abfParseURL2(const URL: string; var Protocol, HostName, FileName,
  UserName, Password, ExtraInfo: string; var Port: Word): Boolean;

//------------------------------------------------------------------------------
// Creates the URL by concatenating of url parts: Protocol, HostName,
// FileName, UserName, Password, ExtraInfo and Port.
// URL = Protocol://[UserName[:Password]@]HostName[:Port]/FileName[[?]ExtraInfo]
function abfCreateURL2(const Protocol, HostName, FileName, UserName,
  Password, ExtraInfo: string; Port: Word): string;

//------------------------------------------------------------------------------
// Canonicalizes a URL, which includes converting unsafe characters and spaces
// into escape sequences. Uses InternetCanonicalizeUrl API function.
// ICU_Flag is one of following values:
//   ICU_BROWSER_MODE - Does not encode or decode characters after "#" or "?",
//     and does not remove trailing white space after "?". If this value is not
//     specified, the entire URL is encoded, and trailing white space is
//     removed.
//   ICU_DECODE - Converts all %XX sequences to characters, including escape
//     sequences, before the URL is parsed.
//   ICU_ENCODE_SPACES_ONLY - Encodes spaces only.
//   ICU_NO_ENCODE - Does not convert unsafe characters to escape sequences.
//   ICU_NO_META - Does not remove meta sequences (such as "." and "..") from
//     the URL.
function abfCanonicalizeUrl(const URL: string; ICU_Flag: Integer): string;


//==============================================================================
// Download/Upload routines
//==============================================================================

//------------------------------------------------------------------------------
// Downloads file from the URL to FileName. Events can be nil.

function abfDownloadFile(const URL, FileName: string;
  OnError: TabfCopyThreadErrorEvent; OnProgress: TabfCopyThreadProgressEvent;
  OnFinish: TNotifyEvent): Boolean;


{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfStrUtils;


//==============================================================================
//  URL routines
//==============================================================================

//------------------------------------------------------------------------------
// Parses given URL to url parts: Protocol, HostName, FileName, UserName,
// Password, ExtraInfo and Port. Uses WinInet Api.
// URL = Protocol://[UserName[:Password]@]HostName[:Port]/FileName?[ExtraInfo]
// Date: 09/15/2000

function abfParseURL(const URL: string; var Protocol, HostName, FileName,
  UserName, Password, ExtraInfo: string; var Port: Word): Boolean;
var
  Components: TURLComponents{LPURL_COMPONENTS};

  //-------------------------------------

  procedure _GetProtocol;
  var
    i: Integer;
  begin
    Protocol := '';
  // Check is Protocol present
    i := Pos(SabfInet_Delimiter_Protocol, URL);
    if i <= 0 then Exit;
  // Get the Protocol value
    Protocol := Copy(URL, 1, i - 1);
  end;{Internal procedure _GetProtocol}

  //-------------------------------------

  procedure _InitValues;
  var
    UrlLength: Integer;
  begin
    UrlLength := Length(URL);
    SetLength(HostName, UrlLength);
    SetLength(FileName, UrlLength);
    SetLength(UserName, UrlLength);
    SetLength(Password, UrlLength);
    SetLength(ExtraInfo, UrlLength);
    abfInitRecord(Components, SizeOf(Components));
    with Components do
    begin
      lpszScheme        := nil;
      dwSchemeLength    := 0;
      nScheme           := INTERNET_SCHEME_DEFAULT;
      lpszHostName      := PChar(HostName);
      dwHostNameLength  := UrlLength;
      lpszUserName      := PChar(UserName);
      dwUserNameLength  := UrlLength;
      lpszPassword      := PChar(Password);
      dwPasswordLength  := UrlLength;
      lpszUrlPath       := PChar(FileName);
      dwUrlPathLength   := UrlLength;
      lpszExtraInfo     := PChar(ExtraInfo);
      dwExtraInfoLength := UrlLength;
    end;
  end;{Internal procedure _InitValues}

  //-------------------------------------

  procedure _UpdateValues;
  begin
    with Components do
    begin
      Port := nPort;
      SetLength(HostName, dwHostNameLength);
      SetLength(FileName, dwUrlPathLength);
      SetLength(UserName, dwUserNameLength);
      SetLength(Password, dwPasswordLength);
      SetLength(ExtraInfo, dwExtraInfoLength);
    end;
  end;{Internal procedure _UpdateValues}

  //-------------------------------------

begin
  _GetProtocol;
  _InitValues;
  Result := InternetCrackUrl(PChar(URL), Length(URL), ICU_DECODE, Components);
  _UpdateValues;
end;{function abfParseURL}


//------------------------------------------------------------------------------
// Creates the URL by concatenating of url parts: Protocol, HostName,
// FileName, UserName, Password, ExtraInfo and Port. Uses WinInet Api.
// URL = Protocol://[UserName[:Password]@]HostName[:Port]/FileName?[ExtraInfo]
// Date: 09/15/2000

function abfCreateURL(const Protocol, HostName, FileName, UserName,
  Password, ExtraInfo: string; Port: Word): string;
var
  Components: TURLComponents{LPURL_COMPONENTS};
  Len: DWORD;

  //-------------------------------------

  procedure _InitValues;
  begin
    Len := INTERNET_MAX_URL_LENGTH;
    SetLength(Result, Len);
    abfInitRecord(Components, SizeOf(Components));
    with Components do
    begin
      lpszScheme        := nil;
      dwSchemeLength    := 0;
      nScheme           := INTERNET_SCHEME_DEFAULT;
      nPort             := Port;
      lpszHostName      := Pointer(HostName);
      dwHostNameLength  := Length(HostName);
      lpszUserName      := Pointer(UserName);
      dwUserNameLength  := Length(UserName);
      lpszPassword      := Pointer(Password);
      dwPasswordLength  := Length(Password);
      lpszUrlPath       := Pointer(FileName);
      dwUrlPathLength   := Length(FileName);
      lpszExtraInfo     := Pointer(ExtraInfo);
      dwExtraInfoLength := Length(ExtraInfo);
    end;
  end;{Internal procedure _InitValues}

  //-------------------------------------

  procedure _SetProtocol;
  var
    i: Integer;
  begin
    if Protocol = '' then Exit;
  // Check is Protocol present, remove if is 
    i := Pos(SabfInet_Delimiter_Protocol, Result);
    if i > 0 then
      Delete(Result, 1, i + Length(SabfInet_Delimiter_Protocol) - 1);
  // Set the Protocol value (with delimiter) 
    if Pos(SabfInet_Delimiter_Protocol, Protocol) <= 0 then
      Result := SabfInet_Delimiter_Protocol + Result;
    Result := Protocol + Result;
  end;{Internal procedure _SetProtocol}

  //-------------------------------------

begin
  _InitValues;
  InternetCreateUrl(Components, 0, PChar(Result), Len);
  SetLength(Result, Len);
  _SetProtocol;
end;{function abfCreateURL}


//------------------------------------------------------------------------------
// Parses given URL to url parts: Protocol, HostName, FileName, UserName,
// Password, ExtraInfo and Port.
// URL = Protocol://[UserName[:Password]@]HostName/[:Port]FileName[[?]ExtraInfo]
// Date: 09/15/2000

function abfParseURL2(const URL: string; var Protocol, HostName, FileName,
  UserName, Password, ExtraInfo: string; var Port: Word): Boolean;
var
  S: string;

  //-------------------------------------

  procedure _GetProtocol;
  var
    i: Integer;
  begin
    Protocol := '';
  // Check is Protocol present
    i := Pos(SabfInet_Delimiter_Protocol, S);
    if i <= 0 then Exit;
  // Get the Protocol value 
    Protocol := Copy(S, 1, i - 1);
  // Fix the source string
    Delete(S, 1, i + Length(SabfInet_Delimiter_Protocol) - 1);
  end;{Internal procedure _GetProtocol}

  //-------------------------------------

  procedure _GetUserNameAndPassword;
  var
    i: Integer;
  begin
    UserName := '';
    Password := '';
  // Check is the UserName present 
    i := Pos(SabfInet_Delimiter_UserName, S);
    if i <= 0 then Exit;
  // Get UserName (possible with Password) 
    UserName := S;
    abfDeleteAfterChar(UserName, SabfInet_Delimiter_UserName);
  // Fix the source string 
    Delete(S, 1, i);
  // Check is the Password present 
    i := Pos(SabfInet_Delimiter_Password, UserName);
    if i <= 0 then Exit;
  // Get Password and fix the UserName string 
    Password := Copy(UserName, i + 1, MaxInt);
    Delete(UserName, i, MaxInt);
  end;{Internal procedure _GetUserNameAndPassword}

  //-------------------------------------

  procedure _GetHostAndFile;
  var
    i: Integer;
  begin
    HostName := S;
    FileName := '';
  // Check is the FileName present 
    i := Pos(SabfInet_Delimiter_Slash, HostName);
    if i <= 0 then Exit;
  // Get FileName and fix the HostName string
    FileName := Copy(HostName, i{ + 1}, MaxInt);
    Delete(HostName, i, MaxInt);
  end;{Internal procedure _GetHostAndFile}

  //-------------------------------------

  procedure _GetPort;
  var
    i: Integer;
  begin
    Port := INTERNET_INVALID_PORT_NUMBER;
  // Check is the Port value present 
    i := Pos(SabfInet_Delimiter_Port, HostName);
    if i <= 0 then Exit;
  // Get Port value and fix the HostName string 
    Port := StrToIntDef(Copy(HostName, i + 1, MaxInt),
      INTERNET_INVALID_PORT_NUMBER);
    Delete(HostName, i, MaxInt);
  end;{Internal procedure _GetPort}

  //-------------------------------------

  procedure _GetExtraInfo;
  var
    i: Integer;
  begin
    ExtraInfo := '';
  // Check is the ExtraInfo value present 
    i := Pos(SabfInet_Delimiter_ExtraInfo, FileName);
    if i <= 0 then Exit;
  // Get ExtraInfo value and fix the FileName string 
    ExtraInfo := Copy(FileName, i + 1, MaxInt);
    Delete(FileName, i, MaxInt);
  end;{Internal procedure _GetExtraInfo}

  //-------------------------------------

begin
  Result := True;
  S := URL;
  _GetProtocol;
  _GetUserNameAndPassword;
  _GetHostAndFile;
  _GetPort;
  _GetExtraInfo;
end;{function abfParseURL2}


//------------------------------------------------------------------------------
// Creates the URL by concatenating of url parts: Protocol, HostName,
// FileName, UserName, Password, ExtraInfo and Port.
// URL = Protocol://[UserName[:Password]@]HostName[:Port]/FileName[[?]ExtraInfo]
// Date: 09/15/2000

function abfCreateURL2(const Protocol, HostName, FileName, UserName,
  Password, ExtraInfo: string; Port: Word): string;
begin
  Result := '';
// Add Protocol 
  if Protocol <> '' then
  begin
    Result := Protocol;
    if Pos(SabfInet_Delimiter_Protocol, Result) <= 0 then
      Result := Result + SabfInet_Delimiter_Protocol;
  end;
// Add UserName [and Password] 
  if UserName <> '' then
  begin
    Result := Result + UserName;
    if Password <> '' then
      Result := Result + SabfInet_Delimiter_Password + Password;
    Result := Result + SabfInet_Delimiter_UserName;
  end;
// Add HostName 
  Result := Result + HostName;
// Add Port if present 
  if Port <> INTERNET_INVALID_PORT_NUMBER then
    Result := Result + SabfInet_Delimiter_Port + IntToStr(Port);
// Add FileName 
  if FileName <> '' then
    Result := abfAddChar(Result, Char(SabfInet_Delimiter_Slash)) + FileName;
// Add ExtraInfo 
  if ExtraInfo <> '' then
    Result := Result + abfAddCharBefore(ExtraInfo, SabfInet_Delimiter_ExtraInfo);
end;{function abfCreateURL2}

//------------------------------------------------------------------------------
// Canonicalizes a URL, which includes converting unsafe characters and spaces
// into escape sequences. Uses InternetCanonicalizeUrl API function.
// ICU_Flag is one of following values:
//   ICU_BROWSER_MODE - Does not encode or decode characters after "#" or "?",
//     and does not remove trailing white space after "?". If this value is not
//     specified, the entire URL is encoded, and trailing white space is
//     removed.
//   ICU_DECODE - Converts all %XX sequences to characters, including escape
//     sequences, before the URL is parsed.
//   ICU_ENCODE_SPACES_ONLY - Encodes spaces only.
//   ICU_NO_ENCODE - Does not convert unsafe characters to escape sequences.
//   ICU_NO_META - Does not remove meta sequences (such as "." and "..") from
//     the URL.
// Date: 09/15/2000

function abfCanonicalizeUrl(const URL: string; ICU_Flag: Integer): string;
var
  Len: DWORD;
begin
  Len := INTERNET_MAX_URL_LENGTH;
  SetLength(Result, Len);
  InternetCanonicalizeUrl(PChar(URL), PChar(Result), Len, ICU_Flag);
  SetLength(Result, Len);
end;


//==============================================================================
// Download/Upload routines
//==============================================================================

//------------------------------------------------------------------------------
// Downloads file from the URL to FileName. Events can be nil.

function abfDownloadFile(const URL, FileName: string;
  OnError: TabfCopyThreadErrorEvent; OnProgress: TabfCopyThreadProgressEvent;
  OnFinish: TNotifyEvent): Boolean;
var
  Thread: TabfCustomCopyThread;
  ThreadClass: TabfCopyThreadClass;
  Protocol, S: string;
  Port: Word;
begin
  Result := False;

// Check file name  
  if not abfIsValidFileName(FileName) then Exit;

// Determine protocol and class of thread by URL, HTTP by default
  abfParseURL2(URL, Protocol, S, S, S, S, S, Port);
  if CompareText(Protocol, SabfInet_Protocol_Ftp) = 0 then
    ThreadClass := TabfFtpCopyThread
  else
    ThreadClass := TabfHttpCopyThread;

// Download using thread
  Thread := ThreadClass.Create(URL, FileName, False, False, OnError,
    OnProgress, OnFinish, nil);
  try
    repeat
      if Application.Terminated then Exit;
      Application.ProcessMessages;
    until Thread.Terminated or Thread.Finished;{}
    Result := True;
  finally
    FreeAndNil(Thread);
  end;
end;




//////////////////////
//////////////////////
//////////////////////
//////////////////////
//////////////////////




function abfInternetCheckConnection: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------
// Function that checks the online status

function IsOnline: Boolean;
var
  Size: Integer;
  PC: Array[0..4] of Char;
  Key: hKey;

 function IsIPPresent: Boolean;
 type
   TaPInAddr = Array[0..10] of PInAddr;
   PaPInAddr = ^TaPInAddr;
 var
   phe: PHostEnt;
   pptr: PaPInAddr;
   Buffer: Array[0..63] of Char;
   I: Integer;
   GInitData: TWSAData;
   IP: String;
 begin
   WSAStartup($101, GInitData);
   Result := False;
   GetHostName(Buffer, SizeOf(Buffer));
   phe := GetHostByName(buffer);
   if phe = nil then Exit;
   pPtr := PaPInAddr(phe^.h_addr_list);
   I := 0;
   while pPtr^[I] <> nil do
    begin
     IP := inet_ntoa(pptr^[I]^);
     Inc(I);
    end;
   WSACleanup;
   Result := (IP <> '') and (IP <> '127.0.0.1');
 end;

begin
  if RegOpenKey(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Services\RemoteAccess', Key) = ERROR_SUCCESS then
   begin
    Size := 4;
    if RegQueryValueEx(Key, 'Remote Connection', nil, nil, @PC, @Size) = ERROR_SUCCESS then
     Result := PC[0] = #1
    else
     Result := IsIPPresent;
    RegCloseKey(Key);
   end
  else Result := IsIPPresent;
end;


//////////////////////
//////////////////////
//////////////////////
//////////////////////
//////////////////////
//////////////////////






//==============================================================================
// TabfCustomInternetCopyThread
//==============================================================================
{ TabfCustomInternetCopyThread }

constructor TabfCustomInternetCopyThread.Create(const ASrcName, ADstName: WideString;
  CreateSuspended, AFreeOnTerminate: Boolean;
  AOnError: TabfCopyThreadErrorEvent; AOnProgress: TabfCopyThreadProgressEvent;
  AOnFinish: TNotifyEvent; const ExtraData: Pointer);
begin
  FAgentName := SabfInet_AgentName;
  FProxy := '';
  FProxyBypass := '';
  FUseCache := False;
  FUseOwnSession := True;
  inherited Create(ASrcName, ADstName, CreateSuspended, AFreeOnTerminate,
    AOnError, AOnProgress, AOnFinish, ExtraData);
end;

//------------------------------------------------------------------------------
// Creates and initializes a new TabfCustomInternetCopyThread object and assigns
// all internet oriented properties. This constructor should be used as default
// consructor for all internet oriented treads.

constructor TabfCustomInternetCopyThread.CreateForInternet(
  const SessionHandle: HINTERNET; const SrcURL, DstURL, AgentName, Proxy,
  ProxyBypass: string; UseCache, CreateSuspended, AFreeOnTerminate: Boolean;
  AOnError: TabfCopyThreadErrorEvent; AOnProgress: TabfCopyThreadProgressEvent;
  AOnFinish: TNotifyEvent; const ExtraData: Pointer);
begin
  FAgentName := Trim(AgentName);
  if FAgentName = '' then FAgentName := SabfInet_AgentName;
  FProxy := Trim(Proxy);
  FProxyBypass := Trim(ProxyBypass);
  FUseCache := UseCache;
  FSession := nil;
  FUseOwnSession := True;
  if Assigned(SessionHandle) then
  begin
    FSession := SessionHandle;
    FUseOwnSession := False;
  end;
// Call old Create constructor
  inherited Create(SrcURL, DstURL, CreateSuspended, AFreeOnTerminate,
    AOnError, AOnProgress, AOnFinish, ExtraData);
end;

//------------------------------------------------------------------------------
// Same to the CreateForInternet constructor but receives internet based
// parameters separately.

constructor TabfCustomInternetCopyThread.CreateForInternetAllParams(
  const SessionHandle: HINTERNET;
  const SrcProtocol, SrcHostName, SrcFileName, SrcUserName, SrcPassword,
  SrcExtraInfo: string; SrcPort: Word;
  const DstProtocol, DstHostName, DstFileName, DstUserName, DstPassword,
  DstExtraInfo: string; DstPort: Word;
  const AgentName, Proxy, ProxyBypass: string; UseCache, CreateSuspended,
  AFreeOnTerminate: Boolean; AOnError: TabfCopyThreadErrorEvent;
  AOnProgress: TabfCopyThreadProgressEvent; AOnFinish: TNotifyEvent;
  const ExtraData: Pointer);
begin
// Src part
  FSrcProtocol := SrcProtocol;
  FSrcHostName := SrcHostName;
  FSrcFileName := SrcFileName;
  FSrcUserName := SrcUserName;
  FSrcPassword := SrcPassword;
  FSrcExtraInfo := SrcExtraInfo;
  FSrcPort := SrcPort;
  UpdateSrcName;
// Dst part
  FDstProtocol := DstProtocol;
  FDstHostName := DstHostName;
  FDstFileName := DstFileName;
  FDstUserName := DstUserName;
  FDstPassword := DstPassword;
  FDstExtraInfo := DstExtraInfo;
  FDstPort := DstPort;
  UpdateDstName;
// Create at the end because can be created not suspended
  CreateForInternet(SessionHandle, SrcName, DstName, AgentName, Proxy,
    ProxyBypass, UseCache, CreateSuspended, AFreeOnTerminate, AOnError,
    AOnProgress, AOnFinish, ExtraData);
end;

//------------------------------------------------------------------------------

destructor TabfCustomInternetCopyThread.Destroy;
begin
  if FUseOwnSession and Assigned(FSession) then InternetCloseHandle(FSession);
  inherited Destroy;
end;


//==============================================================================
// Main routines

procedure TabfCustomInternetCopyThread.Execute;
begin
  Connect;
  try
    if Terminated then Exit;
  // Get size of the data, CalcTotalSize should be implemented in descendants
    CalcTotalSize;
    if Terminated then Exit;
  // Progress 0%
    Synchronize(FireProgressEvent);
    if Terminated then Exit;
  // Process main routine, the Copy method should be implemented in descendants
    Copy;
    if Terminated then Exit;
  // Progress 100%
    Synchronize(FireProgressEvent);
  finally
    Disconnect;
    Finish;
  end;
end;

//------------------------------------------------------------------------------
// Open the connection

procedure TabfCustomInternetCopyThread.Connect;
var
  dwFlags: DWORD;
begin
// Open session if use own
  if FUseOwnSession then
  begin
    if FProxy = '' then dwFlags := INTERNET_OPEN_TYPE_PRECONFIG
    else dwFlags := INTERNET_OPEN_TYPE_PROXY;
    FSession := InternetOpen(Pointer(FAgentName), dwFlags, Pointer(FProxy),
      Pointer(FProxyBypass), 0);
  end;

// Error if session doesn't exist
  if not Assigned(FSession) then ErrorCode := ERROR_INTERNET_NO_CONTEXT;
end;

//------------------------------------------------------------------------------
// Close the connection

procedure TabfCustomInternetCopyThread.Disconnect;
begin
// Close all opened handles
  if Assigned(FRequest) then InternetCloseHandle(FRequest);
  FRequest := nil;
  if Assigned(FConnect) then InternetCloseHandle(FConnect);
  FConnect := nil;
  if FUseOwnSession and Assigned(FSession) then InternetCloseHandle(FSession);
  FSession := nil;
end;

//------------------------------------------------------------------------------
// Provides copying (downloading) from the Internet

procedure TabfCustomInternetCopyThread.Copy;
var
  DstHandle: THandle;

  //-------------------------------------

  procedure _Copy;
  var
    BytesRead, BytesWrite: DWORD{LongWord};
    Block: array[1..SabfInet_CopyBufferSize] of Byte;
  begin
    FCopiedSize := 0;
  // Copy block by block
    repeat
    // Read block
      BytesRead := 0;
      if not InternetReadFile(FRequest, @Block, SizeOf(Block), BytesRead) then
      begin
        ErrorCode := GetLastError;
        Exit; // the error occured
      end;
      if Terminated then Break;

    // Check for the end of file
      if BytesRead <= 0 then Break;

    // Write block
      BytesWrite := FileWrite(DstHandle, Block, BytesRead);
      if BytesWrite <> BytesRead then
      begin
        ErrorCode := ERROR_WRITE_FAULT;
        Exit; // the error occured
      end;
      Inc(FCopiedSize, BytesRead);
      if Terminated then Break;

   // Call the event if it is specified
      Synchronize(FireProgressEvent);
    until Terminated;
  end;{Internal procedure _Copy}

  //-------------------------------------

begin
  abfForceDirectories(ExtractFilePath(DstName));

  DstHandle := FileCreate(DstName);
  if DstHandle <> INVALID_HANDLE_VALUE then
    try
      _Copy;
    finally
      FileClose(DstHandle);
    end
  else
    ErrorCode := GetLastError;
end;{procedure TabfCustomInternetCopyThread.Copy}


//==============================================================================
// Update routines

procedure TabfCustomInternetCopyThread.UpdateSrcName;
begin
  FSrcName := abfCreateURL2(SrcProtocol, SrcHostName, SrcFileName, SrcUserName,
    SrcPassword, SrcExtraInfo, SrcPort);
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.UpdateDstName;
begin
  FDstName := abfCreateURL2(DstProtocol, DstHostName, DstFileName, DstUserName,
    DstPassword, DstExtraInfo, DstPort);
end;


//==============================================================================
// Properties Get/Set

function TabfCustomInternetCopyThread.GetErrorMessage: string;
begin
  Result := inherited GetErrorMessage;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcName(const A: WideString);
begin
  inherited SetSrcName(A);
  abfParseURL2(FSrcName, FSrcProtocol, FSrcHostName, FSrcFileName, FSrcUserName,
    FSrcPassword, FSrcExtraInfo, FSrcPort);
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstName(const A: WideString);
begin
  inherited SetDstName(A);
  abfParseURL2(FDstName, FDstProtocol, FDstHostName, FDstFileName, FDstUserName,
    FDstPassword, FDstExtraInfo, FDstPort);
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcProtocol(const A: string);
begin
  FSrcProtocol := A;
  UpdateSrcName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcHostName(const A: string);
begin
  FSrcHostName := A;
  UpdateSrcName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcFileName(const A: string);
begin
  FSrcFileName := A;
  UpdateSrcName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcUserName(const A: string);
begin
  FSrcUserName := A;
  UpdateSrcName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcPassword(const A: string);
begin
  FSrcPassword := A;
  UpdateSrcName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcExtraInfo(const A: string);
begin
  FSrcExtraInfo := A;
  UpdateSrcName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetSrcPort(A: Word);
begin
  FSrcPort := A;
  UpdateSrcName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstProtocol(const A: string);
begin
  FDstProtocol := A;
  UpdateDstName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstHostName(const A: string);
begin
  FDstHostName := A;
  UpdateDstName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstFileName(const A: string);
begin
  FDstFileName := A;
  UpdateDstName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstUserName(const A: string);
begin
  FDstUserName := A;
  UpdateDstName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstPassword(const A: string);
begin
  FDstPassword := A;
  UpdateDstName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstExtraInfo(const A: string);
begin
  FDstExtraInfo := A;
  UpdateDstName;
end;

//------------------------------------------------------------------------------

procedure TabfCustomInternetCopyThread.SetDstPort(A: Word);
begin
  FDstPort := A;
  UpdateDstName;
end;


//==============================================================================
// TabfHttpCopyThread
//==============================================================================
{ TabfHttpCopyThread }

procedure TabfHttpCopyThread.Connect;
begin
  if Terminated then Exit;

// Open session
  inherited Connect;
  if Terminated then Exit;

// Open HTTP connection
  if SrcSecured then
    if FSrcPort = INTERNET_INVALID_PORT_NUMBER then
      {F}SrcPort := INTERNET_DEFAULT_HTTPS_PORT; // Fix for secured connection
  FConnect := InternetConnect(FSession, Pointer(FSrcHostName), FSrcPort,
    Pointer(FSrcUserName), Pointer(FSrcPassword), INTERNET_SERVICE_HTTP, 0, 0);
  if not Assigned(FConnect) then
  begin
    ErrorCode := GetLastError;
    Exit; // the error occured
  end;

// Open Request (Don't use access types for IIS)
  FRequest := HttpOpenRequest(FConnect, Pointer(GetMethod), Pointer(FSrcFileName),
    HTTP_VERSION, nil, nil {Pointer(GetAcceptTypes)}, GetFlags, 0);
  if not Assigned(FRequest) then ErrorCode := GetLastError;
end;

//------------------------------------------------------------------------------

procedure TabfHttpCopyThread.CalcTotalSize;
var
  dwSize, dwCode, dwTemp: DWORD;
begin
  FTotalSize := 0;
  if Terminated then Exit;

// Send Request
  if FSrcExtraInfo = '' then
  begin
    if not HttpSendRequest(FRequest, PChar(GetAcceptTypes), $FFFFFFFF, nil, 0) then
//    if not HttpSendRequest(FRequest, nil, 0, nil, 0) then
    begin
      ErrorCode := GetLastError;
      Exit; // the error occured
    end;
  end else
  begin
{ TODO : Verify request header!!! }
    if not HttpSendRequest(FRequest, SabfInet_Http_Query,
      SizeOf(SabfInet_Http_Query), Pointer(FSrcExtraInfo),
      Length(FSrcExtraInfo)) then
    begin
      ErrorCode := GetLastError;
      Exit; // the error occured
    end;
  end;
  if Terminated then Exit;

// Query status
  dwSize := SizeOf(dwCode);
  dwCode := 0;
  dwTemp := 0;
  if not HttpQueryInfo(FRequest, HTTP_QUERY_STATUS_CODE
    or HTTP_QUERY_FLAG_NUMBER, @dwCode, dwSize, dwTemp) then
  begin
    ErrorCode := GetLastError;
    Exit; // the error occured
  end;

  if (dwCode >= HTTP_STATUS_AMBIGUOUS) then
  begin
    ErrorCode := dwCode;
    Exit; // the error occured
  end;
  if Terminated then Exit;

// Query size
  dwSize := SizeOf(FTotalSize);
  if not HttpQueryInfo(FRequest, HTTP_QUERY_CONTENT_LENGTH or
    HTTP_QUERY_FLAG_NUMBER, @FTotalSize, dwSize, dwTemp) then
    ErrorCode := GetLastError;
end;

//------------------------------------------------------------------------------

function TabfHttpCopyThread.GetFlags: DWORD;
begin
  Result := INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_KEEP_CONNECTION;
  if SrcSecured then
    Result := Result or INTERNET_FLAG_SECURE or SECURITY_INTERNET_MASK;
  if not FUseCache then Result := Result or INTERNET_FLAG_RELOAD;
end;

//------------------------------------------------------------------------------

function TabfHttpCopyThread.GetMethod: string;
begin
  if FSrcExtraInfo = '' then Result := SabfInet_Http_Get
  else Result := SabfInet_Http_Post;
end;

//------------------------------------------------------------------------------

function TabfHttpCopyThread.GetAcceptTypes: string;
begin
  Result := 'Accept: */*';
end;


//==============================================================================
// Properties Get/Set

function TabfHttpCopyThread.GetSrcSecured: Boolean;
begin
  Result := (Pos(SabfInet_Protocol_Https, LowerCase(FSrcProtocol)) > 0);
end;

//------------------------------------------------------------------------------

procedure TabfHttpCopyThread.SetSrcSecured(A: Boolean);
begin
  if SrcSecured = A then Exit;
  if A then FSrcProtocol := SabfInet_Protocol_Https
  else FSrcProtocol := SabfInet_Protocol_Http;
  UpdateSrcName;
end;


//==============================================================================
// TabfFtpCopyThread
//==============================================================================
{ TabfFtpCopyThread }

procedure TabfFtpCopyThread.Connect;
var
  dwPassive: DWORD;
begin
  if Terminated then Exit;

// Open session
  inherited Connect;
  if Terminated then Exit;
  
// Open FTP connection
  if SrcPassiveMode then dwPassive := INTERNET_FLAG_PASSIVE else dwPassive := 0;
  FConnect := InternetConnect(FSession, Pointer(FSrcHostName), FSrcPort,
    Pointer(FSrcUserName), Pointer(FSrcPassword), INTERNET_SERVICE_FTP,
    dwPassive, 0);
  if FConnect = nil then ErrorCode := GetLastError;
end;

//------------------------------------------------------------------------------

procedure TabfFtpCopyThread.CalcTotalSize;
var
  FFd: TWin32FindData{WIN32_FIND_DATA};
begin
  if Terminated then Exit;

// Find file and try to get the size value
  FRequest := FtpFindFirstFile(FConnect, Pointer(FSrcFileName), FFd, 0, 0);
  if not Assigned(FRequest) then
  begin
    ErrorCode := GetLastError;
    Exit; // the error occured
  end;

// Get size
  FTotalSize := FFd.nFileSizeLow;
  InternetCloseHandle(FRequest);
end;

//------------------------------------------------------------------------------

procedure TabfFtpCopyThread.Copy;
begin
  if Terminated then Exit;

// Open source file for reading
  FRequest := FTPOpenFile(FConnect, Pointer(FSrcFileName), GENERIC_READ,
    GetFlags, 0);
  if not Assigned(FRequest) then
  begin
    ErrorCode := GetLastError;
    Exit; // the error occured
  end;

// Process copying
  try
    if Terminated then Exit;
    inherited Copy;
  finally
    InternetCloseHandle(FRequest);
  end;
end;

//------------------------------------------------------------------------------

function TabfFtpCopyThread.GetFlags: DWORD;
begin
  Result := INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_KEEP_CONNECTION
    or FTP_TRANSFER_TYPE_BINARY;
  if FSrcPort = INTERNET_INVALID_PORT_NUMBER then
    {F}SrcPort := INTERNET_DEFAULT_FTP_PORT;
  if not FUseCache then Result := Result or INTERNET_FLAG_RELOAD;
end;

//------------------------------------------------------------------------------

end{unit abfInternet}.

