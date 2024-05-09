{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfWAB;

{$I abf.inc}

interface

uses
  Windows, Classes, SysUtils,
  WabDefs, WabApi, WabIab,
  abfClasses, abfComponents,
  abfWABUtils;

{*******************************************************************************
  Units WabDefs, WabApi, WabIab, WabTags, WabCode are used in the abfWAB
product. These unints are part of the Windows Address Book (WAB) functions
interfaces, taken from <http://delphi-jedi.org> site. You can get more
information about these units and the Project JEDI at <http://delphi-jedi.org>
*******************************************************************************}

type
//==============================================================================
// Forward declaration
//==============================================================================

  TabfWAB = class;

//==============================================================================
// TabfWABItem
//==============================================================================

  TabfWABItem = class(TCollectionItem)
  private
    FEntryID: PSBinary;
    FParentID: PSBinary;
    FSearchKey: PSBinary;
    FCreationTime: TFileTime;
    FLastModificationTime: TFileTime;
    FDisplayName: string;
    FItemType: Cardinal;
    FReading: Boolean;
    FWAB: TabfWAB;
    function GetEntryID: PSBinary;
    function GetParentID: PSBinary;
    function GetSearchKey: PSBinary;
    function GetCreationTime: TDateTime;
    function GetLastModificationTime: TDateTime;
    procedure SetEntryID(const AEntryID: PSBinary);
    procedure SetParentID(const AParentID: PSBinary);
    procedure SetSearchKey(const ASearchKey: PSBinary);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    function SetPropsValues(PropsCount: Byte; PropsValues: TSPropsArray;
      DeleteProps: Boolean): Boolean; virtual;
    property Reading: Boolean read FReading write FReading;  
    property WAB: TabfWAB read FWAB;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ApplyUpdates: Boolean; virtual;
    property CreationTime: TDateTime read GetCreationTime;
    property EntryID: PSBinary read GetEntryID;
    property ItemType: Cardinal read FItemType;
    property LastModificationTime: TDateTime read GetLastModificationTime;
    property ParentID: PSBinary read GetParentID;
    property SearchKey: PSBinary read GetSearchKey;
  published
    property DisplayName;
  end;

//==============================================================================
// TabfWABItemsCollection
//==============================================================================

  TabfWABItemsCollection = class(TCollection)
  private
    FWAB: TabfWAB;
  protected
    function GetItem(Index: Integer): TabfWABItem;
    procedure SetItem(Index: Integer; Value: TabfWABItem);
    function GetOwner: TPersistent;{$IFDEF D3}override;{$ELSE}virtual;{$ENDIF}
    property WAB: TabfWAB read FWAB;
  public
    constructor Create(abfWABItemClass: TCollectionItemClass; AWAB: TabfWAB);
    function Add: TabfWABItem;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TabfWABItem;
    property Items[Index: Integer]: TabfWABItem read GetItem write SetItem; default;
  end;

//==============================================================================
// TabfWABContactItem
//==============================================================================

  TabfWABContactDefaultAddress = (wdaDefaultAddress, wdaHomeAddress,
    wdaBusinessAddress);

  TabfWABContactItem = class(TabfWABItem)
  private
    FItemID: Cardinal;
    FMailUser: IMailUser;
    FNamePrefix: string;
    FGivenName: string;
    FMiddleName: string;
    FSurname: string;
    FGender: Cardinal;
    FNickname: string;
    FWeddingAnniversary: TFileTime;
    FBirthday: TFileTime;
    FAddrType: string;
    FEmailAddress: string;
    FAddrTypes: TStrings;
    FDefaultAddressIndex: Integer;
    FEmailAddresses: TStrings;
    FSpouseName: string;
    FChildrenNames: TStrings;
    FPersonalHomePage: string;
    FHomeTelephoneNumber: string;
    FMobileTelephoneNumber: string;
    FPagerTelephoneNumber: string;
    FHomeFaxNumber: string;
    FHomeAddressCity: string;
    FHomeAddressCountry: string;
    FHomeAddressPostalCode: string;
    FHomeAddressStateOrProvince: string;
    FHomeAddressStreet: TStrings;
    FBusinessHomePage: string;
    FCompanyName: string;
    FTitle: string;
    FDepartmentName: string;
    FOfficeLocation: string;
    FBusinessTelephoneNumber: string;
    FBusinessFaxNumber: string;
    FBusinessAddressCity: string;
    FBusinessAddressCountry: string;
    FBusinessAddressPostalCode: string;
    FBusinessAddressStateOrProvince: string;
    FBusinessAddressStreet: TStrings;
    FComment: TStrings;
    FEMailPlainTextOnly: Boolean;
    FIPPhone: string;
    FConferencingServers: TStrings;
    FDefaultConferencingServer: Integer;
    FBackupConferencingServer: Integer;
    FDefaultAddress: TabfWABContactDefaultAddress;
    procedure ItemsChange(Sender: TObject);
    function GetFileTimeProperty(ulPropTag: ULONG): TFileTime;
    function GetULONGProperty(ulPropTag: ULONG): ULONG;
    function GetWeddingAnniversary: TDateTime;
    function GetBirthday: TDateTime;
    function GetAddrType: string;
    function GetEmailAddress: string;
    function GetEMailPlainTextOnly: Boolean;
    procedure SetNamePrefix(const Value: string);
    procedure SetGivenName(const Value: string);
    procedure SetMiddleName(const Value: string);
    procedure SetSurname(const Value: string);
    procedure SetGender(Value: Cardinal);
    procedure SetNickname(const Value: string);
    procedure SetWeddingAnniversary(Value: TDateTime);
    procedure SetBirthday(Value: TDateTime);
    procedure SetAddrType(const Value: string);
    procedure SetEmailAddress(const Value: string);
    procedure SetAddrTypes(Value: TStrings);
    procedure SetDefaultAddressIndex(Value: Integer);
    procedure SetEmailAddresses(Value: TStrings);
    procedure SetSpouseName(const Value: string);
    procedure SetChildrenNames(Value: TStrings);
    procedure SetPersonalHomePage(const Value: string);
    procedure SetHomeTelephoneNumber(const Value: string);
    procedure SetMobileTelephoneNumber(const Value: string);
    procedure SetPagerTelephoneNumber(const Value: string);
    procedure SetHomeFaxNumber(const Value: string);
    procedure SetHomeAddressCity(const Value: string);
    procedure SetHomeAddressCountry(const Value: string);
    procedure SetHomeAddressPostalCode(const Value: string);
    procedure SetHomeAddressStateOrProvince(const Value: string);
    procedure SetHomeAddressStreet(Value: TStrings);
    procedure SetBusinessHomePage(const Value: string);
    procedure SetCompanyName(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetDepartmentName(const Value: string);
    procedure SetOfficeLocation(const Value: string);
    procedure SetBusinessTelephoneNumber(const Value: string);
    procedure SetBusinessFaxNumber(const Value: string);
    procedure SetBusinessAddressCity(const Value: string);
    procedure SetBusinessAddressCountry(const Value: string);
    procedure SetBusinessAddressPostalCode(const Value: string);
    procedure SetBusinessAddressStateOrProvince(const Value: string);
    procedure SetBusinessAddressStreet(Value: TStrings);
    procedure SetComment(Value: TStrings);
    procedure SetEMailPlainTextOnly(Value: Boolean);
    procedure SetIPPhone(const Value: string);
    procedure SetConferencingServers(Value: TStrings);
    procedure SetDefaultConferencingServer(Value: Integer);
    procedure SetBackupConferencingServer(Value: Integer);
    procedure SetDefaultAddress(Value: TabfWABContactDefaultAddress);
  protected
    function CheckIMailUser: Boolean; virtual;
    function SetPropsValues(PropsCount: Byte; PropsValues: TSPropsArray;
      DeleteProps: Boolean): Boolean; override;
    property WAB;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ApplyUpdates: Boolean; override;
    property CreationTime;
    property EntryID;
    property ItemType;
    property LastModificationTime;
    property SearchKey;
  published
    property DisplayName;
    property NamePrefix: string read FNamePrefix write SetNamePrefix;
    property GivenName: string read FGivenName write SetGivenName;
    property MiddleName: string read FMiddleName write SetMiddleName;
    property Surname: string read FSurname write SetSurname;
    property Gender: Cardinal read FGender write SetGender;
    property Nickname: string read FNickname write SetNickname;
    property WeddingAnniversary: TDateTime read GetWeddingAnniversary
      write SetWeddingAnniversary;
    property Birthday: TDateTime read GetBirthday write SetBirthday;
    property AddrType: string read GetAddrType write SetAddrType;
    property EmailAddress: string read GetEmailAddress write SetEmailAddress;
    property AddrTypes: TStrings read FAddrTypes write SetAddrTypes;
    property DefaultAddressIndex: Integer read FDefaultAddressIndex
      write SetDefaultAddressIndex;
    property EmailAddresses: TStrings read FEmailAddresses write SetEmailAddresses;
    property SpouseName: string read FSpouseName write SetSpouseName;
    property ChildrenNames: TStrings read FChildrenNames write SetChildrenNames;
    property PersonalHomePage: string read FPersonalHomePage
      write SetPersonalHomePage;
    property HomeTelephoneNumber: string read FHomeTelephoneNumber
      write SetHomeTelephoneNumber;
    property MobileTelephoneNumber: string read FMobileTelephoneNumber
      write SetMobileTelephoneNumber;
    property PagerTelephoneNumber: string read FPagerTelephoneNumber
      write SetPagerTelephoneNumber;
    property HomeFaxNumber: string read FHomeFaxNumber write SetHomeFaxNumber;
    property HomeAddressCity: string read FHomeAddressCity
      write SetHomeAddressCity;
    property HomeAddressCountry: string read FHomeAddressCountry
      write SetHomeAddressCountry;
    property HomeAddressPostalCode: string read FHomeAddressPostalCode
      write SetHomeAddressPostalCode;
    property HomeAddressStateOrProvince: string read FHomeAddressStateOrProvince
      write SetHomeAddressStateOrProvince;
    property HomeAddressStreet: TStrings read FHomeAddressStreet
      write SetHomeAddressStreet;
    property BusinessHomePage: string read FBusinessHomePage
      write SetBusinessHomePage;
    property CompanyName: string read FCompanyName write SetCompanyName;
    property Title: string read FTitle write SetTitle;
    property DepartmentName: string read FDepartmentName write SetDepartmentName;
    property OfficeLocation: string read FOfficeLocation write SetOfficeLocation;
    property BusinessTelephoneNumber: string read FBusinessTelephoneNumber
      write SetBusinessTelephoneNumber;
    property BusinessFaxNumber: string read FBusinessFaxNumber
      write SetBusinessFaxNumber;
    property BusinessAddressCity: string read FBusinessAddressCity
      write SetBusinessAddressCity;
    property BusinessAddressCountry: string read FBusinessAddressCountry
      write SetBusinessAddressCountry;
    property BusinessAddressPostalCode: string read FBusinessAddressPostalCode
      write SetBusinessAddressPostalCode;
    property BusinessAddressStateOrProvince: string read FBusinessAddressStateOrProvince
      write SetBusinessAddressStateOrProvince;
    property BusinessAddressStreet: TStrings read FBusinessAddressStreet
      write SetBusinessAddressStreet;
    property Comment: TStrings read FComment write SetComment;
    property EMailPlainTextOnly: Boolean read GetEMailPlainTextOnly
      write SetEMailPlainTextOnly default False;
    property IPPhone: string read FIPPhone write SetIPPhone;
    property ConferencingServers: TStrings read FConferencingServers
      write SetConferencingServers;
    property DefaultConferencingServer: Integer read FDefaultConferencingServer
      write SetDefaultConferencingServer;
    property BackupConferencingServer: Integer read FBackupConferencingServer
      write SetBackupConferencingServer;
    property DefaultAddress: TabfWABContactDefaultAddress read FDefaultAddress
      write SetDefaultAddress;
  end;{TabfWABContactItem = class(TCollectionItem)}

//==============================================================================
// TabfWABContactsCollection
//==============================================================================

  TabfWABContactsCollection = class(TabfWABItemsCollection)
  protected
    function GetItem(Index: Integer): TabfWABContactItem;
    procedure SetItem(Index: Integer; Value: TabfWABContactItem);
    function GetOwner: TPersistent;{$IFDEF D3}override;{$ELSE}virtual;{$ENDIF}
  public
    constructor Create(AWAB: TabfWAB);
    function Add: TabfWABContactItem;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TabfWABContactItem;
    property Items[Index: Integer]: TabfWABContactItem read GetItem write SetItem; default;
  end;

//==============================================================================
// TabfWABGroupItem
//==============================================================================

  TabfWABGroupItem = class(TabfWABItem)
  private
    FItemID: Cardinal;
    FDistList: IDistList;
    FHomePage: string;
    FTelephoneNumber: string;
    FFaxNumber: string;
    FCity: string;
    FCountry: string;
    FPostalCode: string;
    FStateOrProvince: string;
    FStreet: TStrings;
    FComment: TStrings;
    FTempVarArray: Variant;
    procedure ItemsChange(Sender: TObject);
    procedure SetHomePage(const Value: string);
    procedure SetTelephoneNumber(const Value: string);
    procedure SetFaxNumber(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetPostalCode(const Value: string);
    procedure SetStateOrProvince(const Value: string);
    procedure SetStreet(Value: TStrings);
    procedure SetComment(Value: TStrings);
  protected
    procedure AddEntry(const PSB: PSBinary);
    function CheckIDistList: Boolean; virtual;
    procedure DoInitDisplayName; virtual;
    function SetPropsValues(PropsCount: Byte; PropsValues: TSPropsArray;
      DeleteProps: Boolean): Boolean; override;
    property WAB;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ApplyUpdates: Boolean; override;
    procedure AddItems(ItemsIndexes: Variant); virtual;
    function GetItemsArray: Variant; virtual;
    function GetContactsArray(ProcessSubtree: Boolean): Variant; virtual;
    procedure RemoveItems(ItemsIndexes: Variant); virtual;
    property CreationTime;
    property EntryID;
    property ItemType;
    property LastModificationTime;
    property SearchKey;
  published
    property DisplayName;
    property HomePage: string read FHomePage write SetHomePage;
    property TelephoneNumber: string read FTelephoneNumber
      write SetTelephoneNumber;
    property FaxNumber: string read FFaxNumber write SetFaxNumber;
    property City: string read FCity write SetCity;
    property Country: string read FCountry write SetCountry;
    property PostalCode: string read FPostalCode write SetPostalCode;
    property StateOrProvince: string read FStateOrProvince
      write SetStateOrProvince;
    property Street: TStrings read FStreet write SetStreet;
    property Comment: TStrings read FComment write SetComment;
  end;{TabfWABGroupItem = class(TCollectionItem)}

//==============================================================================
// TabfWABGroupsCollection
//==============================================================================

  TabfWABGroupsCollection = class(TabfWABItemsCollection)
  protected
    function GetItem(Index: Integer): TabfWABGroupItem;
    procedure SetItem(Index: Integer; Value: TabfWABGroupItem);
    function GetOwner: TPersistent;{$IFDEF D3}override;{$ELSE}virtual;{$ENDIF}
  public
    constructor Create(AWAB: TabfWAB);
    function Add: TabfWABGroupItem;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TabfWABGroupItem;
    property Items[Index: Integer]: TabfWABGroupItem read GetItem write SetItem; default;
  end;

//==============================================================================
// TabfWABFolderItem
//==============================================================================

  TabfWABFolderItem = class(TabfWABItem)
  private
    FABContainer: IABContainer;
    FContainerFlags: Cardinal;
    FDisplayType: Cardinal;
    FDistListTemplateID: PSBinary;
    FItemID: Cardinal;
    FMailUserTemplateID: PSBinary;
    function GetDistListTemplateID: PSBinary;
    function GetMailUserTemplateID: PSBinary;
  protected
    function CheckIABContainer: Boolean; virtual;
    procedure DoInitDisplayName; virtual;
    property WAB;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetItemsArray: Variant; virtual;
    function GetContactsArray(ProcessSubtree: Boolean): Variant; virtual;
    property EntryID;
    property DistListTemplateID: PSBinary read GetDistListTemplateID;
    property MailUserTemplateID: PSBinary read GetMailUserTemplateID;
    property ItemType;
  published
    property DisplayName;
  end;{TabfWABFolderItem = class(TCollectionItem)}

//==============================================================================
// TabfWABFoldersCollection
//==============================================================================

  TabfWABFoldersCollection = class(TabfWABItemsCollection)
  protected
    function GetItem(Index: Integer): TabfWABFolderItem;
    procedure SetItem(Index: Integer; Value: TabfWABFolderItem);
    function GetOwner: TPersistent;{$IFDEF D3}override;{$ELSE}virtual;{$ENDIF}
  public
    constructor Create(AWAB: TabfWAB);
    function Add: TabfWABFolderItem;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TabfWABFolderItem;
    property Items[Index: Integer]: TabfWABFolderItem read GetItem write SetItem; default;
  end;

//==============================================================================
// EabfWABException
//==============================================================================

  EabfWABException = class(EabfException);

//==============================================================================
//  TabfAdviseSink
//==============================================================================
// A wrapper-class of the IMAPIAdviseSink interface.

  TabfAdviseNotifyEvent = procedure(cNotif: ULONG;
    lpNotifications: PNotification) of object;

  TabfAdviseSink = class(TInterfacedObject, IMAPIAdviseSink)
  private
    FOnNotifyEvent: TabfAdviseNotifyEvent;
  public
    function OnNotify(cNotif: ULONG;
      lpNotifications: PNotification): ULONG; stdcall;
    property OnNotifyEvent: TabfAdviseNotifyEvent read FOnNotifyEvent
      write FOnNotifyEvent;
  end;

//==============================================================================
//  TabfWAB
//==============================================================================
// Non-visual component TabfWAB (Windows Address Book). Provides full access to
// the Windows Address Book or to any files in the WAB format.

  TabfWABContentsFilter = (wcfAllContents, wcfSharedFolderOnly,
    wcfProfileFolderOnly, wcfCurrentFolderOnly);
  TabfWABAddToFolder = (wafSharedFolder, wafProfileFolder, wafCurrentFolder);
  TabfWABMode = (wmCurrentWAB, wmWABFile);
  TabfOnDeletingContactEvent = procedure(Sender: TObject;
    var AllowDelete: Boolean) of object;
  TabfOnDeletingGroupEvent = procedure(Sender: TObject;
    var AllowDelete: Boolean) of object;
  TabfOnDeletingItemEvent = procedure(Sender: TObject;
    var AllowDelete: Boolean) of object;

  TabfWAB = class(TabfComponent)
  private
    FLibHandle: THandle;
    FWABOpen: TWABOpen;
    FActive: Boolean;
    FAddrBook: IAddrBook;
    FAddToFolder: TabfWABAddToFolder;
    FAdviseSink: IMAPIAdviseSink;
    FAdviseSinkObject: TabfAdviseSink;
    FCachedUpdates: Boolean;
    FConnection: Cardinal;
    FContacts: TabfWABContactsCollection;
    FGroups: TabfWABGroupsCollection;
    FEnableProfiles: Boolean;
    FEventFired: Boolean;
    FFileName: TFileName;
    FFolders: TabfWABFoldersCollection;
    FContentsFilter: TabfWABContentsFilter;
    FCurrentContainer: IABContainer;
    FCurrentFolder: TabfWABFolderItem;
    FItems: TabfWABItemsCollection;
    FLoadingProgress: Boolean;
    FReadOnly: Boolean;
    FRootContainer: IABContainer;
    FStreamedActive: Boolean;
    FUpdateCount: Integer;
    FWABSharedMode: Boolean;
    FWABMode: TabfWABMode;
    FWABObject: IWabObject;
    FWABVersion: string;
    FOnOpenAddressBook: TNotifyEvent;
    FOnCloseAddressBook: TNotifyEvent;
    FOnDeleteContact: TNotifyEvent;
    FOnDeleteGroup: TNotifyEvent;
    FOnDeleteItem: TNotifyEvent;
    FOnDeletingContact: TabfOnDeletingContactEvent;
    FOnDeletingGroup: TabfOnDeletingGroupEvent;
    FOnDeletingItem: TabfOnDeletingItemEvent;
    FOnInternalItemsChange: TNotifyEvent;
    FOnInternalContactsChange: TNotifyEvent;
    FOnInternalGroupsChange: TNotifyEvent;
    FOnNewContact: TNotifyEvent;
    FOnNewGroup: TNotifyEvent;
    FOnNewItem: TNotifyEvent;
    FOnChangeAddressBook: TNotifyEvent;
    function GetOutlookSharedMode: Boolean;
    function GetCurrentFolder: TabfWABFolderItem;
    function GetRootContainer: IABContainer;
    procedure SetActive(Value: Boolean);
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetContactsCollection(Value: TabfWABContactsCollection);
    procedure SetContentsFilter(Value: TabfWABContentsFilter);
    procedure SetCurrentFolder(Value: TabfWABFolderItem);
    procedure SetFoldersCollection(Value: TabfWABFoldersCollection);
    procedure SetGroupsCollection(Value: TabfWABGroupsCollection);
    procedure SetEnableProfiles(Value: Boolean);
    procedure SetFileName(const Value: TFileName);
    procedure SetItemsCollection(Value: TabfWABItemsCollection);
    procedure SetOutlookSharedMode(Value: Boolean);
    procedure SetWABMode(Value: TabfWABMode);
    procedure SetWABVersion(const Value: string);
    procedure Hook_AB_Notification;
    procedure Unhook_AB_Notification;
  protected
    procedure Loaded; override;
    function GetFolderContentsTable(out ATable: IMAPITable): HResult; virtual;
    function GetRootEntryID(var cEID: ULONG; var pEID: PEntryID): HResult; virtual;
    function GetWABTemplateID(ObjType: ULONG; var ParentID: PSBinary;
      var TemplateID: PSBinary): HResult; virtual;
    function CreateEntry(ObjType: ULONG; var EID: PSBinary): HResult; virtual;
    procedure CheckActive; virtual;
    procedure CheckReadOnly; virtual;
    procedure DeleteEntries(const EntriesArray: TSBinaryArray); virtual;
    procedure DeleteWABItems(WABItems: TabfWABItemsCollection;
      ItemsIndexes: Variant); virtual;
    procedure DoAdviseNotify(cNotif: ULONG; lpNotifications: PNOTIFICATION);
    procedure DoInternalEvents; virtual;
    procedure DoOpenAddressBook; virtual;
    procedure DoCloseAddressBook; virtual;
    procedure DoDeleteContact; virtual;
    procedure DoDeleteGroup; virtual;
    procedure DoDeleteItem; virtual;
    procedure DoDeletingContact(var AllowDelete: Boolean); virtual;
    procedure DoDeletingGroup(var AllowDelete: Boolean); virtual;
    procedure DoDeletingItem(var AllowDelete: Boolean); virtual;
    procedure DoNewContact; virtual;
    procedure DoNewGroup; virtual;
    procedure DoNewItem; virtual;
    procedure DoChangeAddressBook; virtual;
    procedure DoRefreshFolders; virtual;
    procedure DoRefreshFolderContent; virtual;
    property LoadingProgress: Boolean read FLoadingProgress;
    property OnInternalItemsChange: TNotifyEvent read FOnInternalItemsChange
      write FOnInternalItemsChange;
    property OnInternalContactsChange: TNotifyEvent read FOnInternalContactsChange
      write FOnInternalContactsChange;
    property OnInternalGroupsChange: TNotifyEvent read FOnInternalGroupsChange
      write FOnInternalGroupsChange;
    property RootContainer: IABContainer read GetRootContainer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyUpdates; virtual;
    procedure BeginUpdate; virtual;
    procedure CancelUpdates; virtual;
    procedure EndUpdate; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    function ContactDetails(var ContactIndex: Integer): Boolean; virtual;
    procedure DeleteAllContacts; virtual;
    procedure DeleteAllGroups; virtual;
    procedure DeleteAllItems; virtual;
    procedure DeleteContacts(ContactsIndexes: Variant); virtual;
    procedure DeleteGroups(GroupsIndexes: Variant); virtual;
    procedure DeleteItems(ItemsIndexes: Variant); virtual;
    procedure Find; virtual;
    function GetContactIndex(const PSB: PSBinary): Integer; virtual;
    function GetGroupIndex(const PSB: PSBinary): Integer; virtual;
    function GetItemIndex(const PSB: PSBinary): Integer; virtual;
    function GetMeContactIndex(AllowCreate: Boolean): Integer; virtual;
    function GetMeItemIndex(AllowCreate: Boolean): Integer; virtual;
    function GroupDetails(var GroupIndex: Integer): Boolean; virtual;
    function ItemDetails(var ItemIndex: Integer): Boolean; virtual;
    function NewContact: Integer; virtual;
    function NewGroup: Integer; virtual;
    function NewItem(WABItemClass: TCollectionItemClass): Integer; virtual;
    function SetMeContact(ContactIndex: Integer; SelectDialog: Boolean): Boolean;
    function SetMeItem(ItemIndex: Integer; SelectDialog: Boolean): Boolean;
    procedure Refresh; virtual;
    function VCardCreate(ContactIndex: Integer;
      VCardFileName: TFileName): Boolean; virtual;
    function VCardDisplay(const VCardFileName: TFileName): Boolean; virtual;
    function VCardRetrieve(const VCardFileName: TFileName): Boolean; virtual;
    property AddrBook: IAddrBook read FAddrBook;
    property CurrentFolder: TabfWABFolderItem read GetCurrentFolder
      write SetCurrentFolder;
    property Folders: TabfWABFoldersCollection read FFolders
      write SetFoldersCollection stored False;
    property WABObject: IWabObject read FWABObject;
  published
    property About;
    property Active: Boolean read FActive write SetActive default False;
    property AddToFolder: TabfWABAddToFolder read FAddToFolder
      write FAddToFolder default wafProfileFolder;
    property CachedUpdates: Boolean read FCachedUpdates
      write SetCachedUpdates default False;
    property Contacts: TabfWABContactsCollection read FContacts
      write SetContactsCollection stored False;
    property ContentsFilter: TabfWABContentsFilter read FContentsFilter
      write SetContentsFilter default wcfAllContents;
    property Groups: TabfWABGroupsCollection read FGroups
      write SetGroupsCollection stored False;
    property EnableProfiles: Boolean read FEnableProfiles
      write SetEnableProfiles default False;
    property FileName: TFileName read FFileName write SetFileName;
    property Items: TabfWABItemsCollection read FItems
      write SetItemsCollection stored False;
    property OutlookSharedMode: Boolean read GetOutlookSharedMode
      write SetOutlookSharedMode stored False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property WABMode: TabfWABMode read FWABMode write SetWABMode
      default wmCurrentWAB;
    property WABVersion: string read FWABVersion write SetWABVersion stored False;
    property OnOpenAddressBook: TNotifyEvent read FOnOpenAddressBook
      write FOnOpenAddressBook;
    property OnCloseAddressBook: TNotifyEvent read FOnCloseAddressBook
      write FOnCloseAddressBook;
    property OnChangeAddressBook: TNotifyEvent read FOnChangeAddressBook
      write FOnChangeAddressBook;
    property OnDeleteContact: TNotifyEvent read FOnDeleteContact
      write FOnDeleteContact;
    property OnDeleteGroup: TNotifyEvent read FOnDeleteGroup
      write FOnDeleteGroup;
    property OnDeleteItem: TNotifyEvent read FOnDeleteItem
      write FOnDeleteItem;
    property OnDeletingContact: TabfOnDeletingContactEvent read FOnDeletingContact
      write FOnDeletingContact;
    property OnDeletingGroup: TabfOnDeletingGroupEvent read FOnDeletingGroup
      write FOnDeletingGroup;
    property OnDeletingItem: TabfOnDeletingItemEvent read FOnDeletingItem
      write FOnDeletingItem;
    property OnNewContact: TNotifyEvent read FOnNewContact write FOnNewContact;
    property OnNewGroup: TNotifyEvent read FOnNewGroup write FOnNewGroup;
    property OnNewItem: TNotifyEvent read FOnNewItem write FOnNewItem;
  end;

//==============================================================================
// Additional subroutines
//==============================================================================

  function abfLoadWabDll: THandle;
  procedure abfUnloadWabDll(Handle: THandle);

{******************************************************************************}
implementation
{******************************************************************************}

uses
{$IfDef D6}
  Variants,
{$EndIf D6}
  Forms, Controls, ComObj,
  WabTags, WabCode,
  abfVCLConsts, abfSysUtils,
  abfWABConsts;

{$I abf_init.inc}

//==============================================================================
// Private types
//==============================================================================

type
  PFakePCharArr = ^TFakePCharArr;
  TFakePCharArr = array[0..0] of PChar;

//==============================================================================
// The new properties of WAB
//==============================================================================
const
  PR_PARENTS                     = ULONG(PT_MV_BINARY or $800b0000); //shared or not
  PR_IP_PHONE                    = ULONG(PT_TSTRING or $800a0000);
  PR_DEFAULT_ADDRESS             = ULONG(PT_LONG or $80110000);
  PR_CONFERENCING_SERVERS        = ULONG(PT_MV_TSTRING or $80000000);
  PR_DEFAULT_CONFERENCING_SERVER = ULONG(PT_LONG or $80010000);
  PR_BACKUP_CONFERENCING_SERVER  = ULONG(PT_LONG or $80020000);

//==============================================================================
// Contact properties records
//==============================================================================
const
  abfWABItemProperties: record
    Count: ULONG;
    Definition: array[0..50] of ULONG;
  end = (
    Count: 51;
    Definition: (
       PR_PARENTS,
       PR_ENTRYID,
       PR_OBJECT_TYPE,
       PR_SEARCH_KEY,
       PR_CREATION_TIME,
       PR_LAST_MODIFICATION_TIME,
       PR_DISPLAY_NAME,
       PR_DISPLAY_NAME_PREFIX,
       PR_GIVEN_NAME,
       PR_MIDDLE_NAME,
       PR_SURNAME,
       PR_GENDER,
       PR_NICKNAME,
       PR_WEDDING_ANNIVERSARY,
       PR_BIRTHDAY,
       PR_ADDRTYPE,
       PR_EMAIL_ADDRESS,
       PR_CONTACT_ADDRTYPES,
       PR_CONTACT_DEFAULT_ADDRESS_INDEX,
       PR_CONTACT_EMAIL_ADDRESSES,
       PR_SPOUSE_NAME,
       PR_CHILDRENS_NAMES,
       PR_PERSONAL_HOME_PAGE,
       PR_HOME_TELEPHONE_NUMBER,
       PR_MOBILE_TELEPHONE_NUMBER,
       PR_PAGER_TELEPHONE_NUMBER,
       PR_HOME_FAX_NUMBER,
       PR_HOME_ADDRESS_CITY,
       PR_HOME_ADDRESS_COUNTRY,
       PR_HOME_ADDRESS_POSTAL_CODE,
       PR_HOME_ADDRESS_STATE_OR_PROVINCE,
       PR_HOME_ADDRESS_STREET,
       PR_BUSINESS_HOME_PAGE,
       PR_COMPANY_NAME,
       PR_TITLE,
       PR_DEPARTMENT_NAME,
       PR_OFFICE_LOCATION,
       PR_BUSINESS_TELEPHONE_NUMBER,
       PR_BUSINESS_FAX_NUMBER,
       PR_BUSINESS_ADDRESS_CITY,
       PR_BUSINESS_ADDRESS_COUNTRY,
       PR_BUSINESS_ADDRESS_POSTAL_CODE,
       PR_BUSINESS_ADDRESS_STATE_OR_PROVINCE,
       PR_BUSINESS_ADDRESS_STREET,
       PR_COMMENT,
       PR_SEND_INTERNET_ENCODING,
       PR_IP_PHONE,
       PR_CONFERENCING_SERVERS,
       PR_DEFAULT_CONFERENCING_SERVER,
       PR_BACKUP_CONFERENCING_SERVER,
       PR_DEFAULT_ADDRESS
    )
  );

  abfOutlookItemProperties: record
    Count: ULONG;
    Definition: array[0..50] of ULONG;
  end = (
    Count: 51;
    Definition: (
       ULONG(PR_PARENTS or $30000000),
       PR_ENTRYID,
       PR_OBJECT_TYPE,
       PR_SEARCH_KEY,
       PR_CREATION_TIME,
       PR_LAST_MODIFICATION_TIME,
       PR_DISPLAY_NAME,
       PR_DISPLAY_NAME_PREFIX,
       PR_GIVEN_NAME,
       PR_MIDDLE_NAME,
       PR_SURNAME,
       PR_GENDER,
       PR_NICKNAME,
       PR_WEDDING_ANNIVERSARY,
       PR_BIRTHDAY,
       PR_ADDRTYPE,
       PR_EMAIL_ADDRESS,
       PR_CONTACT_ADDRTYPES,
       PR_CONTACT_DEFAULT_ADDRESS_INDEX,
       PR_CONTACT_EMAIL_ADDRESSES,
       PR_SPOUSE_NAME,
       PR_CHILDRENS_NAMES,
       PR_PERSONAL_HOME_PAGE,
       PR_HOME_TELEPHONE_NUMBER,
       PR_MOBILE_TELEPHONE_NUMBER,
       PR_PAGER_TELEPHONE_NUMBER,
       PR_HOME_FAX_NUMBER,
       PR_HOME_ADDRESS_CITY,
       PR_HOME_ADDRESS_COUNTRY,
       PR_HOME_ADDRESS_POSTAL_CODE,
       PR_HOME_ADDRESS_STATE_OR_PROVINCE,
       PR_HOME_ADDRESS_STREET,
       PR_BUSINESS_HOME_PAGE,
       PR_COMPANY_NAME,
       PR_TITLE,
       PR_DEPARTMENT_NAME,
       PR_OFFICE_LOCATION,
       PR_BUSINESS_TELEPHONE_NUMBER,
       PR_BUSINESS_FAX_NUMBER,
       PR_BUSINESS_ADDRESS_CITY,
       PR_BUSINESS_ADDRESS_COUNTRY,
       PR_BUSINESS_ADDRESS_POSTAL_CODE,
       PR_BUSINESS_ADDRESS_STATE_OR_PROVINCE,
       PR_BUSINESS_ADDRESS_STREET,
       PR_COMMENT,
       PR_SEND_INTERNET_ENCODING,
       ULONG(PR_IP_PHONE or $30000000),
       ULONG(PR_CONFERENCING_SERVERS or $30000000),
       ULONG(PR_DEFAULT_CONFERENCING_SERVER or $30000000),
       ULONG(PR_BACKUP_CONFERENCING_SERVER or $30000000),
       ULONG(PR_DEFAULT_ADDRESS or $30000000)
    )
  );

  abfWABFolderProperties: record
    Count: ULONG;
    Definition: array[0..6] of ULONG;
  end = (
    Count: 7;
    Definition: (
       PR_CONTAINER_FLAGS,
       PR_ENTRYID,
       PR_OBJECT_TYPE,
       PR_DEF_CREATE_DL,
       PR_DEF_CREATE_MAILUSER,
       PR_DISPLAY_TYPE,
       PR_DISPLAY_NAME
    )
  );

//==============================================================================
// Private variables
//==============================================================================
var
  _WabDllList: TList = nil;
  _GlobalWABOpen: TWABOpen = nil;
  _GlobalAddrBook: IAddrBook = nil;
  _GlobalWABObject: IWABObject = nil;


//==============================================================================
// Private classes
//==============================================================================
type
  _TWABFileObject = class
  private
    FHandle: THandle;
    FFileName: WideString;
  public
    constructor Create(const AFileName: WideString; AHandle: THandle); virtual;
    destructor Destroy; override;
    property Handle: THandle read FHandle;
    property FileName: WideString read FFileName;
  end;

//------------------------------------------------------------------------------

constructor _TWABFileObject.Create(const AFileName: WideString; AHandle: THandle);
begin
  if not Assigned(_WabDllList) then Abort;

  inherited Create;
  FFileName := AFileName;
  FHandle := AHandle;

  _WabDllList.Add(Self);
end;

//------------------------------------------------------------------------------

destructor _TWABFileObject.Destroy;
begin
  inherited Destroy;
end;


//------------------------------------------------------------------------------

function abfLoadWabDll: THandle;

  //-------------------------------------

  function _LoadWabDllA: THandle;
  var
    FileName, NewName: string;
    FixCurrDir: string;
  begin
    Result := 0;

    if not abfGetWabDllPath(FileName) then Exit;

  // Simply load library, if it was not used before.
    if (_WabDllList.Count = 0) and (GetModuleHandle(PChar(FileName)) = 0) then
    begin
      FixCurrDir := abfGetCurrentDirectoryA;
      Result := LoadLibrary(PChar(FileName));
      if FixCurrDir <> abfGetCurrentDirectoryA then
        abfSetCurrentDirectoryA(FixCurrDir);

      if Result <> 0 then
      begin
        _TWABFileObject.Create('', Result);
        @_GlobalWabOpen := GetProcAddress(Result, 'WABOpen');
        if Assigned(@_GlobalWABOpen) then
          _GlobalWABOpen(_GlobalAddrBook, _GlobalWABObject, nil, 0);
      end;
    end;

  // Make copy of library at the temporary folder, and load a copy
    NewName := abfGetUniqueTempFileName('wab');

    if not abfCopyFile(FileName, NewName) then Exit;

    FixCurrDir := abfGetCurrentDirectoryA;
    Result := LoadLibrary(PChar(NewName));
    if FixCurrDir <> abfGetCurrentDirectoryA then
      abfSetCurrentDirectoryA(FixCurrDir);

    _TWABFileObject.Create(NewName, Result);
  end;

  //-------------------------------------

  function _LoadWabDllW: THandle;
  var
    FileName, NewName: WideString;
    FixCurrDir: WideString;
  begin
    Result := 0;

    if not abfGetWabDllPath(FileName) then Exit;

  // Simply load library, if it was not used before.
    if (_WabDllList.Count = 0) and (GetModuleHandleW(PWideChar(FileName)) = 0) then
    begin
      FixCurrDir := abfGetCurrentDirectoryW;
      Result := LoadLibraryW(PWideChar(FileName));
      if FixCurrDir <> abfGetCurrentDirectoryW then
        abfSetCurrentDirectoryW(FixCurrDir);

      if Result <> 0 then
      begin
        _TWABFileObject.Create('', Result);
        @_GlobalWabOpen := GetProcAddress(Result, 'WABOpen');
        if Assigned(@_GlobalWABOpen) then
          _GlobalWABOpen(_GlobalAddrBook, _GlobalWABObject, nil, 0);
      end;
    end;

  // Make copy of library at the temporary folder, and load a copy
    NewName := abfGetUniqueTempFileNameW('wab');

    if not abfCopyFileW(FileName, NewName) then Exit;

    FixCurrDir := abfGetCurrentDirectoryW;

    Result := LoadLibraryW(PWideChar(NewName));

    if FixCurrDir <> abfGetCurrentDirectoryW then
      abfSetCurrentDirectoryW(FixCurrDir);

    _TWABFileObject.Create(NewName, Result);
  end;

  //-------------------------------------

begin
  Result := 0;
  if not Assigned(_WabDllList) then Exit;

  if not IsWinNT then
    Result := _LoadWabDllA
  else
    Result := _LoadWabDllW
end;

//------------------------------------------------------------------------------

procedure abfUnloadAllWabDll;
var
  i: Integer;
  TempObj: _TWABFileObject;
begin
  if not Assigned(_WabDllList) then Exit;

  with _WabDllList do
    for i := Count - 1 downto 0 do
    begin
      if _TWABFileObject(Items[i]).FileName = '' then
      begin
        _GlobalAddrBook := nil;
        _GlobalWABObject := nil;
        @_GlobalWABOpen := nil;
      end;

      if _TWABFileObject(Items[i]).Handle <> 0 then
        FreeLibrary(_TWABFileObject(Items[i]).Handle);

      if _TWABFileObject(Items[i]).FileName <> '' then
      begin
        if not IsWinNT then
          Windows.DeleteFileA(PAnsiChar(string(_TWABFileObject(Items[i]).FileName)))
        else
          Windows.DeleteFileW(PWideChar(WideString(_TWABFileObject(Items[i]).FileName)));
      end;

      TempObj := _TWABFileObject(Items[i]);
      Delete(i);
      FreeAndNil(TempObj);
    end;
end;

//------------------------------------------------------------------------------

procedure abfUnloadWabDll(Handle: THandle);
var
  i: Integer;
  TempObj: _TWABFileObject;
begin
  if not Assigned(_WabDllList) then Exit;
  try
    with _WabDllList do
      for i := 0 to Count - 1 do
        if _TWABFileObject(Items[i]).Handle = Handle then
        begin
          if Handle <> 0 then FreeLibrary(Handle);

          if _TWABFileObject(Items[i]).FileName <> '' then
          begin
            if not IsWinNT then
              Windows.DeleteFileA(PAnsiChar(string(_TWABFileObject(Items[i]).FileName)))
            else
              Windows.DeleteFileW(PWideChar(WideString(_TWABFileObject(Items[i]).FileName)));
          end;

          TempObj := _TWABFileObject(Items[i]);
          Delete(i);
          FreeAndNil(TempObj);

          Exit;
        end;
  finally
    if _WabDllList.Count = 1 then
      abfUnloadAllWabDll;
  end;
end;

//------------------------------------------------------------------------------

procedure _DeflateStrings(var Value: TStrings; RemoveEmptyLines: Boolean);
var
  TempStr: string;
  i: Integer;
begin
  i := 0;
  while i < Value.Count do
  begin
    TempStr := Trim(Value[i]);
    if (TempStr = '') and RemoveEmptyLines then
      Value.Delete(i)
    else
      begin
        Value[i] := TempStr;
        Inc(i);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure _QuickSortVariantArray(var V: Variant; L, R: Integer);
var
  I, J: Integer;
  P, TempInt: Integer;
begin
  if not VarIsArray(V) then Exit;

  repeat
    I := L;
    J := R;
    P := V[(L + R) shr 1];
    repeat
      while V[I] < P do Inc(I);
      while V[J] > P do Dec(J);
      if I <= J then
      begin
        TempInt := V[I];
        V[I] := V[J];
        V[J] := TempInt;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then _QuickSortVariantArray(V, L, J);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------

procedure _DistinctVariantArray(var V: Variant);
var
  L, H, i, a: Integer;
  TempArray: Variant;
begin
  if not VarIsArray(V) then Exit;

  TempArray := NULL;
  a := 0;

  L := VarArrayLowBound(V, 1);
  H := VarArrayHighBound(V, 1);

  _QuickSortVariantArray(V, L, H);

  for i := L to H do
  begin
    if VarIsNull(TempArray) then
    begin
      TempArray := VarArrayCreate([0, 0], varInteger);
      TempArray[0] := V[i];
    end;

    if TempArray[a] <> V[i] then
    begin
      Inc(a);
      VarArrayRedim(TempArray, a);
      TempArray[a] := V[i];
    end;
  end;

  V := TempArray;
end;

//==============================================================================
// TabfWABItem
//==============================================================================
// Author: LinX
// Date: 19/03/2001

constructor TabfWABItem.Create(ACollection: TCollection);
var
  ST: TSystemTime;
  LT: TFileTime;
begin
  inherited Create(ACollection);

  FWAB := nil;

  if Assigned(ACollection) and (ACollection is TabfWABItemsCollection)
    and Assigned(TabfWABItemsCollection(ACollection).WAB) then
    FWAB := TabfWABItemsCollection(ACollection).WAB;

  if Assigned(FWAB) then
  begin
    abfNewSBinary2(FWAB.WABObject, FEntryID, 0);
    abfNewSBinary2(FWAB.WABObject, FParentID, 0);
    abfNewSBinary2(FWAB.WABObject, FSearchKey, 0);
  end else
  begin
    abfNewSBinary(FEntryID, 0);
    abfNewSBinary(FParentID, 0);
    abfNewSBinary(FSearchKey, 0);
  end;
  
  FDisplayName := SabfWAB_DefaultItemName;
  DateTimeToSystemTime(0, ST);
  SystemTimeToFileTime(ST, LT);
  LocalFileTimeToFileTime(LT, FCreationTime);
  LocalFileTimeToFileTime(LT, FLastModificationTime);
  FItemType := 0;
  FReading := False;
end;

//------------------------------------------------------------------------------

destructor TabfWABItem.Destroy;
begin
  if Assigned(FWAB) then
  begin
    abfFreeSBinary2(FWAB.WABObject, FSearchKey);
    abfFreeSBinary2(FWAB.WABObject, FParentID);
    abfFreeSBinary2(FWAB.WABObject, FEntryID);
  end else
  begin
    abfFreeSBinary(FSearchKey);
    abfFreeSBinary(FParentID);
    abfFreeSBinary(FEntryID);
  end;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfWABItem.Assign(Source: TPersistent);
begin
  if Source is TabfWABItem then
  begin
    DisplayName := TabfWABItem(Source).DisplayName;

    if Assigned(FWAB) then
    begin
      abfMoveSBinary2(FWAB.WABObject, TabfWABItem(Source).EntryID, FEntryID);
      abfMoveSBinary2(FWAB.WABObject, TabfWABItem(Source).ParentID, FParentID);
      abfMoveSBinary2(FWAB.WABObject, TabfWABItem(Source).SearchKey, FSearchKey);
    end else
    begin
      abfMoveSBinary(TabfWABItem(Source).EntryID, FEntryID);
      abfMoveSBinary(TabfWABItem(Source).ParentID, FParentID);
      abfMoveSBinary(TabfWABItem(Source).SearchKey, FSearchKey);
    end;
    FItemType := TabfWABItem(Source).ItemType;
    CopyMemory(@FCreationTime,
      @(TabfWABItem(Source).FCreationTime), SizeOf(FCreationTime));
    CopyMemory(@FLastModificationTime,
      @(TabfWABItem(Source).FLastModificationTime), SizeOf(FLastModificationTime));
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TabfWABItem.ApplyUpdates: Boolean;
begin
  Result := True;
//  raise EabfWABException.Create(SabfWAB_NotImplementedError);
end;

//------------------------------------------------------------------------------

function TabfWABItem.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

//------------------------------------------------------------------------------

procedure TabfWABItem.SetDisplayName(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FDisplayName <> Value then
  begin
    PropsValues[0].ulPropTag := PR_DISPLAY_NAME;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues,  Length(Value) = 0) then
    begin
      FDisplayName := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABItem.SetPropsValues(PropsCount: Byte; PropsValues: TSPropsArray;
  DeleteProps: Boolean): Boolean;
begin
  Result := True;
//  raise EabfWABException.Create(SabfWAB_NotImplementedError);
end;

//------------------------------------------------------------------------------

function TabfWABItem.GetEntryID: PSBinary;
begin
  Result := FEntryID;
end;

//------------------------------------------------------------------------------

function TabfWABItem.GetParentID: PSBinary;
begin
  Result := FParentID;
end;

//------------------------------------------------------------------------------

function TabfWABItem.GetSearchKey: PSBinary;
begin
  Result := FSearchKey;
end;

//------------------------------------------------------------------------------

function TabfWABItem.GetCreationTime: TDateTime;
var
  LT: TFileTime;
  ST: TSystemTime;
begin
  FileTimeToLocalFileTime(FCreationTime, LT);
  FileTimeToSystemTime(LT, ST);
  Result := SystemTimeToDateTime(ST);
end;

//------------------------------------------------------------------------------

function TabfWABItem.GetLastModificationTime: TDateTime;
var
  LT: TFileTime;
  ST: TSystemTime;
begin
  FileTimeToLocalFileTime(FLastModificationTime, LT);
  FileTimeToSystemTime(LT, ST);
  Result := SystemTimeToDateTime(ST);
end;

//------------------------------------------------------------------------------

procedure TabfWABItem.SetEntryID(const AEntryID: PSBinary);
begin
  if Assigned(FWAB) then
    abfMoveSBinary2(FWAB.WABObject, AEntryID, FEntryID)
  else
    abfMoveSBinary(AEntryID, FEntryID);
end;

//------------------------------------------------------------------------------

procedure TabfWABItem.SetParentID(const AParentID: PSBinary);
begin
  if Assigned(FWAB) then
    abfMoveSBinary2(FWAB.WABObject, AParentID, FParentID)
  else
    abfMoveSBinary(AParentID, FParentID);
end;

//------------------------------------------------------------------------------

procedure TabfWABItem.SetSearchKey(const ASearchKey: PSBinary);
var
  PropsValues: TSPropsArray;
begin
  if Assigned(FWAB) then
    abfMoveSBinary2(FWAB.WABObject, ASearchKey, FSearchKey)
  else
    abfMoveSBinary(ASearchKey, FSearchKey);

  if FReading then Exit;

  PropsValues[0].ulPropTag := PR_SEARCH_KEY;
  PropsValues[0].Value.bin.cb := FSearchKey.cb;
  PropsValues[0].Value.bin.lpb := FSearchKey.lpb;
  SetPropsValues(1, PropsValues,  False);
end;

//==============================================================================
// TabfWABItemsCollection
//==============================================================================
// Author: LinX
// Date: 19/03/2001

constructor TabfWABItemsCollection.Create(abfWABItemClass: TCollectionItemClass;
  AWAB: TabfWAB);
begin
  FWAB := AWAB;
  inherited Create(abfWABItemClass);
end;


//------------------------------------------------------------------------------

function TabfWABItemsCollection.GetItem(Index: Integer): TabfWABItem;
begin
  Result := TabfWABItem(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsCollection.SetItem(Index: Integer; Value: TabfWABItem);
begin
  inherited SetItem(Index, Value);
end;

//------------------------------------------------------------------------------

function TabfWABItemsCollection.GetOwner: TPersistent;
begin
  Result := FWAB;
end;

//------------------------------------------------------------------------------

{$IFDEF D3_ONLY}

function TabfWABItemsCollection.Insert(Index: Integer): TabfWABItem;
begin
  Result := Add;
  Result.Index := Index;
end;

{$ELSE}

function TabfWABItemsCollection.Insert(Index: Integer): TabfWABItem;
begin
  Result := TabfWABItem(inherited Insert(Index));
end;

{$ENDIF}

//------------------------------------------------------------------------------

function TabfWABItemsCollection.Add: TabfWABItem;
begin
  Result := TabfWABItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsCollection.Assign(Source: TPersistent);
var
  i, TempIndex: Integer;
begin
  if Source = Self then Exit;

  if Assigned(WAB) then WAB.CheckReadOnly;

  if Source is TabfWABItemsCollection then
  begin
    BeginUpdate;
    try
      if Assigned(WAB) then WAB.DeleteWABItems(Self, NULL);

      Clear;

      if Assigned(WAB) then
      begin
        WAB.Contacts.Clear;
        WAB.Groups.Clear;
      end;

      for i := 0 to TabfWABItemsCollection(Source).Count - 1 do
      begin
        if Assigned(WAB) and
          Assigned(TabfWABItemsCollection(Source).WAB) and
          Assigned(TabfWABItemsCollection(Source).Items[i].EntryID) then
        begin
          if TabfWABItemsCollection(Source).Items[i].ItemType = MAPI_MAILUSER then
          begin
            TempIndex := TabfWABItemsCollection(Source).WAB.GetContactIndex(TabfWABItemsCollection(Source).Items[i].EntryID);
            if TempIndex > -1 then
              WAB.Contacts.Add.Assign(TabfWABItemsCollection(Source).WAB.Contacts[TempIndex]);
          end else
          if TabfWABItemsCollection(Source).Items[i].ItemType = MAPI_DISTLIST then
          begin
            TempIndex := TabfWABItemsCollection(Source).WAB.GetGroupIndex(TabfWABItemsCollection(Source).Items[i].EntryID);
            if TempIndex > -1 then
              WAB.Groups.Add.Assign(TabfWABItemsCollection(Source).WAB.Groups[TempIndex]);
          end;
        end;

      end;
    finally
      EndUpdate;
    end;

    if Assigned(WAB) and (not WAB.CachedUpdates) then WAB.ApplyUpdates;
  end
  else
    inherited Assign(Source);
end;

//==============================================================================
// TabfWABContactItem
//==============================================================================
// Author: LinX
// Date: 06/01/2000

constructor TabfWABContactItem.Create(ACollection: TCollection);
var
  ST: TSystemTime;
  LT: TFileTime;
begin
  inherited Create(ACollection);

  FWAB := nil;

  if Assigned(ACollection) and (ACollection is TabfWABItemsCollection)
    and Assigned(TabfWABItemsCollection(ACollection).WAB) then
    FWAB := TabfWABItemsCollection(ACollection).WAB;

  FDisplayName := SabfWAB_DefaultItemName;
  DateTimeToSystemTime(0, ST);
  SystemTimeToFileTime(ST, LT);
  LocalFileTimeToFileTime(LT, FCreationTime);
  LocalFileTimeToFileTime(LT, FLastModificationTime);
  FItemType := MAPI_MAILUSER;
  FItemID := 0;
  FMailUser := nil;
  FGender := 0;
  FDefaultAddressIndex := 0;
  FDefaultConferencingServer := 0;
  FBackupConferencingServer := 0;
  FDefaultAddress := wdaDefaultAddress;
  FEMailPlainTextOnly := False;
  FAddrTypes := TStringList.Create;
  FEmailAddresses := TStringList.Create;
  FChildrenNames := TStringList.Create;
  FHomeAddressStreet := TStringList.Create;
  FBusinessAddressStreet := TStringList.Create;
  FComment := TStringList.Create;
  FConferencingServers := TStringList.Create;
  DateTimeToSystemTime(0, ST);
  SystemTimeToFileTime(ST, FBirthday);
  SystemTimeToFileTime(ST, FWeddingAnniversary);
  TStringList(FAddrTypes).OnChange := ItemsChange;
  TStringList(FEmailAddresses).OnChange := ItemsChange;
  TStringList(FChildrenNames).OnChange := ItemsChange;
  TStringList(FHomeAddressStreet).OnChange := ItemsChange;
  TStringList(FBusinessAddressStreet).OnChange := ItemsChange;
  TStringList(FComment).OnChange := ItemsChange;
  TStringList(FConferencingServers).OnChange := ItemsChange;
end;

//------------------------------------------------------------------------------

destructor TabfWABContactItem.Destroy;
begin
  TStringList(FConferencingServers).OnChange := nil;
  TStringList(FComment).OnChange := nil;
  TStringList(FBusinessAddressStreet).OnChange := nil;
  TStringList(FHomeAddressStreet).OnChange := nil;
  TStringList(FChildrenNames).OnChange := nil;
  TStringList(FEmailAddresses).OnChange := nil;
  TStringList(FAddrTypes).OnChange := nil;
  FConferencingServers.Free;
  FComment.Free;
  FBusinessAddressStreet.Free;
  FHomeAddressStreet.Free;
  FChildrenNames.Free;
  FEmailAddresses.Free;
  FAddrTypes.Free;
  FMailUser := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.CheckIMailUser: Boolean;
var
  ParentID, NewID: PSBinary;
  Container: IABContainer;
  ObjType: ULONG;
begin
  Result := False;

  if not Assigned(WAB.AddrBook) then Exit;

  if Assigned(FMailUser) then
  begin
    Result := True;
    Exit;
  end;

  if Assigned(EntryID.lpb) then
    WAB.AddrBook.OpenEntry(EntryID.cb, EntryID.lpb, nil, MAPI_MODIFY,
      ObjType, IUnknown(FMailUser));

  if not Assigned(FMailUser) then
    with WAB do
    begin
      ParentID := nil;
      NewID := nil;
      GetWABTemplateID(MAPI_MAILUSER, ParentID, NewID);
      try
        FAddrBook.OpenEntry(ParentID.cb, ParentID.lpb,
          nil, 0, ObjType, IUnknown(Container));
        Container.CreateEntry(NewID.cb, NewID.lpb,
          0, IMAPIProp(FMailUser));
      finally
        abfFreeSBinary2(FWAB.WABObject, ParentID);
        abfFreeSBinary2(FWAB.WABObject, NewID);
      end;
    end;{with WAB do}

  if Assigned(FMailUser) then Result := True;
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.SetPropsValues(PropsCount: Byte; PropsValues: TSPropsArray;
  DeleteProps: Boolean): Boolean;
var
  SPTA: PSPropTagArray;
  i: Integer;
begin
  Result := False;

  if not Assigned(WAB) then Exit;

  try
    WAB.CheckActive;
    WAB.CheckReadOnly;

    if not CheckIMailUser then Exit;

    if Assigned(FMailUser) then
    begin
      GetMem(SPTA, SizeOf(ULONG) + SizeOf(ULONG) * PropsCount);
      try
        SPTA^.cValues := PropsCount;
        for i := 0 to SPTA^.cValues - 1 do
          SPTA^.aulPropTag[i] := PropsValues[i].ulPropTag;

        if DeleteProps then
          FMailUser.DeleteProps(SPTA, nil);
      finally
        FreeMem(SPTA);
      end;

      if not DeleteProps then
        FMailUser.SetProps(PropsCount, @PropsValues, nil);

      if not WAB.CachedUpdates then Result := ApplyUpdates
      else Result := True;
    end;
  except
    FMailUser := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.Assign(Source: TPersistent);
begin
  if not Assigned(WAB) then Exit;

  WAB.CheckReadOnly;

  if Source is TabfWABContactItem then
  begin
    DisplayName := TabfWABItem(Source).DisplayName;
    SetSearchKey(TabfWABItem(Source).SearchKey);
    NamePrefix := TabfWABContactItem(Source).NamePrefix;
    GivenName := TabfWABContactItem(Source).GivenName;
    MiddleName := TabfWABContactItem(Source).MiddleName;
    Surname := TabfWABContactItem(Source).Surname;
    Gender := TabfWABContactItem(Source).Gender;
    Nickname := TabfWABContactItem(Source).Nickname;
    WeddingAnniversary := TabfWABContactItem(Source).WeddingAnniversary;
    Birthday := TabfWABContactItem(Source).Birthday;
    FDefaultAddressIndex := TabfWABContactItem(Source).DefaultAddressIndex;
    AddrTypes.Assign(TabfWABContactItem(Source).AddrTypes);
    EmailAddresses.Assign(TabfWABContactItem(Source).EmailAddresses);
//    EmailAddress := TabfWABContactItem(Source).EmailAddress;

    SpouseName := TabfWABContactItem(Source).SpouseName;
    ChildrenNames.Assign(TabfWABContactItem(Source).ChildrenNames);
    PersonalHomePage := TabfWABContactItem(Source).PersonalHomePage;
    HomeTelephoneNumber := TabfWABContactItem(Source).HomeTelephoneNumber;
    MobileTelephoneNumber := TabfWABContactItem(Source).MobileTelephoneNumber;
    PagerTelephoneNumber := TabfWABContactItem(Source).PagerTelephoneNumber;
    HomeFaxNumber := TabfWABContactItem(Source).HomeFaxNumber;
    HomeAddressCity := TabfWABContactItem(Source).HomeAddressCity;
    HomeAddressCountry := TabfWABContactItem(Source).HomeAddressCountry;
    HomeAddressPostalCode := TabfWABContactItem(Source).HomeAddressPostalCode;
    HomeAddressStateOrProvince := TabfWABContactItem(Source).HomeAddressStateOrProvince;
    HomeAddressStreet.Assign(TabfWABContactItem(Source).HomeAddressStreet);
    BusinessHomePage := TabfWABContactItem(Source).BusinessHomePage;
    CompanyName := TabfWABContactItem(Source).CompanyName;
    Title := TabfWABContactItem(Source).Title;
    DepartmentName := TabfWABContactItem(Source).DepartmentName;
    OfficeLocation := TabfWABContactItem(Source).OfficeLocation;
    BusinessTelephoneNumber := TabfWABContactItem(Source).BusinessTelephoneNumber;
    BusinessFaxNumber := TabfWABContactItem(Source).BusinessFaxNumber;
    BusinessAddressCity := TabfWABContactItem(Source).BusinessAddressCity;
    BusinessAddressCountry := TabfWABContactItem(Source).BusinessAddressCountry;
    BusinessAddressPostalCode := TabfWABContactItem(Source).BusinessAddressPostalCode;
    BusinessAddressStateOrProvince := TabfWABContactItem(Source).BusinessAddressStateOrProvince;
    BusinessAddressStreet.Assign(TabfWABContactItem(Source).BusinessAddressStreet);
    Comment.Assign(TabfWABContactItem(Source).Comment);
    EMailPlainTextOnly := TabfWABContactItem(Source).EMailPlainTextOnly;
    IPPhone := TabfWABContactItem(Source).IPPhone;
    ConferencingServers.Assign(TabfWABContactItem(Source).ConferencingServers);
    DefaultConferencingServer := TabfWABContactItem(Source).DefaultConferencingServer;
    BackupConferencingServer := TabfWABContactItem(Source).BackupConferencingServer;
    DefaultAddress := TabfWABContactItem(Source).DefaultAddress;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.ApplyUpdates: Boolean;
var
  spv: PSPropsArray;
  TempItem: TabfWABItem;
  SPTA: TSPropTagArray;
  i: Cardinal;
begin
  Result := False;
  if not Assigned(WAB) then Exit;

  WAB.CheckActive;
  WAB.CheckReadOnly;

  if CheckIMailUser then
  try
    if not Assigned(EntryID.lpb) then
    begin
      WAB.BeginUpdate;
      try
        Result := FMailUser.SaveChanges(FORCE_SAVE) = S_OK;
        SPTA.cValues := 1;
        SPTA.aulPropTag[0] := PR_ENTRYID;
        spv := nil;
        FMailUser.GetProps(@SPTA, 0, @i, spv);
        try
          SetEntryID(@(spv[0].Value.bin));
          TempItem := TabfWABItem(WAB.Items.FindItemID(FItemID));
          if Assigned(TempItem) then
            TempItem.SetEntryID(@(spv[0].Value.bin));
          WAB.DoNewContact;
        finally
          WAB.WabObject.FreeBuffer(spv);
        end;
      finally
        WAB.EndUpdate;
        WAB.DoChangeAddressBook;
      end;
    end else
    begin
      Result := FMailUser.SaveChanges(FORCE_SAVE) = S_OK;
      WAB.DoChangeAddressBook;
    end;
    FMailUser := nil;
  except
    FMailUser := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.ItemsChange(Sender: TObject);
var
  TempList: TStrings;
  SaveOnChange: TNotifyEvent;
begin
  if not Assigned(WAB) or Reading or WAB.LoadingProgress then Exit;

  SaveOnChange := TStringList(Sender).OnChange;
  TStringList(Sender).OnChange := nil;
  try
    TempList := TStringList.Create;
    try
      if Sender = FAddrTypes then
      begin
        TempList.AddStrings(FAddrTypes);
        _DeflateStrings(TempList, False);
        SetAddrTypes(TempList);
      end else
      if Sender = FEmailAddresses then
      begin
        TempList.AddStrings(FEmailAddresses);
        _DeflateStrings(TempList, False);
        SetEmailAddresses(TempList);
      end else
      if Sender = FChildrenNames then
      begin
        TempList.AddStrings(FChildrenNames);
        _DeflateStrings(TempList, True);
        SetChildrenNames(TempList);
      end else
      if Sender = FHomeAddressStreet then
      begin
        TempList.AddStrings(FHomeAddressStreet);
        SetHomeAddressStreet(TempList);
      end else
      if Sender = FBusinessAddressStreet then
      begin
        TempList.AddStrings(FBusinessAddressStreet);
        SetBusinessAddressStreet(TempList);
      end else
      if Sender = FComment then
      begin
        TempList.AddStrings(FComment);
        SetComment(TempList);
      end else
      if Sender = FConferencingServers then
      begin
        TempList.AddStrings(FConferencingServers);
        SetConferencingServers(TempList);
      end;
    finally
      TempList.Free;
    end;
  finally
    TStringList(Sender).OnChange := SaveOnChange;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.GetFileTimeProperty(ulPropTag: ULONG): TFileTime;
var
  ST: TSystemTime;
  count: Integer;
  spv: PSPropsArray;
  SPTA: TSPropTagArray;
begin
  DateTimeToSystemTime(0, ST);
  SystemTimeToFileTime(ST, Result);

  if CheckIMailUser then
  begin
    count := 0;
    spv := nil;

    SPTA.cValues := 1;
    SPTA.aulPropTag[0] := ulPropTag;
    FMailUser.GetProps(@SPTA, 0, @count, spv);
    if Assigned(spv) and abfIfWABPropExists(spv[0]) then
    try
      CopyMemory(@Result, @spv[0].Value.ft, SizeOf(Result));
    finally
      WAB.WABObject.FreeBuffer(spv);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.GetULONGProperty(ulPropTag: ULONG): ULONG;
var
  count: Integer;
  spv: PSPropsArray;
  SPTA: TSPropTagArray;
begin
  Result := 0;

  if CheckIMailUser then
  begin
    count := 0;
    spv := nil;

    SPTA.cValues := 1;
    SPTA.aulPropTag[0] := ulPropTag;
    FMailUser.GetProps(@SPTA, 0, @count, spv);
    if Assigned(spv) and abfIfWABPropExists(spv[0]) then
    try
      CopyMemory(@Result, @spv[0].Value.ul, SizeOf(Result));
    finally
      WAB.WABObject.FreeBuffer(spv);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.GetWeddingAnniversary: TDateTime;
var
  ST: TSystemTime;
begin
  FWeddingAnniversary := GetFileTimeProperty(PR_WEDDING_ANNIVERSARY);
  FileTimeToSystemTime(FWeddingAnniversary, ST);
  ST.wHour := 0;
  ST.wMinute := 0;
  ST.wSecond := 0;
  ST.wMilliseconds := 0;
  Result := SystemTimeToDateTime(ST);
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.GetBirthday: TDateTime;
var
  ST: TSystemTime;
begin
  FBirthday := GetFileTimeProperty(PR_BIRTHDAY);

  FileTimeToSystemTime(FBirthday, ST);
  ST.wHour := 0;
  ST.wMinute := 0;
  ST.wSecond := 0;
  ST.wMilliseconds := 0;
  Result := SystemTimeToDateTime(ST);
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.GetAddrType: string;
begin
  Result := '';
  if (DefaultAddressIndex < FAddrTypes.Count) then
    Result := FAddrTypes[DefaultAddressIndex];
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.GetEmailAddress: string;
begin
  Result := '';
  if (DefaultAddressIndex < FEmailAddresses.Count) then
    Result := FEmailAddresses[DefaultAddressIndex];
end;

//------------------------------------------------------------------------------

function TabfWABContactItem.GetEMailPlainTextOnly: Boolean;
begin
  FEMailPlainTextOnly := GetULONGProperty(PR_SEND_INTERNET_ENCODING) <> BODY_ENCODING_TEXT_AND_HTML;
  Result := FEMailPlainTextOnly;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetNamePrefix(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FNamePrefix <> Value then
  begin
    PropsValues[0].ulPropTag := PR_DISPLAY_NAME_PREFIX;
    PropsValues[0].Value.lpszW := PWideChar(Value);
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FNamePrefix := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetGivenName(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FGivenName <> Value then
  begin
    PropsValues[0].ulPropTag := PR_GIVEN_NAME;
    PropsValues[0].Value.lpszW := PWideChar(Value);
    if SetPropsValues(1, PropsValues,  Length(Value) = 0) then
    begin
      FGivenName := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetMiddleName(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FMiddleName <> Value then
  begin
    PropsValues[0].ulPropTag := PR_MIDDLE_NAME;
    PropsValues[0].Value.lpszW := PWideChar(Value);
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FMiddleName := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetSurname(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FSurname <> Value then
  begin
    PropsValues[0].ulPropTag := PR_SURNAME;
    PropsValues[0].Value.lpszW := PWideChar(Value);
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FSurname := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetGender(Value: Cardinal);
var
  PropsValues: TSPropsArray;
begin
  if FGender <> Value then
  begin
    PropsValues[0].ulPropTag := PR_GENDER;
    PropsValues[0].Value.l := Value;
    if SetPropsValues(1, PropsValues, Value = 0) then
    begin
      FGender := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetNickname(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FNickname <> Value then
  begin
    PropsValues[0].ulPropTag := PR_NICKNAME;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FNickname := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetWeddingAnniversary(Value: TDateTime);
var
  PropsValues: TSPropsArray;
  ST: TSystemTime;
begin
  PropsValues[0].ulPropTag := PR_WEDDING_ANNIVERSARY;
  DateTimeToSystemTime(Trunc(Value), ST);
  SystemTimeToFileTime(ST, PropsValues[0].Value.ft);
  if CompareFileTime(FWeddingAnniversary, PropsValues[0].Value.ft) <> 0 then
  begin
    if SetPropsValues(1, PropsValues, Value = 0) then
    begin
      FWeddingAnniversary := PropsValues[0].Value.ft;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBirthday(Value: TDateTime);
var
  PropsValues: TSPropsArray;
  ST: TSystemTime;
begin
  PropsValues[0].ulPropTag := PR_BIRTHDAY;
  DateTimeToSystemTime(Trunc(Value), ST);
  SystemTimeToFileTime(ST, PropsValues[0].Value.ft);
  if CompareFileTime(FBirthday, PropsValues[0].Value.ft) <> 0 then
  begin
    if SetPropsValues(1, PropsValues, Value = 0) then
    begin
      FBirthday := PropsValues[0].Value.ft;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetAddrType(const Value: string);
begin
  if AddrType <> Value then
  begin
    if FAddrTypes.Count = 0 then
      AddrTypes.Add(Value)
    else if DefaultAddressIndex < FAddrTypes.Count then
      AddrTypes[DefaultAddressIndex] := Value;

    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetEmailAddress(const Value: string);
begin
  if EmailAddress <> Value then
  begin
    if FEmailAddresses.Count = 0 then
      EmailAddresses.Add(Value)
    else if DefaultAddressIndex < FEmailAddresses.Count then
      EmailAddresses[DefaultAddressIndex] := Value;

    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetAddrTypes(Value: TStrings);
var
  PropsValues: TSPropsArray;
  TempAddrTypes: PFakePCharArr;
  i: Integer;
begin
  Reading := True;
  try
    FillChar(PropsValues[0], Sizeof(PropsValues[0]), 0);

    PropsValues[0].ulPropTag := PR_CONTACT_ADDRTYPES;
    for i := Value.Count to FEmailAddresses.Count - 1 do
      Value.Add(SabfWAB_DefaultAddrType);
    PropsValues[0].Value.MVszA.cValues := Value.Count;

    GetMem(TempAddrTypes, SizeOf(PChar) * Value.Count);
    try
      for i := 0 to Value.Count - 1 do
        TempAddrTypes[i] := PChar(Value[i]);

      PropsValues[0].Value.MVszA.lppszA := Pointer(TempAddrTypes);

      if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
      begin
        FAddrTypes.Assign(Value);
        Changed(False);
      end;
    finally
      FreeMem(TempAddrTypes);
    end;
  finally
    Reading := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetDefaultAddressIndex(Value: Integer);
var
  PropsValues: TSPropsArray;
begin
  if Value < 0 then Exit;
  if FDefaultAddressIndex <> Value then
  begin
    PropsValues[0].ulPropTag := PR_CONTACT_DEFAULT_ADDRESS_INDEX;
    PropsValues[0].Value.l := Value;
    if SetPropsValues(1, PropsValues, (FEmailAddresses.Count <= Value)) then
    begin
      FDefaultAddressIndex := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetEmailAddresses(Value: TStrings);
var
  TempDefaultAddress: Integer;
  TempDefaultEmail: string;
  TempDefaultAddrType: string;
  TempAddrTypes: PFakePCharArr;
  TempEmails: PFakePCharArr;
  PropsValues: TSPropsArray;
  i: Integer;
begin
  Reading := True;
  try
    TempDefaultEmail := '';
    TempDefaultAddrType := '';
    TempDefaultAddress := FDefaultAddressIndex;

    if Value.Count = 0 then FAddrTypes.Clear;

    for i := FAddrTypes.Count to Value.Count - 1 do
      FAddrTypes.Add(SabfWAB_DefaultAddrType);

    GetMem(TempAddrTypes, SizeOf(PChar) * FAddrTypes.Count);
    try
      GetMem(TempEmails, SizeOf(PChar) * Value.Count);
      try
        if Value.IndexOf(FEmailAddress) > -1 then
          TempDefaultAddress := Value.IndexOf(FEmailAddress);

        if Value.Count > 0 then
        begin
          TempDefaultEmail := Value[TempDefaultAddress];
          if (FAddrTypes.Count > 0) and (TempDefaultAddress < FAddrTypes.Count) then
            TempDefaultAddrType := FAddrTypes[TempDefaultAddress]
          else
            TempDefaultAddrType := SabfWAB_DefaultAddrType;
        end;

        for i := 0 to Value.Count - 1 do
          TempEmails[i] := PChar(Value[i]);

        for i := 0 to FAddrTypes.Count - 1 do
          TempAddrTypes[i] := PChar(FAddrTypes[i]);

        PropsValues[0].ulPropTag := PR_ADDRTYPE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(TempDefaultAddrType);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(TempDefaultAddrType);
{$ENDIF}

        PropsValues[1].ulPropTag := PR_EMAIL_ADDRESS;
        PropsValues[1].Value.lpszA := PAnsiChar(TempDefaultEmail);

        PropsValues[2].ulPropTag := PR_CONTACT_ADDRTYPES;
        PropsValues[2].Value.MVszA.cValues := FAddrTypes.Count;
        PropsValues[2].Value.MVszA.lppszA := Pointer(TempAddrTypes);

        PropsValues[3].ulPropTag := PR_CONTACT_EMAIL_ADDRESSES;
        PropsValues[3].Value.MVszA.cValues := Value.Count;
        PropsValues[3].Value.MVszA.lppszA := Pointer(TempEmails);

        PropsValues[4].ulPropTag := PR_CONTACT_DEFAULT_ADDRESS_INDEX;
        PropsValues[4].Value.ul := TempDefaultAddress;

        if SetPropsValues(5, PropsValues, Trim(Value.Text) = '') then
        begin
          FDefaultAddressIndex := TempDefaultAddress;
          FEmailAddresses.Assign(Value);
          Changed(False);
        end;
      finally
        FreeMem(TempEmails);
      end;
    finally
      FreeMem(TempAddrTypes);
    end;
  finally
    Reading := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetSpouseName(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FSpouseName <> Value then
  begin
    PropsValues[0].ulPropTag := PR_SPOUSE_NAME;
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FSpouseName := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetChildrenNames(Value: TStrings);
var
  PropsValues: TSPropsArray;
  TempChildrenNames: PFakePCharArr;
  i: Integer;
begin
  FillChar(PropsValues[0], Sizeof(PropsValues[0]), 0);
  PropsValues[0].ulPropTag := PR_CHILDRENS_NAMES;

  PropsValues[0].Value.MVszA.cValues := Value.Count;

  GetMem(TempChildrenNames, SizeOf(PChar) * Value.Count);
  try
    for i := 0 to Value.Count - 1 do
      TempChildrenNames[i] := PChar(Value[i]);

    PropsValues[0].Value.MVszA.lppszA := Pointer(TempChildrenNames);

    if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
    begin
      FChildrenNames.Assign(Value);
      Changed(False);
    end;
  finally
    FreeMem(TempChildrenNames);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetPersonalHomePage(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FPersonalHomePage <> Value then
  begin
    PropsValues[0].ulPropTag := PR_PERSONAL_HOME_PAGE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FPersonalHomePage := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetHomeTelephoneNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FHomeTelephoneNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_TELEPHONE_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FHomeTelephoneNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetMobileTelephoneNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FMobileTelephoneNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_MOBILE_TELEPHONE_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FMobileTelephoneNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetPagerTelephoneNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FPagerTelephoneNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_PAGER_TELEPHONE_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FPagerTelephoneNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetHomeFaxNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FHomeFaxNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_FAX_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FHomeFaxNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetHomeAddressCity(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FHomeAddressCity <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_CITY;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FHomeAddressCity := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetHomeAddressCountry(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FHomeAddressCountry <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_COUNTRY;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FHomeAddressCountry := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetHomeAddressPostalCode(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FHomeAddressPostalCode <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_POSTAL_CODE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FHomeAddressPostalCode := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetHomeAddressStateOrProvince(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FHomeAddressStateOrProvince <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_STATE_OR_PROVINCE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FHomeAddressStateOrProvince := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetHomeAddressStreet(Value: TStrings);
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_HOME_ADDRESS_STREET;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value.Text);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value.Text);
{$ENDIF}
  if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
  begin
    FHomeAddressStreet.Assign(Value);
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessHomePage(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FBusinessHomePage <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BUSINESS_HOME_PAGE;
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FBusinessHomePage := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetCompanyName(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FCompanyName <> Value then
  begin
    PropsValues[0].ulPropTag := PR_COMPANY_NAME;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FCompanyName := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetTitle(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FTitle <> Value then
  begin
    PropsValues[0].ulPropTag := PR_TITLE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FTitle := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetDepartmentName(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FDepartmentName <> Value then
  begin
    PropsValues[0].ulPropTag := PR_DEPARTMENT_NAME;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FDepartmentName := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetOfficeLocation(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FOfficeLocation <> Value then
  begin
    PropsValues[0].ulPropTag := PR_OFFICE_LOCATION;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FOfficeLocation := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessTelephoneNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FBusinessTelephoneNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BUSINESS_TELEPHONE_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FBusinessTelephoneNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessFaxNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FBusinessFaxNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BUSINESS_FAX_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FBusinessFaxNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessAddressCity(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FBusinessAddressCity <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BUSINESS_ADDRESS_CITY;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FBusinessAddressCity := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessAddressCountry(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FBusinessAddressCountry <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BUSINESS_ADDRESS_COUNTRY;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FBusinessAddressCountry := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessAddressPostalCode(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FBusinessAddressPostalCode <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BUSINESS_ADDRESS_POSTAL_CODE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FBusinessAddressPostalCode := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessAddressStateOrProvince(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FBusinessAddressStateOrProvince <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BUSINESS_ADDRESS_STATE_OR_PROVINCE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FBusinessAddressStateOrProvince := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBusinessAddressStreet(Value: TStrings);
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_BUSINESS_ADDRESS_STREET;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
  if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
  begin
    FBusinessAddressStreet.Assign(Value);
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetComment(Value: TStrings);
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_COMMENT;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value.Text);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value.Text);
{$ENDIF}
  if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
  begin
    FComment.Assign(Value);
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetEMailPlainTextOnly(Value: Boolean);
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_SEND_INTERNET_ENCODING;
  if Value then
    PropsValues[0].Value.ul := BODY_ENCODING_TEXT
  else
    PropsValues[0].Value.ul := BODY_ENCODING_TEXT_AND_HTML;
  if SetPropsValues(1, PropsValues, False) then
  begin
    FEMailPlainTextOnly := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetIPPhone(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FIPPhone <> Value then
  begin
    PropsValues[0].ulPropTag := PR_IP_PHONE;
    if WAB.FWABSharedMode then
      PropsValues[0].ulPropTag := PropsValues[0].ulPropTag or $30000000;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FIPPhone := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetConferencingServers(Value: TStrings);
var
  PropsValues: TSPropsArray;
  TempServers: PFakePCharArr;
  i: Integer;
begin
  FillChar(PropsValues[0], Sizeof(PropsValues[0]), 0);

  PropsValues[0].ulPropTag := PR_CONFERENCING_SERVERS;
  if WAB.FWABSharedMode then
    PropsValues[0].ulPropTag := PropsValues[0].ulPropTag or $30000000;

  PropsValues[0].Value.MVszA.cValues := Value.Count;

  GetMem(TempServers, SizeOf(PChar) * Value.Count);
  try
    for i := 0 to Value.Count - 1 do
      TempServers[i] := PChar(Value[i]);

    PropsValues[0].Value.MVszA.lppszA := Pointer(TempServers);

    if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
    begin
      FConferencingServers.Assign(Value);
      Changed(False);
    end;
  finally
    FreeMem(TempServers);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetDefaultConferencingServer(Value: Integer);
var
  PropsValues: TSPropsArray;
begin
  if FDefaultConferencingServer <> Value then
  begin
    PropsValues[0].ulPropTag := PR_DEFAULT_CONFERENCING_SERVER;
    if WAB.FWABSharedMode then
      PropsValues[0].ulPropTag := PropsValues[0].ulPropTag or $30000000;
    PropsValues[0].Value.l := Value;
    if SetPropsValues(1, PropsValues, (FConferencingServers.Count <= Value)) then
    begin
      FDefaultConferencingServer := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetBackupConferencingServer(Value: Integer);
var
  PropsValues: TSPropsArray;
begin
  if FBackupConferencingServer <> Value then
  begin
    PropsValues[0].ulPropTag := PR_BACKUP_CONFERENCING_SERVER;
    if WAB.FWABSharedMode then
      PropsValues[0].ulPropTag := PropsValues[0].ulPropTag or $30000000;
    PropsValues[0].Value.l := Value;
    if SetPropsValues(1, PropsValues, (FConferencingServers.Count <= Value)) then
    begin
      FBackupConferencingServer := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactItem.SetDefaultAddress(Value: TabfWABContactDefaultAddress);
var
  PropsValues: TSPropsArray;
begin
  if FDefaultAddress <> Value then
  begin
    PropsValues[0].ulPropTag := PR_DEFAULT_ADDRESS;
    if WAB.FWABSharedMode then
      PropsValues[0].ulPropTag := PropsValues[0].ulPropTag or $30000000;
    PropsValues[0].Value.l := Integer(Value);
    if SetPropsValues(1, PropsValues, (Value = wdaDefaultAddress)) then
    begin
      FDefaultAddress := Value;
      Changed(False);
    end;
  end;
end;

//==============================================================================
// TabfWABContactsCollection
//==============================================================================
// Author: LinX
// Date: 19/03/2001

constructor TabfWABContactsCollection.Create(AWAB: TabfWAB);
begin
  inherited Create(TabfWABContactItem, AWAB);
end;

//------------------------------------------------------------------------------

function TabfWABContactsCollection.GetItem(Index: Integer): TabfWABContactItem;
begin
  Result := TabfWABContactItem(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------

procedure TabfWABContactsCollection.SetItem(Index: Integer;
  Value: TabfWABContactItem);
begin
  TabfWABContactItem(Items[Index]).Assign(Value);
end;

//------------------------------------------------------------------------------

function TabfWABContactsCollection.GetOwner: TPersistent;
begin
  Result := FWAB;
end;

//------------------------------------------------------------------------------
{$IFDEF D3_ONLY}

function TabfWABContactsCollection.Insert(Index: Integer): TabfWABContactItem;
begin
  Result := Add;
  Result.Index := Index;
end;

{$ELSE}

function TabfWABContactsCollection.Insert(Index: Integer): TabfWABContactItem;
begin
  Result := TabfWABContactItem(inherited Insert(Index));
end;

{$ENDIF}

//------------------------------------------------------------------------------

function TabfWABContactsCollection.Add: TabfWABContactItem;
begin
  if Assigned(WAB) then
  begin
    WAB.CheckActive;
    WAB.CheckReadOnly;
  end;

  Result := TabfWABContactItem.Create(Self);

  if Assigned(WAB) and Assigned(Result) then
  with WAB.Items.Add do
  begin
    FItemType := MAPI_MAILUSER;
    Result.FItemID := ID;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABContactsCollection.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source = Self then Exit;

  if Assigned(WAB) then WAB.CheckReadOnly;

  if Source is TabfWABContactsCollection then
  begin
    if Assigned(WAB) then WAB.BeginUpdate;
    BeginUpdate;
    try
      if Assigned(WAB) then WAB.DeleteWABItems(Self, NULL);

      Clear;

      if Assigned(WAB) then
        for i := WAB.Items.Count - 1 downto 0 do
        begin
          if WAB.Items[i].ItemType = MAPI_MAILUSER then
            WAB.Items[i].Free;
        end;

      for i := 0 to TabfWABContactsCollection(Source).Count - 1 do
      begin
        Add.Assign(TabfWABContactsCollection(Source).Items[i]);
        if Assigned(WAB) then
          WAB.Items.Add.Assign(TabfWABContactsCollection(Source).Items[i]);
      end;
    finally
      EndUpdate;
      if Assigned(WAB) then WAB.EndUpdate;
    end;

    Exit;
  end;
  inherited Assign(Source);
end;

//==============================================================================
// TabfWABGroupItem
//==============================================================================
// Author: LinX
// Date: 19/03/2001

constructor TabfWABGroupItem.Create(ACollection: TCollection);
var
  ST: TSystemTime;
  LT: TFileTime;
begin
  inherited Create(ACollection);

  FWAB := nil;

  if Assigned(ACollection) and (ACollection is TabfWABItemsCollection)
    and Assigned(TabfWABItemsCollection(ACollection).WAB) then
    FWAB := TabfWABItemsCollection(ACollection).WAB;

  FTempVarArray := NULL;
  FDisplayName := SabfWAB_DefaultItemName;
  DateTimeToSystemTime(0, ST);
  SystemTimeToFileTime(ST, LT);
  LocalFileTimeToFileTime(LT, FCreationTime);
  LocalFileTimeToFileTime(LT, FLastModificationTime);
  FItemType := MAPI_DISTLIST;
  FItemID := 0;
  FDistList := nil;
  FStreet := TStringList.Create;
  FComment := TStringList.Create;
  TStringList(FStreet).OnChange := ItemsChange;
  TStringList(FComment).OnChange := ItemsChange;
end;

//------------------------------------------------------------------------------

destructor TabfWABGroupItem.Destroy;
begin
  TStringList(FComment).OnChange := nil;
  TStringList(FStreet).OnChange := nil;
  FComment.Free;
  FStreet.Free;
  FDistList := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.AddEntry(const PSB: PSBinary);
var
  MailUser: IMailUser;
  Res: ULONG;
begin
  if abfIsEmptyEntryID(PSB) then Exit;

  if WAB.AddrBook.CompareEntryIDs(EntryID.cb, EntryID.lpb, PSB.cb, PSB.lpb,
    0, Res) <> S_OK then Exit;
  if Res <> 0 then Exit;

  FDistList.CreateEntry(PSB.cb, PSB.lpb,
    CREATE_CHECK_DUP_LOOSE, IMAPIProp(MailUser));
  if Assigned(MailUser) then
    MailUser.SaveChanges(FORCE_SAVE);
end;

//------------------------------------------------------------------------------

function TabfWABGroupItem.CheckIDistList: Boolean;
var
  ParentID, NewID: PSBinary;
  Container: IABContainer;
  ObjType: ULONG;
begin
  Result := False;

  if not (Assigned(WAB) and Assigned(WAB.AddrBook)) then Exit;

  if Assigned(FDistList) then
  begin
    Result := True;
    Exit;
  end;

  if Assigned(EntryID.lpb) then
    WAB.AddrBook.OpenEntry(EntryID.cb, EntryID.lpb, nil, MAPI_MODIFY,
      ObjType, IUnknown(FDistList));

  if not Assigned(FDistList) then
    with WAB do
    begin
      ParentID := nil;
      NewID := nil;
      GetWABTemplateID(MAPI_DISTLIST, ParentID, NewID);
      try
        WAB.AddrBook.OpenEntry(ParentID.cb, ParentID.lpb,
          nil, 0, ObjType, IUnknown(Container));
        if Assigned(Container) then
          Container.CreateEntry(NewID.cb, NewID.lpb, 0, IMAPIProp(FDistList));
      finally
        abfFreeSBinary2(FWAB.WABObject, ParentID);
        abfFreeSBinary2(FWAB.WABObject, NewID);
      end;
    end;{with WAB do}

  if Assigned(FDistList) then Result := True;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.DoInitDisplayName;
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_DISPLAY_NAME;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(FDisplayName);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(FDisplayName);
{$ENDIF}
  if CheckIDistList then
    FDistList.SetProps(1, @PropsValues, nil);
end;

//------------------------------------------------------------------------------

function TabfWABGroupItem.SetPropsValues(PropsCount: Byte; PropsValues: TSPropsArray;
  DeleteProps: Boolean): Boolean;
var
  SPTA: PSPropTagArray;
  i: Integer;
begin
  Result := False;

  if Reading then
  begin
    Result := True;
    Exit;
  end;

  if not Assigned(WAB) then Exit;

  try
    WAB.CheckActive;
    WAB.CheckReadOnly;

    if CheckIDistList then
    begin
      GetMem(SPTA, SizeOf(ULONG) + SizeOf(ULONG) * PropsCount);
      try
        SPTA^.cValues := PropsCount;
        for i := 0 to SPTA^.cValues - 1 do
          SPTA^.aulPropTag[i] := PropsValues[i].ulPropTag;

        if DeleteProps then
          FDistList.DeleteProps(SPTA, nil);
      finally
        FreeMem(SPTA);
      end;

      if not DeleteProps then FDistList.SetProps(PropsCount, @PropsValues, nil);

      if not WAB.FCachedUpdates then Result := ApplyUpdates
      else Result := True;
    end;
  except
    FDistList := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.Assign(Source: TPersistent);
begin
  if not Assigned(WAB) then Exit;

  WAB.CheckReadOnly;

  if Source is TabfWABGroupItem then
  begin
    DisplayName := TabfWABItem(Source).DisplayName;
    SetSearchKey(TabfWABItem(Source).SearchKey);
    HomePage := TabfWABGroupItem(Source).HomePage;
    TelephoneNumber := TabfWABGroupItem(Source).TelephoneNumber;
    FaxNumber := TabfWABGroupItem(Source).FaxNumber;
    City := TabfWABGroupItem(Source).City;
    Country := TabfWABGroupItem(Source).Country;
    PostalCode := TabfWABGroupItem(Source).PostalCode;
    StateOrProvince := TabfWABGroupItem(Source).StateOrProvince;
    Street.Assign(TabfWABGroupItem(Source).Street);
    Comment.Assign(TabfWABGroupItem(Source).Comment);
    FTempVarArray := TabfWABGroupItem(Source).GetItemsArray;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABGroupItem.ApplyUpdates: Boolean;
var
  spv: PSPropsArray;
  TempItem: TabfWABItem;
  SPTA: TSPropTagArray;
  i: Cardinal;
begin
  Result := False;
  if not Assigned(WAB) then Exit;

  WAB.CheckActive;
  WAB.CheckReadOnly;

  if CheckIDistList then
  try
    DoInitDisplayName;
    if not Assigned(EntryID.lpb) then
    begin
      WAB.BeginUpdate;
      try
        FDistList.SaveChanges(FORCE_SAVE);
        SPTA.cValues := 1;
        SPTA.aulPropTag[0] := PR_ENTRYID;
        spv := nil;
        FDistList.GetProps(@SPTA, 0, @i, spv);
        try
          SetEntryID(@(spv[0].Value.bin));

          TempItem := TabfWABItem(WAB.Items.FindItemID(FItemID));
          if Assigned(TempItem) then
            TempItem.SetEntryID(@(spv[0].Value.bin));

          WAB.DoNewGroup;
        finally
          WAB.FWabObject.FreeBuffer(spv);
        end;
      finally
        WAB.EndUpdate;
        WAB.DoChangeAddressBook;
      end;
    end else
    begin
      FDistList.SaveChanges(FORCE_SAVE);
      WAB.DoChangeAddressBook;
    end;
    Result := True;
    FDistList := nil;
  except
    FDistList := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.AddItems(ItemsIndexes: Variant);
var
  i, LowBound, HighBound: Integer;


begin
  if not Assigned(WAB) then Exit;

  WAB.CheckActive;
  WAB.CheckReadOnly;

  if VarIsArray(ItemsIndexes) then
  begin
    LowBound := VarArrayLowBound(ItemsIndexes, 1);
    HighBound := VarArrayHighBound(ItemsIndexes, 1);
  end
  else if VarType(ItemsIndexes) = varInteger then
  begin
    LowBound := ItemsIndexes;
    HighBound := ItemsIndexes;
  end
  else if VarType(ItemsIndexes) in [varEmpty, varNull] then
  begin
    LowBound := 0;
    HighBound := WAB.Items.Count - 1;
  end
  else
    Exit;

  if not ApplyUpdates then Exit;
  if not CheckIDistlist then Exit;  

  if VarIsArray(ItemsIndexes) then
  begin
    for i := LowBound to HighBound do
    if (ItemsIndexes[i] >= 0) and (ItemsIndexes[i] < WAB.Items.Count) then
      AddEntry(WAB.Items[ItemsIndexes[i]].EntryID);
  end
  else if VarType(ItemsIndexes) = varInteger then
  begin
    if (ItemsIndexes >= 0) and (ItemsIndexes < WAB.Items.Count) then
      AddEntry(WAB.Items[ItemsIndexes].EntryID);
  end
  else if VarType(ItemsIndexes) in [varEmpty, varNull] then
  begin
    for i := LowBound to HighBound do
      AddEntry(WAB.Items[i].EntryID);
  end;
end;

//------------------------------------------------------------------------------

function TabfWABGroupItem.GetItemsArray: Variant;
const
  ItemsEntryIds: record
    Count: ULONG;
    Definition: array[0..1] of ULONG;
  end = (
    Count: 2;
    Definition: (PR_OBJECT_TYPE, PR_ENTRYID)
  );
var
  Table: IMAPITable;
  TableRow: PSRowSet;
  PSB: PSBinary;
  Res: HResult;
  i, idx: Integer;
begin
  Result := NULL;
  if not Assigned(WAB) then Exit;

  WAB.CheckActive;

  if not CheckIDistlist then Exit;

  Res := FDistList.GetContentsTable(0, Table);
  if (Res <> S_OK) or (Table = nil) then Exit;

  Table.SetColumns(@ItemsEntryIds, 0);
  Table.SeekRow(BOOKMARK_BEGINNING, 0, nil);

  i := 0;
  PSB := nil;

  repeat
    Table.QueryRows(1, 0, TableRow);
    if TableRow.cRows > 0 then
    with TableRow^.aRow[0] do
    begin
      if lpProps[0].Value.l in [MAPI_MAILUSER, MAPI_DISTLIST] then
      begin
        abfMoveSBinary2(FWAB.WABObject, @(lpProps[1].Value.bin), PSB);
        try
          idx := WAB.GetItemIndex(PSB);
          if idx <> -1 then
          begin
            if VarIsNull(Result) then
              Result := VarArrayCreate([0, 0], varInteger)
            else
              VarArrayRedim(Result, i);
            Result[i] := idx;
            Inc(i);
          end;
        finally
          abfFreeSBinary2(WAB.WABObject, PSB);
        end;
      end;
      abfFreeSRowSet(WAB.WabObject, TableRow);
    end
    else
      Break;
  until False;

  if VarIsNull(Result) then Exit;

  _DistinctVariantArray(Result);
end;

//------------------------------------------------------------------------------

function TabfWABGroupItem.GetContactsArray(ProcessSubtree: Boolean): Variant;
var
  V, V1: Variant;
  H, H1, i, y, a: Integer;
begin
  Result := NULL;

  V := GetItemsArray;
  if not VarIsArray(V) then Exit;

  H := VarArrayHighBound(V, 1);

  y := 0;

  for i := 0 to H do
  begin
    if WAB.Items[V[i]].ItemType = MAPI_MAILUSER then
    begin
      if VarIsNull(Result) then
      begin
        Result := VarArrayCreate([0, 0], varInteger);
        Result[0] := WAB.GetContactIndex(WAB.Items[V[i]].EntryID);
      end
      else
      begin
        Inc(y);
        VarArrayRedim(Result, y);
        Result[y] := WAB.GetContactIndex(WAB.Items[V[i]].EntryID);
      end;
    end
    else if ProcessSubtree and (WAB.Items[V[i]].ItemType = MAPI_DISTLIST) then
    begin
      a := WAB.GetGroupIndex(WAB.Items[V[i]].EntryID);
      if (a >= 0) and (a < WAB.Groups.Count) then
        V1 := WAB.Groups[a].GetContactsArray(ProcessSubtree)
      else
        V1 := NULL;
      if VarIsArray(V1) then
      begin
        if not VarIsNull(Result) then
        begin
          H1 := VarArrayHighBound(V1, 1);
          VarArrayRedim(Result, y + H1 + 1);

          for a := 0 to H1 do
            Result[y + a + 1] := V1[a];

          y := VarArrayHighBound(Result, 1);
        end
        else
        begin
          Result := V1;
          y := VarArrayHighBound(Result, 1);
        end;
      end;
    end;
  end;

  if VarIsNull(Result) then Exit;

  _DistinctVariantArray(Result);
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.RemoveItems(ItemsIndexes: Variant);
var
  Entries: TSBinaryArray;
  PSB: PSBinary;
  i, LengthArr, LowBound, HighBound: Integer;
begin
  if not Assigned(WAB) then Exit;

  WAB.CheckActive;
  WAB.CheckReadOnly;

  Entries.cValues := 0;
  LowBound := 0;
  HighBound := 0;

  if VarIsArray(ItemsIndexes) then
  begin
    LowBound := VarArrayLowBound(ItemsIndexes, 1);
    HighBound := VarArrayHighBound(ItemsIndexes, 1);
    LengthArr := HighBound - LowBound + 1;
  end
  else if VarType(ItemsIndexes) = varInteger then LengthArr := 1
  else if VarType(ItemsIndexes) in [varEmpty, varNull] then
    LengthArr := WAB.Items.Count
  else
    Exit;

  GetMem(Entries.lpbin, LengthArr * SizeOf(SBinary));
  try
    PSB := Entries.lpbin;
    if VarIsArray(ItemsIndexes) then
    begin
      for i := LowBound to HighBound do
      if (ItemsIndexes[i] >= 0) and (ItemsIndexes[i] < WAB.Items.Count) then
      begin
        CopyMemory(PSB, WAB.Items[ItemsIndexes[i]].EntryID, SizeOf(SBinary));
        PSB := Pointer(Cardinal(PSB) + SizeOf(SBinary));
        Inc(Entries.cValues);
      end;
    end
    else if VarType(ItemsIndexes) = varInteger then
    begin
      if (ItemsIndexes >= 0) and (ItemsIndexes < WAB.Items.Count) then
      begin
        Entries.cValues := 1;
        CopyMemory(PSB, WAB.Items[ItemsIndexes].EntryID, SizeOf(SBinary));
      end;
    end
    else if VarType(ItemsIndexes) in [varEmpty, varNull] then
    begin
      for i := 0 to LengthArr - 1 do
      begin
        CopyMemory(PSB, WAB.Items[i].EntryID, SizeOf(SBinary));
        PSB := Pointer(Cardinal(PSB) + SizeOf(SBinary));
        Inc(Entries.cValues);
      end;
    end;

    if not CheckIDistlist then Exit;

    if Entries.cValues > 0 then FDistList.DeleteEntries(@Entries, 0);
  finally
    FreeMem(Entries.lpbin);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.ItemsChange(Sender: TObject);
var
  TempList: TStrings;
  SaveOnChange: TNotifyEvent;
begin
  if not Assigned(WAB) or Reading or WAB.LoadingProgress then Exit;
  
  SaveOnChange := TStringList(Sender).OnChange;
  TStringList(Sender).OnChange := nil;
  try
    TempList := TStringList.Create;
    try
      if Sender = FStreet then
      begin
        TempList.AddStrings(FStreet);
        SetStreet(TempList);
      end else
      if Sender = FComment then
      begin
        TempList.AddStrings(FComment);
        SetComment(TempList);
      end else
    finally
      TempList.Free;
    end;
  finally
    TStringList(Sender).OnChange := SaveOnChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetHomePage(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FHomePage <> Value then
  begin
    PropsValues[0].ulPropTag := PR_PERSONAL_HOME_PAGE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FHomePage := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetTelephoneNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FTelephoneNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_TELEPHONE_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FTelephoneNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetFaxNumber(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FFaxNumber <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_FAX_NUMBER;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FFaxNumber := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetCity(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FCity <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_CITY;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FCity := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetCountry(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FCountry <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_COUNTRY;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FCountry := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetPostalCode(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FPostalCode <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_POSTAL_CODE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FPostalCode := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetStateOrProvince(const Value: string);
var
  PropsValues: TSPropsArray;
begin
  if FStateOrProvince <> Value then
  begin
    PropsValues[0].ulPropTag := PR_HOME_ADDRESS_STATE_OR_PROVINCE;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value);
{$ENDIF}
    if SetPropsValues(1, PropsValues, Length(Value) = 0) then
    begin
      FStateOrProvince := Value;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetStreet(Value: TStrings);
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_HOME_ADDRESS_STREET;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value.Text);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value.Text);
{$ENDIF}
  if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
  begin
    FStreet.Assign(Value);
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupItem.SetComment(Value: TStrings);
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_COMMENT;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(Value.Text);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(Value.Text);
{$ENDIF}
  if SetPropsValues(1, PropsValues, Trim(Value.Text) = '') then
  begin
    FComment.Assign(Value);
    Changed(False);
  end;
end;

//==============================================================================
// TabfWABGroupsCollection
//==============================================================================
// Author: LinX
// Date: 19/03/2001

constructor TabfWABGroupsCollection.Create(AWAB: TabfWAB);
begin
  inherited Create(TabfWABGroupItem, AWAB);
end;

//------------------------------------------------------------------------------

function TabfWABGroupsCollection.GetItem(Index: Integer): TabfWABGroupItem;
begin
  Result := TabfWABGroupItem(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupsCollection.SetItem(Index: Integer;
  Value: TabfWABGroupItem);
begin
  TabfWABGroupItem(Items[Index]).Assign(Value);
end;

//------------------------------------------------------------------------------

function TabfWABGroupsCollection.GetOwner: TPersistent;
begin
  Result := FWAB;
end;

//------------------------------------------------------------------------------

{$IFDEF D3_ONLY}

function TabfWABGroupsCollection.Insert(Index: Integer): TabfWABGroupItem;
begin
  Result := Add;
  Result.Index := Index;
end;

{$ELSE}

function TabfWABGroupsCollection.Insert(Index: Integer): TabfWABGroupItem;
begin
  Result := TabfWABGroupItem(inherited Insert(Index));
end;

{$ENDIF}

//------------------------------------------------------------------------------

function TabfWABGroupsCollection.Add: TabfWABGroupItem;
begin
  if Assigned(WAB) then
  begin
    WAB.CheckActive;
    WAB.CheckReadOnly;
  end;

  Result := TabfWABGroupItem.Create(Self);

  if Assigned(WAB) and Assigned(Result) then
  with WAB.Items.Add do
  begin
    FItemType := MAPI_DISTLIST;
    Result.FItemID := ID;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABGroupsCollection.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source = Self then Exit;

  if Assigned(WAB) then WAB.CheckReadOnly;

  if Source is TabfWABGroupsCollection then
  begin
    if Assigned(WAB) then WAB.BeginUpdate;
    BeginUpdate;
    try
      if Assigned(WAB) then WAB.DeleteWABItems(Self, NULL);

      Clear;

      if Assigned(WAB) then
        for i := WAB.Items.Count - 1 downto 0 do
        begin
          if WAB.Items[i].ItemType = MAPI_DISTLIST then
            WAB.Items[i].Free;
        end;

      for i := 0 to TabfWABGroupsCollection(Source).Count - 1 do
      begin
        Add.Assign(TabfWABGroupsCollection(Source).Items[i]);
        if Assigned(WAB) then
          WAB.Items.Add.Assign(TabfWABGroupsCollection(Source).Items[i]);
      end;

      for i := 0 to Self.Count - 1 do
        Self[i].FTempVarArray := NULL;
    finally
      EndUpdate;
      if Assigned(WAB) then WAB.EndUpdate;
    end;

    Exit;
  end;
  inherited Assign(Source);
end;


//==============================================================================
// TabfWABFolderItem
//==============================================================================
// Author: LinX
// Date: 27/02/2002

constructor TabfWABFolderItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FWAB := nil;

  if Assigned(ACollection) and (ACollection is TabfWABItemsCollection)
    and Assigned(TabfWABItemsCollection(ACollection).WAB) then
    FWAB := TabfWABItemsCollection(ACollection).WAB;

  abfNewSBinary2(FWAB.WABObject, FDistListTemplateID, 0);
  abfNewSBinary2(FWAB.WABObject, FMailUserTemplateID, 0);
  FDisplayName := SabfWAB_DefaultItemName;
  FItemType := MAPI_ABCONT;
  FContainerFlags := 0;
  FDisplayType := 0;
  FItemID := 0;
  FABContainer := nil;
end;

//------------------------------------------------------------------------------

destructor TabfWABFolderItem.Destroy;
begin
  FABContainer := nil;
  abfFreeSBinary2(FWAB.WABObject, FMailUserTemplateID);
  abfFreeSBinary2(FWAB.WABObject, FDistListTemplateID);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfWABFolderItem.CheckIABContainer: Boolean;
var
  ParentID, NewID: PSBinary;
  Container: IABContainer;
  ObjType: ULONG;
  Res: Cardinal;
begin
  Result := False;
  if not (Assigned(WAB) and Assigned(WAB.AddrBook)) then Exit;

  if Assigned(FABContainer) then
  begin
    Result := True;
    Exit;
  end;

  if Assigned(EntryID.lpb) then
  begin
    Res := WAB.AddrBook.OpenEntry(EntryID.cb, EntryID.lpb, nil, MAPI_MODIFY,
      ObjType, IUnknown(FABContainer));
    if Res <> 0 then Beep;
  end
  else
    with WAB do
    begin
      ParentID := nil;
      NewID := nil;
      GetWABTemplateID(MAPI_ABCONT, ParentID, NewID);
      try
        WAB.AddrBook.OpenEntry(ParentID.cb, ParentID.lpb,
          nil, 0, ObjType, IUnknown(Container));
        Container.CreateEntry(NewID.cb, NewID.lpb,
          0, IMAPIProp(FABContainer));
      finally
        abfFreeSBinary2(FWAB.WABObject, ParentID);
        abfFreeSBinary2(FWAB.WABObject, NewID);
      end;
    end;{with WAB do}

  if Assigned(FABContainer) then Result := True;
end;

//------------------------------------------------------------------------------

procedure TabfWABFolderItem.DoInitDisplayName;
var
  PropsValues: TSPropsArray;
begin
  PropsValues[0].ulPropTag := PR_DISPLAY_NAME;
{$IFDEF UNICODE}
    PropsValues[0].Value.lpszW := PWideChar(FDisplayName);
{$ELSE}
    PropsValues[0].Value.lpszA := PAnsiChar(FDisplayName);
{$ENDIF}
  if CheckIABContainer then
    FABContainer.SetProps(1, @PropsValues, nil);
end;

//------------------------------------------------------------------------------

function TabfWABFolderItem.GetDistListTemplateID: PSBinary;
begin
  Result := FDistListTemplateID;
end;

//------------------------------------------------------------------------------

function TabfWABFolderItem.GetMailUserTemplateID: PSBinary;
begin
  Result := FMailUserTemplateID;
end;

//------------------------------------------------------------------------------

procedure TabfWABFolderItem.Assign(Source: TPersistent);
begin
  if not Assigned(WAB) then Exit;

  WAB.CheckReadOnly;

  if Source is TabfWABFolderItem then
  begin
    DisplayName := TabfWABFolderItem(Source).DisplayName;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABFolderItem.GetItemsArray: Variant;
const
  ItemsEntryIds: record
    Count: ULONG;
    Definition: array[0..1] of ULONG;
  end = (
    Count: 2;
    Definition: (PR_OBJECT_TYPE, PR_ENTRYID)
  );
var
  Table: IMAPITable;
  TableRow: PSRowSet;
  PSB: PSBinary;
  Res: HResult;
  i, idx: Integer;
begin
  Result := NULL;
  if not Assigned(WAB) then Exit;

  WAB.CheckActive;

  if not CheckIABContainer then Exit;

  Res := FABContainer.GetContentsTable(0, Table);
  if (Res <> S_OK) or (Table = nil) then Exit;

  Table.SetColumns(@ItemsEntryIds, 0);
  Table.SeekRow(BOOKMARK_BEGINNING, 0, nil);

  i := 0;
  PSB := nil;

  repeat
    Table.QueryRows(1, 0, TableRow);
    if TableRow.cRows > 0 then
    with TableRow^.aRow[0] do
    begin
      if lpProps[0].Value.l in [MAPI_MAILUSER, MAPI_DISTLIST] then
      begin
        abfMoveSBinary2(FWAB.WABObject, @(lpProps[1].Value.bin), PSB);
        try
          idx := WAB.GetItemIndex(PSB);
          if idx <> -1 then
          begin
            if VarIsNull(Result) then
              Result := VarArrayCreate([0, 0], varInteger)
            else
              VarArrayRedim(Result, i);
            Result[i] := idx;
            Inc(i);
          end;
        finally
          abfFreeSBinary2(WAB.WABObject, PSB);
        end;
      end;
      abfFreeSRowSet(WAB.WabObject, TableRow);
    end
    else
      Break;
  until False;

  if VarIsNull(Result) then Exit;

  _DistinctVariantArray(Result);
end;

//------------------------------------------------------------------------------

function TabfWABFolderItem.GetContactsArray(ProcessSubtree: Boolean): Variant;
var
  V, V1: Variant;
  H, H1, i, y, a: Integer;
begin
  Result := NULL;

  V := GetItemsArray;
  if not VarIsArray(V) then Exit;

  H := VarArrayHighBound(V, 1);

  y := 0;

  for i := 0 to H do
  begin
    if WAB.Items[V[i]].ItemType = MAPI_MAILUSER then
    begin
      if VarIsNull(Result) then
      begin
        Result := VarArrayCreate([0, 0], varInteger);
        Result[0] := WAB.GetContactIndex(WAB.Items[V[i]].EntryID);
      end
      else
      begin
        Inc(y);
        VarArrayRedim(Result, y);
        Result[y] := WAB.GetContactIndex(WAB.Items[V[i]].EntryID);
      end;
    end
    else if ProcessSubtree and (WAB.Items[V[i]].ItemType = MAPI_DISTLIST) then
    begin
      a := WAB.GetGroupIndex(WAB.Items[V[i]].EntryID);
      if (a >= 0) and (a < WAB.Groups.Count) then
        V1 := WAB.Groups[a].GetContactsArray(ProcessSubtree)
      else
        V1 := NULL;
      if VarIsArray(V1) then
      begin
        if not VarIsNull(Result) then
        begin
          H1 := VarArrayHighBound(V1, 1);
          VarArrayRedim(Result, y + H1 + 1);

          for a := 0 to H1 do
            Result[y + a + 1] := V1[a];

          y := VarArrayHighBound(Result, 1);
        end
        else
        begin
          Result := V1;
          y := VarArrayHighBound(Result, 1);
        end;
      end;
    end;
  end;

  if VarIsNull(Result) then Exit;

  _DistinctVariantArray(Result);
end;

//==============================================================================
// TabfWABFoldersCollection
//==============================================================================
// Author: LinX
// Date: 27/02/2002

constructor TabfWABFoldersCollection.Create(AWAB: TabfWAB);
begin
  inherited Create(TabfWABFolderItem, AWAB);
end;

//------------------------------------------------------------------------------

function TabfWABFoldersCollection.GetItem(Index: Integer): TabfWABFolderItem;
begin
  Result := TabfWABFolderItem(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------

procedure TabfWABFoldersCollection.SetItem(Index: Integer;
  Value: TabfWABFolderItem);
begin
  TabfWABFolderItem(Items[Index]).Assign(Value);
end;

//------------------------------------------------------------------------------

function TabfWABFoldersCollection.GetOwner: TPersistent;
begin
  Result := FWAB;
end;

//------------------------------------------------------------------------------

{$IFDEF D3_ONLY}

function TabfWABFoldersCollection.Insert(Index: Integer): TabfWABFolderItem;
begin
  Result := Add;
  Result.Index := Index;
end;

{$ELSE}

function TabfWABFoldersCollection.Insert(Index: Integer): TabfWABFolderItem;
begin
  Result := TabfWABFolderItem(inherited Insert(Index));
end;

{$ENDIF}

//------------------------------------------------------------------------------

function TabfWABFoldersCollection.Add: TabfWABFolderItem;
begin
  if Assigned(WAB) then
  begin
    WAB.CheckActive;
    WAB.CheckReadOnly;
  end;

  Result := TabfWABFolderItem.Create(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWABFoldersCollection.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;

  if Assigned(WAB) then WAB.CheckReadOnly;

  raise EabfWABException.Create(SabfWAB_NotImplementedError);
end;

//==============================================================================
//  TabfAdviseSink
//==============================================================================
// A wrapper-class of the IMAPIAdviseSink interface.
// Author: LinX
// Date: 06/01/2000

function TabfAdviseSink.OnNotify(cNotif: ULONG; lpNotifications: PNotification): ULONG;
begin
  Result := S_OK;
  if Assigned(FOnNotifyEvent) then FOnNotifyEvent(cNotif, lpNotifications);
end;

//==============================================================================
//  TabfWAB
//==============================================================================
// Non-visual component TabfWAB (Windows Address Book). Provides full access to
// the Windows Address Book or to any files in the WAB format.
// Author: LinX
// Created: 06/01/2000
// Changed: 02/10/2002

constructor TabfWAB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (not abfGetWabVersion(FWABVersion)) and (csDesigning in ComponentState) then
    MessageBoxEx(ParentHandle, PChar(SabfWAB_ErrorLoadingWABAPI),
    PChar(SabfError), MB_OK or MB_ICONERROR,  LANG_NEUTRAL);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FLibHandle := 0;
  FActive := False;
  FAddToFolder := wafProfileFolder;
  FCachedUpdates := False;
  FConnection := 0;
  FContacts := TabfWABContactsCollection.Create(Self);
  FContentsFilter := wcfAllContents;
  FEnableProfiles := False;
  FEventFired := False;
  FFolders := TabfWABFoldersCollection.Create(Self);
  FGroups := TabfWABGroupsCollection.Create(Self);
  FItems := TabfWABItemsCollection.Create(TabfWABItem, Self);
  FLoadingProgress := False;
  FReadOnly := False;
  FUpdateCount := 0;
  FWABSharedMode := False;
  FWABMode := wmCurrentWAB;
end;

//------------------------------------------------------------------------------

destructor TabfWAB.Destroy;
begin
  Close;
  FreeAndNil(FItems);
  FreeAndNil(FGroups);
  FreeAndNil(FContacts);
  FreeAndNil(FFolders);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.Loaded;
begin
  inherited Loaded;
  if FStreamedActive then Active := True;
  if not (csDesigning in ComponentState) then
    if not _ASK then _TADA;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetOutlookSharedMode: Boolean;
begin
  Result := abfIsOutlookSharedMode;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetCurrentFolder: TabfWABFolderItem;
var
  i: Integer;
  pEID: PEntryID;
  Res, cEID: ULONG;
begin
  Result := nil;

  if Assigned(FCurrentFolder) then
    Result := FCurrentFolder
  else
    begin
      pEID := nil;
      cEID := 0;
      Res := FAddrBook.GetPAB(cEID, pEID);
      try
        for i := 0 to FFolders.Count - 1 do
        begin
          if (FAddrBook.CompareEntryIDs(FFolders[i].EntryID.cb, FFolders[i].EntryID.lpb,
            cEID, pEID, 0, Res) = S_OK) and (Res <> 0) then
          begin
            Result := FFolders[i];
            CurrentFolder := Result;
            Break;
          end;
        end;
      finally
        //use ONLY(!) FWABObject.FreeBuffer
        if Assigned(pEID) then FWABObject.FreeBuffer(pEID);
      end;
    end;
end;


//------------------------------------------------------------------------------

function TabfWAB.GetRootContainer: IABContainer;
var
  ObjType: ULONG;
begin
  Result := nil;
  if not Assigned(FAddrBook) then Exit;

  if not Assigned(FRootContainer) then
    FAddrBook.OpenEntry(0, nil, nil, MAPI_MODIFY, ObjType, IUnknown(FRootContainer));
  Result := FRootContainer;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
  begin
    FStreamedActive := Value;
  end else
  if (FActive <> Value) then
  begin
    if Value then Open else Close;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetCachedUpdates(Value: Boolean);
begin
  if FCachedUpdates <> Value then
  begin
    FCachedUpdates := Value;
    if not (csReading in ComponentState) then
    begin
      if not FCachedUpdates then ApplyUpdates else
      begin
        BeginUpdate;
        try
          Refresh;
        finally
          EndUpdate;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetContactsCollection(Value: TabfWABContactsCollection);
begin
  FContacts.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetContentsFilter(Value: TabfWABContentsFilter);
begin
  if FContentsFilter <> Value then
  begin
    if FActive then Close;
    FContentsFilter := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetCurrentFolder(Value: TabfWABFolderItem);
var
  ObjType: ULONG;
begin
  if FCurrentFolder <> Value then
  begin
    FCurrentFolder := Value;

    if Assigned(FCurrentFolder) and Assigned(FCurrentFolder.EntryID) then
      FAddrBook.OpenEntry(FCurrentFolder.EntryID.cb, FCurrentFolder.EntryID.lpb,
        nil, 0, ObjType, IUnknown(FCurrentContainer))
    else
      FCurrentContainer := nil;
    if not FLoadingProgress then DoRefreshFolderContent;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetFoldersCollection(Value: TabfWABFoldersCollection);
begin
  FFolders.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetGroupsCollection(Value: TabfWABGroupsCollection);
begin
  FGroups.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetEnableProfiles(Value: Boolean);
begin
  if FEnableProfiles <> Value then
  begin
    if FActive and (FWABMode <> wmWABFile) then Close;
    FEnableProfiles := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetFileName(const Value: TFileName);
begin
  if FFileName <> Trim(Value) then
  begin
    if FActive and (FWABMode = wmWABFile) then Close;
    FFileName := Trim(Value);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetItemsCollection(Value: TabfWABItemsCollection);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetOutlookSharedMode(Value: Boolean);
begin
  if abfSetOutlookSharedMode(Value) and FActive then Close;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetWABMode(Value: TabfWABMode);
begin
  if FWABMode <> Value then
  begin
    if FActive then Close;
    FWABMode := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.SetWABVersion(const Value: string);
begin
  //fake method
end;

//------------------------------------------------------------------------------

procedure TabfWAB.Hook_AB_Notification;
var
  EntryID: PEntryID;
  EntryIDSize: ULONG;
begin
  FAddrBook.GetPAB(EntryIDSize, EntryID);
  try
    FAdviseSinkObject := TabfAdviseSink.Create;
    FAdviseSink := FAdviseSinkObject;
    FAddrBook.Advise(EntryIDSize, EntryID,
      fnevObjectModified or fnevTableModified,
      FAdviseSink, ULONG(FConnection));
    FAdviseSinkObject.OnNotifyEvent := DoAdviseNotify;
  finally
    FWabObject.FreeBuffer(EntryID);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.Unhook_AB_Notification;
begin
  if FConnection > 0 then
  begin
    FAdviseSinkObject.OnNotifyEvent := nil;
    try
      if Assigned(FAddrBook) then
        FAddrBook.Unadvise(FConnection);
    finally
      FConnection := 0;
      FAdviseSink := nil;
    end;
  end
  else if Assigned(FAdviseSinkObject) then FAdviseSink := nil;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetFolderContentsTable(out ATable: IMAPITable): HResult;
var
  pEID: PEntryID;
  cEID: ULONG;
  Flags, ObjType: ULONG;
  Container: IABContainer;
begin
  Result := MAPI_E_CALL_FAILED;

  if abfIsOutlookSharedMode then
    Flags := 0
  else
    Flags := WAB_LOCAL_CONTAINERS;

  if FEnableProfiles and (FContentsFilter <> wcfCurrentFolderOnly) then
    Flags := Flags or WAB_PROFILE_CONTENTS;

  if Assigned(CurrentFolder) and Assigned(FCurrentContainer) then
    Result := FCurrentContainer.GetContentsTable(Flags, ATable)
  else
    begin
      if GetRootEntryID(cEID, pEID) <> S_OK then Exit;
      try
        FAddrBook.OpenEntry(cEID, pEID, nil, 0, ObjType, IUnknown(Container));
      finally
        if Assigned(pEID) then FWABObject.FreeBuffer(pEID);
      end;
      if not Assigned(Container) then Exit;
      Result := Container.GetContentsTable(Flags, ATable);
    end;

  if (Result <> S_OK) or (ATable = nil) then Exit;

  if FWABSharedMode then
    ATable.SetColumns(@abfOutlookItemProperties, 0)
  else
    ATable.SetColumns(@abfWABItemProperties, 0);
end;

//------------------------------------------------------------------------------

function TabfWAB.GetRootEntryID(var cEID: ULONG; var pEID: PEntryID): HResult;
begin
  Result := MAPI_E_CALL_FAILED;

  if not (Assigned(FAddrBook) and Assigned(FWABObject)) then Exit;

  if Assigned(pEID) then FWABObject.FreeBuffer(pEID);
  cEID := 0;
  pEID := nil;

  if (FContentsFilter = wcfProfileFolderOnly) or abfIsOutlookSharedMode then
    Result := FAddrBook.GetPAB(cEID, pEID)
  else
    Result := S_OK;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetWABTemplateID(ObjType: ULONG;
  var ParentID: PSBinary; var TemplateID: PSBinary): HResult;
const
  CreateProps: record
    Count: ULONG;
    Values: array[0..1] of ULONG;
  end = (
    Count: 2; Values: (PR_DEF_CREATE_MAILUSER, PR_DEF_CREATE_DL)
  );
var
  NewProps: ULONG;
  CreateEIDs: PSPropsArray;
begin
  Result := MAPI_E_CALL_FAILED;

  if not (ObjType in [MAPI_MAILUSER, MAPI_DISTLIST]) then //MAPI_ABCONT,
  begin
    Result := MAPI_E_INVALID_PARAMETER;
    Exit;
  end;

  if not Assigned(RootContainer) then Exit;

  CreateEIDs := nil;
  Result := RootContainer.GetProps(@CreateProps, 0, @NewProps,
    CreateEIDs);
  try
    if Result <> S_OK then Exit;

{      if ObjType = MAPI_ABCONT then
    begin
//        abfMoveSBinary(@(CreateEIDs^[0].Value.bin), );
      abfFreeSBinary(TemplateID);
      abfNewSBinary(TemplateID, 21);

      CopyMemory(TemplateID.lpb, ParentID.lpb, TemplateID.cb);
      FillChar(Pointer(Cardinal(TemplateID.lpb) + 20)^, 1, 7);
    end;{}
{    if ObjType = MAPI_ABCONT then
    begin
      abfFreeSBinary(TemplateID);
      abfNewSBinary(TemplateID, 21);
      CopyMemory(TemplateID.lpb, FCurrentFolder.EntryID.lpb, TemplateID.cb);
    end;
{}

    case ObjType of
      MAPI_MAILUSER: abfMoveSBinary2(FWABObject, @(CreateEIDs^[0].Value.bin), TemplateID);
      MAPI_DISTLIST: abfMoveSBinary2(FWABObject, @(CreateEIDs^[1].Value.bin), TemplateID);
    else
      abfMoveSBinary2(FWABObject, @(CreateEIDs^[0].Value.bin), TemplateID);
    end;
  finally
    if Assigned(CreateEIDs) then FWabObject.FreeBuffer(CreateEIDs);
  end;

  if (FAddToFolder = wafCurrentFolder)
    and Assigned(FCurrentFolder) and Assigned(FCurrentFolder.EntryID) then
  begin
    abfMoveSBinary2(FWABObject, FCurrentFolder.EntryID, ParentID);
  end else
  begin
    abfNewSBinary2(FWABObject, ParentID, 0);
    if FAddToFolder = wafProfileFolder then
      FAddrBook.GetPAB(ParentID.cb, PEntryID(ParentID.lpb));
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetContactIndex(const PSB: PSBinary): Integer;
var
  Res: ULONG;
  i: Integer;
begin
  Result := -1;
  for i := 0 to FContacts.Count - 1 do
  with FContacts[i] do
  begin
    FAddrBook.CompareEntryIDs(EntryID.cb, PEntryID(EntryID.lpb),
      PSB.cb, PEntryID(PSB.lpb), 0, Res);
    if Res <> 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetGroupIndex(const PSB: PSBinary): Integer;
var
  Res: ULONG;
  i: Integer;
begin
  Result := -1;
  for i := 0 to FGroups.Count - 1 do
  with FGroups[i] do
  begin
    FAddrBook.CompareEntryIDs(EntryID.cb, PEntryID(EntryID.lpb),
      PSB.cb, PEntryID(PSB.lpb), 0, Res);
    if Res <> 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetItemIndex(const PSB: PSBinary): Integer;
var
  Res: ULONG;
  i: Integer;
begin
  Result := -1;
  for i := 0 to FItems.Count - 1 do
  with FItems[i] do
  begin
    FAddrBook.CompareEntryIDs(EntryID.cb, PEntryID(EntryID.lpb),
      PSB.cb, PEntryID(PSB.lpb), 0, Res);
    if Res <> 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetMeContactIndex(AllowCreate: Boolean): Integer;
var
  PSB: PSBinary;
  ulFlags, dwAction: Cardinal;
begin
  Result := -1;

  if not Assigned(FAddrBook) then Exit;

  if ReadOnly then AllowCreate := False;
  if AllowCreate then
    ulFlags := 0
  else
    ulFlags := WABOBJECT_ME_NOCREATE;

  abfNewSBinary2(FWABObject, PSB, 0);
  try
    if FWABObject.GetMe(FAddrBook, ulFlags, @dwAction, PSB,
      ULONG(ParentHandle)) = S_OK then
    begin
      if dwAction = WABOBJECT_ME_NEW then DoRefreshFolderContent;
      Result := GetContactIndex(PSB);
    end;
  finally
    abfFreeSBinary2(FWABObject, PSB);
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.GetMeItemIndex(AllowCreate: Boolean): Integer;
var
  PSB: PSBinary;
  ulFlags, dwAction: Cardinal;
begin
  Result := -1;

  if ReadOnly then AllowCreate := False;
  if AllowCreate then
    ulFlags := 0
  else
    ulFlags := WABOBJECT_ME_NOCREATE;

  abfNewSBinary2(FWABObject, PSB, 0);
  try
    if FWABObject.GetMe(FAddrBook, ulFlags, @dwAction, PSB,
      ULONG(ParentHandle)) = S_OK then
    begin
      if dwAction = WABOBJECT_ME_NEW then DoRefreshFolderContent;
      Result := GetItemIndex(PSB);
    end;
  finally
    abfFreeSBinary2(FWABObject, PSB);
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.GroupDetails(var GroupIndex: Integer): Boolean;
var
  TempHandle: ULONG;
  PSB: PSBinary;
begin
  Result := False;

  TempHandle := ParentHandle;

  CheckActive;
  CheckReadOnly;

  BeginUpdate;
  try
    if GroupIndex < 0 then GroupIndex := 0
    else if GroupIndex >= FGroups.Count then
      GroupIndex := FGroups.Count - 1;
    abfNewSBinary2(FWABObject, PSB, FGroups[GroupIndex].EntryID.cb);
    try
      CopyMemory(PSB.lpb, FGroups[GroupIndex].EntryID.lpb, PSB.cb);
      if FAddrBook.Details(TempHandle, nil, nil,
        PSB.cb, PEntryID(PSB.lpb), nil, nil, nil, 0) = S_OK then
      begin
        DoRefreshFolderContent;
        GroupIndex := GetGroupIndex(PSB);
        Result := (GroupIndex > -1) and (GroupIndex < FGroups.Count);
      end;
    finally
      abfFreeSBinary2(FWABObject, PSB);
    end;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.ItemDetails(var ItemIndex: Integer): Boolean;
var
  TempHandle: ULONG;
  PSB: PSBinary;
begin
  Result := False;

  TempHandle := ParentHandle;

  CheckActive;
  CheckReadOnly;

  BeginUpdate;
  try
    if ItemIndex < 0 then ItemIndex := 0
    else if ItemIndex >= FItems.Count then
      ItemIndex := FItems.Count - 1;
    abfNewSBinary2(FWABObject, PSB, FItems[ItemIndex].EntryID.cb);
    try
      CopyMemory(PSB.lpb, FItems[ItemIndex].EntryID.lpb, PSB.cb);
      if FAddrBook.Details(TempHandle, nil, nil,
        PSB.cb, PEntryID(PSB.lpb), nil, nil, nil, 0) = S_OK then
      begin
        DoRefreshFolderContent;
        ItemIndex := GetItemIndex(PSB);
        Result := (ItemIndex > -1) and (ItemIndex < FItems.Count);
      end;
    finally
      abfFreeSBinary2(FWABObject, PSB);
    end;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.CreateEntry(ObjType: ULONG; var EID: PSBinary): HResult;
var
  ParentID, NewID: PSBinary;
begin
  Result := MAPI_E_CALL_FAILED;

  CheckActive;
  CheckReadOnly;

  try
    ParentID := nil;
    NewID := nil;
    GetWABTemplateID(ObjType, ParentID, NewID);
    try
      Result := FAddrBook.NewEntry(ParentHandle, 0,
        ParentID.cb, ParentID.lpb, NewID.cb, NewID.lpb,
        EID.cb, PEntryID(EID.lpb));
    finally
      abfFreeSBinary2(FWABObject, ParentID);
      abfFreeSBinary2(FWABObject, NewID);
    end;
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.CheckActive;
begin
  if not Active then
    raise EabfWABException.Create(SabfWAB_NotOpenedError);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.CheckReadOnly;
begin
  if (not FLoadingProgress) and ReadOnly then
    raise EabfWABException.Create(SabfWAB_ReadOnlyError);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteEntries(const EntriesArray: TSBinaryArray);
var
  pEID: PEntryID;
  cEID, ObjType: ULONG;
  Container: IABContainer;
begin
  CheckActive;
  CheckReadOnly;

  if EntriesArray.cValues <= 0 then Exit;

  pEID := nil;
  cEID := 0;

  GetRootEntryID(cEID, pEID);
  try
    FAddrBook.OpenEntry(cEID, pEID, nil, 0, ObjType,
      IUnknown(Container));
    Container.DeleteEntries(@EntriesArray, 0);
  finally
    if Assigned(pEID) then FWABObject.FreeBuffer(pEID);
    Container := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteWABItems(WABItems: TabfWABItemsCollection;
  ItemsIndexes: Variant);
var
  Entries: TSBinaryArray;
  PSB: PSBinary;
  i, LengthArr, LowBound, HighBound: Integer;
begin
  CheckActive;
  CheckReadOnly;

  Entries.cValues := 0;
  LowBound := 0;
  HighBound := 0;
  if VarIsArray(ItemsIndexes) then
  begin
    LowBound := VarArrayLowBound(ItemsIndexes, 1);
    HighBound := VarArrayHighBound(ItemsIndexes, 1);
    LengthArr := HighBound - LowBound + 1;
  end
  else if VarType(ItemsIndexes) = varInteger then LengthArr := 1
  else if VarType(ItemsIndexes) in [varEmpty, varNull] then
    LengthArr := WABItems.Count
  else
    Exit;
  GetMem(Entries.lpbin, LengthArr * SizeOf(SBinary));
  try
    PSB := Entries.lpbin;
    if VarIsArray(ItemsIndexes) then
    begin
      for i := LowBound to HighBound do
      if (ItemsIndexes[i] >= 0) and (ItemsIndexes[i] < WABItems.Count) then
      begin
        CopyMemory(PSB, WABItems[ItemsIndexes[i]].EntryID, SizeOf(SBinary));
        PSB := Pointer(Cardinal(PSB) + SizeOf(SBinary));
        Inc(Entries.cValues);
      end;
    end
    else if VarType(ItemsIndexes) = varInteger then
    begin
      if (ItemsIndexes >= 0) and (ItemsIndexes < WABItems.Count) then
      begin
        Entries.cValues := 1;
        CopyMemory(PSB, WABItems[ItemsIndexes].EntryID, SizeOf(SBinary));
      end;
    end
    else if VarType(ItemsIndexes) in [varEmpty, varNull] then
    begin
      for i := 0 to LengthArr - 1 do
      begin
        CopyMemory(PSB, WABItems[i].EntryID, SizeOf(SBinary));
        PSB := Pointer(Cardinal(PSB) + SizeOf(SBinary));
        Inc(Entries.cValues);
      end;
    end;
    if Entries.cValues > 0 then DeleteEntries(Entries);
  finally
    FreeMem(Entries.lpbin);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoAdviseNotify(cNotif: ULONG; lpNotifications: PNOTIFICATION);
begin
  FEventFired := True;
  if (lpNotifications.ulEventType = fnevObjectModified) then
    DoChangeAddressBook;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoInternalEvents;
begin
  if Assigned(FOnInternalItemsChange) then FOnInternalItemsChange(Self);
  if Assigned(FOnInternalContactsChange) then FOnInternalContactsChange(Self);
  if Assigned(FOnInternalGroupsChange) then FOnInternalGroupsChange(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoOpenAddressBook;
begin
  DoInternalEvents;
  if Assigned(FOnOpenAddressBook) then FOnOpenAddressBook(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoCloseAddressBook;
begin
  DoInternalEvents;
  if Assigned(FOnCloseAddressBook) then FOnCloseAddressBook(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoDeleteContact;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(FOnDeleteContact) then FOnDeleteContact(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoDeleteGroup;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(FOnDeleteGroup) then FOnDeleteGroup(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoDeleteItem;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(FOnDeleteItem) then FOnDeleteItem(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoDeletingContact(var AllowDelete: Boolean);
begin
  if Assigned(FOnDeletingContact) then FOnDeletingContact(Self, AllowDelete);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoDeletingGroup(var AllowDelete: Boolean);
begin
  if Assigned(FOnDeletingGroup) then FOnDeletingGroup(Self, AllowDelete);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoDeletingItem(var AllowDelete: Boolean);
begin
  if Assigned(FOnDeletingItem) then FOnDeletingItem(Self, AllowDelete);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoNewContact;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(FOnNewContact) then FOnNewContact(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoNewGroup;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(FOnNewGroup) then FOnNewGroup(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoNewItem;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(FOnNewItem) then FOnNewItem(Self);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoChangeAddressBook;
var
  SaveNotifyEvent: TabfAdviseNotifyEvent;
begin
  if FUpdateCount > 0 then Exit;
  if FEventFired then FEventFired := False else Exit;
  SaveNotifyEvent := FAdviseSinkObject.OnNotifyEvent;
  FAdviseSinkObject.OnNotifyEvent := nil;
  try
    Refresh;
    DoInternalEvents;
    if Assigned(FOnChangeAddressBook) then FOnChangeAddressBook(Self);
  finally
    FAdviseSinkObject.OnNotifyEvent := SaveNotifyEvent;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoRefreshFolders;
var
  TempFolder: TabfWABFolderItem;
  Table: IMAPITable;
  TableRow: PSRowSet;
  Res: HResult;

  //-------------------------------------

   function _CanAdd(APropExists: Boolean): Boolean;
   begin
     Result := True;
{     Result := FWABSharedMode or (not FEnableProfiles)
      or ((not APropExists) and (FContentsFilter <> wcfProfileFolderOnly))
      or (APropExists and (FContentsFilter <> wcfSharedFolderOnly));{}
   end;

  //-------------------------------------

   procedure _AddFolder;
   begin
     TempFolder := FFolders.Add;
     with TableRow^.aRow[0] do
     with TempFolder do
     begin
       Reading := True;
       try
         if abfIfWABPropExists(lpProps[0]) then
           FContainerFlags := lpProps[0].Value.ul;
         SetEntryID(@(lpProps[1].Value.bin));
{         if abfIfWABPropExists(lpProps[3]) then
           abfMoveSBinary2(WAB.WABObject, @(lpProps[3].Value.bin), FDistListTemplateID);
         if abfIfWABPropExists(lpProps[4]) then
           abfMoveSBinary2(WAB.WABObject, @(lpProps[4].Value.bin), FMailUserTemplateID);{}
         if abfIfWABPropExists(lpProps[5]) then
           FDisplayType := lpProps[5].Value.ul;
         if abfIfWABPropExists(lpProps[6]) then
           FDisplayName := lpProps[6].Value.lpszA;
       finally
         Reading := False;
       end;
     end;
   end;

  //-------------------------------------

begin
  if (not Assigned(FAddrBook)) or (not Assigned(FWabObject)) then Exit;
  if not Assigned(RootContainer) then Exit;

  FFolders.BeginUpdate;
  try
    CurrentFolder := nil;
    FFolders.Clear;

    Res := RootContainer.GetContentsTable(WAB_LOCAL_CONTAINERS, Table);

    if (Res <> S_OK) or (Table = nil) then Exit;

    Table.SetColumns(@abfWABFolderProperties, 0);

    Table.SeekRow(BOOKMARK_BEGINNING, 0, nil);
    repeat
      Table.QueryRows(1, 0, TableRow);
      if TableRow.cRows > 0 then
        with TableRow.aRow[0] do
        begin
          if _CanAdd(abfIfWABPropExists(lpProps[0])) then
          begin
            if lpProps[2].Value.ul = MAPI_ABCONT then
              _AddFolder;
          end;
          abfFreeSRowSet(FWabObject, TableRow);
        end
      else
        Break;
    until False;
  finally
    FFolders.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DoRefreshFolderContent;
var
  TempContact: TabfWABContactItem;
  TempGroup: TabfWABGroupItem;
  TempItem: TabfWABItem;
  Table: IMAPITable;
  TableRow: PSRowSet;
  Res: HResult;

  //-------------------------------------

   function _CanAdd(APropExists: Boolean): Boolean;
   begin
     Result := FWABSharedMode or (not FEnableProfiles)
      or ((not APropExists) and (FContentsFilter <> wcfProfileFolderOnly))
      or (APropExists and (FContentsFilter <> wcfSharedFolderOnly));
   end;

  //-------------------------------------

   procedure _AddContact;
   begin
     TempContact := FContacts.Add;
     with TempContact do
     with TableRow^.aRow[0] do
     begin
       Reading := True;
       try
         if abfIfWABPropExists(lpProps[0])
           and (lpProps[0].Value.MVbin.cValues > 0) then
             SetParentID(lpProps[0].Value.MVbin.lpbin);
         SetEntryID(@(lpProps[1].Value.bin));
         SetSearchKey(@(lpProps[3].Value.bin));
         if abfIfWABPropExists(lpProps[4]) then CopyMemory(@FCreationTime,
           @(lpProps[4].Value.ft), SizeOf(FCreationTime));
         if abfIfWABPropExists(lpProps[5]) then CopyMemory(@FLastModificationTime,
           @(lpProps[5].Value.ft), SizeOf(FLastModificationTime));
         if abfIfWABPropExists(lpProps[6]) then
           FDisplayName := lpProps^[6].Value.lpszA;
         if abfIfWABPropExists(lpProps[7]) then
           FNamePrefix := lpProps[7].Value.lpszA;
         if abfIfWABPropExists(lpProps[8]) then
           FGivenName := lpProps[8].Value.lpszA;
         if abfIfWABPropExists(lpProps[9]) then
           FMiddleName := lpProps[9].Value.lpszA;
         if abfIfWABPropExists(lpProps[10]) then
           FSurname := lpProps[10].Value.lpszA;
         if abfIfWABPropExists(lpProps[11]) then
           FGender := lpProps[11].Value.l;
         if abfIfWABPropExists(lpProps[12]) then
           FNickname := lpProps[12].Value.lpszA;
         if abfIfWABPropExists(lpProps[13]) then CopyMemory(@FWeddingAnniversary,
           @(lpProps[13].Value.ft), SizeOf(TFileTime));
         if abfIfWABPropExists(lpProps[14]) then CopyMemory(@FBirthday,
           @(lpProps[14].Value.ft), SizeOf(TFileTime));
         if abfIfWABPropExists(lpProps[15]) then
           FAddrType := lpProps[15].Value.lpszA;
         if abfIfWABPropExists(lpProps[16]) then
           FEmailAddress := lpProps[16].Value.lpszA;
         abfCopyWABStringsArray(lpProps[17], FAddrTypes);
         if FAddrTypes.Count = 0 then FAddrTypes.Add(FAddrType);
         if abfIfWABPropExists(lpProps[18]) then
           FDefaultAddressIndex := lpProps[18].Value.l;
         abfCopyWABStringsArray(lpProps[19], FEmailAddresses);
         if FEmailAddresses.Count = 0 then FEmailAddresses.Add(FEmailAddress);
         if abfIfWABPropExists(lpProps[20]) then
           FSpouseName := lpProps[20].Value.lpszA;
         abfCopyWABStringsArray(lpProps[21], FChildrenNames);
         if abfIfWABPropExists(lpProps[22]) then
           FPersonalHomePage := lpProps[22].Value.lpszA;
         if abfIfWABPropExists(lpProps[23]) then
           FHomeTelephoneNumber := lpProps[23].Value.lpszA;
         if abfIfWABPropExists(lpProps[24]) then
           FMobileTelephoneNumber := lpProps[24].Value.lpszA;
         if abfIfWABPropExists(lpProps[25]) then
           FPagerTelephoneNumber := lpProps[25].Value.lpszA;
         if abfIfWABPropExists(lpProps[26]) then
           FHomeFaxNumber := lpProps[26].Value.lpszA;
         if abfIfWABPropExists(lpProps[27]) then
           FHomeAddressCity := lpProps[27].Value.lpszA;
         if abfIfWABPropExists(lpProps[28]) then
           FHomeAddressCountry := lpProps[28].Value.lpszA;
         if abfIfWABPropExists(lpProps[29]) then
           FHomeAddressPostalCode := lpProps[29].Value.lpszA;
         if abfIfWABPropExists(lpProps[30]) then
           FHomeAddressStateOrProvince := lpProps[30].Value.lpszA;
         if abfIfWABPropExists(lpProps[31]) then
           FHomeAddressStreet.Text := lpProps[31].Value.lpszA;
         if abfIfWABPropExists(lpProps[32]) then
           FBusinessHomePage := lpProps[32].Value.lpszA;
         if abfIfWABPropExists(lpProps[33]) then
           FCompanyName := lpProps[33].Value.lpszA;
         if abfIfWABPropExists(lpProps[34]) then
           FTitle := lpProps[34].Value.lpszA;
         if abfIfWABPropExists(lpProps[35]) then
           FDepartmentName := lpProps[35].Value.lpszA;
         if abfIfWABPropExists(lpProps[36]) then
           FOfficeLocation := lpProps[36].Value.lpszA;
         if abfIfWABPropExists(lpProps[37]) then
           FBusinessTelephoneNumber := lpProps[37].Value.lpszA;
         if abfIfWABPropExists(lpProps[38]) then
           FBusinessFaxNumber := lpProps[38].Value.lpszA;
         if abfIfWABPropExists(lpProps[39]) then
           FBusinessAddressCity := lpProps[39].Value.lpszA;
         if abfIfWABPropExists(lpProps[40]) then
           FBusinessAddressCountry := lpProps[40].Value.lpszA;
         if abfIfWABPropExists(lpProps[41]) then
           FBusinessAddressPostalCode := lpProps[41].Value.lpszA;
         if abfIfWABPropExists(lpProps[42]) then
           FBusinessAddressStateOrProvince := lpProps[42].Value.lpszA;
         if abfIfWABPropExists(lpProps[43]) then
           FBusinessAddressStreet.Text := lpProps[43].Value.lpszA;
         if abfIfWABPropExists(lpProps[44]) then
           FComment.Text := lpProps[44].Value.lpszA;
         if abfIfWABPropExists(lpProps[45]) then
           FEMailPlainTextOnly := lpProps[45].Value.ul = BODY_ENCODING_TEXT;
         if abfIfWABPropExists(lpProps[46]) then
           FIPPhone := lpProps[46].Value.lpszA;
         abfCopyWABStringsArray(lpProps[47], FConferencingServers);
         if abfIfWABPropExists(lpProps[48]) then
           FDefaultConferencingServer := lpProps[48].Value.l;
         if abfIfWABPropExists(lpProps[49]) then
           FBackupConferencingServer := lpProps[49].Value.l;
         if abfIfWABPropExists(lpProps[50]) then
           FDefaultAddress := TabfWABContactDefaultAddress(lpProps[50].Value.ul);
       finally
         Reading := False;
       end;

       TempItem := TabfWABItem(FItems.FindItemID(FItemID));
       if Assigned(TempItem) then
         TempItem.Assign(TempContact);
     end;
   end;

  //-------------------------------------

   procedure _AddGroup;
   begin
     TempGroup := FGroups.Add;
     with TableRow^.aRow[0] do
     with TempGroup do
     begin
       Reading := True;
       try
         if abfIfWABPropExists(lpProps[0])
           and (lpProps[0].Value.MVbin.cValues > 0) then
             SetParentID(lpProps[0].Value.MVbin.lpbin);
         SetEntryID(@(lpProps[1].Value.bin));
         SetSearchKey(@(lpProps[3].Value.bin));
         if abfIfWABPropExists(lpProps[4]) then CopyMemory(@FCreationTime,
           @(lpProps[4].Value.ft), SizeOf(FCreationTime));
         if abfIfWABPropExists(lpProps[5]) then CopyMemory(@FLastModificationTime,
           @(lpProps[5].Value.ft), SizeOf(FLastModificationTime));
         if abfIfWABPropExists(lpProps[6]) then
           FDisplayName := lpProps[6].Value.lpszA;
         if abfIfWABPropExists(lpProps[22]) then
           FHomePage := lpProps[22].Value.lpszA;
         if abfIfWABPropExists(lpProps[23]) then
           FTelephoneNumber := lpProps[23].Value.lpszA;
         if abfIfWABPropExists(lpProps[26]) then
           FFaxNumber := lpProps[26].Value.lpszA;
         if abfIfWABPropExists(lpProps[27]) then
           FCity := lpProps[27].Value.lpszA;
         if abfIfWABPropExists(lpProps[28]) then
           FCountry := lpProps[28].Value.lpszA;
         if abfIfWABPropExists(lpProps[29]) then
           FPostalCode := lpProps[29].Value.lpszA;
         if abfIfWABPropExists(lpProps[30]) then
           FStateOrProvince := lpProps[30].Value.lpszA;
         if abfIfWABPropExists(lpProps[31]) then
           FStreet.Text := lpProps[31].Value.lpszA;
         if abfIfWABPropExists(lpProps[44]) then
           FComment.Text := lpProps[44].Value.lpszA;
       finally
         Reading := False;
       end;

       TempItem := TabfWABItem(FItems.FindItemID(FItemID));
       if Assigned(TempItem) then
         TempItem.Assign(TempGroup);
     end;
   end;

  //-------------------------------------

begin
  FContacts.BeginUpdate;
  FGroups.BeginUpdate;
  FItems.BeginUpdate;
  try
    FItems.Clear;
    FContacts.Clear;
    FGroups.Clear;

    Res := GetFolderContentsTable(Table);

    if (Res <> S_OK) or (Table = nil) then Exit;

    Table.SeekRow(BOOKMARK_BEGINNING, 0, nil);
    repeat
      Table.QueryRows(1, 0, TableRow);
      if TableRow.cRows > 0 then
        with TableRow.aRow[0] do
        begin
          if _CanAdd(abfIfWABPropExists(lpProps[0])) then
          begin
            if lpProps[2].Value.ul = MAPI_MAILUSER then
              _AddContact
            else if lpProps[2].Value.ul = MAPI_DISTLIST then
              _AddGroup;
          end;
          abfFreeSRowSet(FWabObject, TableRow);
        end
      else
        Break;
    until False;
  finally
    FItems.EndUpdate;
    FGroups.EndUpdate;
    FContacts.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.ApplyUpdates;
var
  i, TempContact, TempGroup: Integer;
begin
  if not Active then Exit;

  BeginUpdate;
  try
    TempContact := 0;
    TempGroup := 0;

    for i := 0 to Items.Count - 1 do
    begin
      if Items[i].ItemType = MAPI_MAILUSER then
      begin
        Contacts[TempContact].ApplyUpdates;
        Items[i].Assign(Contacts[TempContact]);
        Inc(TempContact);
      end
      else if Items[i].ItemType = MAPI_DISTLIST then
      begin
        Groups[TempGroup].ApplyUpdates;
        Items[i].Assign(Groups[TempGroup]);
        Inc(TempGroup);
      end;
    end;

    for i := 0 to Groups.Count - 1 do
      if VarIsArray(Groups[i].FTempVarArray) then
      begin
        Groups[i].AddItems(Groups[i].FTempVarArray);
        Groups[i].FTempVarArray := NULL;
      end;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.CancelUpdates;
begin
  BeginUpdate;
  try
    Refresh;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.Open;
var
  WP: TWabParam;
begin
  Close;
  if not FActive then
  begin
    FWABSharedMode := OutlookSharedMode and (FWABMode = wmCurrentWAB);

    FillChar(WP, Sizeof(WP), 0);
    WP.cbSize := Sizeof(WP);
    WP.hwnd := Application.Handle;

    if (FWABMode = wmWABFile) then
      WP.szFileName := PAnsiChar(AnsiString(FFileName))
    else if FEnableProfiles and (not FWABSharedMode) then
      WP.ulFlags := WAB_ENABLE_PROFILES;

    if FLibHandle = 0 then
    begin
      FLibHandle := abfLoadWabDll;
      if FLibHandle <> 0 then
        @FWabOpen := GetProcAddress(FLibHandle, 'WABOpen');
    end;

    try
      if not Assigned(@FWabOpen) then Abort;
      FWabOpen(FAddrBook, FWABObject, @WP, 0);
    except
      FAddrBook := nil;
      FWABObject := nil;
      CurrentFolder := nil;
      FRootContainer := nil;      
      raise EabfWABException.Create(SabfWAB_ErrorLoadingWABAPI);
    end;

    if Assigned(FAddrBook) and Assigned(FWABObject) then
    begin
      FActive := True;
      Refresh;
      Hook_AB_Notification;
      DoOpenAddressBook;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.Close;
begin
  try
    Unhook_AB_Notification;
    Sleep(0);
    if Assigned(FItems) then FItems.Clear;
    if Assigned(FContacts) then FContacts.Clear;
    if Assigned(FGroups) then FGroups.Clear;
    if Assigned(FFolders) then FFolders.Clear;
    FCurrentFolder := nil;
    FCurrentContainer := nil;
    FRootContainer := nil;
    FAddrBook := nil;
    FWABObject := nil;
    Sleep(0);
    @FWABOpen := nil;
    abfUnloadWabDll(FLibHandle);
    FLibHandle := 0;
    if FActive then DoCloseAddressBook;
  finally
    FActive := False;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.ContactDetails(var ContactIndex: Integer): Boolean;
var
  TempHandle: ULONG;
  PSB: PSBinary;
begin
  Result := False;

  TempHandle := ParentHandle;

  CheckActive;
  CheckReadOnly;

  BeginUpdate;
  try
    if ContactIndex < 0 then ContactIndex := 0
    else if ContactIndex >= FContacts.Count then
      ContactIndex := FContacts.Count - 1;
    abfNewSBinary2(FWABObject, PSB, FContacts[ContactIndex].EntryID.cb);
    try
      CopyMemory(PSB.lpb, FContacts[ContactIndex].EntryID.lpb, PSB.cb);
      if FAddrBook.Details(TempHandle, nil, nil,
        PSB.cb, PEntryID(PSB.lpb), nil, nil, nil, 0) = S_OK then
      begin
        DoRefreshFolderContent;
        ContactIndex := GetContactIndex(PSB);
        Result := (ContactIndex > -1) and (ContactIndex < FContacts.Count);
      end;
    finally
      abfFreeSBinary2(FWABObject, PSB);
    end;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteAllContacts;
begin
  DeleteContacts(NULL);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteAllGroups;
begin
  DeleteGroups(NULL);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteAllItems;
begin
  DeleteItems(NULL);
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteContacts(ContactsIndexes: Variant);
var
  AllowDelete: Boolean;
begin
  CheckActive;
  CheckReadOnly;

  AllowDelete := True;
  DoDeletingContact(AllowDelete);
  if not AllowDelete then Exit;
  
  BeginUpdate;
  try
    DeleteWABItems(FContacts, ContactsIndexes);
    DoRefreshFolderContent;
    DoDeleteContact;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteGroups(GroupsIndexes: Variant);
var
  AllowDelete: Boolean;
begin
  CheckActive;
  CheckReadOnly;

  AllowDelete := True;
  DoDeletingGroup(AllowDelete);
  if not AllowDelete then Exit;
  
  BeginUpdate;
  try
    DeleteWABItems(FGroups, GroupsIndexes);
    DoRefreshFolderContent;
    DoDeleteGroup;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.DeleteItems(ItemsIndexes: Variant);
var
  AllowDelete: Boolean;
begin
  CheckActive;
  CheckReadOnly;

  AllowDelete := True;
  DoDeletingItem(AllowDelete);
  if not AllowDelete then Exit;

  BeginUpdate;
  try
    DeleteWABItems(FItems, ItemsIndexes);
    DoRefreshFolderContent;
    DoDeleteItem;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.Find;
begin
  CheckActive;

  if Assigned(FWABObject) and Assigned(FAddrBook) then
    FWABObject.Find(FAddrBook, ParentHandle);
end;

//------------------------------------------------------------------------------

function TabfWAB.NewContact: Integer;
var
  PSB: PSBinary;
begin
  Result := -1;

  CheckActive;
  CheckReadOnly;

  abfNewSBinary2(FWABObject, PSB, 0);
  BeginUpdate;
  try
    if CreateEntry(MAPI_MAILUSER, PSB) = S_OK then
    begin
      DoRefreshFolderContent;
      Result := GetContactIndex(PSB);
      if Result >= 0 then DoNewContact;
    end;
  finally
    abfFreeSBinary2(FWABObject, PSB);
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.NewGroup: Integer;
var
  PSB: PSBinary;
begin
  Result := -1;

  CheckActive;
  CheckReadOnly;

  abfNewSBinary2(FWABObject, PSB, 0);
  BeginUpdate;
  try
    if CreateEntry(MAPI_DISTLIST, PSB) = S_OK then
    begin
      DoRefreshFolderContent;
      Result := GetGroupIndex(PSB);
      if Result >= 0 then DoNewGroup;
    end;
  finally
    abfFreeSBinary2(FWABObject, PSB);
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.NewItem(WABItemClass: TCollectionItemClass): Integer;
var
  i: Integer;
begin
  Result := -1;

  CheckActive;
  CheckReadOnly;

  BeginUpdate;
  try
    if WABItemClass = TabfWABContactItem then
    begin
      i := NewContact;
      if i >= 0 then Result := GetItemIndex(FContacts[i].EntryID);
    end
    else if WABItemClass = TabfWABGroupItem then
    begin
      i := NewGroup;
      if i >= 0 then Result := GetItemIndex(FGroups[i].EntryID);
    end;
    if Result >= 0 then DoNewItem;
  finally
    EndUpdate;
    DoChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.SetMeContact(ContactIndex: Integer;
  SelectDialog: Boolean): Boolean;
var
  ulFlags: Cardinal;
  NewMeContact: Boolean;
begin
  Result := False;
  NewMeContact := False;

  CheckActive;
  CheckReadOnly;

  if (ContactIndex < 0) or (ContactIndex >= FContacts.Count) then
    ContactIndex := GetMEContactIndex(False);

  if not SelectDialog and (ContactIndex = -1) then Exit;

  if ContactIndex = -1 then
  begin
    ContactIndex := GetMEContactIndex(True);
    if ContactIndex = -1 then Exit;
    NewMeContact := True;
  end;

  if not NewMeContact then
  begin
    if SelectDialog then
      ulFlags := MAPI_DIALOG
    else
      ulFlags := 0;
    Result := FWABObject.SetMe(FAddrBook, ulFlags, FContacts[ContactIndex].EntryID^,
      ULONG(ParentHandle)) = S_OK;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.SetMeItem(ItemIndex: Integer; SelectDialog: Boolean): Boolean;
var
  ulFlags: Cardinal;
  NewMeItem: Boolean;
begin
  Result := False;
  NewMeItem := False;

  CheckActive;
  CheckReadOnly;

  if (ItemIndex < 0) or (ItemIndex >= FItems.Count) then
    ItemIndex := GetMEItemIndex(False);

  if not SelectDialog and (ItemIndex = -1) then Exit;

  if ItemIndex = -1 then
  begin
    ItemIndex := GetMEItemIndex(True);
    if ItemIndex = -1 then Exit;
    NewMeItem := True;
  end;

  if not NewMeItem then
  begin
    if SelectDialog then
      ulFlags := MAPI_DIALOG
    else
      ulFlags := 0;
    if FItems[ItemIndex].ItemType <> MAPI_MAILUSER then Exit;
    Result := FWABObject.SetMe(FAddrBook, ulFlags, FItems[ItemIndex].EntryID^,
      ULONG(ParentHandle)) = S_OK;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWAB.Refresh;
begin
  if FLoadingProgress then Exit;
  if (not Assigned(FAddrBook)) or (not Assigned(FWabObject)) then Exit;

  FLoadingProgress := True;
  try
    DoRefreshFolders;
    DoRefreshFolderContent;
  finally
    FLoadingProgress := False;
  end;
end;

//------------------------------------------------------------------------------

function TabfWAB.VCardCreate(ContactIndex: Integer; VCardFileName: TFileName): Boolean;
var
  ObjType: ULONG;
begin
  Result := False;
  CheckActive;  

  if (ContactIndex < 0) or (ContactIndex >= FContacts.Count) then Exit;

  if Trim(VCardFileName) = '' then
    VCardFileName := FContacts[ContactIndex].DisplayName + '.vcf';

  with FContacts[ContactIndex] do
    if Assigned(EntryID.lpb) then
    begin
      FAddrBook.OpenEntry(EntryID.cb, EntryID.lpb, nil, 0,
        ObjType, IUnknown(FMailUser));
      Result := FWABObject.VCardCreate(FAddrBook, 0,
        Pointer(VCardFileName), FMailUser) = S_OK;
    end;
end;

//------------------------------------------------------------------------------

function TabfWAB.VCardDisplay(const VCardFileName: TFileName): Boolean;
begin
  CheckActive;
  CheckReadOnly;

  Result := FWABObject.VCardDisplay(FAddrBook, ParentHandle,
    Pointer(VCardFileName)) = S_OK;
end;

//------------------------------------------------------------------------------

function TabfWAB.VCardRetrieve(const VCardFileName: TFileName): Boolean;
var
  NewUser, MailUser: IMailUser;
  ParentID, NewID: PSBinary;
  Container: IABContainer;
  PropsCount, ObjType: ULONG;
  spv: PSPropsArray;
begin
  CheckActive;
  CheckReadOnly;

  Result := FWABObject.VCardRetrieve(FAddrBook, WAB_VCARD_FILE,
    Pointer(VCardFileName), NewUser) = S_OK;

  if not Result then Exit;

  ParentID := nil;
  NewID := nil;
  GetWABTemplateID(MAPI_MAILUSER, ParentID, NewID);
  try
    spv := nil;
    NewUser.GetProps(nil, 0, @PropsCount, spv);
    try
      NewUser := nil;
      FAddrBook.OpenEntry(ParentID.cb, ParentID.lpb,
        nil, 0, ObjType, IUnknown(Container));
      Container.CreateEntry(NewID.cb, NewID.lpb,
        0, IMAPIProp(MailUser));
      MailUser.SetProps(PropsCount, spv, nil);
    finally
      FWabObject.FreeBuffer(spv);
    end;
  finally
    abfFreeSBinary2(FWABObject, ParentID);
    abfFreeSBinary2(FWABObject, NewID);
  end;

  Result := MailUser.SaveChanges(FORCE_SAVE) = S_OK;
end;

{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

  _WabDllList := TList.Create;

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfWAB]);

{******************************************************************************}
finalization
{******************************************************************************}

  abfUnloadAllWabDll;
  FreeAndNil(_WabDllList);

end{unit abfWAB}.
