unit KnownFolders;

interface

type
  PKNOWNFOLDERID   = ^KNOWNFOLDERID;
  KNOWNFOLDERID    = TGUID;
  REFKNOWNFOLDERID = ^KNOWNFOLDERID;

const
// Display Name       : Get Programs
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: Add New Programs (found in the Add or Remove Programs item
//                      in the Control Panel)
// Legacy Default Path: Not applicable — virtual folder
  FOLDERID_AddNewPrograms        : KNOWNFOLDERID = '{DE61D971-5EBC-4F02-A3A9-6C82895E5C04}';

// Display Name       : Administrative Tools
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Start Menu\Programs\Administrative Tools
// CSIDL Equivalent   : CSIDL_ADMINTOOLS
// Legacy Display Name: Administrative Tools
// Legacy Default Path: %USERPROFILE%\Start Menu\Programs\Administrative Tools
  FOLDERID_AdminTools            : KNOWNFOLDERID = '{724EF170-A42D-4FEF-9F26-B60E846FBA4F}';

// Display Name       : Installed Updates
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: None, new in Windows Vista. In earlier versions of Microsoft Windows,
//                      the information on this page was included in Add or Remove Programs
//                      if the Show updates box was checked.
// Legacy Default Path: Not applicable
  FOLDERID_AppUpdates            : KNOWNFOLDERID = '{A305CE99-F527-492B-8B1A-7E76FA98D6E4}';

// Display Name       : Temporary Burn Folder
// Folder Type        : PERUSER
// Default Path       : %LOCALAPPDATA%\Microsoft\Windows\Burn\Burn
// CSIDL Equivalent   : CSIDL_CDBURN_AREA
// Legacy Display Name: CD Burning
// Legacy Default Path: %USERPROFILE%\Local Settings\Application Data\Microsoft\CD Burning
  FOLDERID_CDBurning             : KNOWNFOLDERID = '{9E52AB10-F80D-49DF-ACB8-4330F5687855}';

// Display Name       : Programs and Features
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: Add or Remove Programs
// Legacy Default Path: Not applicable — virtual folder
  FOLDERID_ChangeRemovePrograms  : KNOWNFOLDERID = '{DF7266AC-9274-4867-8D55-3BD661DE872D}';

// Display Name       : Administrative Tools
// Folder Type        : COMMON
// Default Path       : %ALLUSERSPROFILE%\Microsoft\Windows\Start Menu\Programs\Administrative Tools
// CSIDL Equivalent   : CSIDL_COMMON_ADMINTOOLS
// Legacy Display Name: Administrative Tools
// Legacy Default Path: %ALLUSERSPROFILE%\Start Menu\Programs\Administrative Tools
  FOLDERID_CommonAdminTools      : KNOWNFOLDERID = '{D0384E7D-BAC3-4797-8F14-CBA229B392B5}';

// Display Name       : OEM Links
// Folder Type        : COMMON
// Default Path       : %ALLUSERSPROFILE%\OEM Links
// CSIDL Equivalent   : CSIDL_COMMON_OEM_LINKS
// Legacy Display Name: OEM Links
// Legacy Default Path: %ALLUSERSPROFILE%\OEM Links
  FOLDERID_CommonOEMLinks        : KNOWNFOLDERID = '{C1BAE2D0-10DF-4334-BEDD-7AA20B227A9D}';

// Display Name       : Programs
// Folder Type        : COMMON
// Default Path       : %ALLUSERSPROFILE%\Microsoft\Windows\Start Menu\Programs
// CSIDL Equivalent   : CSIDL_COMMON_PROGRAMS
// Legacy Display Name: Programs
// Legacy Default Path: %ALLUSERSPROFILE%\Start Menu\Programs
  FOLDERID_CommonPrograms        : KNOWNFOLDERID = '{0139D44E-6AFE-49F2-8690-3DAFCAE6FFB8}';

// Display Name       : Start Menu
// Folder Type        : COMMON
// Default Path       : %ALLUSERSPROFILE%\Microsoft\Windows\Start Menu
// CSIDL Equivalent   : CSIDL_COMMON_STARTMENU
// Legacy Display Name: Start Menu
// Legacy Default Path: %ALLUSERSPROFILE%\Start Menu
  FOLDERID_CommonStartMenu       : KNOWNFOLDERID = '{A4115719-D62E-491D-AA7C-E74B8BE3B067}';

// Display Name       : Startup
// Folder Type        : COMMON
// Default Path       : %ALLUSERSPROFILE%\Microsoft\Windows\Start Menu\Programs\StartUp
// CSIDL Equivalent   : CSIDL_COMMON_STARTUP, CSIDL_COMMON_ALTSTARTUP
// Legacy Display Name: Startup
// Legacy Default Path: %ALLUSERSPROFILE%\Start Menu\Programs\StartUp
  FOLDERID_CommonStartup         : KNOWNFOLDERID = '{82A5EA35-D9CD-47C5-9629-E15D2F714E6E}';

// Display Name       : Templates
// Folder Type        : COMMON
// Default Path       : %ALLUSERSPROFILE%\Templates
// CSIDL Equivalent   : CSIDL_COMMON_TEMPLATES
// Legacy Display Name: Templates
// Legacy Default Path: %ALLUSERSPROFILE%\Templates
  FOLDERID_CommonTemplates       : KNOWNFOLDERID = '{B94237E7-57AC-4347-9151-B08C6C32D1F7}';

// Display Name       : Computer
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : CSIDL_DRIVES
// Legacy Display Name: My Computer
// Legacy Default Path: Not applicable — virtual folder
  FOLDERID_ComputerFolder        : KNOWNFOLDERID = '{0AC0837C-BBF8-452A-850D-79D08E667CA7}';

// Display Name       : Conflicts
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable. This KNOWNFOLDERID refers to the Windows Vista
//                      Synchronization Manager. It is not the folder referenced by
//                      the older ISyncMgrConflictFolder.
// Legacy Default Path: Not applicable
  FOLDERID_ConflictFolder        : KNOWNFOLDERID = '{4BFEFB45-347D-4006-A5BE-AC0CB0567192}';

// Display Name       : Network Connections
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : CSIDL_CONNECTIONS
// Legacy Display Name: Network Connections
// Legacy Default Path: Not applicable — virtual folder
  FOLDERID_ConnectionsFolder     : KNOWNFOLDERID = '{6F0CD92B-2E97-45D1-88FF-B0D186B8DEDD}';

// Display Name       : Contacts
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Contacts
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_Contacts              : KNOWNFOLDERID = '{56784854-C6CB-462b-8169-88E350ACB882}';

// Display Name       : Control Panel
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : CSIDL_CONTROLS
// Legacy Display Name: Control Panel
// Legacy Default Path: Not applicable - virtual folder
  FOLDERID_ControlPanelFolder    : KNOWNFOLDERID = '{82A74AEB-AEB4-465C-A014-D097EE346D63}';

// Display Name       : Cookies
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Cookies
// CSIDL Equivalent   : CSIDL_COOKIES
// Legacy Display Name: Cookies
// Legacy Default Path: %USERPROFILE%\Cookies
  FOLDERID_Cookies               : KNOWNFOLDERID = '{2B0F765D-C0E9-4171-908E-08A611B84FF6}';

// Display Name       : Desktop
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Desktop
// CSIDL Equivalent   : CSIDL_DESKTOP, CSIDL_DESKTOPDIRECTORY
// Legacy Display Name: Desktop
// Legacy Default Path: %USERPROFILE%\Desktop
  FOLDERID_Desktop               : KNOWNFOLDERID = '{B4BFCC3A-DB2C-424C-B029-7FE99A87C641}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   : None
// Legacy Display Name: None
// Legacy Default Path: None
  FOLDERID_DeviceMetadataStore   : KNOWNFOLDERID = '{5CE4A5E9-E4EB-479D-B89F-130C02886155}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_Documents             : KNOWNFOLDERID = '{FDD39AD0-238F-46AF-ADB4-6C85480369C7}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_DocumentsLibrary      : KNOWNFOLDERID = '{7B0DB17D-9CD2-4A93-9733-46CC89022E7C}';

// Display Name       : Downloads
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Downloads
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_Downloads             : KNOWNFOLDERID = '{374DE290-123F-4565-9164-39C4925E467B}';

// Display Name       : Favorites
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Favorites
// CSIDL Equivalent   : CSIDL_FAVORITES, CSIDL_COMMON_FAVORITES
// Legacy Display Name: Favorites
// Legacy Default Path: %USERPROFILE%\Favorites
  FOLDERID_Favorites             : KNOWNFOLDERID = '{1777F761-68AD-4D8A-87BD-30B759FA33DD}';

// Display Name       : Fonts
// Folder Type        : FIXED
// Default Path       : %windir%\Fonts
// CSIDL Equivalent   : CSIDL_FONTS
// Legacy Display Name: Fonts
// Legacy Default Path: %windir%\Fonts
  FOLDERID_Fonts                 : KNOWNFOLDERID = '{FD228CB7-AE11-4AE3-864C-16F3910AB8FE}';

// Display Name       : Games
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_Games                 : KNOWNFOLDERID = '{CAC52C1A-B53D-4edc-92D7-6B2E8AC19434}';

// Display Name       : GameExplorer
// Folder Type        : PERUSER
// Default Path       : %LOCALAPPDATA%\Microsoft\Windows\GameExplorer
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_GameTasks             : KNOWNFOLDERID = '{054FAE61-4DD8-4787-80B6-090220C4B700}';

// Display Name       : History
// Folder Type        : PERUSER
// Default Path       : %LOCALAPPDATA%\Microsoft\Windows\History
// CSIDL Equivalent   : CSIDL_HISTORY
// Legacy Display Name: History
// Legacy Default Path: %USERPROFILE%\Local Settings\History
  FOLDERID_History               : KNOWNFOLDERID = '{D9DC8A3B-B784-432E-A781-5A1130A75963}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_ImplicitAppShortcuts  : KNOWNFOLDERID = '{BCB5256F-79F6-4CEE-B725-DC34E402FD46}';

// Display Name       : Temporary Internet Files
// Folder Type        : PERUSER
// Default Path       : %LOCALAPPDATA%\Microsoft\Windows\Temporary Internet Files
// CSIDL Equivalent   : CSIDL_INTERNET_CACHE
// Legacy Display Name: Temporary Internet Files
// Legacy Default Path: %USERPROFILE%\Local Settings\Temporary Internet Files
  FOLDERID_InternetCache         : KNOWNFOLDERID = '{352481E8-33BE-4251-BA85-6007CAEDCF9D}';

// Display Name       : Internet Explorer
// Folder Type        : VIRTUAL
// Default Path       : Not applicable—virtual folder
// CSIDL Equivalent   : CSIDL_INTERNET
// Legacy Display Name: Internet Explorer
// Legacy Default Path: Not applicable — virtual folder
  FOLDERID_InternetFolder        : KNOWNFOLDERID = '{4D9F7874-4E0C-4904-967B-40B0D20C3E4B}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   : None
// Legacy Display Name: None
// Legacy Default Path: None
  FOLDERID_Libraries             : KNOWNFOLDERID = '{1B3EA5DC-B587-4786-B4EF-BD1DC332AEAE}';

// Display Name       : Links
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Links
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_Links                 : KNOWNFOLDERID = '{BFB9D5E0-C6A9-404C-B2B2-AE6DB6AF4968}';

// Display Name       : Local
// Folder Type        : PERUSER
// Default Path       : %LOCALAPPDATA% (%USERPROFILE%\AppData\Local)
// CSIDL Equivalent   : CSIDL_LOCAL_APPDATA
// Legacy Display Name: Application Data
// Legacy Default Path: %USERPROFILE%\Local Settings\Application Data
  FOLDERID_LocalAppData          : KNOWNFOLDERID = '{F1B32785-6FBA-4FCF-9D55-7B8E7F157091}';

// Display Name       : LocalLow
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\AppData\LocalLow
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_LocalAppDataLow       : KNOWNFOLDERID = '{A520A1A4-1780-4FF6-BD18-167343C5AF16}';

// Display Name       : None
// Folder Type        : FIXED
// Default Path       : %windir%\resources\0409 (code page)
// CSIDL Equivalent   : CSIDL_RESOURCES_LOCALIZED
// Legacy Display Name: None
// Legacy Default Path: %windir%\resources\0409 (code page)
  FOLDERID_LocalizedResourcesDir : KNOWNFOLDERID = '{2A00375E-224C-49DE-B8D1-440DF7EF3DDC}';

// Display Name       : Music
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Music
// CSIDL Equivalent   : CSIDL_MYMUSIC
// Legacy Display Name: My Music
// Legacy Default Path: %USERPROFILE%\My Documents\My Music
  FOLDERID_Music                 : KNOWNFOLDERID = '{4BD8D571-6D19-48D3-BE97-422220080E43}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_MusicLibrary          : KNOWNFOLDERID = '{2112AB0A-C86A-4ffe-A368-0DE96E47012E}';

// Display Name       : Network Shortcuts
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Network Shortcuts
// CSIDL Equivalent   : CSIDL_NETHOOD
// Legacy Display Name: NetHood
// Legacy Default Path: %USERPROFILE%\NetHood
  FOLDERID_NetHood               : KNOWNFOLDERID = '{C5ABBF53-E17F-4121-8900-86626FC2C973}';

// Display Name       : Network
// Folder Type        : VIRTUAL
// Default Path       : Not applicable—virtual folder
// CSIDL Equivalent   : CSIDL_NETWORK, CSIDL_COMPUTERSNEARME
// Legacy Display Name: My Network Places
// Legacy Default Path: Not applicable—virtual folder
  FOLDERID_NetworkFolder         : KNOWNFOLDERID = '{D20BEEC4-5CA8-4905-AE3B-BF251EA09B53}';

// Display Name       : Original Images
// Folder Type        : PERUSER
// Default Path       : %LOCALAPPDATA%\Microsoft\Windows Photo Gallery\Original Images
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_OriginalImages        : KNOWNFOLDERID = '{2C36C0AA-5812-4b87-BFD0-4CD0DFB19B39}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_OtherUsers            : KNOWNFOLDERID = '{52528A6B-B9E3-4add-B60D-588C2DBA842D}';

// Display Name       : Slide Shows
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Pictures\Slide Shows
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_PhotoAlbums           : KNOWNFOLDERID = '{69D2CF90-FC33-4FB7-9A0C-EBB0F0FCB43C}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_PicturesLibrary       : KNOWNFOLDERID = '{A990AE9F-A03B-4e80-94BC-9912D7504104}';

// Display Name       : Pictures
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Pictures
// CSIDL Equivalent   : CSIDL_MYPICTURES
// Legacy Display Name: My Pictures
// Legacy Default Path: %USERPROFILE%\My Documents\My Pictures
  FOLDERID_Pictures              : KNOWNFOLDERID = '{33E28130-4E1E-4676-835A-98395C3BC3BB}';

// Display Name       : Playlists
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Music\Playlists
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_Playlists             : KNOWNFOLDERID = '{DE92C1C7-837F-4F69-A3BB-86E631204A23}';

// Display Name       : Printers
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : CSIDL_PRINTERS
// Legacy Display Name: Printers and Faxes
// Legacy Default Path: Not applicable — virtual folder
  FOLDERID_PrintersFolder        : KNOWNFOLDERID = '{76FC4E2D-D6AD-4519-A663-37BD56068185}';

// Display Name       : Printer Shortcuts
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Printer Shortcuts
// CSIDL Equivalent   : CSIDL_PRINTHOOD
// Legacy Display Name: PrintHood
// Legacy Default Path: %USERPROFILE%\PrintHood
  FOLDERID_PrintHood             : KNOWNFOLDERID = '{9274BD8D-CFD1-41C3-B35E-B13F55A758F4}';

// Display Name       : The user's username (%USERNAME%)
// Folder Type        : FIXED
// Default Path       : %USERPROFILE% (%SystemDrive%\Users\%USERNAME%)
// CSIDL Equivalent   : CSIDL_PROFILE
// Legacy Display Name: The user's username (%USERNAME%)
// Legacy Default Path: %USERPROFILE% (%SystemDrive%\Documents and Settings\%USERNAME%)
  FOLDERID_Profile               : KNOWNFOLDERID = '{5E6C858F-0E22-4760-9AFE-EA3317B67173}';

// Display Name       : ProgramData
// Folder Type        : FIXED
// Default Path       : %ALLUSERSPROFILE% (%ProgramData%, %SystemDrive%\ProgramData)
// CSIDL Equivalent   : CSIDL_COMMON_APPDATA
// Legacy Display Name: Application Data
// Legacy Default Path: %ALLUSERSPROFILE%\Application Data
  FOLDERID_ProgramData           : KNOWNFOLDERID = '{62AB5D82-FDC1-4DC3-A9DD-070D1D495D97}';

// Display Name       : Program Files
// Folder Type        : FIXED
// Default Path       : %ProgramFiles% (%SystemDrive%\Program Files)
// CSIDL Equivalent   : CSIDL_PROGRAM_FILES
// Legacy Display Name: Program Files
// Legacy Default Path: %ProgramFiles% (%SystemDrive%\Program Files)
  FOLDERID_ProgramFiles          : KNOWNFOLDERID = '{905E63B6-C1BF-494E-B29C-65B732D3D21A}';

// Display Name       : Program Files
// Folder Type        : FIXED
// Default Path       : %ProgramFiles% (%SystemDrive%\Program Files)
// CSIDL Equivalent   : None
// Legacy Display Name: Program Files
// Legacy Default Path: %ProgramFiles% (%SystemDrive%\Program Files)
  FOLDERID_ProgramFilesX64       : KNOWNFOLDERID = '{6D809377-6AF0-444b-8957-A3773F02200E}';

// Display Name       : Program Files
// Folder Type        : FIXED
// Default Path       : %ProgramFiles% (%SystemDrive%\Program Files)
// CSIDL Equivalent   : CSIDL_PROGRAM_FILESX86
// Legacy Display Name: Program Files
// Legacy Default Path: %ProgramFiles% (%SystemDrive%\Program Files)
  FOLDERID_ProgramFilesX86       : KNOWNFOLDERID = '{7C5A40EF-A0FB-4BFC-874A-C0F2E0B9FA8E}';

// Display Name       : Common Files
// Folder Type        : FIXED
// Default Path       : %ProgramFiles%\Common Files
// CSIDL Equivalent   : CSIDL_PROGRAM_FILES_COMMON
// Legacy Display Name: Common Files
// Legacy Default Path: %ProgramFiles%\Common Files
  FOLDERID_ProgramFilesCommon    : KNOWNFOLDERID = '{F7F1ED05-9F6D-47A2-AAAE-29D317C6F066}';

// Display Name       : Common Files
// Folder Type        : FIXED
// Default Path       : %ProgramFiles%\Common Files
// CSIDL Equivalent   : None
// Legacy Display Name: Common Files
// Legacy Default Path: %ProgramFiles%\Common Files
  FOLDERID_ProgramFilesCommonX64 : KNOWNFOLDERID = '{6365D5A7-0F0D-45e5-87F6-0DA56B6A4F7D}';

// Display Name       : Common Files
// Folder Type        : FIXED
// Default Path       : %ProgramFiles%\Common Files
// CSIDL Equivalent   : CSIDL_PROGRAM_FILES_COMMONX86
// Legacy Display Name: Common Files
// Legacy Default Path: %ProgramFiles%\Common Files
  FOLDERID_ProgramFilesCommonX86 : KNOWNFOLDERID = '{DE974D24-D9C6-4D3E-BF91-F4455120B917}';

// Display Name       : Programs
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Start Menu\Programs
// CSIDL Equivalent   : CSIDL_PROGRAMS
// Legacy Display Name: Programs
// Legacy Default Path: %USERPROFILE%\Start Menu\Programs
  FOLDERID_Programs              : KNOWNFOLDERID = '{A77F5D77-2E2B-44C3-A6A2-ABA601054A51}';

// Display Name       : Public
// Folder Type        : FIXED
// Default Path       : %PUBLIC% (%SystemDrive%\Users\Public)
// CSIDL Equivalent   : None, new for Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_Public                : KNOWNFOLDERID = '{DFDF76A2-C82A-4D63-906A-5644AC457385}';

// Display Name       : Public Desktop
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Desktop
// CSIDL Equivalent   : CSIDL_COMMON_DESKTOPDIRECTORY
// Legacy Display Name: Desktop
// Legacy Default Path: %ALLUSERSPROFILE%\Desktop
  FOLDERID_PublicDesktop         : KNOWNFOLDERID = '{C4AA340D-F20F-4863-AFEF-F87EF2E6BA25}';

// Display Name       : Public Documents
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Documents
// CSIDL Equivalent   : CSIDL_COMMON_DOCUMENTS
// Legacy Display Name: Shared Documents
// Legacy Default Path: %ALLUSERSPROFILE%\Documents
  FOLDERID_PublicDocuments       : KNOWNFOLDERID = '{ED4824AF-DCE4-45A8-81E2-FC7965083634}';

// Display Name       : Public Downloads
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Downloads
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_PublicDownloads       : KNOWNFOLDERID = '{3D644C9B-1FB8-4f30-9B45-F670235F79C0}';

// Display Name GameExplorer
// Folder Type        : COMMON
// Default Path       : %ALLUSERSPROFILE%\Microsoft\Windows\GameExplorer
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_PublicGameTasks       : KNOWNFOLDERID = '{DEBF2536-E1A8-4c59-B6A2-414586476AEA}';

// Display Name Public Music
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Music
// CSIDL Equivalent   : CSIDL_COMMON_MUSIC
// Legacy Display Name: Shared Music
// Legacy Default Path: %ALLUSERSPROFILE%\Documents\My Music
  FOLDERID_PublicMusic           : KNOWNFOLDERID = '{3214FAB5-9757-4298-BB61-92A9DEAA44FF}';

// Display Name       : Public Pictures
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Pictures
// CSIDL Equivalent   : CSIDL_COMMON_PICTURES
// Legacy Display Name: Shared Pictures
// Legacy Default Path: %ALLUSERSPROFILE%\Documents\My Pictures
  FOLDERID_PublicPictures        : KNOWNFOLDERID = '{B6EBFB86-6907-413C-9AF7-4FC2ABF07CC5}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_PublicRingtones       : KNOWNFOLDERID = '{E555AB60-153B-4D17-9F04-A5FE99FC15EC}';

// Display Name       : Public Videos
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Videos
// CSIDL Equivalent   : CSIDL_COMMON_VIDEO
// Legacy Display Name: Shared Video
// Legacy Default Path: %ALLUSERSPROFILE%\Documents\My Videos
  FOLDERID_PublicVideos          : KNOWNFOLDERID = '{2400183A-6185-49FB-A2D8-4A392A602BA3}';

// Display Name       : Quick Launch
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Internet Explorer\Quick Launch
// CSIDL Equivalent   : None
// Legacy Display Name: Quick Launch
// Legacy Default Path: %APPDATA%\Microsoft\Internet Explorer\Quick Launch
  FOLDERID_QuickLaunch           : KNOWNFOLDERID = '{52A4F021-7B75-48A9-9F6B-4B87A210BC8F}';

// Display Name       : Recent Items
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Recent
// CSIDL Equivalent   : CSIDL_RECENT
// Legacy Display Name: My Recent Documents
// Legacy Default Path: %USERPROFILE%\Recent
  FOLDERID_Recent                : KNOWNFOLDERID = '{AE50C081-EBD2-438A-8655-8A092E34987A}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_RecordedTVLibrary     : KNOWNFOLDERID = '{1A6FDBA2-F42D-4358-A798-B74D745926C5}';

// Display Name       : Recycle Bin
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : CSIDL_BITBUCKET
// Legacy Display Name: Recycle Bin
// Legacy Default Path: Not applicable — virtual folder
  FOLDERID_RecycleBinFolder      : KNOWNFOLDERID = '{B7534046-3ECB-4C18-BE4E-64CD4CB7D6AC}';

// Display Name       : Resources
// Folder Type        : FIXED
// Default Path       : %windir%\Resources
// CSIDL Equivalent   : CSIDL_RESOURCES
// Legacy Display Name: Resources
// Legacy Default Path: %windir%\Resources
  FOLDERID_ResourceDir           : KNOWNFOLDERID = '{8AD10C31-2ADB-4296-A8F7-E4701232C972}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_Ringtones             : KNOWNFOLDERID = '{C870044B-F49E-4126-A9C3-B52A1FF411E8}';

// Display Name       : Roaming
// Folder Type        : PERUSER
// Default Path       : %APPDATA% (%USERPROFILE%\AppData\Roaming)
// CSIDL Equivalent   : CSIDL_APPDATA
// Legacy Display Name: Application Data
// Legacy Default Path: %APPDATA% (%USERPROFILE%\Application Data)
  FOLDERID_RoamingAppData        : KNOWNFOLDERID = '{3EB685DB-65F9-4CF6-A03A-E3EF65729F3D}';

// Display Name       : Sample Music
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Music\Sample Music
// CSIDL Equivalent   : None
// Legacy Display Name: Sample Music
// Legacy Default Path: %ALLUSERSPROFILE%\Documents\My Music\Sample Music
  FOLDERID_SampleMusic           : KNOWNFOLDERID = '{B250C668-F57D-4EE1-A63C-290EE7D1AA1F}';

// Display Name       : Sample Pictures
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Pictures\Sample Pictures
// CSIDL Equivalent   : None
// Legacy Display Name: Sample Pictures
// Legacy Default Path: %ALLUSERSPROFILE%\Documents\My Pictures\Sample Pictures
  FOLDERID_SamplePictures        : KNOWNFOLDERID = '{C4900540-2379-4C75-844B-64E6FAF8716B}';

// Display Name       : Sample Playlists
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Music\Sample Playlists
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SamplePlaylists       : KNOWNFOLDERID = '{15CA69B3-30EE-49C1-ACE1-6B5EC372AFB5}';

// Display Name       : Sample Videos
// Folder Type        : COMMON
// Default Path       : %PUBLIC%\Videos\Sample Videos
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SampleVideos          : KNOWNFOLDERID = '{859EAD94-2E85-48AD-A71A-0969CB56A6CD}';

// Display Name       : Saved Games
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Saved Games
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SavedGames            : KNOWNFOLDERID = '{4C5C32FF-BB9D-43b0-B5B4-2D72E54EAAA4}';

// Display Name       : Searches
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Searches
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SavedSearches         : KNOWNFOLDERID = '{7D1D3A04-DEBB-4115-95CF-2F29DA2920DA}';

// Display Name       : Offline Files
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SEARCH_CSC            : KNOWNFOLDERID = '{EE32E446-31CA-4ABA-814F-A5EBD2FD6D5E}';

// Display Name       : Microsoft Office Outlook
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SEARCH_MAPI           : KNOWNFOLDERID = '{98EC0E18-2098-4D44-8644-66979315A281}';

// Display Name       : Search Results
// Folder Type        : VIRTUAL
// Default Path       : Not applicable—virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SearchHome            : KNOWNFOLDERID = '{190337D1-B8CA-4121-A639-6D472D16972A}';

// Display Name       : SendTo
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\SendTo
// CSIDL Equivalent   : CSIDL_SENDTO
// Legacy Display Name: SendTo
// Legacy Default Path: %USERPROFILE%\SendTo
  FOLDERID_SendTo                : KNOWNFOLDERID = '{8983036C-27C0-404B-8F08-102D10DCFD74}';

// Display Name       : Gadgets
// Folder Type        : COMMON
// Default Path       : %ProgramFiles%\Windows Sidebar\Gadgets
// CSIDL Equivalent   : None, new for Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SidebarDefaultParts   : KNOWNFOLDERID = '{7B396E54-9EC5-4300-BE0A-2482EBAE1A26}';

// Display Name       : Gadgets
// Folder Type        : PERUSER
// Default Path       : %LOCALAPPDATA%\Microsoft\Windows Sidebar\Gadgets
// CSIDL Equivalent   : None, new for Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SidebarParts          : KNOWNFOLDERID = '{A75D362E-50FC-4fb7-AC2C-A8BEAA314493}';

// Display Name       : Start Menu
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Start Menu
// CSIDL Equivalent   : CSIDL_STARTMENU
// Legacy Display Name: Start Menu
// Legacy Default Path: %USERPROFILE%\Start Menu
  FOLDERID_StartMenu             : KNOWNFOLDERID = '{625B53C3-AB48-4EC1-BA1F-A1EF4146FC19}';

// Display Name       : Startup
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Start Menu\Programs\StartUp
// CSIDL Equivalent   : CSIDL_STARTUP, CSIDL_ALTSTARTUP
// Legacy Display Name: Startup
// Legacy Default Path: %USERPROFILE%\Start Menu\Programs\StartUp
  FOLDERID_Startup               : KNOWNFOLDERID = '{B97D20BB-F46A-4C97-BA10-5E3608430854}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_SuggestedLocations    : KNOWNFOLDERID = '{1426DD90-DEFB-4C3E-BDFC-9A5C077E562A}';

// Display Name       : Sync Center
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SyncManagerFolder     : KNOWNFOLDERID = '{43668BF8-C14E-49B2-97C9-747784D784B7}';

// Display Name       : Sync Results
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SyncResultsFolder     : KNOWNFOLDERID = '{289A9A43-BE44-4057-A41B-587A76D7E7F9}';

// Display Name       : Sync Setup
// Folder Type        : VIRTUAL
// Default Path       : Not applicable—virtual folder
// CSIDL Equivalent   : None, new in Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_SyncSetupFolder       : KNOWNFOLDERID = '{0F214138-B1D3-4A90-BBA9-27CBC0C5389A}';

// Display Name       : System32
// Folder Type        : FIXED
// Default Path       : %windir%\system32
// CSIDL Equivalent   : CSIDL_SYSTEM
// Legacy Display Name: system32
// Legacy Default Path: %windir%\system32
  FOLDERID_System                : KNOWNFOLDERID = '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}';

// Display Name       : System32
// Folder Type        : FIXED
// Default Path       : %windir%\system32
// CSIDL Equivalent   : CSIDL_SYSTEMX86
// Legacy Display Name: system32
// Legacy Default Path: %windir%\system32
  FOLDERID_SystemX86             : KNOWNFOLDERID = '{D65231B0-B2F1-4857-A4CE-A8E7C6EA7D27}';

// Display Name       : Templates
// Folder Type        : PERUSER
// Default Path       : %APPDATA%\Microsoft\Windows\Templates
// CSIDL Equivalent   : CSIDL_TEMPLATES
// Legacy Display Name: Templates
// Legacy Default Path: %USERPROFILE%\Templates
  FOLDERID_Templates             : KNOWNFOLDERID = '{A63293E8-664E-48DB-A079-DF759E0509F7}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_UserPinned            : KNOWNFOLDERID = '{9E3995AB-1F9C-4F13-B827-48B24B6C7174}';

// Display Name       : Users
// Folder Type        : FIXED
// Default Path       : %SystemDrive%\Users
// CSIDL Equivalent   : None, new for Windows Vista
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_UserProfiles          : KNOWNFOLDERID = '{0762D272-C50A-4BB0-A382-697DCD729B80}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_UserProgramFiles      : KNOWNFOLDERID = '{5CD7AEE2-2219-4A67-B85D-6C9CE15660CB}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_UserProgramFilesCommon: KNOWNFOLDERID = '{BCBD3057-CA5C-4622-B42D-BC56DB0AE516}';

// Display Name       : The user's full name (for instance, Jean Philippe Bagel)
//                      entered when the user account was created.
// Folder Type        : VIRTUAL
// Default Path       : Not applicable — virtual folder
// CSIDL Equivalent   : None
// Legacy Display Name: Not applicable
// Legacy Default Path: Not applicable
  FOLDERID_UsersFiles            : KNOWNFOLDERID = '{F3CE0F7C-4901-4ACC-8648-D5D44B04EF8F}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   : None
// Legacy Display Name: None
// Legacy Default Path: None
  FOLDERID_UsersLibraries        : KNOWNFOLDERID = '{A302545D-DEFF-464b-ABE8-61C8648D939B}';

// Display Name       : Videos
// Folder Type        : PERUSER
// Default Path       : %USERPROFILE%\Videos
// CSIDL Equivalent   : CSIDL_MYVIDEO
// Legacy Display Name: My Videos
// Legacy Default Path: %USERPROFILE%\My Documents\My Videos
  FOLDERID_Videos                : KNOWNFOLDERID = '{18989B1D-99B5-455B-841C-AB7C74E4DDFC}';

// Display Name       :
// Folder Type        :
// Default Path       :
// CSIDL Equivalent   :
// Legacy Display Name:
// Legacy Default Path:
  FOLDERID_VideosLibrary         : KNOWNFOLDERID = '{491E922F-5643-4af4-A7EB-4E7A138D8174}';

// Display Name       : Windows
// Folder Type        : FIXED
// Default Path       : %windir%
// CSIDL Equivalent   : CSIDL_WINDOWS
// Legacy Display Name: WINDOWS
// Legacy Default Path: %windir%
  FOLDERID_Windows               : KNOWNFOLDERID = '{F38BF404-1D43-42F2-9305-67DE0B28FC23}';

implementation

end.
