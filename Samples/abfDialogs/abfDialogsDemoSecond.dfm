�
 TFRMDIALOGSDEMOSECOND 0�<  TPF0TfrmDialogsDemoSecondfrmDialogsDemoSecondLeft� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionabfDialogs Demo 2ClientHeightuClientWidthx
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 	PopupMenupmMainPositionpoScreenCenterScaledShowHint	Visible	OnCreate
FormCreateOnResize
FormResizePixelsPerInch`
TextHeight 	TGroupBoxgrpRunLeftTopWidth1Height� Caption
TabfRunDlg
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder  TLabel	lbRunInfoLeft,TopWidth� HeightFAutoSizeCaption]Implements a standard Run dialog that you can see when press Run menu item in the Start menu.
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	WordWrap	  TImageimgRunTagLeft
TopWidthHeightHint	abfRunDlgAutoSize	Picture.Data
B  TBitmap6  BM6      v   (               �                �    �  ��� ��� ���                                             DDDDDDDDDDDDDDPP#3333333333334DD#3333333333334#3#3EUUUUUUUUUS4#3#3C##S#%####S4#3#3B22U25R222S4#3#3C##%S#S###S4#3#3B222U2U222S4#3#3C###%%S###S4#3#3B2255U2222S4#3#3C##%%U####S4#3#3B225UUU222S4#3#3C###UU%S##S4#3#3B2225U2222S4#3#3C####S####S4#3#3B2225U2222S4#3#3C###UUS###S4#3#3B2225U2222S4#3#3EUUUUUUUUUS4#3#3BP      RRS4#3#3DDDDDDDDDDC4#3#133333333334#3#3333333334#1#3333333334##13333333334##133333333334##3333333333334#1""""""""""""""#3OnClickComponentIconClick  TLabellbRunCaptionLeft
Top>Width+HeightCaptionCaptionFocusControledRunCaption
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TLabel	lbRunTextLeft
TophWidthHeightCaptionTextFocusControl
memRunText
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TButtonbtnRunTagLeft� Top� WidthKHeightCaptionExecute
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderOnClickExecuteClick  TPanel	pnRunIconLeft
Top� WidthjHeight*HintThe icon
BevelInnerbvRaised
BevelOuter	bvLowered
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TImage
imgRunIconLeftTopWidth&Height&Center	  TSpeedButtonbtnRunIconLoadLeft0Top
WidthHeightHintLoad icon from file
Glyph.Data
�   �   BM�       v   (               �                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������������     �� 33330��3333��33330��3333��     �������������   ��� ���� ����������������� ����������LayoutblGlyphRightOnClickbtnRunIconLoadClick  TSpeedButtonbtnRunIconClearLeftHTop
WidthHeightHint
Clear icon
Glyph.Data
�   �   BM�       v   (               �                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ������������������������ ������  ������� ������ ������ � �����  ������ ������  ����� � ���� ����  ��� �� ������������LayoutblGlyphRightOnClickbtnRunIconClearClick  TLabel	lbRunIconLeftTopWidthHeightCaptionNoneVisible   TEditedRunCaptionTagLeft
TopNWidthHeight
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder   TMemo
memRunTextTagLeft
TopxWidthHeight$
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder   	TGroupBoxgrpOWLeftBTopWidth0HeightNCaptionTabfOpenWithDlg
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TImageimgOWTagLeft
TopWidthHeightHintabfOpenWithDlgAutoSize	Picture.Data
B  TBitmap6  BM6      v   (               �                �    �   ��   �   � ��� ��� ���                                 wwwwwwwwwwwwww��VffffffffffffgwwVffffffffffffgVfVfx����������gVfVfvVVVTDVVVV�gVfVfue���4���e�gVfVfvWwws4wwxV�gVfVfugRbbbbbxe�gVfVfvWV&#4&&xV�gVfVfugRbc4bbxe�gVfVfvWV&#3F&xV�gVfVfugRbb34bxe�gVfVfvWV$&#3FxV�gVfVfugR3Bb3Bxe�gVfVfvWU34U3ExV�gVfVfug&#334wue�gVfVfvVrb33FVVV�gVfVfuegwweeeee�gVfVfx����������gVfVfu�      ���gVfVfwwwwwwwwwwvgVfVaffffffffffgVfVfffffffffgVaVfffffffffgVVafffffffffgVVaffffffffffgVVffffffffffffgVaUUUUUUUUUUUUUUVfOnClickComponentIconClick  TLabellbOWInfoLeft,TopWidth� Height5AutoSizeCaption+Implements standard shell Open With dialog.
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	WordWrap	  TLabellbOWFileNameLeft
Top0Width6HeightCaptionFileNameFocusControledOWFileName
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TSpeedButtonbtnOWFileNameTagLeft� Top,WidthHeightHintChoose file
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
Glyph.Data
�   �   BM�       v   (               �                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������������     �� 33330��3333��33330��3333��     �������������   ��� ���� ����������������� ����������
ParentFontOnClickbtnFileNameClick  TEditedOWFileNameTagLeftDTop,WidthsHeight
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder   TButtonbtnOWTagLeft� Top,WidthKHeightCaptionExecute
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderOnClickExecuteClick   	TGroupBoxgrpPILeftTop� Width1Height� CaptionTabfPickIconDlg
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TImageimgPITagLeft
TopWidthHeightHintabfPickIconDlgAutoSize	Picture.Data
B  TBitmap6  BM6      v   (               �                �    �  ��� ��� ���                                             DDDDDDDDDDDDDDPP#3333333333334DD#3333333333334#3#3EUUUUUUUUUS4#3#3C#########S4#3#3B25UUUUU22S4#3#3C#%DDTDE##S4#3#3B25B PE22S4#3#3C#%B"PE##S4#3#3B25B P E22S4#3#3C#%@ P E##S4#3#3B25UUUUU22S4#3#3C#%@ PE##S4#3#3B25@ R"E22S4#3#3C#%@ P"E##S4#3#3B25DDTDE22S4#3#3C#%UUUUU##S4#3#3B222222222S4#3#3EUUUUUUUUUS4#3#3BP      RRS4#3#3DDDDDDDDDDC4#3#133333333334#3#3333333334#1#3333333334##13333333334##133333333334##3333333333334#1""""""""""""""#3OnClickComponentIconClick  TLabellbPIInfoLeft,TopWidth� Height5AutoSizeCaption-Implements standard shell Change Icon dialog.
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	WordWrap	  TLabellbPIFileNameLeft
Top4Width6HeightCaptionFileNameFocusControledPIFileName
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TSpeedButtonbtnPIFileNameTagLeftTop0WidthHeightHintChoose file
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
Glyph.Data
�   �   BM�       v   (               �                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������������     �� 33330��3333��33330��3333��     �������������   ��� ���� ����������������� ����������
ParentFontOnClickbtnFileNameClick  TEditedPIFileNameTagLeftDTop0Width� Height
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder   TButtonbtnPITagLeft� ToplWidthKHeightCaptionExecute
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderOnClickExecuteClick  	TGroupBoxgrpPIIconLargeLeft
TopNWidthZHeight6CaptionLarge icon 
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TImageimgPIIconLargeLeftTopWidth Height Center	  TLabellbPIIconLargeLeft
TopWidthHeightCaptionNone   	TGroupBoxgrpPIIconSmallLeftlTopNWidthZHeight6CaptionSmall icon 
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder TImageimgPIIconSmallLeftTopWidth Height Center	  TLabellbPIIconSmallLeft
TopWidthHeightCaptionNone    	TGroupBoxgrpOPLeftBTopTWidth0HeightRCaptionTabfObjectPropertiesDlg
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TImageimgOPTagLeft
TopWidthHeightHintabfObjectPropertiesDlgAutoSize	Picture.Data
B  TBitmap6  BM6      v   (               �                �    �  ��� ��� ���                                             DDDDDDDDDDDDDD P#3333333333334DD#3333333333334#3#3EUUUUUUUUUS4#3#3C#########S4#3#3B22DDDDDB2S4#3#3C#%UUUUUC#S4#3#3B2433335B2S4#3#3C#$5UUS5C#S4#3#3B2433335B2S4#3#3C#$DDDDEC#S4#3#3B2433335B2S4#3#3C#$5UUS5C#S4#3#3B2433335B2S4#3#3C#$5UUS5C#S4#3#3B2433335B2S4#3#3C#$DDDDE##S4#3#3B222222222S4#3#3EUUUUUUUUUS4#3#3BP      RRS4#3#3DDDDDDDDDDC4#3#133333333334#3#3333333334#1#3333333334##13333333334##133333333334##3333333333334#1""""""""""""""#3OnClickComponentIconClick  TLabellbOPInfoLeft,TopWidth� Height5AutoSizeCaption3Implements standard shell Object Properties dialog.
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	WordWrap	  TLabellbOPFileNameLeft
Top4Width6HeightCaptionFileNameFocusControledOPFileName
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TSpeedButtonbtnOPFileNameTagLeft� Top0WidthHeightHintChoose file
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
Glyph.Data
�   �   BM�       v   (               �                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������������     �� 33330��3333��33330��3333��     �������������   ��� ���� ����������������� ����������
ParentFontOnClickbtnFileNameClick  TEditedOPFileNameTagLeftDTop0WidthsHeight
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder   TButtonbtnOPTagLeft� Top0WidthKHeightCaptionExecute
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderOnClickExecuteClick   	TGroupBoxgrpFindLeftBTop� Width0HeightlCaptionTabfFindDlg
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TImageimgFindTagLeft
TopWidthHeightHint
abfFindDlgAutoSize	Picture.Data
$  TBitmap  BM      V   (                             �   �   ��   �  ��� ��� ���     ffffffffffffff0pEUUUUUUUUUUUUVffEUUUUUUUUUUUUVEUEUgwwwwwWuwwuVEUEUeEEEEEEEuVEUEUdTWwwq TTuVEUEUeEFUU WEEuVEUEUdTVDA GTTuVEUEUeEFwe TGEEuVEUEUdTe%veDGTTuVEUEUeFBRWTDGEEuVEUEUdV$%'TDGTTuVEUEUeFBBWTwwEEuVEUEUdTd$uDTFTTuVEUEUeEFgTDTeEEuVEUEUdTVDDDVTTTuVEUEUeEFfffeEEEuVEUEUdTTTTTTTTTuVEUEUgwwwwwwwwwuVEUEUdp      ttuVEUEUffffffffffeVEUES3UUUUUUUUUUVEUE555UUUUUUUUUVESE555UUUUUUUUUVE5E3S5UUUUUUUUUVE5ES3UUUUUUUUUUVE3EUUUUUUUUUUUUVESDDDDDDDDDDDDDDEU  OnClickComponentIconClick  TLabel
lbFindInfoLeft,TopWidth� Height5AutoSizeCaption&Implements Find Files/Computer dialog.
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	WordWrap	  TLabellbFindDirectoryLeft
Top0Width0HeightCaption	DirectoryFocusControledFindDirectory
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TSpeedButtonbtnFindDirectoryTagLeftTop,WidthHeightHintSelect directory
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
Glyph.Data
�   �   BM�       v   (               �                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������������     �� 33330��3333��33330��3333��     �������������   ��� ���� ����������������� ����������
ParentFontOnClickbtnDirectoryClick  TLabel
lbFindKindLeft"TopNWidthHeightCaptionKindFocusControlcmbFindKind
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TEditedFindDirectoryTagLeft@Top,Width� Height
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder   TButtonbtnFindTagLeft� TopJWidthKHeightCaptionExecute
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderOnClickExecuteClick  	TComboBoxcmbFindKindTagLeft@TopJWidth� HeightStylecsOwnerDrawFixed
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ItemHeight
ParentFontTabOrder   	TGroupBoxgrpCSLeftBTopWidth0HeightRCaptionTabfCreateShortcutDlg
Font.ColorclBlackFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrder TImageimgCSTagLeft
TopWidthHeightHintabfCreateShortcutDlgAutoSize	Picture.Data
  TBitmap  BM      N   (                             �    �  ��� ��� ���     DDDDDDDDDDDDDDPP#3333333333334DD#3333333333334#3#3EUUUUUUUUUS4#3#3C#########S4#3#3B222222222S4#3#3C#%UUUUU##S4#3#3B24""""%22S4#3#3C#$"%""%##S4#3#3B24"R""%22S4#3#3C#$"U""%##S4#3#3B24"URR%22S4#3#3C#$"%UR%##S4#3#3B24""UR%22S4#3#3C#$"%UR%##S4#3#3B24""""%22S4#3#3C#$DDDDE##S4#3#3B222222222S4#3#3EUUUUUUUUUS4#3#3BP      RRS4#3#3DDDDDDDDDDC4#3#133333333334#3#3333333334#1#3333333334##13333333334##133333333334##3333333333334#1""""""""""""""#3  OnClickComponentIconClick  TLabellbCSInfoLeft,TopWidth� Height5AutoSizeCaption=Brings up a dialog that allows the user create a new shortcut
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	WordWrap	  TLabellbCSDirectoryLeft
Top4Width0HeightCaption	DirectoryFocusControledOPFileName
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTransparent	  TSpeedButtonbtnCSDirectoryTagLeft� Top0WidthHeightHintChoose file
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
Glyph.Data
�   �   BM�       v   (               �                       �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ��� ����������������     �� 33330��3333��33330��3333��     �������������   ��� ���� ����������������� ����������
ParentFontOnClickbtnDirectoryClick  TButtonbtnCSExecuteTagLeft� Top0WidthKHeightCaptionExecute
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder OnClickExecuteClick  TEditedCSDirectoryTagLeftDTop0WidthsHeight
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrderTextC:\   
TPopupMenupmMain 	TMenuItem
miAboutAppCaptionAbout...ShortCut OnClick
AboutClick  	TMenuItemmi1Caption-ShortCut   	TMenuItemmiExitCaptionCloseShortCuts�  OnClick
CloseClick   TOpenDialogdlgOpenFile
DefaultExtexeFileEditStylefsEditFilter>Programs (*.exe)|*.exe|Icons (*.ico)|*.ico|All files (*.*)|*.*TitleChoose fileLeft  
TabfRunDlg	abfRunDlgLeftTop  TabfOpenWithDlgabfOpenWithDlgLeftLTop  TabfObjectPropertiesDlgabfObjectPropertiesDlgLeftLTopb  TabfPickIconDlgabfPickIconDlgFileNameC:\WINNT\System32\shell32.dll	IconIndex LeftTop�   TabfFindDlg
abfFindDlgLeftLTop�   TabfBrowseFolderDlgdlgSelectDirTextSelect directoryLeft$Top�   TabfCreateShortcutDlgabfCreateShortcutDlg	DirectoryC:\LeftJTop(   