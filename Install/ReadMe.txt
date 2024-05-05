================================================================
vclInstaller.exe is a Delphi/C++Builder package and help 
manager, allows install and uninstall packages and help files 
into the Delphi/C++Builder IDE.

VCL Installer is a freeware, also you can purchase the sources.

Copyright (c) 2000-2008 ABF software, Inc.
All rights reserved.

http://www.abf-dev.com
mailto:info@abf-dev.com
================================================================


Installs/removes packages and help files into/from Delphi/C++Builder IDE

Usage:   vclInstaller.exe config [/u] [/b]
Example: vclInstaller.exe packages.cfg /b
  config - name of the configuration file. See ReadMe.txt for details
  /u     - uninstall switch
  /b     - "build only" switch

Config file is an INI styled file. You can specify packages and help files 
for any version of Delphi or C++Builder. Sections for Delphi are named as [Dx]
and [DxHelp] where x is a number of version. Sections for C++Builder are named 
as [Cx] and [CxHelp] where x is a number of version. Files are listed as 
FileName=Description. It is possible to specify a [Help] section for all 
versions of Delphi/C++Builder together. 

Example:

=======================================
[D5]
units\CompiledPakage_D5.dpl=Already compiled package
units\SourcePakage_D5.dpk=Package that is compiled during installation

[D5Help]
help\Help_D5.dpl=Help for Delphi 5

[C5]
units\CompiledPakage_C5.bpl=Already compiled package
units\SourcePakage_C5.bpk=Package that is compiled during installation

[D5Help]
help\MainHelp.dpl=Help for all versions

=======================================

