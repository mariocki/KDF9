# `ee9` the KDF9 emulator
This repo is a fork of Bill Findlay's KDF9 emulator ([original found here](http://www.findlayw.plus.com/KDF9/#Emulator)) I take no credit at all for the actual emulator code and runtime files, all thanks go to him for his hard work :heart:.

## kal3 and kal4
kal3 and kal4 are from http://settle.ddns.net/KDF9/kalgol/DavidHo/readme.htm

## How to run the `ee9` the KDF9 emulator
1. You will need to build everything first, see below for instructions.
2. Read the user guide in the Documents folder for basic usage instructions.
3. Other more detailed documentation can also be found in the Documents folder.

## What different between this repo and the original
- Source code changes 
    - Write KDF9.log to the `{KDF9RUNTIME}/logs` folder.
    - don't assume everyone uses black on white terminals. :unamused:
    - Use the `KDF9RUNTIME` env variable to determine the default paths for `Binaries` and `Data`.
    - Use the `KDF9RUNTIME` env variable to determine the default paths for MT* in `mtp`.
- Converted to using Makefiles to build everything (see below).
- Removed special build case for Cygwin as Windows10 + WSL2 is a better alternative in this use-case.
- I don't include pre-build binaries.

## Folder structure
- `src` Main source code for ee9.
- `builddefs` Gnat adc pragma files for the various build options.
- `kal3` Source code for kal3.
- `kal4` Source code for kal4.
- `kalgol` Data files using during the compilation of Kidsgrove Algol.
- `mkchan` Source code for mkchan.
- `runtime` Runtime environment.
    - `runtime/Kidsgrove` Kidsgrove Algol source files.
    - `runtime/Whetstone` Whetstone Algol source files.
    - `runtime/Binary` Compiled KDF9 binaries.
    - `runtime/Data` Data files.
    - `runtime/FW0Files` Various FW0 files for director or Whetstone interpreter etc.
    - `runtime/logs` log files are written in here.
    - `runtime/test` test scripts.
    - `runtime/setting` Various settings files which canb be copied to settings_[12].txt.
    - `runtime/Assembly` Usercode assembly source code (once compiled the binaries are placed in `runtime/Binary`).
- `Documents` Various KDF9 and Whetstone related PDFs/HTML pages.

## What has not been changed
- The License, it's still GPLv3. :thumbsup:

## How to build `ee9` (the KDF9 emulator)
- `make ee9`
    Build ee9 in-place in the src folder.
    There are various slightly different build options - see the top of the Makefile in the root folder to make changes if desired.

- `make kal3|kal4|mkchan`
    Build the requested executable in-place in its respective folder.

- `make all`
    Builds ee9/kal3/kal4/mkchan in-place.

- `make install`
    Builds the executables and installs them along with the scripts into `/usr/local/bin` and copies the base runtime to `/usr/local/lib/kdf9`.

- `make distclean`
    Removes all intermediate and transient files from compilation but leaves the runtime intact.
    Also deletes all files created during the execution of the runtime to leave the folder structure 
    exactly as was when first extracted/downloaded.
    
## Building from source
Building from source should be the same on any modern Linux or MacOS installation.

Simply `cd` to the root folder and type `make install` as root.
Once completed execute the `kdf9_setup` command to create a runtime named `.kdf9/` in your `$HOME`.

### Ubuntu and other Debian derived distributions.
Packages required: `make` `build-essentials` `bison` `gnat`.

*let me know if I have missed aything*

### Windows
Only Windows10 + WSL2 is supported.

Ensure you are running Windows 10 version 2004 or above [See here for details](https://docs.microsoft.com/en-us/windows/whats-new/whats-new-windows-10-version-2004).

Enable [WSL2](https://docs.microsoft.com/en-us/windows/wsl/install-win10).

Choose a distribution of your choice and follow the instructions for Linux given above.

I also *highly* recommend you install Microsoft's new [Windows Terminal](https://github.com/microsoft/terminal).

### ToDo
- [ ] Improve the Makefile to install the executables and shell scripts into `/usr/local/bin` and data files to `/usr/local/lib/KDF9`. This may require code changes to the ADA though :(
- [ ] Create a Docker container?