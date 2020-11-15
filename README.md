# `ee9` the KDF9 emulator
This repo is a fork of FindlayW's KDF9 emulator ([original found here](http://www.findlayw.plus.com/KDF9/emulation/emulator.html)) I take no credit at all for the actual emulator code and runtime files, all thanks go to him for his hard work :heart:.

## kal3 and kal4
kal3 and kal4 are from http://settle.ddns.net/KDF9/kalgol/DavidHo/readme.htm

## How to run the `ee9` the KDF9 emulator
1. You will need to build everything first, see below for instructions.
2. Read the user guide in the Documents folder for basic usage instructions.
3. Other more detailed documentation can also be found in the Documents folder.

## What different between this repo and the original
- Source code changes 
    - Write KDF9.log to the `{KDFROOT}/logs` folder 
    - don't assume everyone uses black on white terminals :unamused:
    - Use the `KDFROOT` env variable to determine the default paths for `Binaries` and `Data`
- Converted to using Makefiles to build everything (see below).
- Removed special build case for Cygwin as Windows10 + WSL2 is a better alternative in this use-case.


## Folder structure
- `src` Main source code for ee9
- `builddefs` Gnat adc pragma files for the various build options.
- `kal3` Source code for kal3
- `kal4` Source code for kal4
- `kalgol` Data files using during the compilation of Kidsgrove Algol
- `mkchan` Source code for mkchan
- `runtime` Runtime environment
    - `runtime/Kidsgrove` Kidsgrove Algol source files
    - `runtime/Whetstone` Whetstone Algol source files
    - `runtime/Binary` Compiled KDF9 binaries
    - `runtime/Data` Data files
    - `runtime/FW0Files` Various FW0 files for director or Whetstone interpreter etc
    - `runtime/logs` log files are written in here
    - `runtime/test` test scripts
    - `runtime/setting` Various settings files which canb be copied to settings_[12].txt
    - `runtime/Assembly` Usercode assembly source code (once compiled the binaries are placed in `runtime/Binary`)
- `Documents` Various KDF9 and Whetstone related PDFs/HTML pages.

## What has not been changed
- I don't include pre-build binaries.
- The License, it's still GPLv3 :thumbsup:

## How to build `ee9` (the KDF9 emulator)
- `make ee9`
    Build ee9 in-place in the src folder.
    There are various slightly different build options - see the top of the Makefile in the root folder to make changes if desired.

- `make kal3`
    Build kal3 in-place in the kal3 folder.

- `make all`
    Builds ee9 and kal3 in-place.

- `make deploy`
    Builds ee9 and kal3, copies them into `runtime` and also resets the runtime to a 'clean' environment.

- `make test`
    As per `make deploy` but also then runs the `ee9_self_test` script.

- `make clean`
    Removes all intermediate and transient files. Only really needs to be used if you have really messed up your runtime.

## Building from source
Building from source should be the same on any modern Linux or MacOS installation.

Simply `cd` to the root folder and type `make deploy`.

### Ubuntu and other Debian derived distributions.
Packages required: `make` `build-essentials` `bison` `gnat` `dos2unix`

*let me know if I have missed aything*

### Windows
Only Windows10 + WSL2 is supported.

Ensure you are running Windows 10 version 2004 or above [See here for details](https://docs.microsoft.com/en-us/windows/whats-new/whats-new-windows-10-version-2004)

Enable [WSL2](https://docs.microsoft.com/en-us/windows/wsl/install-win10)

Choose a distribution of your choice and follow the instructions for Linux given above.

I also *highly* recommend you install Microsoft's new [Windows Terminal](https://github.com/microsoft/terminal)

### ToDo
- [ ] Improve the Makefile to install the executables and shell scripts into `/usr/local/bin` and data files to `/usr/local/lib/KDF9`. This may require code changes to the ADA though :(
- [ ] Create a Docker container?