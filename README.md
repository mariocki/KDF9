# KDF9
This repo is a copy of FindlayW's KDF9 emulator ([original found here](http://www.findlayw.plus.com/KDF9/emulation/emulator.html)) and all thanks go to him for his hard work.

## What different between this repo and the original
- Converted to using Makefiles to build everything (see below).
- Removed special build case for Raspberry PI as I don't think it is required anymore (please let me know if I am incorrect).
- Removed special build case for Cygwin as Windows10 + WSL2 is a better alternative in this use-case.
- Removed all .bat files (see above re Cygwin).
- Amended all shell scripts to convert output to UTF8 for correct display on modern terminals.
- Replaced KDF9Flex with a sed script to perform conversion between modern glyphs and those expected by the Whetstone compiler.

## Folder structure
- `src` Main source code for ee9
- `builddefs` Gnat adc pragma files for the various build options.
- `kal3` Source code for kal3
- `runtime` Runtime environment
    - `runtime/Algol` Whetstone Algol source files
    - `runtime/Binary` Compiled KDF9 binaries
    - `runtime/Assembly` Usercode assembly source code (one compiled the binaries are placed in `runtime/Binary`)
- `Documents` Various KDF9 and Whetstone related PDFs/HTML pages.

## What has not been changed
- The ADA code is unchanged.
- The test scripts are unchanged (but slightly broken ... caveat emptor).
- The License, it's still GPLv3.

## Using Make to build and run KDF9
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
Building on source should be the same on any modern Linux or MacOS installation.

### Ubuntu and other Debian derived distributions.
Packages required:
    `build-essentials`
    `yacc`
    `gnatmake`
    * let me know if I have missed aything *

### Windows
Only Windows10 + WSL2 is supported.
Ensure you are running Windows 10 version 2004 (or above) (See here for details[https://docs.microsoft.com/en-us/windows/whats-new/whats-new-windows-10-version-2004])
Enable WSL2 [https://docs.microsoft.com/en-us/windows/wsl/install-win10]
Choose a distribution of your choice and follow the instructions for Linux above

I also *highly* recommend you install Microsoft's new (Windows Terminal[https://github.com/microsoft/terminal])
