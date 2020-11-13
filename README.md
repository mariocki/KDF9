# KDF9 emulator `ee9`
This repo is a copy of FindlayW's KDF9 emulator ([original found here](http://www.findlayw.plus.com/KDF9/emulation/emulator.html)) I take no credit at all for the actual emulator code and runtime files, all thanks go to him for his hard work :heart:.

## How to run the `ee9` the KDF9 emulator
1. You will need to build everything first, see below for instructions.
2. Read the Readme.pdf in the root folder for basic usage instructions.
3. Read the more detailed documentation found in the `Documents` folder.

## What different between this repo and the original
- Converted to using Makefiles to build everything (see below).
- Removed special build case for Raspberry PI as I don't think it is required anymore (please let me know if I am incorrect).
- Removed special build case for Cygwin as Windows10 + WSL2 is a better alternative in this use-case.
- Removed all .bat files (see above re Cygwin).
- Amended all shell scripts to convert output to UTF8 for correct display on modern terminals.
- Replaced KDF9Flex with a sed script to perform conversion between modern glyphs and those expected by the Whetstone compiler.

### Differences in the Whetstone interpreter
- Amended the `whet` shell script to look for files with a suffix of `.w60` and if found will apply the conversion to `.a60` format expected by the Whetstone interpreter. This allows you to write Algol60 code in a more modern format without the need to manually convert before interpreting.
- `~` is used as the exponent i.e. `layout := format({+d.dddddddd~+nd});` converts to `layout := format(_[+d.ddddddddº+nd_]);`
- `\` is used for integer division `z := (a \ y);` converts to `z := (a ÷ y);`
- `|` is not replaced as in KDF9Flex



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
- The ADA code is unchanged :smiley:
- I don't include pre-build binaries.
- The test scripts are unchanged (but slightly broken ... caveat emptor :fire:).
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
Packages required:
    `build-essentials`
    `yacc`
    `gnatmake`
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