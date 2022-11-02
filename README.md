# `ee9` the KDF9 emulator
This repo is a fork of Bill Findlay's KDF9 emulator ([original found here](http://www.findlayw.plus.com/KDF9/#Emulator)) I take no credit at all for the actual emulator code and runtime files, all thanks go to him for his hard work :heart:.

## kal3 and kal4
kal3 and kal4 are from http://settle.ddns.net/KDF9/kalgol/DavidHo/readme.htm

## How to run the `ee9` the KDF9 emulator
1. You will need to build everything first, see below for instructions.
2. Read the user guide in the Documents folder for basic usage instructions.
3. Other more detailed documentation can also be found in the Documents folder.

## What different between this repo and the original
- Source code changes:
    - `ee9` honors the `$KDF9RUNTIME` environment variable for all host IO (log files, devices etc).
    - don't assume everyone uses (or wants to use) black on white terminals. :unamused:
- Converted to using Makefiles to build everything (see below).
- Amended all the scripts to:
    - Parameters are now passed by command line arguments rather than position.
    - Honor `$KDF9RUNTIME` when reading assembly, source-code, data and binary files.
- Removed special build case for Cygwin.
- I don't include pre-build binaries.

## Folder structure
- `src` Main source code for ee9.
- `builddefs` Gnat adc pragma files for the various build options.
- `kal3` Source code for kal3.
- `kal4` Source code for kal4.
- `kalgol` Data files using during the compilation of Kidsgrove Algol.
- `mkchan` Source code for mkchan.
- `scripts` Bash scripts to simplify the execution of ee9.
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

### Installing`ee9` (the KDF9 emulator) and usage
### Docker
Build the docker image

`make docker`

And then run and connect:

`docker run -it --rm mariocki/kdf9`

### Locally
Simply `cd` to the root folder and type `sudo make install`.

If the build fails see the 'Required Dependencies' section below to ensure you have all the neccesary packages installed.

By default the executables and scripts are installed into `/usr/local/bin` and the default runtime is placed in `/usr/local/share/kdf9/`. If you wish to change these locations you can do so by specifying a value for `prefix` during build.
    `make -n prefix=/opt/kdf9 install`

### Running ee9
Once completed execute the `kdf9_setup` command to create the ee9 runtime. By default this will be placed in a folder named `.kdf9/` in your `$HOME` directory but this can by changed by specifying a location in the `$KDF9RUNTIME` environment variable.

All the scripts as detailed in Bill Findlay's documentation remain mainly as-was but now use command line switches, you can pass the `-h` switch to see basic help.

## Required dependencies.
#### Ubuntu and other Debian derived distributions.
Pakages required: `make` `build-essentials` `bison` `gnat`.
#### Fedora etc
*please let me know*
### MacOs
*please let me know, specifically any [Homebrew](https://brew.sh/) or [MacPorts](https://www.macports.org/) instructions*
### Windows
Only Windows10 + WSL2 is supported.

Ensure you are running Windows 10 version 2004 or above [See here for details](https://docs.microsoft.com/en-us/windows/whats-new/whats-new-windows-10-version-2004).

Enable [WSL2](https://docs.microsoft.com/en-us/windows/wsl/install-win10).

Choose a distribution of your choice and follow the instructions for your chosen distro as given above in the Linux section.

I also *highly* recommend you install Microsoft's new [Windows Terminal](https://aka.ms/terminal).
### More detailed build instructions
Building from source should be the same on any modern Linux or MacOS installation provided the correct packages have been installed.

- `make ee9|mtp|to_9_from_1934`
    Build `ee9`, `mtp`, `to_9_from_1934` in-place in the src folder.
    There are various slightly different build options - see the top of the Makefile in the root folder to make changes if desired.

- `make kal3|kal4|mkchan`
    Build the requested executable in-place in its respective folder.

- `make all`
    Builds all of `ee9`, `kal3`, `kal4`, `mkchan`, `to_9_from_1934` and `mtp`.

- `make update`
    Fetches any updates for the `kal3`, `kal4` and `kalgol` runtimes from http://settle.ddns.net/KDF9/kalgol/DavidHo/readme.htm

- `make install`
    Installs all executables, scripts and the runtime system. The default installation location is into `/usr/local`, however you can change this by specifying a new `prefix` as shown below:

    `make -n prefix=/opt/kdf9 install`

- `make distclean`
    Removes all intermediate and transient files created during compilation.

*let me know if I have missed anything*
