# Makefile for Delphi 11/12 CE (Windows/macOS) and Lazarus (Linux/Windows/macOS)

PROJECT_NAME = kayte
SRC_FILES = $(PROJECT_NAME).lpr
DELPHI_OUT = bin/$(PROJECT_NAME).exe
FPC_OUT = bin/$(PROJECT_NAME)

# Detect OS
ifeq ($(OS),Windows_NT)
    # Windows-specific settings
    DELPHI_COMPILER = dcc32
    DELPHI_FLAGS = -U..\units\ -B -Q
    FPC_COMPILER = fpc
    EXEC_EXT = .exe
else ifeq ($(shell uname), Darwin)
    # macOS-specific settings
    #DELPHI_COMPILER = dccosx
    #DELPHI_FLAGS = -U../units/ -B -Q
    FPC_COMPILER = fpc
    EXEC_EXT =
else
    # Linux/Unix settings
    FPC_COMPILER = fpc
    EXEC_EXT =
endif

# Default target
all: $(DELPHI_OUT) $(FPC_OUT)

# Delphi target (Windows/macOS only)
$(DELPHI_OUT):
ifeq ($(OS),Windows_NT)
	@mkdir -p bin
	$(DELPHI_COMPILER) $(SRC_FILES) $(DELPHI_FLAGS) -Ebin
else ifeq ($(shell uname), Darwin)
	@mkdir -p bin
	$(DELPHI_COMPILER) $(SRC_FILES) $(DELPHI_FLAGS) -Ebin
endif

# Free Pascal target (cross-platform)
$(FPC_OUT):
	@mkdir -p bin
	$(FPC_COMPILER) $(SRC_FILES) -obin/$(PROJECT_NAME)$(EXEC_EXT)

# Clean target
clean:
	rm -f bin/$(PROJECT_NAME)$(EXEC_EXT)

# Check if COMSPEC (Windows command processor) is defined
ifdef COMSPEC
    ifneq ($(findstring $(OS_SOURCE),$(OSNeedsComspecToRunBatch)),)
        ifndef RUNBATCH
            RUNBATCH = $(COMSPEC) /C
        endif
    endif
endif

# Set path separator based on platform
ifdef inUnix
    PATHSEP = /
else
    PATHSEP = $(subst /,\,/)
    ifdef inCygWin
        PATHSEP = /
    endif
endif

# Set BASEDIR depending on PWD availability
ifdef PWD
    BASEDIR := $(subst \,/,$(shell $(PWD)))
    ifdef inCygWin
        ifneq ($(findstring /cygdrive/,$(BASEDIR)),)
            BASENODIR := $(patsubst /cygdrive%,%,$(BASEDIR))
            BASEDRIVE := $(firstword $(subst /, ,$(BASENODIR)))
            BASEDIR := $(subst /cygdrive/$(BASEDRIVE)/,$(BASEDRIVE):/,$(BASEDIR))
        endif
    endif
else
    BASEDIR = .
endif

# Configure ECHO command for OS2 systems
ifdef inOS2
    ifndef ECHO
        ECHO := $(strip $(wildcard $(addsuffix /gecho$(SRCEXEEXT),$(SEARCHPATH))))
        ifeq ($(ECHO),)
            ECHO := $(strip $(wildcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH))))
            ifeq ($(ECHO),)
                ECHO = echo
            else
                ECHO := $(firstword $(ECHO))
            endif
        else
            ECHO := $(firstword $(ECHO))
        endif
    endif
    export ECHO
endif

# Define FPC (Free Pascal Compiler) if not already set
ifndef FPC
    ifdef PP
        FPC = $(PP)
    endif
endif

ifndef FPC
    FPCPROG := $(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH))))
    ifneq ($(FPCPROG),)
        FPCPROG := $(firstword $(FPCPROG))
        ifneq ($(CPU_TARGET),)
            FPC := $(shell $(FPCPROG) -P$(CPU_TARGET) -PB)
        else
            FPC := $(shell $(FPCPROG) -PB)
        endif
        ifneq ($(findstring Error,$(FPC)),)
            override FPC = $(firstword $(strip $(wildcard $(addsuffix /ppc386$(SRCEXEEXT),$(SEARCHPATH)))))
        else
            ifeq ($(strip $(wildcard $(FPC))),)
                FPC := $(firstword $(FPCPROG))
            endif
        endif
    else
        override FPC = $(firstword $(strip $(wildcard $(addsuffix /ppc386$(SRCEXEEXT),$(SEARCHPATH)))))
    endif
endif

# Clean up FPC path formatting
override FPC := $(subst $(SRCEXEEXT),,$(FPC))
override FPC := $(subst \,/,$(FPC))$(SRCEXEEXT)

# Define default FPC binary name if not set
FPC ?= fpc

# Attempt to locate the FPC compiler
FOUNDFPC := $(strip $(wildcard $(FPC)))

# If not found, search in common directories
ifeq ($(FOUNDFPC),)
    SEARCHPATHS := /usr/bin /usr/local/bin /bin /opt/local/bin
    FOUNDFPC := $(firstword $(wildcard $(addsuffix /$(FPC),$(SEARCHPATHS))))
    ifeq ($(FOUNDFPC),)
        # Additional OS-specific search paths
        ifeq ($(OS),Windows_NT)
            FOUNDFPC := $(strip $(wildcard C:/FPC/$(FPC).exe))
            ifeq ($(FOUNDFPC),)
                $(error Compiler $(FPC) not found in standard paths or C:/FPC)
            endif
        else ifeq ($(shell uname),Darwin)
            SEARCHPATHS += /opt/homebrew/bin
            FOUNDFPC := $(firstword $(wildcard $(addsuffix /$(FPC),$(SEARCHPATHS))))
            ifeq ($(FOUNDFPC),)
                $(error Compiler $(FPC) not found on macOS in standard paths)
            endif
        else ifeq ($(shell uname),Linux)
            # Optionally extend paths for Linux distributions
            SEARCHPATHS += /snap/bin /usr/local/fpc/bin
            FOUNDFPC := $(firstword $(wildcard $(addsuffix /$(FPC),$(SEARCHPATHS))))
            ifeq ($(FOUNDFPC),)
                $(error Compiler $(FPC) not found on Linux in standard paths)
            endif
        endif
    endif
endif

# If found, set FPC variable to its location
FPC := $(FOUNDFPC)

# Gather compiler information
ifndef FPC_COMPILERINFO
    FPC_COMPILERINFO := $(shell $(FPC) -iVSPTPSOTO)
endif
ifndef FPC_VERSION
    FPC_VERSION := $(word 1, $(FPC_COMPILERINFO))
endif

# Export relevant variables
export FPC FPC_VERSION FPC_COMPILERINFO
unexport CHECKDEPEND ALLDEPENDENCIES

# Set CPU and OS targets
ifndef CPU_TARGET
    ifdef CPU_TARGET_DEFAULT
        CPU_TARGET = $(CPU_TARGET_DEFAULT)
    endif
endif
ifndef OS_TARGET
    ifdef OS_TARGET_DEFAULT
        OS_TARGET = $(OS_TARGET_DEFAULT)
    endif
endif

# Set source CPU and OS details from compiler info
ifndef CPU_SOURCE
    CPU_SOURCE := $(word 2, $(FPC_COMPILERINFO))
endif
ifndef CPU_TARGET
    CPU_TARGET := $(word 3, $(FPC_COMPILERINFO))
endif
ifndef OS_SOURCE
    OS_SOURCE := $(word 4, $(FPC_COMPILERINFO))
endif
ifndef OS_TARGET
    OS_TARGET := $(word 5, $(FPC_COMPILERINFO))
endif

# Full target and source names
FULL_TARGET := $(CPU_TARGET)-$(OS_TARGET)
FULL_SOURCE := $(CPU_SOURCE)-$(OS_SOURCE)

# Set architecture and FPC options for specific targets
ifeq ($(CPU_TARGET), armeb)
    ARCH = arm
    override FPCOPT += -Cb
else ifeq ($(CPU_TARGET), armel)
    ARCH = arm
    override FPCOPT += -CaEABI
else
    ARCH = $(CPU_TARGET)
endif

# Embedded architecture checks and sub-architecture settings
ifeq ($(FULL_TARGET), arm-embedded)
    ifeq ($(SUBARCH),)
        $(error When compiling for arm-embedded, a sub-architecture (e.g., SUBARCH=armv4t or SUBARCH=armv7m) must be defined)
    endif
    override FPCOPT += -Cp$(SUBARCH)
endif
ifeq ($(FULL_TARGET), avr-embedded)
    ifeq ($(SUBARCH),)
        $(error When compiling for avr-embedded, a sub-architecture (e.g., SUBARCH=avr25 or SUBARCH=avr35) must be defined)
    endif
    override FPCOPT += -Cp$(SUBARCH)
endif
ifeq ($(FULL_TARGET), mipsel-embedded)
    ifeq ($(SUBARCH),)
        $(error When compiling for mipsel-embedded, a sub-architecture (e.g., SUBARCH=pic32mx) must be defined)
    endif
    override FPCOPT += -Cp$(SUBARCH)
endif

# Define suffixes based on OS compatibility
ifneq ($(findstring $(OS_SOURCE), $(LIMIT83fs)),)
    TARGETSUFFIX = $(OS_TARGET)
    SOURCESUFFIX = $(OS_SOURCE)
else
    ifneq ($(findstring $(OS_TARGET), $(LIMIT83fs)),)
        TARGETSUFFIX = $(OS_TARGET)
    else
        TARGETSUFFIX = $(FULL_TARGET)
    endif
endif
SOURCESUFFIX := $(FULL_SOURCE)

# Determine if cross-compilation is needed
ifneq ($(FULL_TARGET), $(FULL_SOURCE))
    CROSSCOMPILE = 1
endif

# Check if FULL_TARGET is defined
ifdef FULL_TARGET
$(FULL_TARGET):
	@echo "Target '$(FULL_TARGET)' is valid."
else
$(error FULL_TARGET is not defined. Please run fpcmake first)
endif

.PHONY: $(FULL_TARGET)
# Set hierarchical variables for BSD and Linux
ifneq ($(findstring $(OS_TARGET), $(BSDs)),)
    BSDhier = 1
endif
ifeq ($(OS_TARGET), linux)
    linuxHier = 1
endif

# Set build environment variables
ifndef CROSSCOMPILE
    BUILDFULLNATIVE = 1
    export BUILDFULLNATIVE
endif
ifdef BUILDFULLNATIVE
    BUILDNATIVE = 1
    export BUILDNATIVE
endif

# Export necessary variables
export OS_TARGET OS_SOURCE ARCH CPU_TARGET CPU_SOURCE FULL_TARGET FULL_SOURCE TARGETSUFFIX SOURCESUFFIX CROSSCOMPILE

# Configure FPCDIR path
ifdef FPCDIR
    override FPCDIR := $(subst \,/,$(FPCDIR))
    ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)
        override FPCDIR = wrong
    endif
else
    override FPCDIR = wrong
endif

ifdef DEFAULT_FPCDIR
    ifeq ($(FPCDIR), wrong)
        override FPCDIR := $(subst \,/,$(DEFAULT_FPCDIR))
        ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)
            override FPCDIR = wrong
        endif
    endif
endif

# Fallbacks for FPCDIR based on system paths
ifeq ($(FPCDIR), wrong)
    ifdef inUnix
        override FPCDIR = /usr/local/lib/fpc/$(FPC_VERSION)
        ifeq ($(wildcard $(FPCDIR)/units),)
            override FPCDIR = /usr/lib/fpc/$(FPC_VERSION)
        endif
    else
        override FPCDIR := $(subst /$(FPC),,$(firstword $(strip $(wildcard $(addsuffix /$(FPC),$(SEARCHPATH))))))
        override FPCDIR := $(FPCDIR)/..
        ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)
            override FPCDIR := $(FPCDIR)/..
            ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)
                override FPCDIR = $(BASEDIR)
                ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl)),)
                    override FPCDIR = c:/pp
                endif
            endif
        endif
    endif
endif

# Configure cross-compilation binaries directory
ifndef CROSSBINDIR
    CROSSBINDIR := $(wildcard $(FPCDIR)/bin/$(TARGETSUFFIX))
endif

# Darwin-specific configuration for iOS and macOS
ifneq ($(findstring $(OS_TARGET), darwin iphonesim ios),)
    ifneq ($(findstring $(OS_SOURCE), darwin ios),)
        DARWIN2DARWIN = 1
    endif
endif

# Set BINUTILSPREFIX for cross-compilation
ifndef BINUTILSPREFIX
    ifndef CROSSBINDIR
        ifdef CROSSCOMPILE
            ifneq ($(OS_TARGET), msdos)
                ifndef DARWIN2DARWIN
                    ifneq ($(CPU_TARGET), jvm)
                        BINUTILSPREFIX = $(CPU_TARGET)-$(OS_TARGET)-
                        ifeq ($(OS_TARGET), android)
                            ifeq ($(CPU_TARGET), arm)
                                BINUTILSPREFIX = arm-linux-androideabi-
                            else ifeq ($(CPU_TARGET), i386)
                                BINUTILSPREFIX = i686-linux-android-
                            else
                                BINUTILSPREFIX = $(CPU_TARGET)-linux-android-
                            endif
                        endif
                    endif
                endif
            endif
        endif
    endif
endif


# Define units and packages directories
UNITSDIR := $(wildcard $(FPCDIR)/units/$(TARGETSUFFIX))
ifeq ($(UNITSDIR),)
    UNITSDIR := $(wildcard $(FPCDIR)/units/$(OS_TARGET))
endif

PACKAGESDIR := $(wildcard $(FPCDIR) $(FPCDIR)/packages)

# Set FPCFPMAKE for cross-compilation or native compilation
ifndef FPCFPMAKE
    ifdef CROSSCOMPILE
        ifeq ($(strip $(wildcard $(addsuffix /compiler/ppc$(SRCEXEEXT),$(FPCDIR)))),)
            FPCPROG := $(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH))))
            ifneq ($(FPCPROG),)
                FPCPROG := $(firstword $(FPCPROG))
                FPCFPMAKE := $(shell $(FPCPROG) -PB)
                ifeq ($(strip $(wildcard $(FPCFPMAKE))),)
                    FPCFPMAKE := $(firstword $(FPCPROG))
                endif
            else
                override FPCFPMAKE := $(firstword $(strip $(wildcard $(addsuffix /ppc386$(SRCEXEEXT),$(SEARCHPATH)))))
            endif
        else
            FPCFPMAKE := $(wildcard $(addsuffix /compiler/ppc$(SRCEXEEXT),$(FPCDIR)))
            FPMAKE_SKIP_CONFIG = -n
            export FPCFPMAKE FPMAKE_SKIP_CONFIG
        endif
    else
        FPMAKE_SKIP_CONFIG = -n
        FPCFPMAKE = $(FPC)
    endif
endif

# Append required directories if defined
ifdef REQUIRE_UNITSDIR
    override UNITSDIR += $(REQUIRE_UNITSDIR)
endif
ifdef REQUIRE_PACKAGESDIR
    override PACKAGESDIR += $(REQUIRE_PACKAGESDIR)
endif

# Define installation directory structure
ifdef ZIPINSTALL
    ifneq ($(findstring $(OS_TARGET), $(UNIXs)),)
        UNIXHier = 1
    endif
else
    ifneq ($(findstring $(OS_SOURCE), $(UNIXs)),)
        UNIXHier = 1
    endif
endif

ifndef INSTALL_PREFIX
    ifdef PREFIX
        INSTALL_PREFIX = $(PREFIX)
    endif
endif

ifndef INSTALL_PREFIX
    ifdef UNIXHier
        INSTALL_PREFIX = /usr/local
    else
        ifdef INSTALL_FPCPACKAGE
            INSTALL_BASEDIR := /pp
        else
            INSTALL_BASEDIR := /$(PACKAGE_NAME)
        endif
    endif
endif

export INSTALL_PREFIX

ifdef INSTALL_FPCSUBDIR
    export INSTALL_FPCSUBDIR
endif

# Distribution directory
ifndef DIST_DESTDIR
    DIST_DESTDIR := $(BASEDIR)
endif
export DIST_DESTDIR

# Set compiler target directory based on PACKAGEDIR_MAIN if available
ifndef COMPILER_UNITTARGETDIR
    ifdef PACKAGEDIR_MAIN
        COMPILER_UNITTARGETDIR = $(PACKAGEDIR_MAIN)/units/$(TARGETSUFFIX)
    else
        COMPILER_UNITTARGETDIR = units/$(TARGETSUFFIX)
    endif
endif

ifndef COMPILER_TARGETDIR
    COMPILER_TARGETDIR = .
endif

# Define INSTALL_BASEDIR based on hierarchy
ifndef INSTALL_BASEDIR
    ifdef UNIXHier
        ifdef INSTALL_FPCPACKAGE
            INSTALL_BASEDIR := $(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)
        else
            INSTALL_BASEDIR := $(INSTALL_PREFIX)/lib/$(PACKAGE_NAME)
        endif
    else
        INSTALL_BASEDIR := $(INSTALL_PREFIX)
    endif
endif

# Define installation paths for binaries, units, libraries, and sources
ifndef INSTALL_BINDIR
    ifdef UNIXHier
        INSTALL_BINDIR := $(INSTALL_PREFIX)/bin
    else
        INSTALL_BINDIR := $(INSTALL_BASEDIR)/bin
        ifdef INSTALL_FPCPACKAGE
            ifdef CROSSCOMPILE
                ifdef CROSSINSTALL
                    INSTALL_BINDIR := $(INSTALL_BINDIR)/$(SOURCESUFFIX)
                else
                    INSTALL_BINDIR := $(INSTALL_BINDIR)/$(TARGETSUFFIX)
                endif
            else
                INSTALL_BINDIR := $(INSTALL_BINDIR)/$(TARGETSUFFIX)
            endif
        endif
    endif
endif

ifndef INSTALL_UNITDIR
    INSTALL_UNITDIR := $(INSTALL_BASEDIR)/units/$(TARGETSUFFIX)
    ifdef INSTALL_FPCPACKAGE
        ifdef PACKAGE_NAME
            INSTALL_UNITDIR := $(INSTALL_UNITDIR)/$(PACKAGE_NAME)
        endif
    endif
endif

ifndef INSTALL_LIBDIR
    ifdef UNIXHier
        INSTALL_LIBDIR := $(INSTALL_PREFIX)/lib
    else
        INSTALL_LIBDIR := $(INSTALL_UNITDIR)
    endif
endif

ifndef INSTALL_SOURCEDIR
    ifdef UNIXHier
        ifdef BSDhier
            SRCPREFIXDIR = share/src
        else ifdef linuxHier
            SRCPREFIXDIR = share/src
        else
            SRCPREFIXDIR = src
        endif

        ifdef INSTALL_FPCPACKAGE
            ifdef INSTALL_FPCSUBDIR
                INSTALL_SOURCEDIR := $(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION)/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)
            else
                INSTALL_SOURCEDIR := $(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION)/$(PACKAGE_NAME)
            endif
        else
            INSTALL_SOURCEDIR := $(INSTALL_PREFIX)/$(SRCPREFIXDIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
        endif
    else
        ifdef INSTALL_FPCPACKAGE
            ifdef INSTALL_FPCSUBDIR
                INSTALL_SOURCEDIR := $(INSTALL_BASEDIR)/source/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)
            else
                INSTALL_SOURCEDIR := $(INSTALL_BASEDIR)/source/$(PACKAGE_NAME)
            endif
        else
            INSTALL_SOURCEDIR := $(INSTALL_BASEDIR)/source
        endif
    endif
endif

# Set documentation directory
ifndef INSTALL_DOCDIR
    ifdef UNIXHier
        DOCPREFIXDIR := share/doc
        ifdef BSDhier
            DOCPREFIXDIR := share/doc
        else ifdef linuxHier
            DOCPREFIXDIR := share/doc
        else
            DOCPREFIXDIR := doc
        endif
        ifdef INSTALL_FPCPACKAGE
            INSTALL_DOCDIR := $(INSTALL_PREFIX)/$(DOCPREFIXDIR)/fpc-$(FPC_VERSION)/$(PACKAGE_NAME)
        else
            INSTALL_DOCDIR := $(INSTALL_PREFIX)/$(DOCPREFIXDIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
        endif
    else
        INSTALL_DOCDIR := $(INSTALL_BASEDIR)/doc/$(PACKAGE_NAME)
    endif
endif

# Set examples directory
ifndef INSTALL_EXAMPLEDIR
    ifdef UNIXHier
        ifdef INSTALL_FPCPACKAGE
            ifdef BSDhier
                INSTALL_EXAMPLEDIR := $(INSTALL_PREFIX)/share/examples/fpc-$(FPC_VERSION)/$(PACKAGE_NAME)
            else ifdef linuxHier
                INSTALL_EXAMPLEDIR := $(INSTALL_DOCDIR)/examples
            else
                INSTALL_EXAMPLEDIR := $(INSTALL_PREFIX)/doc/fpc-$(FPC_VERSION)/examples/$(PACKAGE_NAME)
            endif
        else
            INSTALL_EXAMPLEDIR := $(INSTALL_PREFIX)/share/examples/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
        endif
    else
        INSTALL_EXAMPLEDIR := $(INSTALL_BASEDIR)/examples/$(PACKAGE_NAME)
    endif
endif

# Set data and shared directories
ifndef INSTALL_DATADIR
    INSTALL_DATADIR = $(INSTALL_BASEDIR)
endif
ifndef INSTALL_SHAREDDIR
    INSTALL_SHAREDDIR = $(INSTALL_PREFIX)/lib
endif

# Set cross-compile settings
ifdef CROSSCOMPILE
    ifndef CROSSBINDIR
        CROSSBINDIR := $(wildcard $(CROSSTARGETDIR)/bin/$(SOURCESUFFIX))
        ifeq ($(CROSSBINDIR),)
            CROSSBINDIR := $(wildcard $(INSTALL_BASEDIR)/cross/$(TARGETSUFFIX)/bin/$(FULL_SOURCE))
        endif
    endif
else
    CROSSBINDIR =
endif

# Define file extensions for different operating systems
BATCHEXT = .bat
LOADEREXT = .as
EXEEXT = .exe
PPLEXT = .ppl
PPUEXT = .ppu
OEXT = .o
ASMEXT = .s
SMARTEXT = .sl
STATICLIBEXT = .a
SHAREDLIBEXT = .so
SHAREDLIBPREFIX = libfp
STATICLIBPREFIX = libp
IMPORTLIBPREFIX = libimp
RSTEXT = .rst
EXEDBGEXT = .dbg

# OS-specific settings
ifeq ($(OS_TARGET),go32v1)
    STATICLIBPREFIX =
    SHORTSUFFIX = v1
endif

ifeq ($(OS_TARGET),go32v2)
    STATICLIBPREFIX =
    SHORTSUFFIX = dos
    IMPORTLIBPREFIX =
endif

ifeq ($(OS_TARGET),watcom)
    STATICLIBPREFIX =
    OEXT = .obj
    ASMEXT = .asm
    SHAREDLIBEXT = .dll
    SHORTSUFFIX = wat
    IMPORTLIBPREFIX =
endif

# Android, Linux, BSD, and other Unix-like systems
ifneq ($(CPU_TARGET),jvm)
    ifeq ($(OS_TARGET),android)
        BATCHEXT = .sh
        EXEEXT =
        HASSHAREDLIB = 1
        SHORTSUFFIX = lnx
    endif
endif

ifeq ($(OS_TARGET),linux)
    BATCHEXT = .sh
    EXEEXT =
    HASSHAREDLIB = 1
    SHORTSUFFIX = lnx
endif

ifeq ($(OS_TARGET),dragonfly)
    BATCHEXT = .sh
    EXEEXT =
    HASSHAREDLIB = 1
    SHORTSUFFIX = df
endif

ifeq ($(OS_TARGET),freebsd)
    BATCHEXT = .sh
    EXEEXT =
    HASSHAREDLIB = 1
    SHORTSUFFIX = fbs
endif

ifeq ($(OS_TARGET),netbsd)
    BATCHEXT = .sh
    EXEEXT =
    HASSHAREDLIB = 1
    SHORTSUFFIX = nbs
endif

ifeq ($(OS_TARGET),openbsd)
    BATCHEXT = .sh
    EXEEXT =
    HASSHAREDLIB = 1
    SHORTSUFFIX = obs
endif

# Windows and OS/2
ifeq ($(OS_TARGET),win32)
    SHAREDLIBEXT = .dll
    SHORTSUFFIX = w32
endif

ifeq ($(OS_TARGET),os2)
    BATCHEXT = .cmd
    AOUTEXT = .out
    STATICLIBPREFIX =
    SHAREDLIBEXT = .dll
    SHORTSUFFIX = os2
    ECHO = echo
    IMPORTLIBPREFIX =
endif

# Define additional OS-specific settings (e.g., Amiga, Atari, BeOS, Haiku, Solaris)
ifeq ($(OS_TARGET),amiga)
    EXEEXT =
    SHAREDLIBEXT = .library
    SHORTSUFFIX = amg
endif

ifeq ($(OS_TARGET),beos)
    BATCHEXT = .sh
    EXEEXT =
    SHORTSUFFIX = be
endif

ifeq ($(OS_TARGET),haiku)
    BATCHEXT = .sh
    EXEEXT =
    SHORTSUFFIX = hai
endif

ifeq ($(OS_TARGET),aix)
    BATCHEXT = .sh
    EXEEXT =
    SHAREDLIBEXT = .a
    SHORTSUFFIX = aix
endif

ifeq ($(OS_TARGET),java)
    OEXT = .class
    ASMEXT = .j
    SHAREDLIBEXT = .jar
    SHORTSUFFIX = java
endif

ifeq ($(OS_TARGET),msdos)
    STATICLIBPREFIX =
    STATICLIBEXT = .a
    SHORTSUFFIX = d16
endif

ifeq ($(OS_TARGET),embedded)
    ifeq ($(CPU_TARGET),i8086)
        STATICLIBPREFIX =
        STATICLIBEXT = .a
    else
        EXEEXT = .bin
    endif
    SHORTSUFFIX = emb
endif

ifeq ($(OS_TARGET),win16)
    STATICLIBPREFIX =
    STATICLIBEXT = .a
    SHAREDLIBEXT = .dll
    SHORTSUFFIX = w16
endif
ifneq ($(findstring $(OS_SOURCE),$(LIMIT83fs)),)
FPCMADE=fpcmade.$(SHORTSUFFIX)
ZIPSUFFIX=$(SHORTSUFFIX)
ZIPCROSSPREFIX=
ZIPSOURCESUFFIX=src
ZIPEXAMPLESUFFIX=exm
else
FPCMADE=fpcmade.$(TARGETSUFFIX)
ZIPSOURCESUFFIX=.source
ZIPEXAMPLESUFFIX=.examples
ifdef CROSSCOMPILE
ZIPSUFFIX=.$(SOURCESUFFIX)
ZIPCROSSPREFIX=$(TARGETSUFFIX)-
else
ZIPSUFFIX=.$(TARGETSUFFIX)
ZIPCROSSPREFIX=
endif
endif
ifndef ECHO
ECHO:=$(strip $(wildcard $(addsuffix /gecho$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ECHO),)
ECHO:=$(strip $(wildcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ECHO),)
ECHO= __missing_command_ECHO
else
ECHO:=$(firstword $(ECHO))
endif
else
ECHO:=$(firstword $(ECHO))
endif
endif
export ECHO
ifndef DATE
DATE:=$(strip $(wildcard $(addsuffix /gdate$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(DATE),)
DATE:=$(strip $(wildcard $(addsuffix /date$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(DATE),)
DATE= __missing_command_DATE
else
DATE:=$(firstword $(DATE))
endif
else
DATE:=$(firstword $(DATE))
endif
endif
export DATE
ifndef GINSTALL
GINSTALL:=$(strip $(wildcard $(addsuffix /ginstall$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(GINSTALL),)
GINSTALL:=$(strip $(wildcard $(addsuffix /install$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(GINSTALL),)
GINSTALL= __missing_command_GINSTALL
else
GINSTALL:=$(firstword $(GINSTALL))
endif
else
GINSTALL:=$(firstword $(GINSTALL))
endif
endif
export GINSTALL
ifndef CPPROG
CPPROG:=$(strip $(wildcard $(addsuffix /cp$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(CPPROG),)
CPPROG= __missing_command_CPPROG
else
CPPROG:=$(firstword $(CPPROG))
endif
endif

export CPPROG
ifndef RMPROG
RMPROG:=$(strip $(wildcard $(addsuffix /rm$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(RMPROG),)
RMPROG= __missing_command_RMPROG
else
RMPROG:=$(firstword $(RMPROG))
endif
endif
export RMPROG
ifndef MVPROG
MVPROG:=$(strip $(wildcard $(addsuffix /mv$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(MVPROG),)
MVPROG= __missing_command_MVPROG
else
MVPROG:=$(firstword $(MVPROG))
endif
endif
export MVPROG
ifndef MKDIRPROG
MKDIRPROG:=$(strip $(wildcard $(addsuffix /gmkdir$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(MKDIRPROG),)
MKDIRPROG:=$(strip $(wildcard $(addsuffix /mkdir$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(MKDIRPROG),)
MKDIRPROG= __missing_command_MKDIRPROG
else
MKDIRPROG:=$(firstword $(MKDIRPROG))
endif
else
MKDIRPROG:=$(firstword $(MKDIRPROG))
endif
endif
export MKDIRPROG
ifndef ECHOREDIR
ifndef inUnix
ECHOREDIR=echo
else
ECHOREDIR=$(ECHO)
endif
endif
ifndef COPY
COPY:=$(CPPROG) -fp
endif
ifndef COPYTREE
COPYTREE:=$(CPPROG) -Rfp
endif
ifndef MKDIRTREE
MKDIRTREE:=$(MKDIRPROG) -p
endif
ifndef MOVE
MOVE:=$(MVPROG) -f
endif
ifndef DEL
DEL:=$(RMPROG) -f
endif
ifndef DELTREE
DELTREE:=$(RMPROG) -rf
endif
ifndef INSTALL
ifdef inUnix
INSTALL:=$(GINSTALL) -c -m 644
else
INSTALL:=$(COPY)
endif
endif
ifndef INSTALLEXE
ifdef inUnix
INSTALLEXE:=$(GINSTALL) -c -m 755
else
INSTALLEXE:=$(COPY)
endif
endif
ifndef MKDIR
MKDIR:=$(GINSTALL) -m 755 -d
endif
export ECHOREDIR COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MKDIR
ifndef PPUMOVE
PPUMOVE:=$(strip $(wildcard $(addsuffix /ppumove$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(PPUMOVE),)
PPUMOVE= __missing_command_PPUMOVE
else
PPUMOVE:=$(firstword $(PPUMOVE))
endif
endif
export PPUMOVE
ifndef FPCMAKE
FPCMAKE:=$(strip $(wildcard $(addsuffix /fpcmake$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(FPCMAKE),)
FPCMAKE= __missing_command_FPCMAKE
else
FPCMAKE:=$(firstword $(FPCMAKE))
endif
endif
export FPCMAKE
ifndef ZIPPROG
ZIPPROG:=$(strip $(wildcard $(addsuffix /zip$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ZIPPROG),)
ZIPPROG= __missing_command_ZIPPROG
else
ZIPPROG:=$(firstword $(ZIPPROG))
endif
endif
export ZIPPROG
ifndef TARPROG
TARPROG:=$(strip $(wildcard $(addsuffix /gtar$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(TARPROG),)
TARPROG:=$(strip $(wildcard $(addsuffix /tar$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(TARPROG),)
TARPROG= __missing_command_TARPROG
else
TARPROG:=$(firstword $(TARPROG))
endif
else
TARPROG:=$(firstword $(TARPROG))
endif
endif
export TARPROG
ASNAME=$(BINUTILSPREFIX)as
LDNAME=$(BINUTILSPREFIX)ld
ARNAME=$(BINUTILSPREFIX)ar
RCNAME=$(BINUTILSPREFIX)rc
NASMNAME=$(BINUTILSPREFIX)nasm
ifndef ASPROG
ifdef CROSSBINDIR
ASPROG=$(CROSSBINDIR)/$(ASNAME)$(SRCEXEEXT)
else
ASPROG=$(ASNAME)
endif
endif
ifndef LDPROG
ifdef CROSSBINDIR
LDPROG=$(CROSSBINDIR)/$(LDNAME)$(SRCEXEEXT)
else
LDPROG=$(LDNAME)
endif
endif
ifndef RCPROG
ifdef CROSSBINDIR
RCPROG=$(CROSSBINDIR)/$(RCNAME)$(SRCEXEEXT)
else
RCPROG=$(RCNAME)
endif
endif
ifndef ARPROG
ifdef CROSSBINDIR
ARPROG=$(CROSSBINDIR)/$(ARNAME)$(SRCEXEEXT)
else
ARPROG=$(ARNAME)
endif
endif
ifndef NASMPROG
ifdef CROSSBINDIR
NASMPROG=$(CROSSBINDIR)/$(NASMNAME)$(SRCEXEEXT)
else
NASMPROG=$(NASMNAME)
endif
endif
AS=$(ASPROG)
LD=$(LDPROG)
RC=$(RCPROG)
AR=$(ARPROG)
NASM=$(NASMPROG)
ifdef inUnix
PPAS=./ppas$(SRCBATCHEXT)
else
PPAS=ppas$(SRCBATCHEXT)
endif
ifdef inUnix
LDCONFIG=ldconfig
else
LDCONFIG=
endif
ifdef DATE
DATESTR:=$(shell $(DATE) +%Y%m%d)
else
DATESTR=
endif
ZIPOPT=-9
ZIPEXT=.zip
ifeq ($(USETAR),bz2)
TAROPT=vj
TAREXT=.tar.bz2
else
TAROPT=vz
TAREXT=.tar.gz
endif
ifndef NOCPUDEF
override FPCOPTDEF=$(ARCH)
endif
ifneq ($(OS_TARGET),$(OS_SOURCE))
override FPCOPT+=-T$(OS_TARGET)
endif
ifneq ($(CPU_TARGET),$(CPU_SOURCE))
override FPCOPT+=-P$(ARCH)
endif
ifeq ($(OS_SOURCE),openbsd)
override FPCOPT+=-FD$(NEW_BINUTILS_PATH)
override FPCMAKEOPT+=-FD$(NEW_BINUTILS_PATH)
override FPMAKE_BUILD_OPT+=-FD$(NEW_BINUTILS_PATH)
endif
ifndef CROSSBOOTSTRAP
ifneq ($(BINUTILSPREFIX),)
override FPCOPT+=-XP$(BINUTILSPREFIX)
endif
ifneq ($(BINUTILSPREFIX),)
override FPCOPT+=-Xr$(RLINKPATH)
endif
endif
ifndef CROSSCOMPILE
ifneq ($(BINUTILSPREFIX),)
override FPCMAKEOPT+=-XP$(BINUTILSPREFIX)
override FPMAKE_BUILD_OPT+=-XP$(BINUTILSPREFIX)
endif
endif
ifdef UNITDIR
override FPCOPT+=$(addprefix -Fu,$(UNITDIR))
endif
ifdef LIBDIR
override FPCOPT+=$(addprefix -Fl,$(LIBDIR))
endif
ifdef OBJDIR
override FPCOPT+=$(addprefix -Fo,$(OBJDIR))
endif
ifdef INCDIR
override FPCOPT+=$(addprefix -Fi,$(INCDIR))
endif
ifdef LINKSMART
override FPCOPT+=-XX
endif
ifdef CREATESMART
override FPCOPT+=-CX
endif
ifdef DEBUG
override FPCOPT+=-gl
override FPCOPTDEF+=DEBUG
endif
ifdef RELEASE
FPCCPUOPT:=-O2
override FPCOPT+=-Ur -Xs $(FPCCPUOPT) -n
override FPCOPTDEF+=RELEASE
endif
ifdef STRIP
override FPCOPT+=-Xs
endif
ifdef OPTIMIZE
override FPCOPT+=-O2
endif
ifdef VERBOSE
override FPCOPT+=-vwni
endif
ifdef COMPILER_OPTIONS
override FPCOPT+=$(COMPILER_OPTIONS)
endif
ifdef COMPILER_UNITDIR
override FPCOPT+=$(addprefix -Fu,$(COMPILER_UNITDIR))
endif
ifdef COMPILER_LIBRARYDIR
override FPCOPT+=$(addprefix -Fl,$(COMPILER_LIBRARYDIR))
endif
ifdef COMPILER_OBJECTDIR
override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))
endif
ifdef COMPILER_INCLUDEDIR
override FPCOPT+=$(addprefix -Fi,$(COMPILER_INCLUDEDIR))
endif
ifdef CROSSBINDIR
override FPCOPT+=-FD$(CROSSBINDIR)
endif
ifdef COMPILER_TARGETDIR
override FPCOPT+=-FE$(COMPILER_TARGETDIR)
ifeq ($(COMPILER_TARGETDIR),.)
override TARGETDIRPREFIX=
else
override TARGETDIRPREFIX=$(COMPILER_TARGETDIR)/
endif
endif
ifdef COMPILER_UNITTARGETDIR
override FPCOPT+=-FU$(COMPILER_UNITTARGETDIR)
ifeq ($(COMPILER_UNITTARGETDIR),.)
override UNITTARGETDIRPREFIX=
else
override UNITTARGETDIRPREFIX=$(COMPILER_UNITTARGETDIR)/
endif
else
ifdef COMPILER_TARGETDIR
override COMPILER_UNITTARGETDIR=$(COMPILER_TARGETDIR)
override UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)
endif
endif
ifdef CREATESHARED
override FPCOPT+=-Cg
endif
ifneq ($(findstring $(OS_TARGET),dragonfly freebsd openbsd netbsd linux solaris),)
ifneq ($(findstring $(CPU_TARGET),x86_64 mips mipsel),)
override FPCOPT+=-Cg
endif
endif
ifdef LINKSHARED
endif
ifdef OPT
override FPCOPT+=$(OPT)
endif
ifdef FPMAKEBUILDOPT
override FPMAKE_BUILD_OPT+=$(FPMAKEBUILDOPT)
endif
ifdef FPCOPTDEF
override FPCOPT+=$(addprefix -d,$(FPCOPTDEF))
endif
ifdef CFGFILE
override FPCOPT+=@$(CFGFILE)
endif
ifdef USEENV
override FPCEXTCMD:=$(FPCOPT)
override FPCOPT:=!FPCEXTCMD
export FPCEXTCMD
endif
override AFULL_TARGET=$(CPU_TARGET)-$(OS_TARGET)
override AFULL_SOURCE=$(CPU_SOURCE)-$(OS_SOURCE)
ifneq ($(AFULL_TARGET),$(AFULL_SOURCE))
override ACROSSCOMPILE=1
endif
ifdef ACROSSCOMPILE
override FPCOPT+=$(CROSSOPT)
endif
override COMPILER:=$(strip $(FPC) $(FPCOPT))
ifneq (,$(findstring -sh ,$(COMPILER)))
UseEXECPPAS=1
endif
ifneq (,$(findstring -s ,$(COMPILER)))
ifeq ($(FULL_SOURCE),$(FULL_TARGET))
UseEXECPPAS=1
endif
endif
ifneq ($(UseEXECPPAS),1)
EXECPPAS=
else
ifdef RUNBATCH
EXECPPAS:=@$(RUNBATCH) $(PPAS)
else
EXECPPAS:=@$(PPAS)
endif
endif
ifdef TARGET_RSTS
override RSTFILES=$(addsuffix $(RSTEXT),$(TARGET_RSTS))
override CLEANRSTFILES+=$(RSTFILES)
endif
.PHONY: fpc_clean fpc_cleanall fpc_distclean
ifdef EXEFILES
override CLEANEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEFILES))
override CLEANEXEDBGFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEDBGFILES))
endif
ifdef CLEAN_PROGRAMS
override CLEANEXEFILES+=$(addprefix $(TARGETDIRPREFIX),$(addsuffix $(EXEEXT), $(CLEAN_PROGRAMS)))
override CLEANEXEDBGFILES+=$(addprefix $(TARGETDIRPREFIX),$(addsuffix $(EXEDBGEXT), $(CLEAN_PROGRAMS)))
endif
ifdef CLEAN_UNITS
override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UNITS))
endif
ifdef CLEANPPUFILES
override CLEANPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILES)) $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(CLEANPPUFILES))) $(addprefix $(IMPORTLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(CLEANPPUFILES)))
ifdef DEBUGSYMEXT
override CLEANPPULINKFILES+=$(subst $(PPUEXT),$(DEBUGSYMEXT),$(CLEANPPUFILES))
endif
override CLEANPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPPUFILES))
override CLEANPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPPULINKFILES)))
endif
fpc_clean: $(CLEANTARGET)
ifdef CLEANEXEFILES
	-$(DEL) $(CLEANEXEFILES)
endif
ifdef CLEANEXEDBGFILES
	-$(DELTREE) $(CLEANEXEDBGFILES)
endif
ifdef CLEANPPUFILES
	-$(DEL) $(CLEANPPUFILES)
endif
ifneq ($(CLEANPPULINKFILES),)
	-$(DEL) $(CLEANPPULINKFILES)
endif
ifdef CLEANRSTFILES
	-$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))
endif
ifdef CLEAN_FILES
	-$(DEL) $(CLEAN_FILES)
endif
ifdef LIB_NAME
	-$(DEL) $(LIB_NAME) $(LIB_FULLNAME)
endif
	-$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res link.res $(FPCEXTFILE) $(REDIRFILE)
	-$(DEL) *$(ASMEXT) *_ppas$(BATCHEXT) ppas$(BATCHEXT) ppaslink$(BATCHEXT)
fpc_cleanall: $(CLEANTARGET)
ifdef CLEANEXEFILES
	-$(DEL) $(CLEANEXEFILES)
endif
ifdef COMPILER_UNITTARGETDIR
ifdef CLEANPPUFILES
	-$(DEL) $(CLEANPPUFILES)
endif
ifneq ($(CLEANPPULINKFILES),)
	-$(DEL) $(CLEANPPULINKFILES)
endif
ifdef CLEANRSTFILES
	-$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))
endif
endif
ifdef CLEAN_FILES
	-$(DEL) $(CLEAN_FILES)
endif
	-$(DELTREE) units
	-$(DELTREE) bin
	-$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)
ifneq ($(PPUEXT),.ppu)
	-$(DEL) *.o *.ppu *.a
endif
	-$(DELTREE) *$(SMARTEXT)
	-$(DEL) fpcmade.* Package.fpc $(PPAS) script.res link.res $(FPCEXTFILE) $(REDIRFILE)
	-$(DEL) *_ppas$(BATCHEXT) ppas$(BATCHEXT) ppaslink$(BATCHEXT)
ifdef AOUTEXT
	-$(DEL) *$(AOUTEXT)
endif
ifdef DEBUGSYMEXT
	-$(DEL) *$(DEBUGSYMEXT)
endif
ifdef LOCALFPMAKEBIN
	-$(DEL) $(LOCALFPMAKEBIN)
	-$(DEL) $(FPMAKEBINOBJ)
endif
fpc_distclean: cleanall
.PHONY: fpc_baseinfo
override INFORULES+=fpc_baseinfo
fpc_baseinfo:
	@$(ECHO)
	@$(ECHO)  == Package info ==
	@$(ECHO)  Package Name..... $(PACKAGE_NAME)
	@$(ECHO)  Package Version.. $(PACKAGE_VERSION)
	@$(ECHO)
	@$(ECHO)  == Configuration info ==
	@$(ECHO)
	@$(ECHO)  FPC.......... $(FPC)
	@$(ECHO)  FPC Version.. $(FPC_VERSION)
	@$(ECHO)  Source CPU... $(CPU_SOURCE)
	@$(ECHO)  Target CPU... $(CPU_TARGET)
	@$(ECHO)  Source OS.... $(OS_SOURCE)
	@$(ECHO)  Target OS.... $(OS_TARGET)
	@$(ECHO)  Full Source.. $(FULL_SOURCE)
	@$(ECHO)  Full Target.. $(FULL_TARGET)
	@$(ECHO)  SourceSuffix. $(SOURCESUFFIX)
	@$(ECHO)  TargetSuffix. $(TARGETSUFFIX)
	@$(ECHO)  FPC fpmake... $(FPCFPMAKE)
	@$(ECHO)
	@$(ECHO)  == Directory info ==
	@$(ECHO)
	@$(ECHO)  Required pkgs... $(REQUIRE_PACKAGES)
	@$(ECHO)
	@$(ECHO)  Basedir......... $(BASEDIR)
	@$(ECHO)  FPCDir.......... $(FPCDIR)
	@$(ECHO)  CrossBinDir..... $(CROSSBINDIR)
	@$(ECHO)  UnitsDir........ $(UNITSDIR)
	@$(ECHO)  PackagesDir..... $(PACKAGESDIR)
	@$(ECHO)
	@$(ECHO)  GCC library..... $(GCCLIBDIR)
	@$(ECHO)  Other library... $(OTHERLIBDIR)
	@$(ECHO)
	@$(ECHO)  == Tools info ==
	@$(ECHO)
	@$(ECHO)  As........ $(AS)
	@$(ECHO)  Ld........ $(LD)
	@$(ECHO)  Ar........ $(AR)
	@$(ECHO)  Rc........ $(RC)
	@$(ECHO)
	@$(ECHO)  Mv........ $(MVPROG)
	@$(ECHO)  Cp........ $(CPPROG)
	@$(ECHO)  Rm........ $(RMPROG)
	@$(ECHO)  GInstall.. $(GINSTALL)
	@$(ECHO)  Echo...... $(ECHO)
	@$(ECHO)  Shell..... $(SHELL)
	@$(ECHO)  Date...... $(DATE)
	@$(ECHO)  FPCMake... $(FPCMAKE)
	@$(ECHO)  PPUMove... $(PPUMOVE)
	@$(ECHO)  Zip....... $(ZIPPROG)
	@$(ECHO)
	@$(ECHO)  == Object info ==
	@$(ECHO)
	@$(ECHO)  Target Loaders........ $(TARGET_LOADERS)
	@$(ECHO)  Target Units.......... $(TARGET_UNITS)
	@$(ECHO)  Target Implicit Units. $(TARGET_IMPLICITUNITS)
	@$(ECHO)  Target Programs....... $(TARGET_PROGRAMS)
	@$(ECHO)  Target Dirs........... $(TARGET_DIRS)
	@$(ECHO)  Target Examples....... $(TARGET_EXAMPLES)
	@$(ECHO)  Target ExampleDirs.... $(TARGET_EXAMPLEDIRS)
	@$(ECHO)
	@$(ECHO)  Clean Units......... $(CLEAN_UNITS)
	@$(ECHO)  Clean Files......... $(CLEAN_FILES)
	@$(ECHO)
	@$(ECHO)  Install Units....... $(INSTALL_UNITS)
	@$(ECHO)  Install Files....... $(INSTALL_FILES)
	@$(ECHO)
	@$(ECHO)  == Install info ==
	@$(ECHO)
	@$(ECHO)  DateStr.............. $(DATESTR)
	@$(ECHO)  ZipName.............. $(ZIPNAME)
	@$(ECHO)  ZipPrefix............ $(ZIPPREFIX)
	@$(ECHO)  ZipCrossPrefix....... $(ZIPCROSSPREFIX)
	@$(ECHO)  ZipSuffix............ $(ZIPSUFFIX)
	@$(ECHO)  FullZipName.......... $(FULLZIPNAME)
	@$(ECHO)  Install FPC Package.. $(INSTALL_FPCPACKAGE)
	@$(ECHO)
	@$(ECHO)  Install base dir..... $(INSTALL_BASEDIR)
	@$(ECHO)  Install binary dir... $(INSTALL_BINDIR)
	@$(ECHO)  Install library dir.. $(INSTALL_LIBDIR)
	@$(ECHO)  Install units dir.... $(INSTALL_UNITDIR)
	@$(ECHO)  Install source dir... $(INSTALL_SOURCEDIR)
	@$(ECHO)  Install doc dir...... $(INSTALL_DOCDIR)
	@$(ECHO)  Install example dir.. $(INSTALL_EXAMPLEDIR)
	@$(ECHO)  Install data dir..... $(INSTALL_DATADIR)
	@$(ECHO)
	@$(ECHO)  Dist destination dir. $(DIST_DESTDIR)
	@$(ECHO)  Dist zip name........ $(DIST_ZIPNAME)
	@$(ECHO)
.PHONY: fpc_info
fpc_info: $(INFORULES)
.PHONY: fpc_makefile fpc_makefiles fpc_makefile_sub1 fpc_makefile_sub2 \
	fpc_makefile_dirs
fpc_makefile:
	$(FPCMAKE) -w -T$(OS_TARGET) Makefile.fpc
fpc_makefile_sub1:
ifdef TARGET_DIRS
	$(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGET_DIRS))
endif
ifdef TARGET_EXAMPLEDIRS
	$(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGET_EXAMPLEDIRS))
endif
fpc_makefile_sub2: $(addsuffix _makefile_dirs,$(TARGET_DIRS) $(TARGET_EXAMPLEDIRS))
fpc_makefile_dirs: fpc_makefile_sub1 fpc_makefile_sub2
fpc_makefiles: fpc_makefile fpc_makefile_dirs
all:
debug:
smart:
release:
units:
examples:
shared:
install:
sourceinstall:
exampleinstall:
distinstall:
zipinstall:
zipsourceinstall:
zipexampleinstall:
zipdistinstall:
clean:
distclean:
cleanall:
info: fpc_info
makefiles: fpc_makefiles
.PHONY: all debug smart release units examples shared install sourceinstall exampleinstall distinstall zipinstall zipsourceinstall zipexampleinstall zipdistinstall clean distclean cleanall info makefiles
ifneq ($(wildcard fpcmake.loc),)
include fpcmake.loc
endif
