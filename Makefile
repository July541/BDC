ARCH=$(shell uname -m)
UNAME=$(shell uname | tr 'A-Z' 'a-z')

ifeq ($(UNAME), mingw64_nt)
BDC_BINARY=C:\cabal\bin\BDC.exe
else
BDC_BINARY=$(HOME)/.cabal/bin/BDC
endif
BDC_TAG=v$(shell sed -n 's/^version: *//p' *.cabal)
BDC_PACKAGE=BDC-$(BDC_TAG)-$(UNAME)-$(ARCH)

UPX_VERSION=3.94
UPX_NAME=upx-$(UPX_VERSION)-amd64_$(UNAME)
UPX_BINARY=$(HOME)/.cabal/bin/upx

ifeq ($(UNAME), linux)
ARCHIVE=tar.gz
ARCHIVE_CREATE=tar czf
ARCHIVE_EXTRACT=tar xvzf
else
ARCHIVE=zip
ARCHIVE_CREATE=zip -r
ARCHIVE_EXTRACT=unzip
endif

ifeq ($(UNAME), linux)
COMPRESS_BIN_DEPS=$(UPX_BINARY)
COMPRESS_BIN=upx
else
COMPRESS_BIN_DEPS=
COMPRESS_BIN=ls
endif

CABAL=cabal

# Default target.
.PHONY: build
build: $(BDC_BINARY)

# When we want to do a release.
.PHONY: artifact
artifact: $(BDC_PACKAGE).$(ARCHIVE)
	mkdir -p artifacts
	cp $(BDC_PACKAGE).$(ARCHIVE) artifacts/

$(BDC_PACKAGE).$(ARCHIVE): $(BDC_BINARY) $(COMPRESS_BIN_DEPS)
	mkdir -p $(BDC_PACKAGE)
	cp $(BDC_BINARY) $(BDC_PACKAGE)/
	$(COMPRESS_BIN) $(BDC_PACKAGE)/BDC
	cp ChangeLog.md $(BDC_PACKAGE)/
	cp LICENSE $(BDC_PACKAGE)/
	cp -r data $(BDC_PACKAGE)/
	$(ARCHIVE_CREATE) $(BDC_PACKAGE).$(ARCHIVE) $(BDC_PACKAGE)

$(BDC_BINARY):
	$(CABAL) install

# UPX is used to compress the resulting binary.  We currently don't use this on
# Mac OS.
$(UPX_BINARY):
	curl -Lo /tmp/$(UPX_NAME).tar.xz \
	    https://github.com/upx/upx/releases/download/v$(UPX_VERSION)/$(UPX_NAME).tar.xz
	cd /tmp && tar xf $(UPX_NAME).tar.xz
	mv /tmp/$(UPX_NAME)/upx $(UPX_BINARY)
	upx --version

.PHONY: test
test:
	cabal build --test
