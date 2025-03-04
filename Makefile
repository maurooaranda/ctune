# Copyright (C) 2019-2025 Mauro Aranda

# This file is part of ctune.

# ctune is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# ctune is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with ctune.  If not, see <https://www.gnu.org/licenses/>.

## Programs used.
EMACS = emacs
EMACSFLAGS = -batch -f batch-byte-compile
MAKEINFO ?= makeinfo

## Variables (some might not be used right now).
PACKAGE = ctune
PACKAGE_BUGREPORT = maurooaranda@gmail.com
PACKAGE_NAME = ctune
PACKAGE_STRING = ctune 0.2
PACKAGE_TARNAME = ctune-0.2
PACKAGE_URL = 
PACKAGE_VERSION = 0.2
PACKAGE_HTML_MANUAL_DIR = $(PACKAGE_NAME)$(PACKAGE_VERSION)-html-manual
DISTDIR = $(PACKAGE_TARNAME)
DISTFILES = COPYING README Makefile doclicense.texi docstyle.texi \
ctune.texi ctune.info ctune.el
MAKEHTML_FLAGS = --html --output=$(PACKAGE_HTML_MANUAL_DIR)

## Targets.

.PHONY: all info clean dist

all: ctune.elc

info: ctune.info

# TODO: dvi, pdf, ps?
html: ctune.html

ctune.elc: ctune.el
	$(EMACS) $(EMACSFLAGS) ctune.el

ctune.info: ctune.texi
	$(MAKEINFO) ctune.texi

ctune.html: ctune.texi
	$(MAKEINFO) $(MAKEHTML_FLAGS) ctune.texi

clean:
	-rm -f ctune.elc
	-rm -f ctune.info
	-rm -f $(PACKAGE_TARNAME).tar.gz
	-rm -f -r $(PACKAGE_HTML_MANUAL_DIR)

# We don't provide install target.  That would make us install in site-lisp
# and the user may not want to install system-wide.
# I can't figure out a way to install in a already set load-path directory,
# without messing with the init file, so leave it as it is.

dist: info
	mkdir --parents $(DISTDIR)
	cp --parents $(DISTFILES) $(DISTDIR)
	tar -cf $(PACKAGE_TARNAME).tar $(DISTDIR)
	rm -R $(DISTDIR)
	gzip $(PACKAGE_TARNAME).tar

check:
	$(EMACS) -batch	-L ./ -L ./tests -l ert -l ctune-tests -f ert-run-tests-batch-and-exit ./tests/output.log
