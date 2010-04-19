# Copyright 2009 INRIA, Université Bordeaux 1
# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

LIBS += $(HWLOC_top_builddir)/src/libhwloc.la

EXTRA_DIST = test-hwloc-distrib.output

# Only build the utilities if we're building in standalone mode
if HWLOC_BUILD_UTILS
bin_PROGRAMS = lstopo hwloc-calc hwloc-bind hwloc-distrib
endif

lstopo_SOURCES = lstopo.h lstopo.c lstopo-color.c lstopo-text.c lstopo-draw.c lstopo-fig.c
if HWLOC_HAVE_CAIRO
lstopo_SOURCES += lstopo-cairo.c
endif
if HWLOC_HAVE_XML
lstopo_SOURCES += lstopo-xml.c
endif
if HWLOC_HAVE_WINDOWS
lstopo_SOURCES += lstopo-windows.c
endif
lstopo_CFLAGS = $(HWLOC_CAIRO_CFLAGS) $(HWLOC_XML_CFLAGS)
lstopo_LDADD = $(HWLOC_CAIRO_LIBS) $(HWLOC_XML_LIBS) -lm $(HWLOC_TERMCAP_LIBS) $(HWLOC_X11_LIBS)

hwloc_calc_SOURCES = hwloc-calc.c hwloc-calc.h

# Only run the tests if we're building standalone, because the tests
# call hwloc executables.
if HWLOC_BUILD_TESTS
if !HWLOC_HAVE_MINGW32
TESTS = test-hwloc-distrib.sh
endif !HWLOC_HAVE_MINGW32
endif HWLOC_BUILD_TESTS

# Only install man pages if we're building in standalone mode
if HWLOC_BUILD_UTILS
man_pages = hwloc.7 lstopo.1 hwloc-bind.1 hwloc-distrib.1 hwloc-calc.1
EXTRA_DIST += $(man_pages:.1=.1in)
nodist_man_MANS = $(man_pages)

.1in.1:
	@ echo Creating $@ man page...
	@ sed -e 's/#PACKAGE_NAME#/@PACKAGE_NAME@/g' \
	  -e 's/#PACKAGE_VERSION#/@PACKAGE_VERSION@/g' \
	  -e 's/#HWLOC_DATE#/@HWLOC_RELEASE_DATE@/g' \
	  > $@ < $<

.3in.3:
	@ echo Creating $@ man page...
	@ sed -e 's/#PACKAGE_NAME#/@PACKAGE_NAME@/g' \
	  -e 's/#PACKAGE_VERSION#/@PACKAGE_VERSION@/g' \
	  -e 's/#HWLOC_DATE#/@HWLOC_RELEASE_DATE@/g' \
	  > $@ < $<

.7in.7:
	@ echo Creating $@ man page...
	@ sed -e 's/#PACKAGE_NAME#/@PACKAGE_NAME@/g' \
	  -e 's/#PACKAGE_VERSION#/@PACKAGE_VERSION@/g' \
	  -e 's/#HWLOC_DATE#/@HWLOC_RELEASE_DATE@/g' \
	  > $@ < $<

install-exec-hook: install-man
	rm -f $(DESTDIR)$(bindir)/hwloc-ls$(EXEEXT) $(DESTDIR)$(bindir)/hwloc-info$(EXEEXT) $(DESTDIR)$(bindir)/hwloc-mask$(EXEEXT)
	cd $(DESTDIR)$(bindir) && $(LN_S) lstopo$(EXEEXT) hwloc-ls$(EXEEXT) && $(LN_S) lstopo$(EXEEXT) hwloc-info$(EXEEXT) && $(LN_S) hwloc-calc$(EXEEXT) hwloc-mask$(EXEEXT)
	rm -f $(DESTDIR)$(man1dir)/hwloc-ls.1 $(DESTDIR)$(man1dir)/hwloc-info.1 $(DESTDIR)$(man1dir)/hwloc-mask.1
	cd $(DESTDIR)$(man1dir) && $(LN_S) lstopo.1 hwloc-ls.1 && $(LN_S) lstopo.1 hwloc-info.1 && $(LN_S) hwloc-calc.1 hwloc-mask.1

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/hwloc-ls$(EXEEXT) $(DESTDIR)$(bindir)/hwloc-info$(EXEEXT) $(DESTDIR)$(bindir)/hwloc-mask$(EXEEXT)
	rm -f $(DESTDIR)$(man1dir)/hwloc-ls.1 $(DESTDIR)$(man1dir)/hwloc-info.1 $(DESTDIR)$(man1dir)/hwloc-mask.1

distclean-local:
	rm -f $(man_pages)
endif
