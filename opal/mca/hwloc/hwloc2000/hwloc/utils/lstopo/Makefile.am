# Copyright © 2009-2017 Inria.  All rights reserved.
# Copyright © 2009-2012, 2014 Université Bordeaux
# Copyright © 2009-2014 Cisco Systems, Inc.  All rights reserved.
#
# See COPYING in top-level directory.

# This makefile is only reached when building in standalone mode

AM_CFLAGS = $(HWLOC_CFLAGS)
# Add -I of utils/hwloc for misc.h
AM_CPPFLAGS = $(HWLOC_CPPFLAGS) -I$(top_srcdir)/utils/hwloc
AM_LDFLAGS = $(HWLOC_LDFLAGS)

LDADD = $(HWLOC_top_builddir)/hwloc/libhwloc.la

bin_PROGRAMS = \
        lstopo-no-graphics

EXTRA_DIST = \
        test-lstopo.output

lstopo_no_graphics_SOURCES = \
        lstopo.h \
        lstopo.c \
        lstopo-color.c \
        lstopo-draw.c \
        lstopo-fig.c \
        lstopo-ascii.c \
        lstopo-text.c \
        lstopo-xml.c

lstopo_no_graphics_CFLAGS = $(AM_CFLAGS)
lstopo_no_graphics_LDADD = $(LDADD) -lm $(HWLOC_TERMCAP_LIBS)

lstopo_SOURCES = $(lstopo_no_graphics_SOURCES)
lstopo_CPPFLAGS = $(AM_CPPFLAGS)
lstopo_LDADD = $(lstopo_no_graphics_LDADD)
if HWLOC_HAVE_CAIRO
bin_PROGRAMS += lstopo
lstopo_SOURCES += lstopo-cairo.c
lstopo_CPPFLAGS += -DLSTOPO_HAVE_GRAPHICS $(HWLOC_X11_CPPFLAGS)
lstopo_CFLAGS = $(lstopo_no_graphics_CFLAGS) $(HWLOC_CAIRO_CFLAGS)
lstopo_LDADD += $(HWLOC_CAIRO_LIBS) $(HWLOC_X11_LIBS)
endif
if HWLOC_HAVE_WINDOWS
bin_PROGRAMS += lstopo lstopo-win
lstopo_SOURCES += lstopo-windows.c
lstopo_CPPFLAGS += -DLSTOPO_HAVE_GRAPHICS
if HWLOC_HAVE_USER32
lstopo_LDADD += -luser32
endif
lstopo_win_SOURCES = $(lstopo_SOURCES)
lstopo_win_CPPFLAGS = $(lstopo_CPPFLAGS)
lstopo_win_CFLAGS = $(lstopo_CFLAGS) -mwindows
lstopo_win_LDADD = $(lstopo_LDADD)
endif

man1_pages = lstopo-no-graphics.1
EXTRA_DIST += $(man1_pages:.1=.1in)
nodist_man_MANS = $(man1_pages)

if !HWLOC_HAVE_MINGW32
TESTS = \
        test-lstopo.sh
endif !HWLOC_HAVE_MINGW32

if HWLOC_HAVE_CAIRO
# only installed when lstopo is built with Cairo/X11 support
# so that no terminal is required
APPLICATIONSdir = @datarootdir@/applications
dist_APPLICATIONS_DATA = lstopo.desktop
endif

SEDMAN = $(SED) -e 's/\#PACKAGE_NAME\#/@PACKAGE_NAME@/g' \
	  -e 's/\#PACKAGE_VERSION\#/@PACKAGE_VERSION@/g' \
	  -e 's/\#HWLOC_DATE\#/@HWLOC_RELEASE_DATE@/g'

.1in.1:
	@ echo Creating $@ man page...
	@ $(SEDMAN) \
	  > $@ < $<

install-exec-hook:
	rm -f $(DESTDIR)$(bindir)/hwloc-ls$(EXEEXT)
	cd $(DESTDIR)$(bindir) && $(LN_S) lstopo-no-graphics$(EXEEXT) hwloc-ls$(EXEEXT)
if !HWLOC_HAVE_WINDOWS
if !HWLOC_HAVE_CAIRO
	rm -f $(DESTDIR)$(bindir)/lstopo
	cd $(DESTDIR)$(bindir) && $(LN_S) lstopo-no-graphics$(EXEEXT) lstopo$(EXEEXT) || true
endif
endif

install-data-hook:
	rm -f $(DESTDIR)$(man1dir)/hwloc-ls.1
	cd $(DESTDIR)$(man1dir) && $(LN_S) lstopo-no-graphics.1 hwloc-ls.1
	rm -f $(DESTDIR)$(man1dir)/lstopo.1
	cd $(DESTDIR)$(man1dir) && $(LN_S) lstopo-no-graphics.1 lstopo.1

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/hwloc-ls$(EXEEXT)
if !HWLOC_HAVE_WINDOWS
if !HWLOC_HAVE_CAIRO
	rm -f $(DESTDIR)$(bindir)/lstopo$(EXEEXT)
endif
endif
	rm -f $(DESTDIR)$(man1dir)/hwloc-ls.1 $(DESTDIR)$(man1dir)/lstopo.1

distclean-local:
	rm -f $(nodist_man_MANS)
