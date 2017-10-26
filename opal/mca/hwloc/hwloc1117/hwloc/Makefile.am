# Copyright © 2009-2016 Inria.  All rights reserved.
# Copyright © 2009      Université Bordeaux
# Copyright © 2009-2014 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.

# Note that the -I directory must *exactly* match what was specified
# via AC_CONFIG_MACRO_DIR in configure.ac.
ACLOCAL_AMFLAGS = -I ./config

SUBDIRS = src include
if HWLOC_BUILD_STANDALONE
SUBDIRS += utils tests contrib/systemd
# We need doc/ if HWLOC_BUILD_DOXYGEN, or during make install if HWLOC_INSTALL_DOXYGEN.
# There's no INSTALL_SUBDIRS, so always enter doc/ and check HWLOC_BUILD/INSTALL_DOXYGEN there
SUBDIRS += doc
endif

# Do not let automake automatically add the non-standalone dirs to the
# distribution tarball if we're building in embedded mode.
DIST_SUBDIRS = $(SUBDIRS)

# Only install the pkg file if we're building in standalone mode (and not on Windows)
if HWLOC_BUILD_STANDALONE
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = hwloc.pc
endif

# Only install the valgrind suppressions file if we're building in standalone mode
if HWLOC_BUILD_STANDALONE
dist_pkgdata_DATA = contrib/hwloc-valgrind.supp
endif

#
# "make distcheck" requires that tarballs are able to be able to "make
# dist", so we have to include config/distscript.sh.
#
EXTRA_DIST = \
        README VERSION COPYING AUTHORS \
        config/hwloc_get_version.sh \
        config/distscript.sh

# Only install entire visual studio subdirectory if we're building in standalone mode
if HWLOC_BUILD_STANDALONE
EXTRA_DIST += contrib/windows
endif

if HWLOC_BUILD_STANDALONE
dist-hook:
	sh "$(top_srcdir)/config/distscript.sh" "$(top_srcdir)" "$(distdir)" "$(HWLOC_VERSION)"
endif HWLOC_BUILD_STANDALONE

#
# Build the documenation and top-level README file
#
if HWLOC_BUILD_STANDALONE
.PHONY: doc readme
doc readme:
	$(MAKE) -C doc
endif HWLOC_BUILD_STANDALONE

if HWLOC_BUILD_STANDALONE
if HWLOC_HAVE_WINDOWS
#
# Winball specific rules
#
install-data-local:
	sed -e 's/$$/'$$'\015'/ < $(srcdir)/README > $(DESTDIR)$(prefix)/README.txt
	sed -e 's/$$/'$$'\015'/ < $(srcdir)/NEWS > $(DESTDIR)$(prefix)/NEWS.txt
	sed -e 's/$$/'$$'\015'/ < $(srcdir)/COPYING > $(DESTDIR)$(prefix)/COPYING.txt
uninstall-local:
	rm -f $(DESTDIR)$(prefix)/README.txt $(DESTDIR)$(prefix)/NEWS.txt $(DESTDIR)$(prefix)/COPYING.txt
endif HWLOC_HAVE_WINDOWS
endif HWLOC_BUILD_STANDALONE
