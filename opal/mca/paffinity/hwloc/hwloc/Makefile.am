# Copyright © 2009      INRIA, Université Bordeaux 1
# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.

# Note that the -I directory must *exactly* match what was specified
# via AC_CONFIG_MACRO_DIR in configure.ac.
ACLOCAL_AMFLAGS = -I ./config

SUBDIRS = src utils tests include doc

# Only install the pkg file if we're building in standalone mode
if HWLOC_BUILD_STANDALONE
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = hwloc.pc
endif

#
# "make distcheck" requires that tarballs are able to be able to "make
# dist", so we have to include config/distscript.csh.
#

EXTRA_DIST = \
        README VERSION COPYING AUTHORS \
        config/hwloc_get_version.sh \
        config/distscript.csh

#
# Double check that we generated both the doxygen docs and a new copy
# of the top-level README file.
#

cannot-dist:
	@echo "ERROR: Did not build both of the doxygen docs and README."
	@echo "ERROR: This tarball is not complete!"
	@echo "ERROR: Cowardly refusing to complete successfully..."
	@exit 1

# Refuse to make dist if we can't make the doxygen stuff (note that
# BUILD_DOXYGEN will automatically be false if we're not building
# standalone).
if !HWLOC_BUILD_DOXYGEN
dist-hook: cannot-dist
else
if !HWLOC_BUILD_README
dist-hook: cannot-dist
else
dist-hook:
	csh "$(top_srcdir)/config/distscript.csh" "$(top_srcdir)" "$(distdir)" "$(HWLOC_VERSION)" "$(HWLOC_SVN_R)"
endif HWLOC_BUILD_README
endif HWLOC_BUILD_DOXYGEN

#
# Build the top-level README file
#

if HWLOC_BUILD_STANDALONE
.PHONY: doc readme
doc readme:
	$(MAKE) -C doc readme
endif HWLOC_BUILD_STANDALONE
