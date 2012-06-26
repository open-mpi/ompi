#
# Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

# main library setup
noinst_LTLIBRARIES = libmca_stems.la
libmca_stems_la_SOURCES =

# local files
headers = stems.h
libmca_stems_la_SOURCES += $(headers)

# Manual pages
nodist_man_MANS = 
EXTRA_DIST = 

# Ensure that the man pages are rebuilt if the orca_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/orca/include/orca_config.h

# Conditionally install the header files
if WANT_INSTALL_HEADERS
orcadir = $(includedir)/openmpi/$(subdir)
nobase_orca_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h 
	rm -f $(nodist_man_MANS)
