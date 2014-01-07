#
# Copyright (c)      2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

# main library setup
noinst_LTLIBRARIES = libmca_sstore.la
libmca_sstore_la_SOURCES =

# local files
headers = sstore.h
libmca_sstore_la_SOURCES += $(headers)

# Manual pages
nodist_man_MANS = orte_sstore.7
EXTRA_DIST = $(nodist_man_MANS:.7=.7in)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ortedir = $(ompiincludedir)/$(subdir)
nobase_orte_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
	rm -f $(nodist_man_MANS)
