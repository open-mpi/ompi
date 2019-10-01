#
# Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_pcompress.la
libmca_pcompress_la_SOURCES =

# local files
headers = pcompress.h
libmca_pcompress_la_SOURCES += $(headers)

# Ensure that the man pages are rebuilt if the pmix_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/pmix/include/pmix_config.h

# Conditionally install the header files
if WANT_INSTALL_HEADERS
pmixdir = $(pmixincludedir)/$(subdir)
nobase_pmix_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
	rm -f $(nodist_man_MANS)
