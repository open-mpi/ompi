#
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_hook.la
libmca_hook_la_SOURCES =

# local files
headers = hook.h
libmca_hook_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
nobase_ompi_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
