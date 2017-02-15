#
# Copyright (c) 2011-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016 -2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_leia.la
libmca_leia_la_SOURCES =

# local files
headers = leia.h
libmca_leia_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
