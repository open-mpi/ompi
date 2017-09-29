#
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_pool.la
libmca_pool_la_SOURCES =

# local files
headers = pool.h
libmca_pool_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
