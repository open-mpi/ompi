#
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_cid.la
libmca_cid_la_SOURCES =

# local files
headers = cid.h
libmca_cid_la_SOURCES += $(headers) $(nodist_headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
nobase_ompi_HEADERS = $(headers)
nobase_nodist_ompi_HEADERS = $(nodist_headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
