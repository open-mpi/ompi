#
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_rte.la
libmca_rte_la_SOURCES =

# local files
headers = rte.h
libmca_rte_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
nobase_ompi_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
