#
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_dfs.la
libmca_dfs_la_SOURCES =

# local files
headers = dfs.h dfs_types.h
libmca_dfs_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ortedir = $(ompiincludedir)/$(subdir)
nobase_orte_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
