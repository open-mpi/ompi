#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# we do not want -I$(srcdir) in DEFAULT_INCLUDES
# otherwise there can be a conflict between system hwloc.h and opal/mca/hwloc/hwloc.h
# so just hack DEFAULT_INCLUDES with only what we need
DEFAULT_INCLUDES = -I$(top_builddir)/opal/include

# main library setup
noinst_LTLIBRARIES = libmca_hwloc.la
libmca_hwloc_la_SOURCES =

# local files
headers = hwloc.h
libmca_hwloc_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
