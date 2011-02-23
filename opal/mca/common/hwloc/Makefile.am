#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS =
if OPAL_COMMON_HWLOC_INTERNAL
SUBDIRS += hwloc
endif

# Need to include these files so that these directories are carried in
# the tarball (in case someone invokes autogen.sh on a dist tarball).
EXTRA_DIST = \
        hwloc/doc/README.txt \
        hwloc/tests/README.txt \
        hwloc/utils/README.txt

# Headers and sources
headers = common_hwloc.h
sources = common_hwloc.c

# See a lengthy explanation of "common" component Makefile.ams in
# ompi/mca/common/sm/Makefile.am.

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = libmca_common_hwloc.la
comp_noinst = libmca_common_hwloc_noinst.la

if MCA_BUILD_opal_common_hwloc_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

libmca_common_hwloc_la_SOURCES = $(headers) $(sources)
libmca_common_hwloc_la_LDFLAGS = \
    -version-info $(libmca_opal_common_hwloc_so_version) \
    $(opal_common_hwloc_LDFLAGS)
libmca_common_hwloc_la_LIBADD = $(opal_common_hwloc_LIBS)

libmca_common_hwloc_noinst_la_SOURCES = $(headers) $(sources)
libmca_common_hwloc_noinst_la_LDFLAGS = $(opal_common_hwloc_LDFLAGS)
libmca_common_hwloc_noinst_la_LIBADD = $(opal_common_hwloc_LIBS)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/$(subdir)
ompi_HEADERS = $(headers)
endif

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically

all-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi
