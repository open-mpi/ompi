#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# A word of explanation...
#
# We link this component to common/sm/libmca_common_sm.la. This is
# because all shared-memory based components (e.g., mpool, ptl, etc.)
# need to share some common code and data.  There's two cases:
#
# 1. libmca_common_sm.la is a shared library.  By linking that shared
# library to all components that need it, the OS linker will
# automatically load it into the process as necessary, and there will
# only be one copy (i.e., all the components will share *one* copy of
# the code and data).
#
# 2. libmca_common_sm.la is a static library.  In this case, it will
# be rolled up into the top-level libmpi.la.  It will also be rolled
# into each component, but then the component will also be rolled up
# into the upper-level libmpi.la.  Linkers universally know how to
# "figure this out" so that we end up with only one copy of the code
# and data.

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
	mpool_sm.h \
	mpool_sm_module.c \
	mpool_sm_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_sm_DSO
component_noinst =
component_install = mca_mpool_sm.la
else
component_noinst = libmca_mpool_sm.la
component_install =
endif

common = $(top_ompi_builddir)/src/mca/common/sm/libmca_common_sm.la

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_sm_la_SOURCES = $(sources)
mca_mpool_sm_la_LDFLAGS = -module -avoid-version
mca_mpool_sm_la_LIBADD = $(common)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_sm_la_SOURCES = $(sources)
libmca_mpool_sm_la_LDFLAGS = -module -avoid-version
libmca_mpool_sm_la_LIBADD = $(common)
