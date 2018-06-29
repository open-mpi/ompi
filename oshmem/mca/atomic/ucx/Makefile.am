#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = ../../spml/ucx

ucx_sources = \
	atomic_ucx.h \
	atomic_ucx_module.c \
	atomic_ucx_component.c \
	atomic_ucx_fadd.c \
	atomic_ucx_cswap.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_oshmem_atomic_ucx_DSO
component_noinst =
component_install = mca_atomic_ucx.la
else
component_noinst = libmca_atomic_ucx.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_atomic_ucx_la_CFLAGS = $(atomic_ucx_CFLAGS)
mca_atomic_ucx_la_CPPFLAGS = $(atomic_ucx_CPPFLAGS)
mca_atomic_ucx_la_SOURCES = $(ucx_sources)
mca_atomic_ucx_la_LIBADD = $(top_builddir)/oshmem/liboshmem.la \
	$(atomic_ucx_LIBS) \
	$(top_builddir)/oshmem/mca/spml/libmca_spml.la \
	$(top_builddir)/oshmem/mca/spml/ucx/mca_spml_ucx.la
mca_atomic_ucx_la_LDFLAGS = -module -avoid-version $(atomic_ucx_LDFLAGS)
mca_atomic_ucx_la_DEPENDENCIES = \
	$(top_builddir)/oshmem/mca/spml/libmca_spml.la \
	$(top_builddir)/oshmem/mca/spml/ucx/mca_spml_ucx.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_atomic_ucx_la_SOURCES =$(ucx_sources)
libmca_atomic_ucx_la_CFLAGS = $(atomic_ucx_CFLAGS)
libmca_atomic_ucx_la_CPPFLAGS = $(atomic_ucx_CPPFLAGS)
libmca_atomic_ucx_la_LDFLAGS = -module -avoid-version $(atomic_ucx_LDFLAGS)

