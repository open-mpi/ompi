# -*- shell-script -*-
#
#
# Copyright (c) 2011 Mellanox Technologies. All rights reserved.
# Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
#

AM_CPPFLAGS = $(coll_hcoll_CPPFLAGS) -DCOLL_HCOLL_HOME=\"$(coll_hcoll_HOME)\" $(coll_hcoll_extra_CPPFLAGS)

coll_hcoll_sources = \
		coll_hcoll.h \
		coll_hcoll_debug.h \
		coll_hcoll_dtypes.h \
		coll_hcoll_module.c \
		coll_hcoll_component.c \
		coll_hcoll_rte.c \
		coll_hcoll_ops.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_hcoll_DSO
component_noinst =
component_install = mca_coll_hcoll.la
else
component_noinst = libmca_coll_hcoll.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES  = $(component_install)
mca_coll_hcoll_la_SOURCES = $(coll_hcoll_sources)
mca_coll_hcoll_la_LIBADD  = $(coll_hcoll_LIBS)
mca_coll_hcoll_la_LDFLAGS = -module -avoid-version $(coll_hcoll_LDFLAGS)

noinst_LTLIBRARIES           = $(component_noinst)
libmca_coll_hcoll_la_SOURCES = $(coll_hcoll_sources)
libmca_coll_hcoll_la_LIBADD  = $(coll_hcoll_LIBS)
libmca_coll_hcoll_la_LDFLAGS = -module -avoid-version $(coll_hcoll_LDFLAGS)

