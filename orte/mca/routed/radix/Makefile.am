#
# Copyright (c) 2007      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	routed_radix.h \
	routed_radix.c \
	routed_radix_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_routed_radix_DSO
component_noinst =
component_install = mca_routed_radix.la
else
component_noinst = libmca_routed_radix.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_routed_radix_la_SOURCES = $(sources)
mca_routed_radix_la_LDFLAGS = -module -avoid-version
mca_routed_radix_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_routed_radix_la_SOURCES = $(sources)
libmca_routed_radix_la_LDFLAGS = -module -avoid-version

