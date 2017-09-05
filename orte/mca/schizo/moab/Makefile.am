#
# Copyright (c) 2016-2017 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS= $(schizo_moab_CPPFLAGS)

sources = \
          schizo_moab_component.c \
          schizo_moab.h \
          schizo_moab.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_schizo_moab_DSO
component_noinst =
component_install = mca_schizo_moab.la
else
component_noinst = libmca_schizo_moab.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_schizo_moab_la_SOURCES = $(sources)
mca_schizo_moab_la_LDFLAGS = -module -avoid-version $(schizo_moab_LDFLAGS)
mca_schizo_moab_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la \
	$(schizo_moab_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_schizo_moab_la_SOURCES = $(sources)
libmca_schizo_moab_la_LDFLAGS = -module -avoid-version $(schizo_moab_LDFLAGS)
libmca_schizo_moab_la_LIBADD = $(schizo_moab_LIBS)
