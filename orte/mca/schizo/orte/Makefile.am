#
# Copyright (c) 2016      Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
          schizo_orte_component.c \
          schizo_orte.h \
          schizo_orte.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_schizo_orte_DSO
component_noinst =
component_install = mca_schizo_orte.la
else
component_noinst = libmca_schizo_orte.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_schizo_orte_la_SOURCES = $(sources)
mca_schizo_orte_la_LDFLAGS = -module -avoid-version
mca_schizo_orte_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_schizo_orte_la_SOURCES = $(sources)
libmca_schizo_orte_la_LDFLAGS = -module -avoid-version

