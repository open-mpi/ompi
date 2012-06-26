#
# Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

CFLAGS = $(stems_orte_CFLAGS)
AM_CPPFLAGS = $(stems_orte_CPPFLAGS)

dist_pkgdata_DATA = help-orca-stems-orte.txt

sources = \
        stems_orte.h \
        stems_orte_component.c \
        stems_orte_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orca_stems_orte_DSO
component_noinst =
component_install = mca_stems_orte.la
else
component_noinst = libmca_stems_orte.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_stems_orte_la_SOURCES = $(sources)
mca_stems_orte_la_LDFLAGS = -module -avoid-version $(stems_orte_LDFLAGS)
mca_stems_orte_la_LIBADD = $(stems_orte_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_stems_orte_la_SOURCES = $(sources)
libmca_stems_orte_la_LDFLAGS = -module -avoid-version $(stems_orte_LDFLAGS)
libmca_stems_orte_la_LIBADD = $(stems_orte_LIBS)

