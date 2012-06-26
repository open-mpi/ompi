#
# Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

AM_CPPFLAGS = \
    $(LTDLINCL)

dist_pkgdata_DATA = help-orca-stems-none.txt

sources = \
        stems_none.h \
        stems_none_component.c \
        stems_none_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orca_stems_none_DSO
component_noinst =
component_install = mca_stems_none.la
else
component_noinst = libmca_stems_none.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_stems_none_la_SOURCES = $(sources)
mca_stems_none_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_stems_none_la_SOURCES = $(sources)
libmca_stems_none_la_LDFLAGS = -module -avoid-version
