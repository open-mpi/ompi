#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-state-staged.txt

sources = \
        state_staged.h \
        state_staged_component.c \
        state_staged.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_state_staged_DSO
component_noinst =
component_install = mca_state_staged.la
else
component_noinst = libmca_state_staged.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_staged_la_SOURCES = $(sources)
mca_state_staged_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_staged_la_SOURCES =$(sources)
libmca_state_staged_la_LDFLAGS = -module -avoid-version
