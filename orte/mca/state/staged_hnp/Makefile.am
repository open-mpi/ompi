#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-state-staged-hnp.txt

sources = \
        state_staged_hnp.h \
        state_staged_hnp_component.c \
        state_staged_hnp.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_state_staged_hnp_DSO
component_noinst =
component_install = mca_state_staged_hnp.la
else
component_noinst = libmca_state_staged_hnp.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_staged_hnp_la_SOURCES = $(sources)
mca_state_staged_hnp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_staged_hnp_la_SOURCES =$(sources)
libmca_state_staged_hnp_la_LDFLAGS = -module -avoid-version
