#
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        state_orted.h \
        state_orted_component.c \
        state_orted.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_state_orted_DSO
component_noinst =
component_install = mca_state_orted.la
else
component_noinst = libmca_state_orted.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_orted_la_SOURCES = $(sources)
mca_state_orted_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_orted_la_SOURCES =$(sources)
libmca_state_orted_la_LDFLAGS = -module -avoid-version
