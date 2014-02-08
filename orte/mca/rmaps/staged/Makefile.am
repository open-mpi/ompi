#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        rmaps_staged.c \
        rmaps_staged.h \
        rmaps_staged_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rmaps_staged_DSO
component_noinst =
component_install = mca_rmaps_staged.la
else
component_noinst = libmca_rmaps_staged.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmaps_staged_la_SOURCES = $(sources)
mca_rmaps_staged_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmaps_staged_la_SOURCES =$(sources)
libmca_rmaps_staged_la_LDFLAGS = -module -avoid-version
