#
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        dfs_test.h \
        dfs_test_component.c \
        dfs_test.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_dfs_test_DSO
component_noinst =
component_install = mca_dfs_test.la
else
component_noinst = libmca_dfs_test.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_dfs_test_la_SOURCES = $(sources)
mca_dfs_test_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_dfs_test_la_SOURCES =$(sources)
libmca_dfs_test_la_LDFLAGS = -module -avoid-version
