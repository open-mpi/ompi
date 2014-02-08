#
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        dfs_app.h \
        dfs_app_component.c \
        dfs_app.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_dfs_app_DSO
component_noinst =
component_install = mca_dfs_app.la
else
component_noinst = libmca_dfs_app.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_dfs_app_la_SOURCES = $(sources)
mca_dfs_app_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_dfs_app_la_SOURCES =$(sources)
libmca_dfs_app_la_LDFLAGS = -module -avoid-version
