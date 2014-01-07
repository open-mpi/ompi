#
# Copyright (c) 2013      Intel, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        errmgr_default_tool.h \
        errmgr_default_tool_component.c \
        errmgr_default_tool.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_errmgr_default_tool_DSO
component_noinst =
component_install = mca_errmgr_default_tool.la
else
component_noinst = libmca_errmgr_default_tool.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_errmgr_default_tool_la_SOURCES = $(sources)
mca_errmgr_default_tool_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_errmgr_default_tool_la_SOURCES =$(sources)
libmca_errmgr_default_tool_la_LDFLAGS = -module -avoid-version
