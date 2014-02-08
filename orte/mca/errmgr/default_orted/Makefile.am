#
# Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        errmgr_default_orted.h \
        errmgr_default_orted_component.c \
        errmgr_default_orted.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_errmgr_default_orted_DSO
component_noinst =
component_install = mca_errmgr_default_orted.la
else
component_noinst = libmca_errmgr_default_orted.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_errmgr_default_orted_la_SOURCES = $(sources)
mca_errmgr_default_orted_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_errmgr_default_orted_la_SOURCES =$(sources)
libmca_errmgr_default_orted_la_LDFLAGS = -module -avoid-version
