#
# Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        grpcomm_mcast.h \
	grpcomm_mcast.c \
        grpcomm_mcast_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_grpcomm_mcast_DSO
component_noinst =
component_install = mca_grpcomm_mcast.la
else
component_noinst = libmca_grpcomm_mcast.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_grpcomm_mcast_la_SOURCES = $(sources)
mca_grpcomm_mcast_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_grpcomm_mcast_la_SOURCES =$(sources)
libmca_grpcomm_mcast_la_LDFLAGS = -module -avoid-version
