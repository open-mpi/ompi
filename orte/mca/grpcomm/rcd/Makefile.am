#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(grpcomm_rcd_CPPFLAGS)

sources = \
	grpcomm_rcd.h \
	grpcomm_rcd.c \
	grpcomm_rcd_component.c

# Make the output library in this rcdory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_grpcomm_rcd_DSO
component_noinst =
component_install = mca_grpcomm_rcd.la
else
component_noinst = libmca_grpcomm_rcd.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_grpcomm_rcd_la_SOURCES = $(sources)
mca_grpcomm_rcd_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_grpcomm_rcd_la_SOURCES =$(sources)
libmca_grpcomm_rcd_la_LDFLAGS = -module -avoid-version
