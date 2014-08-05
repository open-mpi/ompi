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

AM_CPPFLAGS = $(grpcomm_brks_CPPFLAGS)

sources = \
	grpcomm_brks.h \
	grpcomm_brks.c \
	grpcomm_brks_component.c

# Make the output library in this brksory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_grpcomm_brks_DSO
component_noinst =
component_install = mca_grpcomm_brks.la
else
component_noinst = libmca_grpcomm_brks.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_grpcomm_brks_la_SOURCES = $(sources)
mca_grpcomm_brks_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_grpcomm_brks_la_SOURCES =$(sources)
libmca_grpcomm_brks_la_LDFLAGS = -module -avoid-version
