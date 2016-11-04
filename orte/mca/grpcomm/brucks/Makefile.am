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

AM_CPPFLAGS = $(grpcomm_brucks_CPPFLAGS)

sources = \
	grpcomm_brucks.h \
	grpcomm_brucks_module.c \
	grpcomm_brucks_component.c

# Make the output library in this brucksory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_grpcomm_brucks_DSO
component_noinst =
component_install = mca_grpcomm_brucks.la
else
component_noinst = libmca_grpcomm_brucks.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_grpcomm_brucks_la_SOURCES = $(sources)
mca_grpcomm_brucks_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_grpcomm_brucks_la_SOURCES =$(sources)
libmca_grpcomm_brucks_la_LDFLAGS = -module -avoid-version
