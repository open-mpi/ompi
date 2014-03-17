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

AM_CPPFLAGS = $(grpcomm_pmi_CPPFLAGS)

sources = \
	grpcomm_pmi.h \
	grpcomm_pmi_module.c \
	grpcomm_pmi_component.c

if WANT_PMI2_SUPPORT
sources += pmi2_pmap_parser.c
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_grpcomm_pmi_DSO
component_noinst =
component_install = mca_grpcomm_pmi.la
else
component_noinst = libmca_grpcomm_pmi.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_grpcomm_pmi_la_SOURCES = $(sources)
mca_grpcomm_pmi_la_LDFLAGS = -module -avoid-version $(grpcomm_pmi_LDFLAGS)
mca_grpcomm_pmi_la_LIBADD = $(grpcomm_pmi_LIBS) \
	$(top_ompi_builddir)/opal/mca/common/pmi/libmca_common_pmi.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_grpcomm_pmi_la_SOURCES =$(sources)
libmca_grpcomm_pmi_la_LDFLAGS = -module -avoid-version $(grpcomm_pmi_LDFLAGS)
libmca_grpcomm_pmi_la_LIBADD = $(grpcomm_pmi_LIBS)
