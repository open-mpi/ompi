#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(common_pmi_CPPFLAGS)

sources = common_pmi.h common_pmi.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_common_pmi_DSO
component_noinst =
component_install = mca_common_pmi.la
else
component_noinst = libmca_common_pmi.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_common_pmi_la_SOURCES = $(sources)
mca_common_pmi_la_LDFLAGS = -module -avoid-version $(common_pmi_LDFLAGS)
mca_common_pmi_la_LIBADD = $(common_pmi_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_common_pmi_la_SOURCES =$(sources)
libmca_common_pmi_la_LDFLAGS = -module -avoid-version $(common_pmi_LDFLAGS)
libmca_common_pmi_la_LIBADD = $(common_pmi_LIBS)
