#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(pubsub_pmi_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_pubsub_pmi_DSO
component_noinst =
component_install = mca_pubsub_pmi.la
else
component_noinst = libmca_pubsub_pmi.la
component_install =
endif

local_sources = \
	pubsub_pmi.c \
	pubsub_pmi.h \
	pubsub_pmi_component.c

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pubsub_pmi_la_SOURCES = $(local_sources)
mca_pubsub_pmi_la_LDFLAGS = -module -avoid-version $(pubsub_pmi_LDFLAGS)
mca_pubsub_pmi_la_LIBADD = $(pubsub_pmi_LIBS) \
	$(top_ompi_builddir)/opal/mca/common/pmi/libmca_common_pmi.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_pubsub_pmi_la_SOURCES = $(local_sources)
libmca_pubsub_pmi_la_LIBADD = $(pubsub_pmi_LIBS)
libmca_pubsub_pmi_la_LDFLAGS = -module -avoid-version $(pubsub_pmi_LDFLAGS)

