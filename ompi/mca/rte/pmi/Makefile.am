#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(rte_pmi_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_rte_pmi_DSO
component_noinst =
component_install = mca_rte_pmi.la
else
component_noinst = libmca_rte_pmi.la
component_install =
endif

local_sources = \
	rte_pmi.h \
	rte_pmi_internal.h \
	rte_pmi_coll.c \
	rte_pmi_component.c \
	rte_pmi_name.c \
	rte_pmi_error.c \
	rte_pmi_db.c \
	rte_pmi_comm.c

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rte_pmi_la_SOURCES = $(local_sources)
mca_rte_pmi_la_LDFLAGS = -module -avoid-version $(rte_pmi_LDFLAGS)
mca_rte_pmi_la_LIBADD = $(rte_pmi_LIBS) \
	$(top_ompi_builddir)/orte/mca/common/pmi/libmca_common_pmi.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_rte_pmi_la_SOURCES = $(local_sources)
libmca_rte_pmi_la_LIBADD = $(rte_pmi_LIBS)
libmca_rte_pmi_la_LDFLAGS = -module -avoid-version $(rte_pmi_LDFLAGS)

