#
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


dist_ompidata_DATA = help-ompi-dpm-orte.txt

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_dpm_orte_DSO
component_noinst =
component_install = mca_dpm_orte.la
else
component_noinst = libmca_dpm_orte.la
component_install =
endif

local_sources = \
	dpm_orte.c \
	dpm_orte.h \
	dpm_orte_component.c

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_dpm_orte_la_SOURCES = $(local_sources)
mca_dpm_orte_la_LDFLAGS = -module -avoid-version $(dpm_orte_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_dpm_orte_la_SOURCES = $(local_sources)
libmca_dpm_orte_la_LIBADD = $(dpm_orte_LIBS)
libmca_dpm_orte_la_LDFLAGS = -module -avoid-version $(dpm_orte_LDFLAGS)

