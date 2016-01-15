#
# Copyright (c) 2013-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2013-2015 Inria.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

monitoring_sources  = \
	pml_monitoring.c \
	pml_monitoring.h \
	pml_monitoring_comm.c \
	pml_monitoring_component.c \
	pml_monitoring_iprobe.c \
	pml_monitoring_irecv.c \
	pml_monitoring_isend.c \
	pml_monitoring_start.c

if MCA_BUILD_ompi_pml_monitoring_DSO
component_noinst =
component_install = mca_pml_monitoring.la
else
component_noinst = libmca_pml_monitoring.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_monitoring_la_SOURCES = $(monitoring_sources)
mca_pml_monitoring_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_monitoring_la_SOURCES = $(monitoring_sources)
libmca_pml_monitoring_la_LDFLAGS = -module -avoid-version
