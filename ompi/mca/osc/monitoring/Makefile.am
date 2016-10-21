#
# Copyright (c) 2016 Inria.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

monitoring_sources  = \
	osc_monitoring.h \
	osc_monitoring_comm.c \
	osc_monitoring_component.c \
	osc_monitoring_accumulate.c \
	osc_monitoring_passive_target.c \
	osc_monitoring_active_target.c \
	osc_monitoring_dynamic.c \
	osc_monitoring_module.c

if MCA_BUILD_ompi_osc_monitoring_DSO
component_noinst =
component_install = mca_osc_monitoring.la
else
component_noinst = libmca_osc_monitoring.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_monitoring_la_SOURCES = $(monitoring_sources)
mca_osc_monitoring_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_monitoring_la_SOURCES = $(monitoring_sources)
libmca_osc_monitoring_la_LDFLAGS = -module -avoid-version
