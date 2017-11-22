#
# Copyright (c) 2016-2018 Inria.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

monitoring_sources  = \
	osc_monitoring.h \
	osc_monitoring_comm.h \
	osc_monitoring_component.c \
	osc_monitoring_accumulate.h \
	osc_monitoring_passive_target.h \
	osc_monitoring_active_target.h \
	osc_monitoring_dynamic.h \
	osc_monitoring_module.h \
	osc_monitoring_template.h

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
mca_osc_monitoring_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
		$(OMPI_TOP_BUILDDIR)/ompi/mca/common/monitoring/libmca_common_monitoring.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_monitoring_la_SOURCES = $(monitoring_sources)
libmca_osc_monitoring_la_LDFLAGS = -module -avoid-version

DISTCLEANFILES = osc_monitoring_template_gen.h
