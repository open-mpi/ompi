#
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2024 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2020-2021 Sandia National Laboratories. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

EXTRA_DIST = post_configure.sh

if MCA_BUILD_ompi_part_persist_aggregated_DSO
component_noinst =
component_install = mca_part_persist_aggregated.la
else
component_noinst = libmca_part_persist_aggregated.la
component_install =
endif

local_sources = \
	part_persist_aggregated.c \
	part_persist_aggregated.h \
	part_persist_aggregated_component.c \
	part_persist_aggregated_component.h \
	part_persist_aggregated_recvreq.h \
	part_persist_aggregated_recvreq.c \
	part_persist_aggregated_request.h \
	part_persist_aggregated_request.c \
	part_persist_aggregated_sendreq.h \
	part_persist_aggregated_sendreq.c 

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_part_persist_aggregated_la_SOURCES = $(local_sources)
mca_part_persist_aggregated_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(part_persist_aggregated_LIBS)
mca_part_persist_aggregated_la_LDFLAGS = -module -avoid-version $(part_persist_aggregated_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_part_persist_aggregated_la_SOURCES = $(local_sources)
libmca_part_persist_aggregated_la_LIBADD = $(part_persist_aggregated_LIBS)
libmca_part_persist_aggregated_la_LDFLAGS = -module -avoid-version $(part_persist_aggregated_LDFLAGS)

