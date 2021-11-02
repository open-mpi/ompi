#
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      High Performance Computing Center Stuttgart,
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

if MCA_BUILD_ompi_part_persist_DSO
component_noinst =
component_install = mca_part_persist.la
else
component_noinst = libmca_part_persist.la
component_install =
endif

local_sources = \
	part_persist.c \
	part_persist.h \
	part_persist_component.c \
	part_persist_component.h \
	part_persist_recvreq.h \
	part_persist_recvreq.c \
	part_persist_request.h \
	part_persist_request.c \
	part_persist_sendreq.h \
	part_persist_sendreq.c 

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_part_persist_la_SOURCES = $(local_sources)
mca_part_persist_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(part_persist_LIBS)
mca_part_persist_la_LDFLAGS = -module -avoid-version $(part_persist_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_part_persist_la_SOURCES = $(local_sources)
libmca_part_persist_la_LIBADD = $(part_persist_LIBS)
libmca_part_persist_la_LDFLAGS = -module -avoid-version $(part_persist_LDFLAGS)

