#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_portals_DSO
component_noinst =
component_install = mca_btl_portals.la
else
component_noinst = libmca_btl_portals.la
component_install =
endif

EXTRA_DIST = \
	btl_portals_compat_utcp.c \
	btl_portals_compat_redstorm.c

portals_SOURCES = \
	btl_portals.h \
	btl_portals_compat.h \
	btl_portals_endpoint.h \
	btl_portals_frag.h \
	btl_portals_send.h \
	btl_portals_recv.h \
	btl_portals.c \
	btl_portals_component.c \
	btl_portals_frag.c \
	btl_portals_send.c \
	btl_portals_recv.c \
	btl_portals_rdma.c

portals_nodist_SOURCES = \
	btl_portals_compat.c

AM_CPPFLAGS = $(btl_portals_CPPFLAGS)

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_portals_la_SOURCES = $(portals_SOURCES)
nodist_mca_btl_portals_la_SOURCES = $(portals_nodist_SOURCES)
mca_btl_portals_la_LIBADD = $(btl_portals_LIBS)
mca_btl_portals_la_LDFLAGS = -module -avoid-version $(btl_portals_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_portals_la_SOURCES = $(portals_SOURCES)
nodist_libmca_btl_portals_la_SOURCES = $(portals_nodist_SOURCES)
libmca_btl_portals_la_LIBADD = $(btl_portals_LIBS)
libmca_btl_portals_la_LDFLAGS = -module -avoid-version $(btl_portals_LDFLAGS)

dist-clean-local:
	rm -f btl_portals_compat.c
