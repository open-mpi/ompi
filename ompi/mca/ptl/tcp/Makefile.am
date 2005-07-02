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

sources = \
	ptl_tcp.c \
	ptl_tcp.h \
    ptl_tcp_addr.h \
	ptl_tcp_component.c \
	ptl_tcp_peer.c \
	ptl_tcp_peer.h \
	ptl_tcp_proc.c \
	ptl_tcp_proc.h \
	ptl_tcp_recvfrag.c \
	ptl_tcp_recvfrag.h \
	ptl_tcp_sendfrag.c \
	ptl_tcp_sendfrag.h \
	ptl_tcp_sendreq.c \
	ptl_tcp_sendreq.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ptl_tcp_DSO
lib =
lib_sources = 
component = mca_ptl_tcp.la
component_sources = $(sources)
else
lib = libmca_ptl_tcp.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_ptl_tcp_la_SOURCES = $(component_sources)
mca_ptl_tcp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_ptl_tcp_la_SOURCES = $(lib_sources)
libmca_ptl_tcp_la_LDFLAGS = -module -avoid-version

