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

AM_CPPFLAGS = $(ptl_mx_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ptl_mx_DSO
component_noinst =
component_install = mca_ptl_mx.la
else
component_noinst = libmca_ptl_mx.la
component_install =
endif

ptl_mx_SOURCES = ptl_mx.c ptl_mx.h ptl_mx_component.c ptl_mx_module.c ptl_mx_module.h \
    ptl_mx_peer.c ptl_mx_peer.h ptl_mx_proc.c ptl_mx_proc.h \
    ptl_mx_recvfrag.c ptl_mx_recvfrag.h ptl_mx_sendfrag.c ptl_mx_sendfrag.h

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_mx_la_SOURCES = $(mx_SOURCES)
mca_ptl_mx_la_LIBADD = $(ptl_mx_LIBX)
mca_ptl_mx_la_LDFLAGS = -module -avoid-version $(ptl_mx_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_mx_la_SOURCES = $(ptl_mx_SOURCES)
libmca_ptl_mx_la_LIBADD = $(ptl_mx_LIBS)
libmca_ptl_mx_la_LDFLAGS = -module -avoid-version $(ptl_mx_LDFLAGS)
