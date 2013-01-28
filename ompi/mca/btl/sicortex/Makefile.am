#
# Copyright (c) 2008-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_sicortex_CPPFLAGS)

btl_sicortex_sources = btl_sicortex.c btl_sicortex.h btl_sicortex_component.c btl_sicortex_endpoint.c \
    btl_sicortex_endpoint.h btl_sicortex_frag.c btl_sicortex_frag.h btl_sicortex_proc.c btl_sicortex_proc.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_sicortex_DSO
component_noinst =
component_install = mca_btl_sicortex.la
else
component_noinst = libmca_btl_sicortex.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_sicortex_la_SOURCES = $(btl_sicortex_sources)
mca_btl_sicortex_la_CFLAGS  =  -DBCAST
mca_btl_sicortex_la_LIBADD = -lscmpi_debug $(btl_sicortex_LIBS)
mca_btl_sicortex_la_LDFLAGS = -module -avoid-version  $(btl_sicortex_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_sicortex_la_SOURCES = $(btl_sicortex_sources)
libmca_btl_sicortex_la_CFLAGS  =  -DBCAST
libmca_btl_sicortex_la_LIBADD =  -lscmpi_debug $(btl_sicortex_LIBS)
libmca_btl_sicortex_la_LDFLAGS = -module -avoid-version   $(btl_sicortex_LDFLAGS)
