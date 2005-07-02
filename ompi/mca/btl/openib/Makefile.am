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
    btl_openib.c \
    btl_openib.h \
    btl_openib_component.c \
    btl_openib_endpoint.c \
    btl_openib_endpoint.h \
    btl_openib_frag.c \
    btl_openib_frag.h \
    btl_openib_proc.c \
    btl_openib_proc.h \
    btl_openib_error.h
     
# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_openib_DSO
lib =
lib_sources =
component = mca_btl_openib.la
component_sources = $(sources)
else
lib = libmca_btl_openib.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_btl_openib_la_SOURCES = $(component_sources)
mca_btl_openib_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_btl_openib_la_SOURCES = $(lib_sources)
libmca_btl_openib_la_LDFLAGS = -module -avoid-version
