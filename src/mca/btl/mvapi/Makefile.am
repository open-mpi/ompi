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
    btl_ib.c \
    btl_ib.h \
    btl_ib_component.c \
    btl_ib_endpoint.c \
    btl_ib_endpoint.h \
    btl_ib_frag.c \
    btl_ib_frag.h \
    btl_ib_proc.c \
    btl_ib_proc.h \
    btl_ib_error.h
     
# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_ib_DSO
lib =
lib_sources =
component = mca_btl_ib.la
component_sources = $(sources)
else
lib = libmca_btl_ib.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_btl_ib_la_SOURCES = $(component_sources)
mca_btl_ib_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_btl_ib_la_SOURCES = $(lib_sources)
libmca_btl_ib_la_LDFLAGS = -module -avoid-version
