#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Sandia National Laboratories. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options



AM_CPPFLAGS=$(btl_ud_CPPFLAGS) 

sources = \
    btl_ud.c \
    btl_ud.h \
    btl_ud_component.c \
    btl_ud_endpoint.c \
    btl_ud_endpoint.h \
    btl_ud_frag.c \
    btl_ud_frag.h \
    btl_ud_proc.c \
    btl_ud_proc.h
     
# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_ud_DSO
lib =
lib_sources =
component = mca_btl_ud.la
component_sources = $(sources)
else
lib = libmca_btl_ud.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_btl_ud_la_SOURCES = $(component_sources)
mca_btl_ud_la_LDFLAGS = -module -avoid-version $(btl_ud_LDFLAGS) 
mca_btl_ud_la_LIBADD = \
    $(btl_ud_LIBS) \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la
    

noinst_LTLIBRARIES = $(lib)
libmca_btl_ud_la_SOURCES = $(lib_sources)
libmca_btl_ud_la_LDFLAGS= -module -avoid-version $(btl_ud_LDFLAGS) 
libmca_btl_ud_la_LIBADD=$(btl_ud_LIBS)
