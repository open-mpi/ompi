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

sources =
include src/Makefile.extra

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_tm_DSO
lib =
lib_sources = 
component = mca_pls_tm.la
component_sources = $(sources)
else
lib = libmca_pls_tm.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_pls_tm_la_SOURCES = $(component_sources)
mca_pls_tm_la_LDFLAGS = -module -avoid-version $(pls_tm_LDFLAGS)
mca_pls_tm_la_LIBADD = $(pls_tm_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_pls_tm_la_SOURCES = $(lib_sources)
libmca_pls_tm_la_LDFLAGS = -module -avoid-version $(pls_tm_LDFLAGS)
libmca_pls_tm_la_LIBADD = $(pls_tm_LIBS)
