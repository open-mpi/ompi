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

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include $(pls_bproc_seed_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_bproc_seed_DSO
component_noinst =
component_install = mca_pls_bproc_seed.la
else
component_noinst = libmca_pls_bproc_seed.la
component_install =
endif

sources = \
	pls_bproc_seed.h \
	pls_bproc_seed.c \
	pls_bproc_seed_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pls_bproc_seed_la_SOURCES = $(sources)
mca_pls_bproc_seed_la_LIBADD = $(pls_bproc_seed_LIBS)
mca_pls_bproc_seed_la_LDFLAGS = -module -avoid-version $(pls_bproc_seed_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pls_bproc_seed_la_SOURCES = $(sources)
libmca_pls_bproc_seed_la_LIBADD = $(pls_bproc_seed_LIBS)
libmca_pls_bproc_seed_la_LDFLAGS = -module -avoid-version $(pls_bproc_seed_LDFLAGS)
