#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Need to use our own AUTOMAKE_OPTIONS -- cannot include
# Makefile.options from main ompi tree because Automake now defers
# "include" statements until "make" time.  Hence, if we're building
# this component outside the ompi tree, the include statement will
# fail.

AUTOMAKE_OPTIONS	= foreign dist-bzip2


EXTRA_DIST = VERSION

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_hierarch_DSO
component_noinst =
component_install = mca_coll_hierarch.la
else
component_noinst = libmca_coll_hierarch.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_hierarch_la_SOURCES = $(sources)
mca_coll_hierarch_la_LIBADD = $(LIBMPI_LA)
mca_coll_hierarch_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_hierarch_la_SOURCES = $(sources)
libmca_coll_hierarch_la_LIBADD = src/libmca_coll_hierarch.la
libmca_coll_hierarch_la_LDFLAGS = -module -avoid-version

# Source files

sources = \
        coll_hierarch.h \
        coll_hierarch.c \
        coll_hierarch_barrier.c \
        coll_hierarch_bcast.c \
        coll_hierarch_component.c \
        coll_hierarch_reduce.c 