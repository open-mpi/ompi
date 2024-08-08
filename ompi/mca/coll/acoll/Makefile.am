#
# Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(coll_acoll_CPPFLAGS)

sources = \
        coll_acoll.h \
        coll_acoll_utils.h \
        coll_acoll_allgather.c \
        coll_acoll_bcast.c \
        coll_acoll_gather.c \
        coll_acoll_reduce.c \
        coll_acoll_allreduce.c \
        coll_acoll_barrier.c \
        coll_acoll_component.c \
        coll_acoll_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_acoll_DSO
component_noinst =
component_install = mca_coll_acoll.la
else
component_noinst = libmca_coll_acoll.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_acoll_la_SOURCES = $(sources)
mca_coll_acoll_la_LDFLAGS = -module -avoid-version $(coll_acoll_LDFLAGS)
mca_coll_acoll_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la $(coll_acoll_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_acoll_la_SOURCES =$(sources)
libmca_coll_acoll_la_LIBADD = $(coll_acoll_LIBS)
libmca_coll_acoll_la_LDFLAGS = -module -avoid-version $(coll_acoll_LDFLAGS)
