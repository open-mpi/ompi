#
# Copyright (c) 2021-2023 Computer Architecture and VLSI Systems (CARV)
#                         Laboratory, ICS Forth. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_opaldata_DATA = help-coll-xhc.txt

sources = \
    coll_xhc.h \
    coll_xhc_atomic.h \
    coll_xhc.c \
    coll_xhc_component.c \
    coll_xhc_module.c \
    coll_xhc_bcast.c \
    coll_xhc_barrier.c \
    coll_xhc_reduce.c \
    coll_xhc_allreduce.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_xhc_DSO
component_install += mca_coll_xhc.la
else
component_noinst += libmca_coll_xhc.la
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_xhc_la_SOURCES = $(sources)
mca_coll_xhc_la_LDFLAGS = -module -avoid-version
mca_coll_xhc_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_xhc_la_SOURCES = $(sources)
libmca_coll_xhc_la_LDFLAGS = -module -avoid-version
