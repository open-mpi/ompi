#
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        coll_spacc.h \
        coll_spacc_component.c \
        coll_spacc_module.c \
        coll_spacc_allreduce.c \
        coll_spacc_reduce.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_spacc_DSO
component_noinst =
component_install = mca_coll_spacc.la
else
component_noinst = libmca_coll_spacc.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_spacc_la_SOURCES = $(sources)
mca_coll_spacc_la_LDFLAGS = -module -avoid-version
mca_coll_spacc_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_spacc_la_SOURCES =$(sources)
libmca_coll_spacc_la_LDFLAGS = -module -avoid-version
