#
# Copyright (c) 2012-2020 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        coll_ftagree.h \
        coll_ftagree_component.c \
        coll_ftagree_module.c \
        coll_ftagree.c \
        coll_ftagree_era.h \
        coll_ftagree_earlyreturning.c \
        coll_ftagree_earlyterminating.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_ftagree_DSO
component_noinst =
component_install = mca_coll_ftagree.la
else
component_noinst = libmca_coll_ftagree.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_ftagree_la_SOURCES = $(sources)
mca_coll_ftagree_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_ftagree_la_SOURCES =$(sources)
libmca_coll_ftagree_la_LDFLAGS = -module -avoid-version
