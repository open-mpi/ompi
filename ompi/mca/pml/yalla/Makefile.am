#
# Copyright (C) Mellanox Technologies Ltd. 2001-2014.  ALL RIGHTS RESERVED.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(pml_yalla_CPPFLAGS)

local_sources = \
        pml_yalla.h \
        pml_yalla.c \
        pml_yalla_request.h \
        pml_yalla_request.c \
        pml_yalla_datatype.h \
        pml_yalla_datatype.c \
        pml_yalla_freelist.h \
        pml_yalla_component.c

if MCA_BUILD_ompi_pml_yalla_DSO
component_noinst =
component_install = mca_pml_yalla.la
else
component_noinst = libmca_pml_yalla.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_yalla_la_SOURCES = $(local_sources)
mca_pml_yalla_la_LIBADD = $(pml_yalla_LIBS)
mca_pml_yalla_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_yalla_la_SOURCES = $(local_sources)
libmca_pml_yalla_la_LIBADD = $(pml_yalla_LIBS)
libmca_pml_yalla_la_LDFLAGS = -module -avoid-version

