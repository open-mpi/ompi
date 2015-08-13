#
# Copyright (c) 2001-2014 Mellanox Technologies Ltd. ALL RIGHTS RESERVED.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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
mca_pml_yalla_la_LDFLAGS = -module -avoid-version $(pml_yalla_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_yalla_la_SOURCES = $(local_sources)
libmca_pml_yalla_la_LIBADD = $(pml_yalla_LIBS)
libmca_pml_yalla_la_LDFLAGS = -module -avoid-version $(pml_yalla_LDFLAGS)

