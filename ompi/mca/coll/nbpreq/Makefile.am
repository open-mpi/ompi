#
# Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        coll_nbpreq.h \
        coll_nbpreq_component.c \
        coll_nbpreq_module.c \
        coll_nbpreq_request.h \
        coll_nbpreq_request.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_nbpreq_DSO
component_noinst =
component_install = mca_coll_nbpreq.la
else
component_noinst = libmca_coll_nbpreq.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_nbpreq_la_SOURCES = $(sources)
mca_coll_nbpreq_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_nbpreq_la_SOURCES =$(sources)
libmca_coll_nbpreq_la_LDFLAGS = -module -avoid-version
