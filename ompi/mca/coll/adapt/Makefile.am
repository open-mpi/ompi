#
# Copyright (c) 2014-2020 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


sources = \
	coll_adapt_component.c \
	coll_adapt_module.c \
	coll_adapt_bcast.c \
	coll_adapt_ibcast.c \
	coll_adapt_reduce.c \
	coll_adapt_ireduce.c \
	coll_adapt.h \
	coll_adapt_algorithms.h \
	coll_adapt_context.h \
	coll_adapt_context.c \
	coll_adapt_inbuf.c \
	coll_adapt_inbuf.h \
	coll_adapt_item.c \
	coll_adapt_item.h \
	coll_adapt_topocache.c \
	coll_adapt_topocache.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_adapt_DSO
component_install += mca_coll_adapt.la
else
component_noinst += libmca_coll_adapt.la
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_adapt_la_SOURCES = $(sources)
mca_coll_adapt_la_LDFLAGS = -module -avoid-version
mca_coll_adapt_la_LIBADD =

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_adapt_la_SOURCES =$(sources)
libmca_coll_adapt_la_LDFLAGS = -module -avoid-version
