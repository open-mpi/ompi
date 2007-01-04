#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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

sources = \
        ns_proxy.h \
	ns_proxy_cell_fns.c \
	ns_proxy_diag_fns.c \
	ns_proxy_general_fns.c \
	ns_proxy_job_fns.c \
        ns_proxy_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ns_proxy_DSO
component_noinst =
component_install = mca_ns_proxy.la
else
component_noinst = libmca_ns_proxy.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ns_proxy_la_SOURCES = $(sources)
mca_ns_proxy_la_LDFLAGS = -module -avoid-version
mca_ns_proxy_la_LIBADD = \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_ns_proxy_la_SOURCES =$(sources)
libmca_ns_proxy_la_LDFLAGS = -module -avoid-version
