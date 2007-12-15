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

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_proxy_DSO
component_noinst =
component_install = mca_pls_proxy.la
else
component_noinst = libmca_pls_proxy.la
component_install =
endif

proxy_SOURCES = \
    pls_proxy.c \
    pls_proxy.h \
    pls_proxy_component.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pls_proxy_la_SOURCES = $(proxy_SOURCES)
mca_pls_proxy_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pls_proxy_la_SOURCES = $(proxy_SOURCES)
libmca_pls_proxy_la_LDFLAGS = -module -avoid-version
