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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_gm_CPPFLAGS)

gm_sources = \
    btl_gm.c \
    btl_gm.h \
    btl_gm_component.c \
    btl_gm_endpoint.c \
    btl_gm_endpoint.h \
    btl_gm_frag.c \
    btl_gm_frag.h \
    btl_gm_proc.c \
    btl_gm_proc.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_btl_gm_DSO
component_noinst =
component_install = mca_btl_gm.la
else
component_noinst = libmca_btl_gm.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_gm_la_SOURCES = $(gm_sources)
mca_btl_gm_la_LIBADD = $(btl_gm_LIBS)
mca_btl_gm_la_LDFLAGS = -module -avoid-version $(btl_gm_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_gm_la_SOURCES = $(gm_sources)
libmca_btl_gm_la_LIBADD = $(btl_gm_LIBS)
libmca_btl_gm_la_LDFLAGS = -module -avoid-version $(btl_gm_LDFLAGS)
