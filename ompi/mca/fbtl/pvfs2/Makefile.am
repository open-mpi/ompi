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
# Copyright (c) 2008-2011 University of Houston. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_fbtl_pvfs2_DSO
component_noinst =
component_install = mca_fbtl_pvfs2.la
else
component_noinst = libmca_fbtl_pvfs2.la
component_install =
endif

# Source files

fbtl_pvfs2_sources = \
        fbtl_pvfs2.h \
        fbtl_pvfs2.c \
        fbtl_pvfs2_component.c \
        fbtl_pvfs2_preadv.c \
        fbtl_pvfs2_ipreadv.c \
        fbtl_pvfs2_pwritev.c \
        fbtl_pvfs2_ipwritev.c

AM_CPPFLAGS = $(fbtl_pvfs2_CPPFLAGS)

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fbtl_pvfs2_la_SOURCES = $(fbtl_pvfs2_sources)
mca_fbtl_pvfs2_la_LIBADD = $(fbtl_pvfs2_LIBS)
mca_fbtl_pvfs2_la_LDFLAGS = -module -avoid-version $(fbtl_pvfs2_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_fbtl_pvfs2_la_SOURCES = $(fbtl_pvfs2_sources)
libmca_fbtl_pvfs2_la_LIBADD = $(fbtl_pvfs2_LIBS)
libmca_fbtl_pvfs2_la_LDFLAGS = -module -avoid-version $(fbtl_pvfs2_LDFLAGS)
