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
        pls_slurm.h \
        pls_slurm_component.c \
        pls_slurm_module.c

dist_pkgdata_DATA = help-pls-slurm.txt

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_slurm_DSO
component_noinst =
component_install = mca_pls_slurm.la
else
component_noinst = libmca_pls_slurm.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pls_slurm_la_SOURCES = $(sources)
mca_pls_slurm_la_LDFLAGS = -module -avoid-version
mca_pls_slurm_la_LIBADD = \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_pls_slurm_la_SOURCES =$(sources)
libmca_pls_slurm_la_LDFLAGS = -module -avoid-version
