#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

if OMPI_BUILD_ras_hostfile_DSO
component_noinst =
component_install = mca_ras_hostfile.la
else
component_noinst = libmca_ras_hostfile.la
component_install =
endif

hostfile_SOURCES = \
    ras_hostfile.h \
    ras_hostfile_module.c \
    ras_hostfile_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_hostfile_la_SOURCES = $(hostfile_SOURCES)
mca_ras_hostfile_la_LIBADD = \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la
mca_ras_hostfile_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_hostfile_la_SOURCES = $(hostfile_SOURCES)
libmca_ras_hostfile_la_LIBADD =
libmca_ras_hostfile_la_LDFLAGS = -module -avoid-version
