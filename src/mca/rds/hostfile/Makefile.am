#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_srcdir)/config/Makefile.options

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_rds_hostfile_DSO
component_noinst =
component_install = mca_rds_hostfile.la
else
component_noinst = libmca_rds_hostfile.la
component_install =
endif

AM_CPPFLAGS = -I$(top_builddir)/src -DORTE_SYSCONFDIR="\"$(sysconfdir)\""
AM_LFLAGS = -Porte_rds_hostfile_
LEX_OUTPUT_ROOT = lex.orte_rds_hostfile_

proxy_SOURCES = \
    rds_hostfile_lex.l \
    rds_hostfile_lex.h \
    rds_hostfile.c \
    rds_hostfile.h \
    rds_hostfile_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_rds_hostfile_la_SOURCES = $(proxy_SOURCES)
mca_rds_hostfile_la_LIBADD = 
mca_rds_hostfile_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rds_hostfile_la_SOURCES = $(proxy_SOURCES)
libmca_rds_hostfile_la_LIBADD =
libmca_rds_hostfile_la_LDFLAGS = -module -avoid-version
