#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

SUBDIRS = src

EXTRA_DIST = VERSION

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_llm_hostfile_DSO
component_noinst = 
component_install = mca_llm_hostfile.la
else
component_noinst = libmca_llm_hostfile.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_llm_hostfile_la_SOURCES =
mca_llm_hostfile_la_LIBADD = src/libmca_llm_hostfile.la
mca_llm_hostfile_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_llm_hostfile_la_SOURCES =
libmca_llm_hostfile_la_LIBADD = src/libmca_llm_hostfile.la
libmca_llm_hostfile_la_LDFLAGS = -module -avoid-version
