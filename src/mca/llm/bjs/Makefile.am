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

include $(top_ompi_srcdir)/config/Makefile.options

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

sources = \
	llm_bjs.h \
	llm_bjs.c \
	llm_bjs_component.c


if OMPI_BUILD_llm_bjs_DSO
component_noinst =
component_install = mca_llm_bjs.la
else
component_noinst = libmca_llm_bjs.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_llm_bjs_la_SOURCES = $(sources)
mca_llm_bjs_la_LIBADD =
mca_llm_bjs_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_llm_bjs_la_SOURCES = $(sources)
libmca_llm_bjs_la_LIBADD = 
libmca_llm_bjs_la_LDFLAGS = -module -avoid-version
