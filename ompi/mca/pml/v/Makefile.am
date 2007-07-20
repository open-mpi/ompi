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

if OMPI_BUILD_pml_v_DSO
component_noinst =
component_install = mca_pml_v.la
else
component_noinst = libmca_pml_v.la
component_install =
endif

SUBDIRS = $(MCA_pml_v_PROTOCOLS)

local_sources = \
	pml_v.c \
	pml_v.h \
	pml_v_component.c \
        pml_v_protocol.h \
        pml_v_protocol_base.h \
        pml_v_protocol_base.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_v_la_SOURCES = $(local_sources)
mca_pml_v_la_LIBADD =
mca_pml_v_la_CFLAGS = -Wno-variadic-macros 
mca_pml_v_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_v_la_SOURCES = $(local_sources)
libmca_pml_v_la_LIBADD = 
libmca_pml_v_la_CFLAGS = -Wno-variadic-macros
libmca_pml_v_la_LDFLAGS = -module -avoid-version

