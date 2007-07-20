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
# protocol_<name>.la (for DSO builds) or libprotocol_<name>.la
# (for static builds).

if OMPI_BUILD_pml_v_DSO
component_noinst = 
component_install = mca_vprotocol_example.la
else
component_noinst = libmca_vprotocol_example.la
component_install =
endif

local_sources = \
        vprotocol_example.h \
        vprotocol_example.c \
        vprotocol_example_component.c \
        vprotocol_example_proc.c \
        vprotocol_example_comm.c \
        vprotocol_example_progress.c \
        vprotocol_example_start.c \
        vprotocol_example_recv.c \
        vprotocol_example_send.c \
        vprotocol_example_probe.c \
        vprotocol_example_wait.h \
        vprotocol_example_wait.c 
mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_vprotocol_example_la_SOURCES = $(local_sources)
mca_vprotocol_example_la_LIBADD = 
mca_vprotocol_example_la_CFLAGS = -Wno-variadic-macros
mca_vprotocol_example_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_vprotocol_example_la_SOURCES = $(local_sources)
libmca_vprotocol_example_la_LIBADD = 
libmca_vprotocol_example_la_CFLAGS = -Wno-variadic-macros
libmca_vprotocol_example_la_LDFLAGS = -module -avoid-version
