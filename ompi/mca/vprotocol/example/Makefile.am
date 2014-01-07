#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# protocol_<name>.la (for DSO builds) or libprotocol_<name>.la
# (for static builds).

if MCA_BUILD_ompi_vprotocol_example_DSO
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
mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_vprotocol_example_la_SOURCES = $(local_sources)
mca_vprotocol_example_la_LIBADD = 
mca_vprotocol_example_la_CFLAGS =
mca_vprotocol_example_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_vprotocol_example_la_SOURCES = $(local_sources)
libmca_vprotocol_example_la_LIBADD = 
libmca_vprotocol_example_la_CFLAGS =
libmca_vprotocol_example_la_LDFLAGS = -module -avoid-version
