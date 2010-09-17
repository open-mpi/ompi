#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_pml_example_DSO
component_noinst =
component_install = mca_pml_example.la
else
component_noinst = libmca_pml_example.la
component_install =
endif

local_sources = \
	pml_example.c \
	pml_example.h \
	pml_example_cancel.c \
	pml_example_component.c \
	pml_example_iprobe.c \
	pml_example_irecv.c \
	pml_example_isend.c \
	pml_example_ptl.c \
	pml_example_ptl.h \
	pml_example_proc.c \
	pml_example_proc.h \
	pml_example_progress.c \
	pml_example_recvfrag.c \
	pml_example_recvfrag.h \
	pml_example_recvreq.c \
	pml_example_recvreq.h \
	pml_example_sendreq.c \
	pml_example_sendreq.h \
	pml_example_start.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_example_la_SOURCES = $(local_sources)
mca_pml_example_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_example_la_SOURCES = $(local_sources)
libmca_pml_example_la_LDFLAGS = -module -avoid-version

