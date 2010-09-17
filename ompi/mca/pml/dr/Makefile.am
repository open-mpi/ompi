#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = \
	help-mpi-pml-dr.txt

dr_sources  = \
	pml_dr.c \
	pml_dr.h \
	pml_dr_comm.c \
	pml_dr_comm.h \
	pml_dr_component.c \
	pml_dr_component.h \
	pml_dr_endpoint.c \
	pml_dr_endpoint.h \
	pml_dr_hdr.h \
	pml_dr_iprobe.c \
	pml_dr_irecv.c \
	pml_dr_isend.c \
	pml_dr_recvfrag.c \
	pml_dr_recvfrag.h \
	pml_dr_recvreq.c \
	pml_dr_recvreq.h \
	pml_dr_sendreq.c \
	pml_dr_sendreq.h \
	pml_dr_start.c \
        pml_dr_vfrag.h \
	pml_dr_vfrag.c 

if MCA_BUILD_ompi_pml_dr_DSO
component_noinst =
component_install = mca_pml_dr.la
else
component_noinst = libmca_pml_dr.la
component_install =
endif


mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_dr_la_SOURCES = $(dr_sources)
mca_pml_dr_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_dr_la_SOURCES = $(dr_sources)
libmca_pml_dr_la_LDFLAGS = -module -avoid-version
