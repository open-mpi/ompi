#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#						 All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#						 All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#						 University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#						 All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



ob1_sources  = \
	pml_dr.c \
	pml_dr.h \
	pml_dr_comm.c \
	pml_dr_comm.h \
	pml_dr_component.c \
	pml_dr_component.h \
	pml_dr_hdr.h \
	pml_dr_iprobe.c \
	pml_dr_irecv.c \
	pml_dr_isend.c \
	pml_dr_proc.c \
	pml_dr_proc.h \
	pml_dr_progress.c \
	pml_dr_recvfrag.c \
	pml_dr_recvfrag.h \
	pml_dr_recvreq.c \
	pml_dr_recvreq.h \
	pml_dr_sendreq.c \
	pml_dr_sendreq.h \
	pml_dr_start.c \
	pml_dr_vfrag.c 

if OMPI_BUILD_pml_dr_DSO
component_noinst =
component_install = mca_pml_dr.la
else
component_noinst = libmca_pml_dr.la
component_install =
endif


mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_dr_la_SOURCES = $(ob1_sources)
mca_pml_dr_la_LIBADD = \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la
mca_pml_dr_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_dr_la_SOURCES = $(ob1_sources)
libmca_pml_dr_la_LIBADD =
libmca_pml_dr_la_LDFLAGS = -module -avoid-version
