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

include $(top_ompi_srcdir)/config/Makefile.options

teg_sources = \
	pml_teg.c \
	pml_teg.h \
	pml_teg_cancel.c \
	pml_teg_component.c \
	pml_teg_component.h \
	pml_teg_iprobe.c \
	pml_teg_irecv.c \
	pml_teg_isend.c \
	pml_teg_ptl.c \
	pml_teg_ptl.h \
	pml_teg_proc.c \
	pml_teg_proc.h \
	pml_teg_progress.c \
	pml_teg_recvfrag.c \
	pml_teg_recvfrag.h \
	pml_teg_recvreq.c \
	pml_teg_recvreq.h \
	pml_teg_sendreq.c \
	pml_teg_sendreq.h \
	pml_teg_start.c \
        pml_ptl_array.c \
        pml_ptl_array.h

if OMPI_BUILD_pml_teg_DSO
component_noinst =
component_install = mca_pml_teg.la
else
component_noinst = libmca_pml_teg.la
component_install =
endif


mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_teg_la_SOURCES = $(teg_sources)
mca_pml_teg_la_LIBADD = 
mca_pml_teg_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_teg_la_SOURCES = $(teg_sources)
libmca_pml_teg_la_LIBADD =
libmca_pml_teg_la_LDFLAGS = -module -avoid-version

