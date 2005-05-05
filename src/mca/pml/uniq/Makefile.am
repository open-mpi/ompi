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

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pml_uniq_DSO
component_noinst =
component_install = mca_pml_uniq.la
else
component_noinst = libmca_pml_uniq.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_uniq_la_SOURCES = $(pml_uniq_la_sources)
mca_pml_uniq_la_LIBADD =
mca_pml_uniq_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_uniq_la_SOURCES = $(pml_uniq_la_sources)
libmca_pml_uniq_la_LIBADD =
libmca_pml_uniq_la_LDFLAGS = -module -avoid-version

pml_uniq_la_sources = pml_uniq.c pml_uniq.h pml_uniq_cancel.c pml_uniq_component.c \
   pml_uniq_component.h pml_uniq_iprobe.c pml_uniq_irecv.c pml_uniq_isend.c \
   pml_uniq_ptl.c pml_uniq_ptl.h pml_uniq_proc.c pml_uniq_proc.h pml_uniq_progress.c \
   pml_uniq_recvfrag.c pml_uniq_recvfrag.h pml_uniq_recvreq.c pml_uniq_recvreq.h \
   pml_uniq_sendreq.c pml_uniq_sendreq.h pml_uniq_start.c

