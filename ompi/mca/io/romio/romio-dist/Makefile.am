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

SUBDIRS = include adio mpi-io 
DIST_SUBDIRS = $(SUBDIRS) doc util

EXTRA_DIST = README COPYRIGHT README_OMPI

noinst_LTLIBRARIES = libromio_dist.la 
libromio_dist_la_SOURCES = 
libromio_dist_la_LIBADD  = \
	adio/libadio.la \
	mpi-io/libmpi-io.la \
	$(MCA_io_romio_STATIC_LTLIBS)
libromio_dist_la_DEPENDENCIES = \
	$(libromio_dist_la_LIBADD)
libromio_dist_la_LDFLAGS = -module -avoid-version
