## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

include $(top_srcdir)/mpi-io/glue/default/Makefile.mk
include $(top_srcdir)/mpi-io/glue/mpich/Makefile.mk

if !BUILD_ROMIO_EMBEDDED
romio_other_sources += \
	mpi-io/glue/large_count.c
endif
