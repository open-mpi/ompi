## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if MPIO_GLUE_OPENMPI
romio_other_sources +=              \
    mpi-io/glue/openmpi/mpio_file.c \
    mpi-io/glue/openmpi/mpio_err.c
endif MPIO_GLUE_OPENMPI

