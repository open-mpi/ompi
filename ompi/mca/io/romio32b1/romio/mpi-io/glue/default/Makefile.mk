## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if MPIO_GLUE_DEFAULT
romio_other_sources +=              \
    mpi-io/glue/default/mpio_file.c \
    mpi-io/glue/default/mpio_err.c
endif MPIO_GLUE_DEFAULT

