##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

if MPIO_GLUE_MPICH
romio_other_sources +=             \
    mpi-io/glue/mpich/mpio_file.c \
    mpi-io/glue/mpich/mpio_err.c
endif MPIO_GLUE_MPICH
