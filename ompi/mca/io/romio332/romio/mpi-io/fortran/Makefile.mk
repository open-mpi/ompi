## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_F77_BINDINGS

romio_mpi_sources +=              \
    mpi-io/fortran/closef.c       \
    mpi-io/fortran/deletef.c      \
    mpi-io/fortran/fsyncf.c       \
    mpi-io/fortran/get_amodef.c   \
    mpi-io/fortran/get_atomf.c    \
    mpi-io/fortran/get_bytofff.c  \
    mpi-io/fortran/get_errhf.c    \
    mpi-io/fortran/get_extentf.c  \
    mpi-io/fortran/get_groupf.c   \
    mpi-io/fortran/get_infof.c    \
    mpi-io/fortran/get_posn_shf.c \
    mpi-io/fortran/get_posnf.c    \
    mpi-io/fortran/get_sizef.c    \
    mpi-io/fortran/get_viewf.c    \
    mpi-io/fortran/iotestf.c      \
    mpi-io/fortran/iowaitf.c      \
    mpi-io/fortran/iread_atf.c    \
    mpi-io/fortran/iread_shf.c    \
    mpi-io/fortran/ireadf.c       \
    mpi-io/fortran/iwrite_atf.c   \
    mpi-io/fortran/iwrite_shf.c   \
    mpi-io/fortran/iwritef.c      \
    mpi-io/fortran/openf.c        \
    mpi-io/fortran/preallocf.c    \
    mpi-io/fortran/rd_atallbf.c   \
    mpi-io/fortran/rd_atallef.c   \
    mpi-io/fortran/read_allbf.c   \
    mpi-io/fortran/read_allef.c   \
    mpi-io/fortran/read_allf.c    \
    mpi-io/fortran/read_atallf.c  \
    mpi-io/fortran/read_atf.c     \
    mpi-io/fortran/read_ordbf.c   \
    mpi-io/fortran/read_ordef.c   \
    mpi-io/fortran/read_ordf.c    \
    mpi-io/fortran/read_shf.c     \
    mpi-io/fortran/readf.c        \
    mpi-io/fortran/seek_shf.c     \
    mpi-io/fortran/seekf.c        \
    mpi-io/fortran/set_atomf.c    \
    mpi-io/fortran/set_errhf.c    \
    mpi-io/fortran/set_infof.c    \
    mpi-io/fortran/set_sizef.c    \
    mpi-io/fortran/set_viewf.c    \
    mpi-io/fortran/wr_atallbf.c   \
    mpi-io/fortran/wr_atallef.c   \
    mpi-io/fortran/write_allbf.c  \
    mpi-io/fortran/write_allef.c  \
    mpi-io/fortran/write_allf.c   \
    mpi-io/fortran/write_atallf.c \
    mpi-io/fortran/write_atf.c    \
    mpi-io/fortran/write_ordbf.c  \
    mpi-io/fortran/write_ordef.c  \
    mpi-io/fortran/write_ordf.c   \
    mpi-io/fortran/write_shf.c    \
    mpi-io/fortran/writef.c

endif BUILD_F77_BINDINGS

