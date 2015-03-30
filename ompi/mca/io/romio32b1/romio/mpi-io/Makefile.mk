## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

include $(top_srcdir)/mpi-io/glue/Makefile.mk
include $(top_srcdir)/mpi-io/fortran/Makefile.mk

AM_CPPFLAGS += -I$(top_builddir)/mpi-io -I$(top_srcdir)/mpi-io
noinst_HEADERS += mpi-io/mpioimpl.h mpi-io/mpioprof.h

romio_mpi_sources +=          \
    mpi-io/close.c            \
    mpi-io/delete.c           \
    mpi-io/fsync.c            \
    mpi-io/get_amode.c        \
    mpi-io/get_atom.c         \
    mpi-io/get_bytoff.c       \
    mpi-io/get_extent.c       \
    mpi-io/get_group.c        \
    mpi-io/get_info.c         \
    mpi-io/get_posn.c         \
    mpi-io/get_posn_sh.c      \
    mpi-io/get_size.c         \
    mpi-io/get_view.c         \
    mpi-io/iread.c            \
    mpi-io/iread_all.c        \
    mpi-io/iread_at.c         \
    mpi-io/iread_atall.c      \
    mpi-io/iread_sh.c         \
    mpi-io/iwrite.c           \
    mpi-io/iwrite_all.c       \
    mpi-io/iwrite_at.c        \
    mpi-io/iwrite_atall.c     \
    mpi-io/iwrite_sh.c        \
    mpi-io/open.c             \
    mpi-io/prealloc.c         \
    mpi-io/rd_atallb.c        \
    mpi-io/rd_atalle.c        \
    mpi-io/read.c             \
    mpi-io/read_all.c         \
    mpi-io/read_allb.c        \
    mpi-io/read_alle.c        \
    mpi-io/read_at.c          \
    mpi-io/read_atall.c       \
    mpi-io/read_ord.c         \
    mpi-io/read_ordb.c        \
    mpi-io/read_orde.c        \
    mpi-io/read_sh.c          \
    mpi-io/register_datarep.c \
    mpi-io/seek.c             \
    mpi-io/seek_sh.c          \
    mpi-io/set_atom.c         \
    mpi-io/set_info.c         \
    mpi-io/set_size.c         \
    mpi-io/set_view.c         \
    mpi-io/wr_atallb.c        \
    mpi-io/wr_atalle.c        \
    mpi-io/write.c            \
    mpi-io/write_all.c        \
    mpi-io/write_allb.c       \
    mpi-io/write_alle.c       \
    mpi-io/write_at.c         \
    mpi-io/write_atall.c      \
    mpi-io/write_ord.c        \
    mpi-io/write_ordb.c       \
    mpi-io/write_orde.c       \
    mpi-io/write_sh.c


# non-MPI/PMPI sources that will be included in libromio
romio_other_sources +=       \
    mpi-io/mpich_fileutil.c \
    mpi-io/mpir-mpioinit.c   \
    mpi-io/mpiu_greq.c \
    mpi-io/mpiu_external32.c

# helper variables for conditionally compiled sources
mpio_request_sources=   \
    mpi-io/ioreq_c2f.c  \
    mpi-io/ioreq_f2c.c  \
    mpi-io/iotest.c     \
    mpi-io/iotestall.c  \
    mpi-io/iotestany.c  \
    mpi-io/iotestsome.c \
    mpi-io/iowait.c     \
    mpi-io/iowaitall.c  \
    mpi-io/iowaitany.c  \
    mpi-io/iowaitsome.c

mpio_extra_sources =  \
    mpi-io/get_errh.c \
    mpi-io/set_errh.c

# not used in MPICH, we use generalized requests instead
if BUILD_MPIO_REQUEST
romio_other_sources += $(mpio_request_sources)
endif BUILD_MPIO_REQUEST

# not used in MPICH
if BUILD_MPIO_ERRHAN
romio_other_sources += $(mpio_request_sources)
endif BUILD_MPIO_ERRHAN

