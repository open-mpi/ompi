#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DHAVE_MPI_INFO \
        -DNO_AIO -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include \
        -I../adio/include -I../include -I../../src

#SUBDIRS = fortran 

noinst_LTLIBRARIES = libmca_io_romio_romio_dist_mpi_io.la

libmca_io_romio_romio_dist_mpi_io_la_SOURCES = \
      mpioimpl.h mpioprof.h close.c read.c open.c write.c set_view.c \
      iread.c iwrite.c iotest.c iowait.c seek.c get_posn.c \
      delete.c read_all.c read_at.c \
      read_atall.c iread_at.c iwrite_at.c write_all.c get_bytoff.c \
      write_at.c write_atall.c get_view.c get_group.c get_amode.c \
      get_extent.c fsync.c get_atom.c set_atom.c \
      set_size.c get_size.c prealloc.c file_f2c.c file_c2f.c \
      ioreq_f2c.c ioreq_c2f.c set_info.c get_info.c \
      rd_atallb.c rd_atalle.c read_allb.c read_alle.c wr_atallb.c \
      wr_atalle.c write_allb.c write_alle.c \
      get_posn_sh.c iread_sh.c read_sh.c write_sh.c \
      iwrite_sh.c seek_sh.c read_ord.c read_orde.c write_ordb.c \
      read_ordb.c write_ord.c write_orde.c get_errh.c set_errh.c

#libmca_io_romio_dist_mpi_io_la_LIBADD  = \
#  fortran/libmca_io_romio_dist_mpio_fortran.la \
#$(MCA_io_romio_STATIC_LTLIBS)
#
#libmca_io_romio_dist_mpi_io_la_DEPENDENCIES = \
# $(libmca_io_romio_dist_mpi_io_la_LIBADD)

