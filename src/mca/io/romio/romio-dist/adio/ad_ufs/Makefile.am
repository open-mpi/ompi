# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DHAVE_MPI_INFO \
        -DNO_AIO -DUFS -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include \
        -I../../adio/include -I../../include -I../../../src

noinst_LTLIBRARIES = libmca_io_romio_romio_dist_adio_ad_ufs.la
libmca_io_romio_romio_dist_adio_ad_ufs_la_SOURCES = \
        ad_ufs.h ad_ufs_close.c ad_ufs_read.c \
        ad_ufs_open.c ad_ufs_write.c ad_ufs_done.c \
        ad_ufs_fcntl.c ad_ufs_iread.c ad_ufs_iwrite.c ad_ufs_wait.c \
        ad_ufs_resize.c ad_ufs.c

