# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DHAVE_MPI_INFO \
        -DNO_AIO -DNFS -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include \
        -I../../adio/include -I../../include -I../../../src

noinst_LTLIBRARIES = libmca_io_romio_romio_dist_adio_ad_nfs.la
libmca_io_romio_romio_dist_adio_ad_nfs_la_SOURCES = \
    ad_nfs.c ad_nfs_close.c ad_nfs_done.c ad_nfs_fcntl.c           \
    ad_nfs_flush.c ad_nfs_getsh.c ad_nfs_hints.c ad_nfs_iread.c    \
    ad_nfs_iwrite.c ad_nfs_open.c ad_nfs_rdcoll.c ad_nfs_read.c    \
    ad_nfs_resize.c ad_nfs_seek.c ad_nfs_setsh.c ad_nfs_wait.c     \
    ad_nfs_wrcoll.c ad_nfs_write.c

