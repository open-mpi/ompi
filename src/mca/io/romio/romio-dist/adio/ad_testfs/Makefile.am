#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_testfs.la
libadio_testfs_la_SOURCES = \
        ad_testfs.c \
        ad_testfs.h \
        ad_testfs_close.c \
        ad_testfs_delete.c \
        ad_testfs_done.c \
        ad_testfs_fcntl.c \
        ad_testfs_flush.c \
        ad_testfs_getsh.c \
        ad_testfs_hints.c \
        ad_testfs_iread.c \
        ad_testfs_iwrite.c \
        ad_testfs_open.c \
        ad_testfs_rdcoll.c \
        ad_testfs_read.c \
        ad_testfs_resize.c \
        ad_testfs_seek.c \
        ad_testfs_setsh.c \
        ad_testfs_wait.c \
        ad_testfs_wrcoll.c \
        ad_testfs_write.c
