#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_ufs.la
libadio_ufs_la_SOURCES = \
        ad_ufs.c \
        ad_ufs_close.c \
        ad_ufs_done.c \
        ad_ufs_fcntl.c \
        ad_ufs_flush.c \
        ad_ufs.h \
        ad_ufs_hints.c \
        ad_ufs_iread.c \
        ad_ufs_iwrite.c \
        ad_ufs_open.c \
        ad_ufs_rdcoll.c \
        ad_ufs_read.c \
        ad_ufs_resize.c \
        ad_ufs_seek.c \
        ad_ufs_wait.c \
        ad_ufs_wrcoll.c \
        ad_ufs_write.c
