#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_xfs.la
libadio_xfs_la_SOURCES = \
        ad_xfs.c \
        ad_xfs.h \
        ad_xfs_close.c \
        ad_xfs_done.c \
        ad_xfs_fcntl.c \
        ad_xfs_flush.c \
        ad_xfs_hints.c \
        ad_xfs_iread.c \
        ad_xfs_iwrite.c \
        ad_xfs_open.c \
        ad_xfs_rdcoll.c \
        ad_xfs_read.c \
        ad_xfs_resize.c \
        ad_xfs_seek.c \
        ad_xfs_wait.c \
        ad_xfs_wrcoll.c \
        ad_xfs_write.c
