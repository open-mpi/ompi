#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_pfs.la
libadio_pfs_la_SOURCES = \
        ad_pfs.c \
        ad_pfs_close.c \
        ad_pfs_done.c \
        ad_pfs_fcntl.c \
        ad_pfs_flush.c \
        ad_pfs.h \
        ad_pfs_hints.c \
        ad_pfs_iread.c \
        ad_pfs_iwrite.c \
        ad_pfs_open.c \
        ad_pfs_rdcoll.c \
        ad_pfs_read.c \
        ad_pfs_resize.c \
        ad_pfs_seek.c \
        ad_pfs_wait.c \
        ad_pfs_wrcoll.c \
        ad_pfs_write.c
