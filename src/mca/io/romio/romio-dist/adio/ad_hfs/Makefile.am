#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_hfs.la
libadio_hfs_la_SOURCES = \
        ad_hfs.c \
        ad_hfs.h \
        ad_hfs_close.c \
        ad_hfs_done.c \
        ad_hfs_fcntl.c \
        ad_hfs_flush.c \
        ad_hfs_hints.c \
        ad_hfs_iread.c \
        ad_hfs_iwrite.c \
        ad_hfs_open.c \
        ad_hfs_rdcoll.c \
        ad_hfs_read.c \
        ad_hfs_resize.c \
        ad_hfs_seek.c \
        ad_hfs_wait.c \
        ad_hfs_wrcoll.c \
        ad_hfs_write.c
