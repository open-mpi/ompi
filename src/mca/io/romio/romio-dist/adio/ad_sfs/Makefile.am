#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_sfs.la
libadio_sfs_la_SOURCES = \
        ad_sfs.c \
        ad_sfs.h \
        ad_sfs_close.c \
        ad_sfs_done.c \
        ad_sfs_fcntl.c \
        ad_sfs_flush.c \
        ad_sfs_hints.c \
        ad_sfs_iread.c \
        ad_sfs_iwrite.c \
        ad_sfs_open.c \
        ad_sfs_rdcoll.c \
        ad_sfs_read.c \
        ad_sfs_resize.c \
        ad_sfs_seek.c \
        ad_sfs_wait.c \
        ad_sfs_wrcoll.c \
        ad_sfs_write.c
