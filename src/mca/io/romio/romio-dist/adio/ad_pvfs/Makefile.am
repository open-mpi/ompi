#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_pvfs.la
libadio_pvfs_la_SOURCES = \
        ad_pvfs.c \
        ad_pvfs.h \
        ad_pvfs_close.c \
        ad_pvfs_delete.c \
        ad_pvfs_done.c \
        ad_pvfs_fcntl.c \
        ad_pvfs_flush.c \
        ad_pvfs_hints.c \
        ad_pvfs_iread.c \
        ad_pvfs_iwrite.c \
        ad_pvfs_open.c \
        ad_pvfs_rdcoll.c \
        ad_pvfs_read.c \
        ad_pvfs_resize.c \
        ad_pvfs_seek.c \
        ad_pvfs_wait.c \
        ad_pvfs_wrcoll.c \
        ad_pvfs_write.c
