#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_ntfs.la
libadio_ntfs_la_SOURCES = \
        ad_ntfs.c \
        ad_ntfs.h \
        ad_ntfs_close.c \
        ad_ntfs_done.c \
        ad_ntfs_fcntl.c \
        ad_ntfs_flush.c \
        ad_ntfs_hints.c \
        ad_ntfs_iread.c \
        ad_ntfs_iwrite.c \
        ad_ntfs_open.c \
        ad_ntfs_rdcoll.c \
        ad_ntfs_read.c \
        ad_ntfs_resize.c \
        ad_ntfs_seek.c \
        ad_ntfs_wait.c \
        ad_ntfs_wrcoll.c \
        ad_ntfs_write.c
