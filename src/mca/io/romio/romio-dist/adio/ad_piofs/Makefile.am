#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_piofs.la
libadio_piofs_la_SOURCES = \
        ad_piofs.c \
        ad_piofs.h \
        ad_piofs_close.c \
        ad_piofs_done.c \
        ad_piofs_fcntl.c \
        ad_piofs_flush.c \
        ad_piofs_hints.c \
        ad_piofs_iread.c \
        ad_piofs_iwrite.c \
        ad_piofs_open.c \
        ad_piofs_rdcoll.c \
        ad_piofs_read.c \
        ad_piofs_resize.c \
        ad_piofs_seek.c \
        ad_piofs_wait.c \
        ad_piofs_wrcoll.c \
        ad_piofs_write.c
