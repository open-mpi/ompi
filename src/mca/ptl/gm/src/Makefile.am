# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include \
    -I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include \
    -I/home/devel/gm-2.0.1_Linux/include \
    -L/home/devel/gm-2.0.1_Linux/lib 


noinst_LTLIBRARIES = libmca_ptl_gm.la
libmca_ptl_gm_la_SOURCES = \
	ptl_gm.c \
    ptl_gm.h \
    ptl_gm_proc.c \
    ptl_gm_proc.h \
    ptl_gm_module.c \
    ptl_gm_req.c \
    ptl_gm_req.h \
    ptl_address.h \
    ptl_gm_priv.h \
    ptl_gm_sendfrag.c \
    ptl_gm_sendfrag.h \
    ptl_gm_peer.h


