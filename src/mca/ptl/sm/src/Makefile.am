# -*- makefile -*-
#
# $HEADER$
#

include $(top_lam_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_lam_builddir)/src/include \
        -I$(top_lam_srcdir)/src \
        -I$(top_lam_srcdir)/src/include

noinst_LTLIBRARIES = libmca_ptl_sm.la
libmca_ptl_sm_la_SOURCES = \
	ptl_sm.c \
	ptl_sm.h \
	ptl_sm_mmap.c \
	ptl_sm_mmap.h \
	ptl_sm_module.c \
	ptl_sm_sendreq.c \
	ptl_sm_sendfrag.c \
	ptl_sm_recvfrag.c
