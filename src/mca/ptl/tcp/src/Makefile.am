# -*- makefile -*-
#
# $HEADER$
#

include $(top_lam_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_lam_builddir)/src/include \
        -I$(top_lam_srcdir)/src \
        -I$(top_lam_srcdir)/src/include

noinst_LTLIBRARIES = libmca_ptl_tcp.la
libmca_ptl_tcp_la_SOURCES = \
	ptl_tcp.c \
	ptl_tcp.h \
        ptl_tcp_addr.h \
	ptl_tcp_peer.c \
	ptl_tcp_peer.h \
	ptl_tcp_proc.c \
	ptl_tcp_proc.h \
	ptl_tcp_module.c \
	ptl_tcp_recvfrag.c \
	ptl_tcp_recvfrag.h \
	ptl_tcp_sendfrag.c \
	ptl_tcp_sendfrag.h \
	ptl_tcp_sendreq.c \
	ptl_tcp_sendreq.h
