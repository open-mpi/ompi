# -*- makefile -*-
#
# $HEADER$
#

include $(top_lam_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_lam_builddir)/src/include \
        -I$(top_lam_srcdir)/src \
        -I$(top_lam_srcdir)/src/include

noinst_LTLIBRARIES = libmca_coll_basic.la
libmca_coll_basic_la_SOURCES = \
	pml_teg.c \
	pml_teg.h \
	pml_teg_comm.h \
	pml_teg_isend.c \
	pml_teg_proc.c \
	pml_teg_proc.h \
	pml_teg_sendreq.c \
	pml_teg_sendreq.h \
	pml_ptl_array.c  \
	pml_ptl_array.h 
