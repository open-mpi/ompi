# -*- makefile -*-
#
# $HEADER$
#

include $(top_lam_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_lam_builddir)/src/include \
        -I$(top_lam_srcdir)/src \
        -I$(top_lam_srcdir)/src/include

noinst_LTLIBRARIES = libmca_pml_teg.la
libmca_pml_teg_la_SOURCES = \
	pml_teg.c \
	pml_teg.h \
	pml_teg_free.c \
	pml_teg_irecv.c \
	pml_teg_isend.c \
	pml_teg_module.c \
	pml_teg_proc.c \
	pml_teg_proc.h \
	pml_teg_progress.c \
	pml_teg_recvreq.c \
	pml_teg_recvreq.h \
	pml_teg_sendreq.c \
	pml_teg_sendreq.h \
	pml_teg_start.c \
	pml_teg_test.c \
	pml_teg_wait.c \
	pml_ptl_array.c  \
	pml_ptl_array.h 
