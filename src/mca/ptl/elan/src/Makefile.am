# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options


# FIXME: to make the path to qsnet2libs a configure option

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include  \
	-I/usr/lib/qsnet/elan4/include \
	-I/home/1/yuw/elan/qsnet2libs-1.6.4-1/include \
	-I/home/1/yuw/elan/qsnet2libs-1.6.4-1/elan4lib/include \
	-I/home/1/yuw/elan/qsnet2libs-1.6.4-1/elan4lib/elan4 \
	-I/home/1/yuw/elan/qsnet2libs-1.6.4-1/elan4lib/common 

AM_LDFLAGS = -lelan -lelanctrl -lrms -lrmscall -lelan4

noinst_LTLIBRARIES = libmca_ptl_elan.la
libmca_ptl_elan_la_SOURCES = \
	elan_config.h \
	ptl_elan.h \
	ptl_elan_frag.h \
	ptl_elan_req.h \
	ptl_elan_proc.h \
	ptl_elan_addr.h \
	ptl_elan_peer.h \
	ptl_elan_frag.c \
	ptl_elan_req.c \
	ptl_elan_proc.c \
	ptl_elan_module.c \
	ptl_elan_priv.c \
	ptl_elan_init.c \
	ptl_elan.c 


