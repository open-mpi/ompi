# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

#Assumption. We support only i686 for now.
#AM_CPPFLAGS = -fPIC -D_ELAN4 -D_REENTRANT -O3 -malign-double -march=i686 

AM_CPPFLAGS = -fPIC -D_ELAN4 -D_REENTRANT -O3 -march=i686 
AM_CPPFLAGS += -I$(top_ompi_builddir)/src/include                  \
	-I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include  \
	-I/usr/lib/qsnet/elan4/include 

noinst_LTLIBRARIES = libmca_ptl_elan.la
libmca_ptl_elan_la_SOURCES = \
	elan_config.h        \
	ptl_elan.h           \
	ptl_elan_frag.h      \
	ptl_elan_proc.h      \
	ptl_elan_peer.h      \
	ptl_elan_priv.h      \
	ptl_elan_frag.c      \
	ptl_elan_proc.c      \
	ptl_elan_priv.c      \
	ptl_elan_peer.c      \
	ptl_elan_init.c      \
	ptl_elan_comm_init.c \
	ptl_elan_module.c    \
	ptl_elan.c  
