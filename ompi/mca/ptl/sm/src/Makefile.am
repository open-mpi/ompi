#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_ptl_sm.la librecompile.la
libmca_ptl_sm_la_LIBADD = librecompile.la
libmca_ptl_sm_la_SOURCES = \
        ptl_sm.c \
        ptl_sm.h \
        ptl_sm_address.h \
        ptl_sm_component.c \
        ptl_sm_frag.h \
        ptl_sm_frag.c \
        ptl_sm_peer.h \
        ptl_sm_recvfrag.h \
        ptl_sm_recvfrag.c \
        ptl_sm_send.c \
        ptl_sm_sendreq.c \
        ptl_sm_sendreq.h \
        ptl_sm_sendfrag.c \
        ptl_sm_sendfrag.h

libmca_ptl_sm_la_DEPENDENCIES = librecompile.la

nodist_librecompile_la_SOURCES = \
	ptl_sm_send_alternate.c
librecompile_la_CPPFLAGS = -DSM_COMMON_BASE_ADDR

#
# The "send alternate" file is really the same as the "send" file, but
# compiled with a different #define
#
ptl_sm_send_alternate.c:
	if test ! -f $@; then \
		ln -sf $(srcdir)/ptl_sm_send.c $@; \
	fi

