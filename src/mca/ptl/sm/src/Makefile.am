#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# A word of explanation...
#
# We link this component to common/sm/libmca_common_sm.la. This is
# because all shared-memory based components (e.g., mpool, ptl, etc.)
# need to share some common code and data.  There's two cases:
#
# 1. libmca_common_sm.la is a shared library.  By linking that shared
# library to all components that need it, the OS linker will
# automatically load it into the process as necessary, and there will
# only be one copy (i.e., all the components will share *one* copy of
# the code and data).
#
# 2. libmca_common_sm.la is a static library.  In this case, it will
# be rolled up into the top-level libmpi.la.  It will also be rolled
# into each component, but then the component will also be rolled up
# into the upper-level libmpi.la.  Linkers universally know how to
# "figure this out" so that we end up with only one copy of the code
# and data.

include $(top_ompi_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_ptl_sm.la librecompile.la
libmca_ptl_sm_la_LIBADD = \
	librecompile.la \
        $(top_ompi_builddir)/src/mca/common/sm/libmca_common_sm.la
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
		ln -s ptl_sm_send.c $@; \
	fi

