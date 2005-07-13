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

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ptl_sm_DSO
component_noinst =
component_install = mca_ptl_sm.la
else
component_noinst = libmca_ptl_sm.la
component_install =
endif

EXTRA_DIST = 
sm_SOURCES = src/ptl_sm.c src/ptl_sm.h src/ptl_sm_address.h src/ptl_sm_component.c \
        src/ptl_sm_frag.h src/ptl_sm_frag.c src/ptl_sm_peer.h src/ptl_sm_recvfrag.h \
        src/ptl_sm_recvfrag.c src/ptl_sm_send.c src/ptl_sm_sendreq.c \
        src/ptl_sm_sendreq.h src/ptl_sm_sendfrag.c src/ptl_sm_sendfrag.h

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_sm_la_SOURCES = $(sm_SOURCES)
mca_ptl_sm_la_DEPENDENCIES = librecompile.la
mca_ptl_sm_la_LIBADD = librecompile.la
mca_ptl_sm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst) librecompile.la
libmca_ptl_sm_la_SOURCES = $(sm_SOURCES)
libmca_ptl_sm_la_DEPENDENCIES = librecompile.la
libmca_ptl_sm_la_LIBADD = librecompile.la
libmca_ptl_sm_la_LDFLAGS = -module -avoid-version

nodist_librecompile_la_SOURCES = src/ptl_sm_send_alternate.c
librecompile_la_CPPFLAGS = -DSM_COMMON_BASE_ADDR

#
# The "send alternate" file is really the same as the "send" file, but
# compiled with a different #define
#
$(srcdir)/src/ptl_sm_send_alternate.c:
	if test ! -f $@; then \
		ln -sf ptl_sm_send.c $@; \
	fi

