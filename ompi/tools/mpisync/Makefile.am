#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2014      Artem Polyakov <artpol84@gmail.com>
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2017-2022 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

EXTRA_DIST = mpirun_prof \
	     ompi_timing_post

if OPAL_INSTALL_TIMING_BINARIES

bin_PROGRAMS = mpisync
bin_SCRIPTS = mpirun_prof ompi_timing_post

endif

mpisync_SOURCES = \
        hpctimer.h \
        hpctimer.c \
        mpigclock.h \
        mpigclock.c \
        sync.c

mpisync_LDADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la
mpisync_LDADD += $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
