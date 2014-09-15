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
# Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2014      Artem Polyakov <artpol84@gmail.com>
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



AM_CFLAGS = \
            -DOPAL_CONFIGURE_USER="\"@OPAL_CONFIGURE_USER@\"" \
            -DOPAL_CONFIGURE_HOST="\"@OPAL_CONFIGURE_HOST@\"" \
            -DOPAL_CONFIGURE_DATE="\"@OPAL_CONFIGURE_DATE@\"" \
            -DOMPI_BUILD_USER="\"$$USER\"" \
            -DOMPI_BUILD_HOST="\"`hostname`\"" \
            -DOMPI_BUILD_DATE="\"`date`\"" \
            -DOMPI_BUILD_CFLAGS="\"@CFLAGS@\"" \
            -DOMPI_BUILD_CPPFLAGS="\"@CPPFLAGS@\"" \
            -DOMPI_BUILD_CXXFLAGS="\"@CXXFLAGS@\"" \
            -DOMPI_BUILD_CXXCPPFLAGS="\"@CXXCPPFLAGS@\"" \
            -DOMPI_BUILD_FFLAGS="\"@FFLAGS@\"" \
            -DOMPI_BUILD_FCFLAGS="\"@FCFLAGS@\"" \
            -DOMPI_BUILD_LDFLAGS="\"@LDFLAGS@\"" \
            -DOMPI_BUILD_LIBS="\"@LIBS@\"" \
            -DOPAL_CC_ABSOLUTE="\"@OPAL_CC_ABSOLUTE@\"" \
            -DOMPI_CXX_ABSOLUTE="\"@OMPI_CXX_ABSOLUTE@\""

#if (OPAL_INSTALL_BINARIES && OPAL_ENABLE_TIMING)

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

mpisync_LDADD = $(top_builddir)/ompi/libmpi.la
if OMPI_RTE_ORTE
mpisync_LDADD +=  $(top_builddir)/orte/libopen-rte.la
endif
mpisync_LDADD += $(top_builddir)/opal/libopen-pal.la

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || rm -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

distclean-local:
	rm -f $(man_pages)
