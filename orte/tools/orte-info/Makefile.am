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
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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
            -DOMPI_BUILD_HOST="\"`(hostname || uname -n) | sed 1q`\"" \
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

include $(top_srcdir)/Makefile.ompi-rules

man_pages = orte-info.1
EXTRA_DIST = $(man_pages:.1=.1in)

if OPAL_INSTALL_BINARIES

bin_PROGRAMS = orte-info

dist_ortedata_DATA = help-orte-info.txt

nodist_man_MANS = $(man_pages)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

endif

orte_info_SOURCES = \
        orte-info.h \
        orte-info.c \
        output.c \
        param.c \
        components.c \
        version.c

orte_info_LDADD = \
	$(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la \
	$(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || rm -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

distclean-local:
	rm -f $(man_pages)
