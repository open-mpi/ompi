#
# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = \
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
if OSHMEM_PROFILING
    AM_CPPFLAGS += -DOSHMEM_PROFILING=1
else
    AM_CPPFLAGS += -DOSHMEM_PROFILING=0
endif

if OSHMEM_BUILD_FORTRAN_BINDINGS
    AM_CPPFLAGS += -DOSHMEM_BUILD_FORTRAN_BINDINGS=1
else
    AM_CPPFLAGS += -DOSHMEM_BUILD_FORTRAN_BINDINGS=0
endif

include $(top_srcdir)/Makefile.man-page-rules

nodist_man_MANS =
bin_PROGRAMS =

man_pages = oshmem_info.1

EXTRA_DIST = $(man_pages:.1=.1in)

if PROJECT_OSHMEM
# Only build/install the binary and man pages if we're building oshmem
bin_PROGRAMS += oshmem_info
nodist_man_MANS += $(man_pages)
dist_ompidata_DATA = \
     help-oshmem-info.txt
endif


# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h

oshmem_info_SOURCES = \
        oshmem_info.h \
        oshmem_info.c \
        param.c

oshmem_info_LDADD = $(top_builddir)/ompi/libmpi.la
oshmem_info_LDADD += $(top_builddir)/oshmem/liboshmem.la
if OMPI_RTE_ORTE
oshmem_info_LDADD +=  $(top_builddir)/orte/libopen-rte.la
endif
oshmem_info_LDADD += $(top_builddir)/opal/libopen-pal.la

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || rm -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)

distclean-local:
	rm -f $(man_pages)
