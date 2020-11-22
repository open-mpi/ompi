#
# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2014-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
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
            -DOMPI_BUILD_HOST="\"$${HOSTNAME:-`(hostname || uname -n) | sed 1q`}\"" \
            -DOMPI_BUILD_DATE="\"`$(top_srcdir)/config/getdate.sh`\"" \
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

include $(top_srcdir)/Makefile.ompi-rules

bin_PROGRAMS =

if PROJECT_OSHMEM
# Only build/install the binary pages if we're building oshmem
bin_PROGRAMS += oshmem_info
dist_ompidata_DATA = \
     help-oshmem-info.txt
endif


oshmem_info_SOURCES = \
        oshmem_info.h \
        oshmem_info.c \
        param.c

oshmem_info_LDADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la
oshmem_info_LDADD += $(top_builddir)/oshmem/liboshmem.la
oshmem_info_LDADD += $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
