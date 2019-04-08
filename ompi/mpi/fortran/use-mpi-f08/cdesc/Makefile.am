# -*- makefile -*-
#
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# This Makefile is only relevant if we're building the "use mpi_f08"
# MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

AM_CPPFLAGS = -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

###########################################################################

module_sentinel_file =

if OMPI_FORTRAN_HAVE_C_ISO_FORTRAN
module_sentinel_file += \
        libusempif08_cdesc.la
endif

libusempif08_cdesc_la_SOURCES = \
        bindings.h \
        cdesc.h \
        cdesc.c

noinst_LTLIBRARIES = $(module_sentinel_file)

endif
