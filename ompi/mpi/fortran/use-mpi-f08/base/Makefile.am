# -*- makefile -*-
#
# Copyright (c) 2019      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2020      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
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

# This directory only exists so that we can separate C compilation
# from Fortran compilation.  Specifically: note that Automake's
# Fortran-buidling rules uses CPPFLAGS and AM_CPPFLAGS.  This can
# cause weirdness (e.g.,
# https://github.com/open-mpi/ompi/issues/7253).  So when compiling
# Fortran, we should zero out CPPFLAGS and AM_CPPFLAGS.

# HOWEVER, we have one .c file in the use-mpi-f08 library.  So we have
# to split it out to its own directory / Makefile.am where CPPFLAGS /
# AM_CPPFLAGS are *not* zeroed out.

noinst_LTLIBRARIES = libusempif08_ccode.la

libusempif08_ccode_la_SOURCES = \
        buffer_detach.c

endif
