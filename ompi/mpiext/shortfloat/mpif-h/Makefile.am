#
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Directory where the header file to be included in mpif-ext.h must be
# installed.
ompidir = $(ompiincludedir)/mpiext

# Just like noinst_LTLIBRARIES, set this macro to empty and
# conditionally add to it later.
nodist_ompi_HEADERS =

# Use the Automake conditional to know if we're building the mpif.h
# bindings.
if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS

# This is the header file that is installed.
nodist_ompi_HEADERS += mpiext_shortfloat_mpifh.h

endif
