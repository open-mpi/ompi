! -*- fortran -*-
!
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
!
! $COPYRIGHT$
!
! Additional copyrights may follow
! 
! $HEADER$
!

!     This file exists to have a Fortran-safe header file that has
!     results from configure tests.

#ifndef OMPI_FORTRAN_CONFIGURE_OUTPUT_H
#define OMPI_FORTRAN_CONFIGURE_OUTPUT_H

! Whether we're building the MPI IO interface or not
#define OMPI_PROVIDE_MPI_FILE_INTERFACE 1

! Whether we're using wrapper F08 functions or not
#define OMPI_FORTRAN_NEED_WRAPPER_ROUTINES 0

! Whether we have PRIVATE or not
#define OMPI_FORTRAN_HAVE_PRIVATE 0

! Whether we have PROTECTED or not
#define OMPI_FORTRAN_HAVE_PROTECTED 0

! Whether we have ABSTRACT or not
#define OMPI_FORTRAN_HAVE_ABSTRACT 0

! Whether we have ASYNCHRONOUS or not
#define OMPI_FORTRAN_HAVE_ASYNCHRONOUS 0

! Whether we have ISO C binding or not
#define OMPI_FORTRAN_HAVE_ISO_C_BINDING 1

! Whether we are building the MPI F08 bindings with subarray support or not
#define OMPI_FORTRAN_SUBARRAYS_SUPPORTED .FALSE.

! Line 1 of the ignore TKR syntax
! ...JMS not figured out yet...

! Line 2 of the ignore TKR syntax
#define OMPI_FORTRAN_IGNORE_TKR_TYPE 

! Integers

#define OMPI_HAVE_FORTRAN_INTEGER1 1
#define OMPI_KIND_FORTRAN_INTEGER1 C_SIGNED_CHAR
#define OMPI_SIZEOF_FORTRAN_INTEGER1 1

#define OMPI_HAVE_FORTRAN_INTEGER2 1
#define OMPI_KIND_FORTRAN_INTEGER2 C_SHORT
#define OMPI_SIZEOF_FORTRAN_INTEGER2 2

#define OMPI_HAVE_FORTRAN_INTEGER4 1
#define OMPI_KIND_FORTRAN_INTEGER4 C_INT
#define OMPI_SIZEOF_FORTRAN_INTEGER4 4

#define OMPI_HAVE_FORTRAN_INTEGER8 1
#define OMPI_KIND_FORTRAN_INTEGER8 C_LONG_LONG
#define OMPI_SIZEOF_FORTRAN_INTEGER8 8

#define OMPI_HAVE_FORTRAN_INTEGER16 0
#define OMPI_KIND_FORTRAN_INTEGER16 0
#define OMPI_SIZEOF_FORTRAN_INTEGER16 16

! Reals

#define OMPI_HAVE_FORTRAN_REAL2 0
#define OMPI_KIND_FORTRAN_REAL2 0
#define OMPI_SIZEOF_FORTRAN_REAL2 4

#define OMPI_HAVE_FORTRAN_REAL4 1
#define OMPI_KIND_FORTRAN_REAL4 C_FLOAT
#define OMPI_SIZEOF_FORTRAN_REAL4 4

#define OMPI_HAVE_FORTRAN_REAL8 1
#define OMPI_KIND_FORTRAN_REAL8 C_DOUBLE
#define OMPI_SIZEOF_FORTRAN_REAL8 8

#define OMPI_HAVE_FORTRAN_REAL16 1
#define OMPI_KIND_FORTRAN_REAL16 C_LONG_DOUBLE
#define OMPI_SIZEOF_FORTRAN_REAL16 16

! Complexes

#define OMPI_HAVE_FORTRAN_COMPLEX4 0
#define OMPI_KIND_FORTRAN_COMPLEX4 0
#define OMPI_SIZEOF_FORTRAN_COMPLEX4 4

#define OMPI_HAVE_FORTRAN_COMPLEX8 1
#define OMPI_KIND_FORTRAN_COMPLEX8 C_FLOAT_COMPLEX
#define OMPI_SIZEOF_FORTRAN_COMPLEX8 8

#define OMPI_HAVE_FORTRAN_COMPLEX16 1
#define OMPI_KIND_FORTRAN_COMPLEX16 C_DOUBLE_COMPLEX
#define OMPI_SIZEOF_FORTRAN_COMPLEX16 16

#define OMPI_HAVE_FORTRAN_COMPLEX32 1
#define OMPI_KIND_FORTRAN_COMPLEX32 C_LONG_DOUBLE_COMPLEX
#define OMPI_SIZEOF_FORTRAN_COMPLEX32 32

! Include some post-processing, based on the values from above

#include "ompi/mpi/fortran/configure-fortran-output-bottom.h"

#endif
