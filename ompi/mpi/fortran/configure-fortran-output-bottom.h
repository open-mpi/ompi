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

! This file is included after configure-fortran-output.h, and does some
! conditional logic based on the #define's values from that file.

#ifndef OMPI_FORTRAN_CONFIGURE_OUTPUT_BOTTOM_H
#define OMPI_FORTRAN_CONFIGURE_OUTPUT_BOTTOM_H

! ABSTRACT or not
#if OMPI_FORTRAN_HAVE_ABSTRACT
#define OMPI_ABSTRACT ABSTRACT
#else
#define OMPI_ABSTRACT
#endif

! ASYNCHRONOUS or not
#if OMPI_FORTRAN_HAVE_ASYNCHRONOUS
#define OMPI_ASYNCHRONOUS , ASYNCHRONOUS
#else
#define OMPI_ASYNCHRONOUS
#endif

! PRIVATE or not
#if OMPI_FORTRAN_HAVE_PRIVATE
#define OMPI_PRIVATE , PRIVATE
#else
#define OMPI_PRIVATE
#endif

! PROTECTED or not
#if OMPI_FORTRAN_HAVE_PROTECTED
#define OMPI_PROTECTED , PROTECTED
#else
#define OMPI_PROTECTED
#endif

#endif
