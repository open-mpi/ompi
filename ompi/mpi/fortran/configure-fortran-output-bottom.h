! -*- fortran -*-
!
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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

! Whether we're using wrappers or not.
! Currently, we're *always* using wrappers.  This can be optimized in
! the future for "good" compilers.
#if OMPI_FORTRAN_NEED_WRAPPER_ROUTINES
#define OMPI_F08_INTERFACE_BIND_C(foo)
#else
#define OMPI_F08_INTERFACE_BIND_C(foo) BIND(C,name=foo)
#endif

! PROCEDURE or not
#if OMPI_HAVE_PROCEDURE
#define OMPI_PROCEDURE(name) PROCEDURE(name)
#else
#define OMPI_PROCEDURE(name) EXTERNAL
#endif

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

#endif
