! -*- f90 -*-
!
! Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
!
! $COPYRIGHT$
!
! Additional copyrights may follow
! 
! $HEADER$
!

#include "ompi/mpi/fortran/configure-fortran-output.h"


MODULE mpi_f08_sizeof
   use, intrinsic :: ISO_C_BINDING

interface MPI_Sizeof

#if OMPI_HAVE_FORTRAN_INTEGER1
   module procedure MPI_Sizeof_integer_s_1, MPI_Sizeof_integer_a_1
#endif

#if OMPI_HAVE_FORTRAN_INTEGER2
   module procedure MPI_Sizeof_integer_s_2, MPI_Sizeof_integer_a_2
#endif

#if OMPI_HAVE_FORTRAN_INTEGER4
   module procedure MPI_Sizeof_integer_s_4, MPI_Sizeof_integer_a_4
#endif

#if OMPI_HAVE_FORTRAN_INTEGER8
   module procedure MPI_Sizeof_integer_s_8, MPI_Sizeof_integer_a_8
#endif

#if OMPI_HAVE_FORTRAN_INTEGER16
   module procedure MPI_Sizeof_integer_s_16, MPI_Sizeof_integer_a_16
#endif

#if OMPI_HAVE_FORTRAN_REAL2
   module procedure MPI_Sizeof_real_s_2, MPI_Sizeof_real_a_2
#endif

#if OMPI_HAVE_FORTRAN_REAL4
   module procedure MPI_Sizeof_real_s_4, MPI_Sizeof_real_a_4
#endif

#if OMPI_HAVE_FORTRAN_REAL8
   module procedure MPI_Sizeof_real_s_8, MPI_Sizeof_real_a_8
#endif

#if OMPI_HAVE_FORTRAN_REAL16
   module procedure MPI_Sizeof_real_s_16, MPI_Sizeof_real_a_16
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX4
   module procedure MPI_Sizeof_complex_s_4, MPI_Sizeof_complex_a_4
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX8
   module procedure MPI_Sizeof_complex_s_8, MPI_Sizeof_complex_a_8
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX16
   module procedure MPI_Sizeof_complex_s_16, MPI_Sizeof_complex_a_16
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX32
   module procedure MPI_Sizeof_complex_s_32, MPI_Sizeof_complex_a_32
#endif

end interface MPI_Sizeof


CONTAINS


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sizes of integers
!

#if OMPI_HAVE_FORTRAN_INTEGER1
   subroutine MPI_Sizeof_integer_s_1(x, size, ierror)
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER1), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER1
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_s_1

   subroutine MPI_Sizeof_integer_a_1(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER1), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER1
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_a_1
#endif

#if OMPI_HAVE_FORTRAN_INTEGER2
   subroutine MPI_Sizeof_integer_s_2(x, size, ierror)
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER2), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER2
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_s_2

   subroutine MPI_Sizeof_integer_a_2(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER2), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER2
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_a_2
#endif

#if OMPI_HAVE_FORTRAN_INTEGER4
   subroutine MPI_Sizeof_integer_s_4(x, size, ierror)
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER4), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER4
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_s_4

   subroutine MPI_Sizeof_integer_a_4(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER4), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER4
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_a_4
#endif

#if OMPI_HAVE_FORTRAN_INTEGER8
   subroutine MPI_Sizeof_integer_s_8(x, size, ierror)
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER8), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER8
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_s_8

   subroutine MPI_Sizeof_integer_a_8(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER8), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER8
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_a_8
#endif

#if OMPI_HAVE_FORTRAN_INTEGER16
   subroutine MPI_Sizeof_integer_s_16(x, size, ierror)
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER16), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER16
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_s_16

   subroutine MPI_Sizeof_integer_a_16(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      INTEGER(KIND=OMPI_KIND_FORTRAN_INTEGER16), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_INTEGER16
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_integer_a_16
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sizes of reals
!

#if OMPI_HAVE_FORTRAN_REAL2
   subroutine MPI_Sizeof_real_s_2(x, size, ierror)
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL2), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL2
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_s_2

   subroutine MPI_Sizeof_real_a_2(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL2), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL2
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_a_2
#endif

#if OMPI_HAVE_FORTRAN_REAL4
   subroutine MPI_Sizeof_real_s_4(x, size, ierror)
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL4), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL4
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_s_4

   subroutine MPI_Sizeof_real_a_4(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL4), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL4
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_a_4
#endif

#if OMPI_HAVE_FORTRAN_REAL8
   subroutine MPI_Sizeof_real_s_8(x, size, ierror)
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL8), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL8
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_s_8

   subroutine MPI_Sizeof_real_a_8(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL8), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL8
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_a_8
#endif

#if OMPI_HAVE_FORTRAN_REAL16
   subroutine MPI_Sizeof_real_s_16(x, size, ierror)
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL16), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL16
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_s_16

   subroutine MPI_Sizeof_real_a_16(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      REAL(KIND=OMPI_KIND_FORTRAN_REAL16), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_REAL16
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_real_a_16
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sizes of complex
!

#if OMPI_HAVE_FORTRAN_COMPLEX4
   subroutine MPI_Sizeof_complex_s_4(x, size, ierror)
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX4), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX4
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_s_4

   subroutine MPI_Sizeof_complex_a_4(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX4), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX4
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_a_4
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX8
   subroutine MPI_Sizeof_complex_s_8(x, size, ierror)
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX8), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX8
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_s_8

   subroutine MPI_Sizeof_complex_a_8(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX8), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX8
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_a_8
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX16
   subroutine MPI_Sizeof_complex_s_16(x, size, ierror)
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX16), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX16
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_s_16

   subroutine MPI_Sizeof_complex_a_16(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX16), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX16
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_a_16
#endif

#if OMPI_HAVE_FORTRAN_COMPLEX32
   subroutine MPI_Sizeof_complex_s_32(x, size, ierror)
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX32), INTENT(IN) :: x
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX32
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_s_32

   subroutine MPI_Sizeof_complex_a_32(x, size, ierror)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      COMPLEX(KIND=OMPI_KIND_FORTRAN_COMPLEX32), INTENT(IN) :: x(*)
      INTEGER, INTENT(OUT) :: size
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      size = OMPI_SIZEOF_FORTRAN_COMPLEX32
      if (present(ierror)) ierror = 0

   end subroutine MPI_Sizeof_complex_a_32
#endif


END MODULE mpi_f08_sizeof
