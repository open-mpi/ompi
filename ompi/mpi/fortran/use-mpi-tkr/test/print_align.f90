!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!
! print_align.f90
!   - prints out alignment of real types
!

program main

  type AlignReal
    integer(selected_int_kind(1)) :: a
    real(selected_real_kind(6)) :: w
    character b
    real(selected_real_kind(6)) :: x
    real(selected_real_kind(6)) :: y
    character c
    real(selected_real_kind(6)) :: z
  end type

  type AlignDouble
    integer(selected_int_kind(1)) :: a
    real(selected_real_kind(15)) :: w
    character b
    real(selected_real_kind(15)) :: x
    real(selected_real_kind(15)) :: y

    character c
    real(selected_real_kind(15)) :: z
  end type

  type AlignQuad
    integer(selected_int_kind(1)) :: a
    real(selected_real_kind(31)) :: w
    character b
    real(selected_real_kind(31)) :: x
    real(selected_real_kind(31)) :: y
    character c
    real(selected_real_kind(31)) :: z
  end type

  type AlignComplex
    integer(selected_int_kind(1)) :: a
    complex(selected_real_kind(6)) :: w
    character b
    complex(selected_real_kind(6)) :: x
    complex(selected_real_kind(6)) :: y
    character c
    complex(selected_real_kind(6)) :: z
  end type

  type AlignDoubleComplex
    integer(selected_int_kind(1)) :: a
    complex(selected_real_kind(15)) :: w
    character b
    complex(selected_real_kind(15)) :: x
    complex(selected_real_kind(15)) :: y
    character c
    complex(selected_real_kind(15)) :: z
  end type

  type AlignQuadComplex
    integer(selected_int_kind(1)) :: a
    complex(selected_real_kind(31)) :: w
    character b
    complex(selected_real_kind(31)) :: x
    complex(selected_real_kind(31)) :: y
    character c
    complex(selected_real_kind(31)) :: z
  end type

  external align_c

  type(AlignReal) :: ar
  type(AlignDouble) :: ad
  type(AlignQuad) :: aq
  type(AlignComplex) :: ac
  type(AlignDoubleComplex) :: adc
  type(AlignQuadComplex) :: aqc

  call align_c(ar%a, ar%w, ar%x, ar%y, ar%z)
  print *, "alignment of real: ", ar%a
  call align_c(ad%a, ad%w, ad%x, ad%y, ad%z)
  print *, "alignment of double: ", ad%a
  call align_c(aq%a, aq%w, aq%x, aq%y, aq%z)
  print *, "alignment of quad: ", aq%a
  call align_c(ac%a, ac%w, ac%x, ac%y, ac%z)
  print *, "alignment of complex: ", ac%a
  call align_c(adc%a, adc%w, adc%x, adc%y, adc%z)
  print *, "alignment of double complex: ", adc%a
  call align_c(aqc%a, aqc%w, aqc%x, aqc%y, aqc%z)
  print *, "alignment of quad complex: ", aqc%a

end program main
