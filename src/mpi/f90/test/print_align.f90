!
! Copyright (c) 2004-2005 The Trustees of Indiana University.
!                         All rights reserved.
! Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
!                         All rights reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
!                         University of Stuttgart.  All rights reserved.
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
    character a
    real(selected_real_kind(6)) :: w
    character b
    real(selected_real_kind(6)) :: x
    real(selected_real_kind(6)) :: y
    character c
    real(selected_real_kind(6)) :: z
  end type

  type AlignDouble
    character a
    real(selected_real_kind(15)) :: w
    character b
    real(selected_real_kind(15)) :: x
    real(selected_real_kind(15)) :: y

    character c
    real(selected_real_kind(15)) :: z
  end type

  type AlignQuad
    character a
    real(selected_real_kind(31)) :: w
    character b
    real(selected_real_kind(31)) :: x
    real(selected_real_kind(31)) :: y
    character c
    real(selected_real_kind(31)) :: z
  end type

  type AlignComplex
    character a
    complex(selected_real_kind(6)) :: w
    character b
    complex(selected_real_kind(6)) :: x
    complex(selected_real_kind(6)) :: y
    character c
    complex(selected_real_kind(6)) :: z
  end type

  type AlignDoubleComplex
    character a
    complex(selected_real_kind(15)) :: w
    character b
    complex(selected_real_kind(15)) :: x
    complex(selected_real_kind(15)) :: y
    character c
    complex(selected_real_kind(15)) :: z
  end type

  type AlignQuadComplex
    character a
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
  print *, "is alignment of real"
  call align_c(ad%a, ad%w, ad%x, ad%y, ad%z)
  print *, "is alignment of double"
  call align_c(aq%a, aq%w, aq%x, aq%y, aq%z)
  print *, "is alignment of quad"
  call align_c(ac%a, ac%w, ac%x, ac%y, ac%z)
  print *, "is alignment of complex"
  call align_c(adc%a, adc%w, adc%x, adc%y, adc%z)
  print *, "is alignment of double complex"
  call align_c(aqc%a, aqc%w, aqc%x, aqc%y, aqc%z)
  print *, "is alignment of quad complex"

end program main
