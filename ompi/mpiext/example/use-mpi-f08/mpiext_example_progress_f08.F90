! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$
!

! This file implements the mpi_f08_ext bindings.  It has no file name
! conventions and generally implements whatever the extension needs.

subroutine OMPI_Progress_f08(count, comm, ierror)
  ! mpi_f08_types is an internal Open MPI module (i.e., it isn't part
  ! of the MPI-3 specification) that is built as part of OMPI's F08
  ! bindings.  It contains all the types that we need for MPI stuff.
  ! We use the "only" clause just to be a little nice in the scope of
  ! things that we grab from that file.
  use :: mpi_f08_types, only : MPI_Comm
  
  implicit none
  
  ! Prototype the back-end function in mpif-h that we'll be invoking
  ! at the bottom of this subroutine.  This is a little klunky and
  ! for demonstration purposes only; real extensions might want to
  ! make their own module that is simply used here (e.g., especially
  ! if an extension provides multiple interfaces).
  interface
     ! Note that we list the back-end C function name in the mpif.h
     ! bindings that this interface will invoke.  See below.
     subroutine OMPI_Progress_f(count, comm, ierror) &
          BIND(C, name="OMPI_Progress_f")
       implicit none
       INTEGER, INTENT(IN) :: count, comm
       INTEGER, INTENT(OUT) :: ierror
     end subroutine OMPI_Progress_f
  end interface
  
  ! Types for this subroutine's parameters and local variables.
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
  integer :: count, c_ierror
  
  ! Here we call the the back-end C function in the mpif.h bindings,
  ! but convert the mpi_f08-style MPI handles to mpif.h-style handles
  ! (by taking the MPI_VAL member out of its "struct").
  call OMPI_Progress_f(count, comm%MPI_VAL, c_ierror)
  
  ! ierror is optional in the mpi_f08 bindings, so keep that
  ! convention here, too -- assign to ierror *if it was provided*.
  if (present(ierror)) ierror = c_ierror
end subroutine OMPI_Progress_f08
