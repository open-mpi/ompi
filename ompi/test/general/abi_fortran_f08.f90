! -*- fortran -*-
!
! Copyright (c) 2026      Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
! SPDX-License-Identifier: BSD-3-Clause-Open-MPI
!
! Fortran (mpi_f08) unit test for the ABI Fortran-boolean helpers
! MPI_Abi_get_fortran_booleans and MPI_Abi_set_fortran_booleans.
!
! The sibling C test (abi_fortran.c) drives the internal
! ompi_abi_get/set_fortran_booleans() routines directly.  This test
! instead goes through the generated mpi_f08 bindings, so it exercises the
! LOGICAL_PTR / LOGICAL_PTR_OUT type machinery in the Fortran binding
! generator (ompi/mpi/bindings/ompi_bindings/fortran_type.py): a native
! Fortran LOGICAL temporary is handed to the C wrapper by reference, the C
! code reads/writes its raw bit pattern, and the result is read back as a
! LOGICAL here.  A regression in that path (for example, narrowing the
! temporary to a non-native representation) would make the round-trip
! values below compare unequal.
!
! This program is only built when the mpi_f08 bindings are configured;
! whenever those bindings exist the Open MPI build necessarily knows the
! Fortran compiler's LOGICAL representation (OMPI_HAVE_FORTRAN_LOGICAL),
! so the expected results are:
!
!   - get with the default LOGICAL size succeeds, reports is_set = .true.,
!     and round-trips the compiler-derived .true./.false. values;
!   - get with a non-power-of-two size returns MPI_ERR_ARG;
!   - set returns MPI_ERR_ABI (the implementation owns the representation
!     and rejects user registration).
!
! The program is single-process and needs no launcher.  It reports failure
! by printing an ERROR line and executing "error stop 1" so that a failure
! produces a non-zero exit status for "make check" (a bare Fortran "stop"
! exits zero and would hide the failure).
!
      program abi_fortran_f08
      use mpi_f08
      implicit none

      integer :: ierr, logical_size
      logical :: logical_true, logical_false, is_set

      call MPI_Init(ierr)
      if (MPI_SUCCESS /= ierr) then
         print *, "ERROR: MPI_Init returned ", ierr
         error stop 1
      endif

!     These ABI query routines return their status through the ierror
!     argument rather than invoking an error handler, but install
!     MPI_ERRORS_RETURN on MPI_COMM_WORLD regardless so a future change in
!     error-reporting behavior cannot turn an expected error return into a
!     process abort.
      call MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN, ierr)

!     Size in bytes of a default Fortran LOGICAL.  storage_size() returns a
!     value in bits.
      logical_size = storage_size(logical_true) / 8

!     ---------------------------------------------------------------
!     get with the default LOGICAL size: must succeed, report that the
!     values are available, and return the compiler's native .true. and
!     .false. bit patterns.
!     ---------------------------------------------------------------
      logical_true = .false.
      logical_false = .true.
      is_set = .false.
      call MPI_Abi_get_fortran_booleans(logical_size, logical_true, &
                                        logical_false, is_set, ierr)
      if (MPI_SUCCESS /= ierr) then
         print *, "ERROR: MPI_Abi_get_fortran_booleans(default size) ", &
                  "returned ierr=", ierr
         error stop 1
      endif
      if (.not. is_set) then
         print *, "ERROR: MPI_Abi_get_fortran_booleans(default size) ", &
                  "reported is_set=.false."
         error stop 1
      endif
      if (.not. logical_true) then
         print *, "ERROR: get returned logical_true that is not .true."
         error stop 1
      endif
      if (logical_false) then
         print *, "ERROR: get returned logical_false that is not .false."
         error stop 1
      endif

!     ---------------------------------------------------------------
!     get with a non-power-of-two size must be rejected with MPI_ERR_ARG.
!     ---------------------------------------------------------------
      call MPI_Abi_get_fortran_booleans(3, logical_true, logical_false, &
                                        is_set, ierr)
      if (MPI_ERR_ARG /= ierr) then
         print *, "ERROR: MPI_Abi_get_fortran_booleans(3) expected ", &
                  "MPI_ERR_ARG, got ierr=", ierr
         error stop 1
      endif

!     ---------------------------------------------------------------
!     set must be rejected with MPI_ERR_ABI: this build knows the Fortran
!     compiler's LOGICAL representation, so the get path returns the
!     compiler-derived values and never consults a user registration.
!     ---------------------------------------------------------------
      logical_true = .true.
      logical_false = .false.
      call MPI_Abi_set_fortran_booleans(logical_size, logical_true, &
                                        logical_false, ierr)
      if (MPI_ERR_ABI /= ierr) then
         print *, "ERROR: MPI_Abi_set_fortran_booleans expected ", &
                  "MPI_ERR_ABI, got ierr=", ierr
         error stop 1
      endif

      print *, "abi_fortran_f08: all checks passed"

      call MPI_Finalize(ierr)
      if (MPI_SUCCESS /= ierr) then
         print *, "ERROR: MPI_Finalize returned ", ierr
         error stop 1
      endif

      end program abi_fortran_f08
