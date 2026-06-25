! -*- fortran -*-
!
! Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
! Fortran driver template for the C<->Fortran attribute interlanguage
! test.  Adapted from the open-mpi/ompi-tests simple/attr test
! (attr_fortran_source.F90).  The placeholders FORTRAN_SUB_LINE1,
! FORTRAN_SUB_LINE2, and FORTRAN_*_TYPE are substituted by the Makefile
! to generate the mpif.h, "use mpi", and "use mpi_f08" variants.
!

!     This test checks the 9 possible cases of MPI attributes reads and
!     writes.  See ompi/attribute/attribute.c for details, but
!     here's a summary:
!
!     1. C writes, C reads
!     2. C writes, Fortran MPI-1 reads
!     3. C writes, Fortran MPI-2 reads
!     4. Fortran MPI-1 writes, C reads
!     5. Fortran MPI-1 writes, Fortran MPI-1 reads
!     6. Fortran MPI-1 writes, Fortran MPI-2 reads
!     7. Fortran MPI-2 writes, C reads
!     8. Fortran MPI-2 writes, Fortran MPI-1 reads
!     9. Fortran MPI-2 writes, Fortran MPI-2 reads
!
!     MPI has different rules for each case.  We test them all here.
!     Ugh.
!
      program main
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer ierr
      integer bogus
      integer(KIND=MPI_ADDRESS_KIND) :: aint

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

      call MPI_INIT(ierr)

      aint = 1
      call MPI_WIN_CREATE(bogus, aint, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &
          win, ierr)

      call C_CREATE_KEYVAL(keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3)
      call F_MPI1_CREATE_KEYVAL()
      call F_MPI2_CREATE_KEYVAL()

      call C_WRITE(keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, win)
      call F_MPI1_WRITE()
      call F_MPI2_WRITE()

      call C_READ(keyval_c1, keyval_c2, &
          keyval_comm_c1, keyval_comm_c2, &
          keyval_type_c1, keyval_type_c2, &
          keyval_win_c1, keyval_win_c2, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win)
      call F_MPI1_READ()
      call F_MPI2_READ()

      call MPI_WIN_FREE(win, ierr)

      print *, "Calling MPI_Finalize"
      call MPI_FINALIZE(ierr)
      print *, "Test complete"
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! MPI-1
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI1_CREATE_KEYVAL()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer ierr
      external MPI1_COPY_FN, MPI1_DELETE_FN

      integer f_mpi11_extra, f_mpi12_extra, f_mpi13_extra
      integer(kind=MPI_ADDRESS_KIND) :: f_mpi21_extra, f_mpi22_extra, f_mpi23_extra

      common /extras/ f_mpi21_extra, f_mpi22_extra, f_mpi23_extra, &
           f_mpi11_extra, f_mpi12_extra, f_mpi13_extra

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

      print *, "Creating MPI-1 Fortran keyval 1"

      call MPI_KEYVAL_CREATE(MPI1_COPY_FN, MPI1_DELETE_FN, &
          keyval_f_mpi11, f_mpi11_extra, ierr)
      print *, "Fortran MPI-1: created keyval ", keyval_f_mpi11

      print *, "Creating MPI-1 Fortran keyval 2"

      call MPI_KEYVAL_CREATE(MPI1_COPY_FN, MPI1_DELETE_FN, &
          keyval_f_mpi12, f_mpi12_extra, ierr)
      print *, "Fortran MPI-1: created keyval ", keyval_f_mpi12

      print *, "Creating MPI-1 Fortran keyval 3"

      call MPI_KEYVAL_CREATE(MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN, &
          keyval_f_mpi13, f_mpi13_extra, ierr)
      print *, "Fortran MPI-1: created keyval ", keyval_f_mpi13

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI1_WRITE()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer ierr, key
      integer val_write, val_read
      FORTRAN_COMM_TYPE comm2
      logical flag
      integer tmp

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

! Write attribute 1

      print *, "Writing MPI-1 Fortran attribute 1"
      val_write = FORTRAN_MPI1_VALUE1
      tmp = val_write
      call MPI_ATTR_PUT(MPI_COMM_SELF, keyval_f_mpi11, val_write, ierr)

! Sanity check (unity)

      print *, "MPI-1 Fortran write sanity check"
      val_write = val_write + 1
      call F_MPI1_READ_CHECK(keyval_f_mpi11, tmp, 0)

! Dup the comm; it should follow

      print *, "MPI-1 Fortran duping comm"
      call MPI_COMM_DUP(MPI_COMM_SELF, comm2, ierr);

! Is it there?

      val_read = 0
      call MPI_ATTR_GET(comm2, keyval_f_mpi11, val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading MPI-1 Fortran attribute got flag=.false."
         stop
      endif
      if (val_read .ne. tmp * 2) then
         print *, "ERROR: reading MPI-1 Fortran attribute got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp * 2
         stop
      endif

! Ok

      call MPI_COMM_FREE(comm2, ierr)

! Write attribute 2

      print *, "Writing MPI-1 Fortran attribute 2"
      val_write = FORTRAN_MPI1_VALUE2
      tmp = val_write
      call MPI_ATTR_PUT(MPI_COMM_SELF, keyval_f_mpi12, val_write, ierr)

! Sanity check (unity)

      print *, "MPI-1 Fortran write sanity check"
      val_write = val_write + 1
      call F_MPI1_READ_CHECK(keyval_f_mpi12, tmp, 0)

! Dup the comm; it should follow

      print *, "MPI-1 Fortran duping comm"
      call MPI_COMM_DUP(MPI_COMM_SELF, comm2, ierr);

! Is it there?

      print *, "MPI-1 Fortran reading from duped comm"
      val_read = 0
      call MPI_ATTR_GET(comm2, keyval_f_mpi12, val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading MPI-1 Fortran attribute got flag=.false."
         stop
      endif
      if (val_read .ne. tmp * 2) then
         print *, "ERROR: reading MPI-1 Fortran attribute got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp * 2
         stop
      endif

! Ok

      call MPI_COMM_FREE(comm2, ierr)

! Write attribute 2

      print *, "Writing MPI-1 Fortran attribute 3"
      val_write = FORTRAN_MPI1_VALUE3
      tmp = val_write
      call MPI_ATTR_PUT(MPI_COMM_SELF, keyval_f_mpi13, val_write, ierr)

! Sanity check (unity)

      print *, "MPI-1 Fortran write sanity check"
      val_write = val_write + 1
      call F_MPI1_READ_CHECK(keyval_f_mpi13, tmp, 0)

! Dup the comm; it should NOT follow (since it was created with a
! NULL_COPY callback)

      print *, "MPI-1 Fortran duping comm"
      call MPI_COMM_DUP(MPI_COMM_SELF, comm2, ierr);

! Is it there?

      print *, "MPI-1 Fortran reading from duped comm == foo"
      val_read = 0
      call MPI_ATTR_GET(comm2, keyval_f_mpi13, val_read, flag, ierr)
      if (.true. .eqv.  flag) then
         print *, "ERROR: reading MPI-1 Fortran attribute got flag=.true."
         stop
      endif

! Ok

      call MPI_COMM_FREE(comm2, ierr)

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI1_READ()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

      print *, "Case 2: C writes, Fortran MPI-1 reads"
      call F_MPI1_READ_CHECK(keyval_c1, C_VALUE1, 1)
      call F_MPI1_READ_CHECK(keyval_c2, C_VALUE2, 1)
      call F_MPI1_READ_CHECK(keyval_c3, C_VALUE3, 1)
      call F_MPI1_READ_CHECK(keyval_comm_c1, C_VALUE1, 1)
      call F_MPI1_READ_CHECK(keyval_comm_c2, C_VALUE2, 1)
      call F_MPI1_READ_CHECK(keyval_comm_c3, C_VALUE3, 1)

      print *, "Case 5: Fortran MPI-1 writes, Fortran MPI-1 reads"
      call F_MPI1_READ_CHECK(keyval_f_mpi11, FORTRAN_MPI1_VALUE1, 0)
      call F_MPI1_READ_CHECK(keyval_f_mpi12, FORTRAN_MPI1_VALUE2, 0)
      call F_MPI1_READ_CHECK(keyval_f_mpi13, FORTRAN_MPI1_VALUE3, 0)

      print *, "Case 8: Fortran MPI-2 writes, Fortran MPI-1 reads"
      call F_MPI1_READ_CHECK(keyval_comm_f_mpi21, &
                            FORTRAN_MPI2_VALUE1, 0)
      call F_MPI1_READ_CHECK(keyval_comm_f_mpi22, &
                            FORTRAN_MPI2_VALUE2, 0)
      call F_MPI1_READ_CHECK(keyval_comm_f_mpi23, &
                            FORTRAN_MPI2_VALUE3, 0)

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI1_READ_CHECK(key, expected_value, check_c)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      integer key, expected_value, check_c
      integer ierr
      integer val_read
      logical flag

      val_read = 0
      call MPI_ATTR_GET(MPI_COMM_SELF, key, val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute got flag=.false."
         stop
      endif
      if (check_c == 1) then
         call C_CHECK_MPI1(key, val_read)
      else
         if (val_read .ne. expected_value) then
            print *, "ERROR: reading attribute got value=", &
                val_read
            print *, "ERROR: when expecting value=", expected_value
            stop
         endif
      endif

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MPI1_COPY_FN(oldcomm, &
                             keyval, &
                             extra_state, &
                             in, &
                             out, &
                             flag, &
                             ierr)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      FORTRAN_COMM_TYPE oldcomm
      integer keyval
      integer extra_state
      integer in
      integer out
      logical flag
      integer ierr

      print *, "In MPI1_COPY_FN"
      flag = .TRUE.
      out = in * 2
      extra_state = extra_state + 1
      ierr = MPI_SUCCESS

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MPI1_DELETE_FN(oldcomm, &
                               keyval, &
                               in, &
                               extra_state, &
                               ierr)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      FORTRAN_COMM_TYPE oldcomm
      integer keyval
      integer in
      integer extra_state
      integer ierr

      print *, "In MPI1_DELETE_FN"
      extra_state = extra_state + 1
      ierr = MPI_SUCCESS

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! MPI-2
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_CREATE_KEYVAL()
      implicit none

      call F_MPI2_CREATE_KEYVAL_COMM()
      call F_MPI2_CREATE_KEYVAL_TYPE()
      call F_MPI2_CREATE_KEYVAL_WIN()

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_CREATE_KEYVAL_COMM()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"
      integer ierr
      external MPI2_COPY_FN, MPI2_DELETE_FN

      integer f_mpi11_extra, f_mpi12_extra, f_mpi13_extra
      integer(kind=MPI_ADDRESS_KIND) :: f_mpi21_extra, f_mpi22_extra, f_mpi23_extra

      common /extras/ f_mpi21_extra, f_mpi22_extra, f_mpi23_extra, &
           f_mpi11_extra, f_mpi12_extra, f_mpi13_extra

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

      print *, "Creating MPI-2 Fortran keyval comm 1"
      call MPI_COMM_CREATE_KEYVAL(MPI2_COPY_FN, MPI2_DELETE_FN, &
          keyval_comm_f_mpi21, f_mpi21_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_comm_f_mpi21

      print *, "Creating MPI-1 Fortran keyval comm 2"
      call MPI_COMM_CREATE_KEYVAL(MPI2_COPY_FN, MPI2_DELETE_FN, &
          keyval_comm_f_mpi22, f_mpi22_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_comm_f_mpi22

      print *, "Creating MPI-1 Fortran keyval comm 3"
      call MPI_COMM_CREATE_KEYVAL(MPI_COMM_NULL_COPY_FN, MPI_COMM_NULL_DELETE_FN, &
          keyval_comm_f_mpi23, f_mpi23_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_comm_f_mpi23

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_CREATE_KEYVAL_TYPE()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"
      integer ierr
      external MPI2_COPY_FN, MPI2_DELETE_FN

      integer f_mpi11_extra, f_mpi12_extra, f_mpi13_extra
      integer(kind=MPI_ADDRESS_KIND) :: f_mpi21_extra, f_mpi22_extra, f_mpi23_extra

      common /extras/ f_mpi21_extra, f_mpi22_extra, f_mpi23_extra, &
           f_mpi11_extra, f_mpi12_extra, f_mpi13_extra

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

      call MPI_TYPE_CREATE_KEYVAL(MPI2_COPY_FN, MPI2_DELETE_FN, &
          keyval_type_f_mpi21, f_mpi21_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_type_f_mpi21

      print *, "Creating MPI-1 Fortran keyval type 2"
      call MPI_TYPE_CREATE_KEYVAL(MPI2_COPY_FN, MPI2_DELETE_FN, &
          keyval_type_f_mpi22, f_mpi22_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_type_f_mpi22

      print *, "Creating MPI-1 Fortran keyval type 3"
      call MPI_TYPE_CREATE_KEYVAL(MPI_TYPE_NULL_COPY_FN, MPI_TYPE_NULL_DELETE_FN, &
          keyval_type_f_mpi23, f_mpi23_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_type_f_mpi23

      return
      end
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_CREATE_KEYVAL_WIN()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"
      integer ierr
      external MPI2_COPY_FN, MPI2_DELETE_FN

      integer f_mpi11_extra, f_mpi12_extra, f_mpi13_extra
      integer(kind=MPI_ADDRESS_KIND) :: f_mpi21_extra, f_mpi22_extra, f_mpi23_extra

      common /extras/ f_mpi21_extra, f_mpi22_extra, f_mpi23_extra, &
           f_mpi11_extra, f_mpi12_extra, f_mpi13_extra

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

      print *, "Creating MPI-2 Fortran keyval win 1"
      call MPI_WIN_CREATE_KEYVAL(MPI2_COPY_FN, MPI2_DELETE_FN, &
          keyval_win_f_mpi21, f_mpi21_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_win_f_mpi21

      print *, "Creating MPI-1 Fortran keyval win 2"
      call MPI_WIN_CREATE_KEYVAL(MPI2_COPY_FN, MPI2_DELETE_FN, &
          keyval_win_f_mpi22, f_mpi22_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_win_f_mpi22

      print *, "Creating MPI-1 Fortran keyval win 3"
      call MPI_WIN_CREATE_KEYVAL(MPI_WIN_NULL_COPY_FN, MPI_WIN_NULL_DELETE_FN, &
          keyval_win_f_mpi23, f_mpi23_extra, ierr)
      print *, "Fortran MPI-2: created keyval ", keyval_win_f_mpi23

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_WRITE()
      implicit none

      call F_MPI2_WRITE_COMM()
      call F_MPI2_WRITE_TYPE()
      call F_MPI2_WRITE_WIN()

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_WRITE_COMM()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer ierr, key
      integer(kind=MPI_ADDRESS_KIND) :: val_write, val_read, tmp
      FORTRAN_COMM_TYPE comm2
      logical flag

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

! Write attribute 1

      print *, "Writing MPI-2 Fortran comm attribute 1"
      val_write = FORTRAN_MPI2_VALUE1
      tmp = val_write
      call MPI_COMM_SET_ATTR(MPI_COMM_SELF, keyval_comm_f_mpi21, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran comm write sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_COMM_GET_ATTR(MPI_COMM_SELF, keyval_comm_f_mpi21, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Dup the comm; it should follow

      print *, "MPI-2 Fortran duping comm"
      call MPI_COMM_DUP(MPI_COMM_SELF, comm2, ierr);

! Is it there?

      print *, "MPI-2 Fortran reading from duped comm"
      val_read = 0
      call MPI_COMM_GET_ATTR(comm2, keyval_comm_f_mpi21, val_read, &
          flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading MPI-2 Fortran attribute got flag=.false."
         stop
      endif
      if (val_read .ne. tmp * 2) then
         print *, "ERROR: reading MPI-2 Fortran attribute got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp * 2
         stop
      endif

! Ok

      call MPI_COMM_FREE(comm2, ierr)

! Write attribute 2

      print *, "Writing MPI-2 Fortran comm attribute 2"
      val_write = FORTRAN_MPI2_VALUE2
      tmp = val_write
      call MPI_COMM_SET_ATTR(MPI_COMM_SELF, keyval_comm_f_mpi22, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran write comm sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_COMM_GET_ATTR(MPI_COMM_SELF, keyval_comm_f_mpi22, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Dup the comm; it should follow

      print *, "MPI-2 Fortran duping comm"
      call MPI_COMM_DUP(MPI_COMM_SELF, comm2, ierr);

! Is it there?

      print *, "MPI-2 Fortran reading from duped comm"
      val_read = 0
      call MPI_COMM_GET_ATTR(comm2, keyval_comm_f_mpi22, val_read, &
          flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading MPI-2 Fortran attribute got flag=.false."
         stop
      endif
      if (val_read .ne. tmp * 2) then
         print *, "ERROR: reading MPI-2 Fortran attribute got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp * 2
         stop
      endif

! Ok

      call MPI_COMM_FREE(comm2, ierr)

! Write attribute 3

      print *, "Writing MPI-2 Fortran comm attribute 3"
      val_write = FORTRAN_MPI2_VALUE3
      tmp = val_write
      call MPI_COMM_SET_ATTR(MPI_COMM_SELF, keyval_comm_f_mpi23, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran write comm sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_COMM_GET_ATTR(MPI_COMM_SELF, keyval_comm_f_mpi23, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Dup the comm; it should NOT follow (it was created with a NULL_COPY
! callback)

      print *, "MPI-2 Fortran duping comm"
      call MPI_COMM_DUP(MPI_COMM_SELF, comm2, ierr);

! Is it there?

      print *, "MPI-2 Fortran reading from duped comm"
      val_read = 0
      call MPI_COMM_GET_ATTR(comm2, keyval_comm_f_mpi23, val_read, &
          flag, ierr)
      if (.true. .eqv.  flag) then
         print *, "ERROR: reading MPI-2 Fortran attribute got flag=.true."
         stop
      endif

! Ok

      call MPI_COMM_FREE(comm2, ierr)

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_WRITE_TYPE()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer ierr, key
      integer(kind=MPI_ADDRESS_KIND) :: val_write, val_read, tmp
      FORTRAN_DATATYPE_TYPE type2
      logical flag

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

! Write attribute 1

      print *, "Writing MPI-2 Fortran type attribute 1"
      val_write = FORTRAN_MPI2_VALUE1
      tmp = val_write
      call MPI_TYPE_SET_ATTR(MPI_INTEGER, keyval_type_f_mpi21, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran type write sanity check"
      val_read = 0
      call MPI_TYPE_GET_ATTR(MPI_INTEGER, keyval_type_f_mpi21, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Dup the type; it should follow

      print *, "MPI-2 Fortran duping type"
      call MPI_TYPE_DUP(MPI_INTEGER, type2, ierr);

! Is it there?

      print *, "MPI-2 Fortran reading from duped type"
      val_read = 0
      call MPI_TYPE_GET_ATTR(type2, keyval_type_f_mpi21, val_read, &
          flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading MPI-2 Fortran attribute got flag=.false."
         stop
      endif
      if (val_read .ne. tmp * 2) then
         print *, "ERROR: reading MPI-2 Fortran attribute got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp * 2
         stop
      endif

! Ok

      call MPI_TYPE_FREE(type2, ierr)

! Write attribute 2

      print *, "Writing MPI-2 Fortran type attribute 2"
      val_write = FORTRAN_MPI2_VALUE2
      tmp = val_write
      call MPI_TYPE_SET_ATTR(MPI_INTEGER, keyval_type_f_mpi22, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran type write sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_TYPE_GET_ATTR(MPI_INTEGER, keyval_type_f_mpi22, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Dup the type; it should follow

      print *, "MPI-2 Fortran duping type"
      call MPI_TYPE_DUP(MPI_INTEGER, type2, ierr);

! Is it there?

      print *, "MPI-2 Fortran reading from duped type"
      val_read = 0
      call MPI_TYPE_GET_ATTR(type2, keyval_type_f_mpi22, val_read, &
          flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading MPI-2 Fortran attribute got flag=.false."
         stop
      endif
      if (val_read .ne. tmp * 2) then
         print *, "ERROR: reading MPI-2 Fortran attribute got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp * 2
         stop
      endif

! Ok

      call MPI_TYPE_FREE(type2, ierr)

! Write attribute 3

      print *, "Writing MPI-2 Fortran type attribute 3"
      val_write = FORTRAN_MPI2_VALUE3
      tmp = val_write
      call MPI_TYPE_SET_ATTR(MPI_INTEGER, keyval_type_f_mpi23, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran type write sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_TYPE_GET_ATTR(MPI_INTEGER, keyval_type_f_mpi23, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Dup the type; it should NUL follow (it was created with a NULL_COPY
! callback)

      print *, "MPI-2 Fortran duping type"
      call MPI_TYPE_DUP(MPI_INTEGER, type2, ierr);

! Is it there?

      print *, "MPI-2 Fortran reading from duped type"
      val_read = 0
      call MPI_TYPE_GET_ATTR(type2, keyval_type_f_mpi23, val_read, &
          flag, ierr)
      if (.true. .eqv.  flag) then
         print *, "ERROR: reading MPI-2 Fortran attribute got flag=.true."
         stop
      endif

! Ok

      call MPI_TYPE_FREE(type2, ierr)

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_WRITE_WIN()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer ierr, key
      integer(kind=MPI_ADDRESS_KIND) :: val_write, val_read, tmp
      logical flag

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

! Write attribute 1

      print *, "Writing MPI-2 Fortran win attribute 1"
      val_write = FORTRAN_MPI2_VALUE1
      tmp = val_write
      print *, "Writing to keyval: ", keyval_win_f_mpi21
      call MPI_WIN_SET_ATTR(win, keyval_win_f_mpi21, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran win write sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_WIN_GET_ATTR(win, keyval_win_f_mpi21, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Write attribute 2

      print *, "Writing MPI-2 Fortran win attribute 2"
      val_write = FORTRAN_MPI2_VALUE2
      tmp = val_write
      print *, "Writing to keyval: ", keyval_win_f_mpi22
      call MPI_WIN_SET_ATTR(win, keyval_win_f_mpi22, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran win write sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_WIN_GET_ATTR(win, keyval_win_f_mpi22, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

! Write attribute 3

      print *, "Writing MPI-2 Fortran win attribute 3"
      val_write = FORTRAN_MPI2_VALUE3
      tmp = val_write
      print *, "Writing to keyval: ", keyval_win_f_mpi23
      call MPI_WIN_SET_ATTR(win, keyval_win_f_mpi23, &
          val_write, ierr)

! Sanity check (unity)

      print *, "MPI-2 Fortran win write sanity check"
      val_write = val_write + 1
      val_read = 0
      call MPI_WIN_GET_ATTR(win, keyval_win_f_mpi23, &
          val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute 1 got flag=.false."
         stop
      endif
      if (val_read .ne. tmp) then
         print *, "ERROR: reading attribute 1 got value=", &
                  val_read
         print *, "ERROR: when expecting value=", tmp
         stop
      endif

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_READ()
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      include "values-f.h"

      integer keyval_c1, keyval_c2, keyval_c3
      integer keyval_comm_c1, keyval_comm_c2, keyval_comm_c3
      integer keyval_type_c1, keyval_type_c2, keyval_type_c3
      integer keyval_win_c1, keyval_win_c2, keyval_win_c3
      integer keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13
      integer keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23
      integer keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23
      integer keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23
      FORTRAN_WIN_TYPE win

      common /keyvals/ keyval_c1, keyval_c2, keyval_c3, &
          keyval_comm_c1, keyval_comm_c2, keyval_comm_c3, &
          keyval_type_c1, keyval_type_c2, keyval_type_c3, &
          keyval_win_c1, keyval_win_c2, keyval_win_c3, &
          keyval_f_mpi11, keyval_f_mpi12, keyval_f_mpi13, &
          keyval_comm_f_mpi21, keyval_comm_f_mpi22, keyval_comm_f_mpi23, &
          keyval_type_f_mpi21, keyval_type_f_mpi22, keyval_type_f_mpi23, &
          keyval_win_f_mpi21, keyval_win_f_mpi22, keyval_win_f_mpi23, &
          win

      integer(kind=MPI_ADDRESS_KIND) val1, val2, val3

!     Note that C's 6 keyvals wrote a pointer to a C int, which may not
!     be the same size as an integer(kind=MPI_ADDRESS_KIND).

      print *, "Case 3: C writes, Fortran MPI-2 reads"
      val1 = C_VALUE1
      val2 = C_VALUE2
      val3 = C_VALUE3
      call F_MPI2_READ_COMM_CHECK(keyval_c1, val1, 1)
      call F_MPI2_READ_COMM_CHECK(keyval_c2, val2, 1)
      call F_MPI2_READ_COMM_CHECK(keyval_c3, val3, 1)
      call F_MPI2_READ_COMM_CHECK(keyval_comm_c1, val1, 1)
      call F_MPI2_READ_COMM_CHECK(keyval_comm_c2, val2, 1)
      call F_MPI2_READ_COMM_CHECK(keyval_comm_c3, val3, 1)
      call F_MPI2_READ_TYPE_CHECK(keyval_type_c1, val1, 1)
      call F_MPI2_READ_TYPE_CHECK(keyval_type_c2, val2, 1)
      call F_MPI2_READ_TYPE_CHECK(keyval_type_c3, val3, 1)
      call F_MPI2_READ_WIN_CHECK(win, keyval_win_c1, val1, 1)
      call F_MPI2_READ_WIN_CHECK(win, keyval_win_c2, val2, 1)
      call F_MPI2_READ_WIN_CHECK(win, keyval_win_c3, val3, 1)

      print *, "Case 6: Fortran MPI-1 writes, Fortran MPI-2 reads"
      val1 = FORTRAN_MPI1_VALUE1
      val2 = FORTRAN_MPI1_VALUE2
      val3 = FORTRAN_MPI1_VALUE3
      call F_MPI2_READ_COMM_CHECK(keyval_f_mpi11, val1, 0)
      call F_MPI2_READ_COMM_CHECK(keyval_f_mpi12, val2, 0)
      call F_MPI2_READ_COMM_CHECK(keyval_f_mpi13, val3, 0)

      print *, "Case 9: Fortran MPI-2 writes, Fortran MPI-2 reads"
      val1 = FORTRAN_MPI2_VALUE1
      val2 = FORTRAN_MPI2_VALUE2
      val3 = FORTRAN_MPI2_VALUE3
      call F_MPI2_READ_COMM_CHECK(keyval_comm_f_mpi21, val1, 0)
      call F_MPI2_READ_COMM_CHECK(keyval_comm_f_mpi22, val2, 0)
      call F_MPI2_READ_COMM_CHECK(keyval_comm_f_mpi23, val3, 0)
      call F_MPI2_READ_TYPE_CHECK(keyval_type_f_mpi21, val1, 0)
      call F_MPI2_READ_TYPE_CHECK(keyval_type_f_mpi22, val2, 0)
      call F_MPI2_READ_TYPE_CHECK(keyval_type_f_mpi23, val3, 0)
      call F_MPI2_READ_WIN_CHECK(win, keyval_win_f_mpi21, val1, 0)
      call F_MPI2_READ_WIN_CHECK(win, keyval_win_f_mpi22, val2, 0)
      call F_MPI2_READ_WIN_CHECK(win, keyval_win_f_mpi23, val3, 0)
      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_READ_COMM_CHECK(key, expected_value, c_check)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      integer key
      integer ierr
      logical flag
      integer(kind=MPI_ADDRESS_KIND) val_read, expected_value
      integer c_check

      val_read = 0
      call MPI_COMM_GET_ATTR(MPI_COMM_SELF, key, val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute got flag=.false."
         stop
      endif
      if (c_check == 1) then
         call C_CHECK_MPI2(key, val_read)
      else
         if (val_read .ne. expected_value) then
            print *, "ERROR: reading attribute got value=", &
                val_read
            print *, "ERROR: when expecting value=", expected_value
            stop
         endif
      endif

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_READ_TYPE_CHECK(key, expected_value, c_check)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      integer key
      integer ierr
      logical flag
      integer(kind=MPI_ADDRESS_KIND) val_read, expected_value
      integer c_check

      val_read = 0
      call MPI_TYPE_GET_ATTR(MPI_INTEGER, key, val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute got flag=.false."
         stop
      endif
      if (c_check == 1) then
         call C_CHECK_MPI2(key, val_read)
      else
         if (val_read .ne. expected_value) then
            print *, "ERROR: reading attribute got value=", &
                val_read
            print *, "ERROR: when expecting value=", expected_value
            stop
         endif
      endif

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE F_MPI2_READ_WIN_CHECK(win, key, expected_value, &
          c_check)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      FORTRAN_WIN_TYPE win
      integer key
      integer ierr
      logical flag
      integer(kind=MPI_ADDRESS_KIND) val_read, expected_value
      integer c_check

      val_read = 0
      call MPI_WIN_GET_ATTR(win, key, val_read, flag, ierr)
      if (.false. .eqv.  flag) then
         print *, "ERROR: reading attribute got flag=.false."
         stop
      endif
      if (c_check == 1) then
         call C_CHECK_MPI2(key, val_read)
      else
         if (val_read .ne. expected_value) then
            print *, "ERROR: reading attribute got value=", &
                val_read
            print *, "ERROR: when expecting value=", expected_value
            stop
         endif
      endif

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MPI2_COPY_FN(oldcomm, &
                             keyval, &
                             extra_state, &
                             in, &
                             out, &
                             flag, &
                             ierr)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      FORTRAN_COMM_TYPE oldcomm
      integer keyval
      integer(kind=MPI_ADDRESS_KIND) extra_state, in, out
      logical flag
      integer ierr

      print *, "In MPI2_COPY_FN"
      flag = .TRUE.
      out = in * 2
      extra_state = extra_state + 1
      ierr = MPI_SUCCESS

      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MPI2_DELETE_FN(oldcomm, &
                               keyval, &
                               in, &
                               extra_state, &
                               ierr)
      FORTRAN_SUB_LINE1
      FORTRAN_SUB_LINE2
      FORTRAN_COMM_TYPE oldcomm
      integer keyval
      integer(kind=MPI_ADDRESS_KIND) in, extra_state
      integer ierr

      print *, "In MPI2_DELETE_FN"
      extra_state = extra_state + 1
      ierr = MPI_SUCCESS

      return
      end
