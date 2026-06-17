! -*- f90 -*-
!
! Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
! Regression test for the mpi_f08 buffer attach/detach bindings.
!
! This exercises MPI_Buffer_detach, MPI_Comm_detach_buffer, and
! MPI_Session_detach_buffer (and their attach counterparts) and
! verifies two things that the bindings previously got wrong:
!
! 1. detach must return, in the user's TYPE(C_PTR), the address of
!    the buffer that was attached.  MPI_Buffer_detach used to pass
!    &buffer to the back-end call, so the detached address was
!    dropped into a local variable and the caller's C_PTR was never
!    updated.
!
! 2. when an automatic buffer (MPI_BUFFER_AUTOMATIC) is detached, the
!    binding must hand back the *Fortran* sentinel address, not the C
!    library's internal sentinel ((void *) 4).  That requires the
!    attach wrappers to translate the Fortran sentinel to the C
!    sentinel (so the C library actually engages automatic mode) and
!    the detach wrappers to translate the C sentinel back.
!
! Everything here is local (no communication), so the test runs fine
! as a singleton:  ./buffer_attach_detach_f08
!
! Exits non-zero if any check fails.

program buffer_attach_detach_f08
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, &
                                           c_null_ptr, c_intptr_t, c_int8_t
    use mpi_f08
    implicit none

    integer, parameter :: BUFSIZE = 8192
    ! The C sentinel MPI_BUFFER_AUTOMATIC is ((void *) 4); the Fortran
    ! binding must never expose that value to Fortran code.
    integer(c_intptr_t), parameter :: C_BUFFER_AUTOMATIC = 4_c_intptr_t

    integer(c_int8_t), allocatable, target :: buf(:)
    type(c_ptr) :: addr
    integer :: isize, ierr, nerr
    integer(c_intptr_t) :: ia_buf, ia_comm, ia_sess
    type(MPI_Session) :: session

    nerr = 0

    call MPI_Init(ierr)
    call must("MPI_Init", ierr)
    ! Let the buffer calls report errors via ierr instead of aborting.
    call MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN, ierr)
    call must("MPI_Comm_set_errhandler", ierr)

    allocate(buf(BUFSIZE))

    ! ----------------------------------------------------------------
    ! Normal-buffer round trips: detach must return the attached
    ! address.  (This is the decisive check for the MPI_Buffer_detach
    ! &buffer bug; the comm/session variants already returned the
    ! address correctly and serve as regression guards.)
    ! ----------------------------------------------------------------

    addr = c_null_ptr
    isize = -1
    call MPI_Buffer_attach(buf, BUFSIZE, ierr)
    call must("MPI_Buffer_attach", ierr)
    call MPI_Buffer_detach(addr, isize, ierr)
    call must("MPI_Buffer_detach", ierr)
    call expect_addr("MPI_Buffer_detach returns attached address", &
                     addr, c_loc(buf), nerr)
    call expect_size("MPI_Buffer_detach returns attached size", &
                     isize, BUFSIZE, nerr)

    addr = c_null_ptr
    isize = -1
    call MPI_Comm_attach_buffer(MPI_COMM_SELF, buf, BUFSIZE, ierr)
    call must("MPI_Comm_attach_buffer", ierr)
    call MPI_Comm_detach_buffer(MPI_COMM_SELF, addr, isize, ierr)
    call must("MPI_Comm_detach_buffer", ierr)
    call expect_addr("MPI_Comm_detach_buffer returns attached address", &
                     addr, c_loc(buf), nerr)
    call expect_size("MPI_Comm_detach_buffer returns attached size", &
                     isize, BUFSIZE, nerr)

    call MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, session, ierr)
    call must("MPI_Session_init", ierr)

    addr = c_null_ptr
    isize = -1
    call MPI_Session_attach_buffer(session, buf, BUFSIZE, ierr)
    call must("MPI_Session_attach_buffer", ierr)
    call MPI_Session_detach_buffer(session, addr, isize, ierr)
    call must("MPI_Session_detach_buffer", ierr)
    call expect_addr("MPI_Session_detach_buffer returns attached address", &
                     addr, c_loc(buf), nerr)
    call expect_size("MPI_Session_detach_buffer returns attached size", &
                     isize, BUFSIZE, nerr)

    ! ----------------------------------------------------------------
    ! Automatic-buffer round trips: attach MPI_BUFFER_AUTOMATIC, then
    ! detach.  The returned address must be the Fortran sentinel -- a
    ! real, non-null address that is NOT the C sentinel (void *) 4 --
    ! and must be identical across all three APIs.
    ! ----------------------------------------------------------------

    addr = c_null_ptr
    call MPI_Buffer_attach(MPI_BUFFER_AUTOMATIC, BUFSIZE, ierr)
    call must("MPI_Buffer_attach(AUTOMATIC)", ierr)
    call MPI_Buffer_detach(addr, isize, ierr)
    call must("MPI_Buffer_detach(AUTOMATIC)", ierr)
    ia_buf = transfer(addr, ia_buf)
    call expect_auto("MPI_Buffer_detach maps automatic sentinel", ia_buf, nerr)

    addr = c_null_ptr
    call MPI_Comm_attach_buffer(MPI_COMM_SELF, MPI_BUFFER_AUTOMATIC, BUFSIZE, ierr)
    call must("MPI_Comm_attach_buffer(AUTOMATIC)", ierr)
    call MPI_Comm_detach_buffer(MPI_COMM_SELF, addr, isize, ierr)
    call must("MPI_Comm_detach_buffer(AUTOMATIC)", ierr)
    ia_comm = transfer(addr, ia_comm)
    call expect_auto("MPI_Comm_detach_buffer maps automatic sentinel", ia_comm, nerr)

    addr = c_null_ptr
    call MPI_Session_attach_buffer(session, MPI_BUFFER_AUTOMATIC, BUFSIZE, ierr)
    call must("MPI_Session_attach_buffer(AUTOMATIC)", ierr)
    call MPI_Session_detach_buffer(session, addr, isize, ierr)
    call must("MPI_Session_detach_buffer(AUTOMATIC)", ierr)
    ia_sess = transfer(addr, ia_sess)
    call expect_auto("MPI_Session_detach_buffer maps automatic sentinel", ia_sess, nerr)

    if (ia_buf == ia_comm .and. ia_buf == ia_sess) then
        print '(a)', "PASS: automatic sentinel address consistent across APIs"
    else
        print '(a)', "FAIL: automatic sentinel address differs across APIs"
        nerr = nerr + 1
    end if

    call MPI_Session_finalize(session, ierr)
    call must("MPI_Session_finalize", ierr)

    deallocate(buf)

    if (nerr == 0) then
        print '(a)', "All mpi_f08 buffer attach/detach tests PASSED"
    else
        print '(a,i0,a)', "TEST FAILED: ", nerr, " check(s) failed"
    end if

    call MPI_Finalize(ierr)

    if (nerr /= 0) then
        error stop 1
    end if

contains

    ! Abort on an unexpected MPI error return.
    subroutine must(label, ie)
        character(len=*), intent(in) :: label
        integer, intent(in) :: ie
        if (ie /= MPI_SUCCESS) then
            print '(3a,i0)', "ERROR: ", label, " failed, ierr=", ie
            call MPI_Abort(MPI_COMM_WORLD, 1)
        end if
    end subroutine must

    ! Check that a returned C_PTR matches the expected address.
    subroutine expect_addr(label, got, want, ne)
        character(len=*), intent(in) :: label
        type(c_ptr), intent(in) :: got, want
        integer, intent(inout) :: ne
        if (c_associated(got, want)) then
            print '(2a)', "PASS: ", label
        else
            print '(2a)', "FAIL: ", label
            ne = ne + 1
        end if
    end subroutine expect_addr

    ! Check that a detach returned the size that was attached.
    subroutine expect_size(label, got, want, ne)
        character(len=*), intent(in) :: label
        integer, intent(in) :: got, want
        integer, intent(inout) :: ne
        if (got == want) then
            print '(2a)', "PASS: ", label
        else
            print '(2a,i0)', "FAIL: ", label, got
            ne = ne + 1
        end if
    end subroutine expect_size

    ! Check that an automatic-detach result is the Fortran sentinel:
    ! non-null, and not the C sentinel (void *) 4.
    subroutine expect_auto(label, ia, ne)
        character(len=*), intent(in) :: label
        integer(c_intptr_t), intent(in) :: ia
        integer, intent(inout) :: ne
        if (ia /= 0_c_intptr_t .and. ia /= C_BUFFER_AUTOMATIC) then
            print '(2a)', "PASS: ", label
        else
            print '(2a,i0)', "FAIL: ", label, ia
            ne = ne + 1
        end if
    end subroutine expect_auto

end program buffer_attach_detach_f08
