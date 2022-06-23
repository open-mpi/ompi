! -*- fortran -*-
! Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
! Copyright (c) 2010-2022 The University of Tennessee and the University
!                         of Tennessee research foundation.  All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

! Include the parameters for this extension
! Included from config/ompi_ext.m4 into mpif90-ext.f90
! include '../mpiext/ftmpi/mpif-h/mpiext_ftmpi_mpifh.h'

!
! Communicators
!
interface mpix_comm_revoke
    subroutine mpix_comm_revoke(comm, ierr)
      integer, intent(IN) :: comm
      integer, intent(OUT) :: ierr
    end subroutine mpix_comm_revoke
end interface mpix_comm_revoke

interface pmpix_comm_revoke
    subroutine pmpix_comm_revoke(comm, ierr)
      integer, intent(IN) :: comm
      integer, intent(OUT) :: ierr
    end subroutine pmpix_comm_revoke
end interface pmpix_comm_revoke

interface mpix_comm_is_revoked
    subroutine mpix_comm_is_revoked(comm, flag, ierr)
      integer, intent(IN) :: comm
      logical, intent(OUT) :: flag
      integer, intent(OUT) :: ierr
    end subroutine mpix_comm_is_revoked
end interface mpix_comm_is_revoked

interface pmpix_comm_is_revoked
    subroutine pmpix_comm_is_revoked(comm, flag, ierr)
      integer, intent(IN) :: comm
      logical, intent(OUT) :: flag
      integer, intent(OUT) :: ierr
    end subroutine pmpix_comm_is_revoked
end interface pmpix_comm_is_revoked

interface mpix_comm_shrink
    subroutine mpix_comm_shrink(comm, newcomm, ierr)
      integer, intent(IN) :: comm
      integer, intent(OUT) :: newcomm, ierr
    end subroutine mpix_comm_shrink
end interface mpix_comm_shrink

interface pmpix_comm_shrink
    subroutine pmpix_comm_shrink(comm, newcomm, ierr)
      integer, intent(IN) :: comm
      integer, intent(OUT) :: newcomm, ierr
    end subroutine pmpix_comm_shrink
end interface pmpix_comm_shrink

interface mpix_comm_get_failed
    subroutine mpix_comm_get_failed(comm, failedgrp, ierr)
        integer, intent(IN) :: comm
        integer, intent(OUT) :: failedgrp, ierr
    end subroutine mpix_comm_get_failed
end interface mpix_comm_get_failed

interface pmpix_comm_get_failed
    subroutine pmpix_comm_get_failed(comm, failedgrp, ierr)
        integer, intent(IN) :: comm
        integer, intent(OUT) :: failedgrp, ierr
    end subroutine pmpix_comm_get_failed
end interface pmpix_comm_get_failed

interface mpix_comm_ack_failed
    subroutine mpix_comm_ack_failed(comm, num_to_ack, num_acked, ierr)
        integer, intent(IN) :: comm, num_to_ack
        integer, intent(OUT) :: num_acked, ierr
    end subroutine mpix_comm_ack_failed
end interface mpix_comm_ack_failed

interface pmpix_comm_ack_failed
    subroutine pmpix_comm_ack_failed(comm, num_to_ack, num_acked, ierr)
        integer, intent(IN) :: comm, num_to_ack
        integer, intent(OUT) :: num_acked, ierr
    end subroutine pmpix_comm_ack_failed
end interface pmpix_comm_ack_failed

interface mpix_comm_failure_ack
    subroutine mpix_comm_failure_ack(comm, ierr)
      integer, intent(IN) :: comm
      integer, intent(OUT) :: ierr
    end subroutine mpix_comm_failure_ack
end interface mpix_comm_failure_ack

interface pmpix_comm_failure_ack
    subroutine pmpix_comm_failure_ack(comm, ierr)
      integer, intent(IN) :: comm
      integer, intent(OUT) :: ierr
    end subroutine pmpix_comm_failure_ack
end interface pmpix_comm_failure_ack

interface mpix_comm_failure_get_acked
    subroutine mpix_comm_failure_get_acked(comm, failedgrp, ierr)
        integer, intent(IN) :: comm
        integer, intent(OUT) :: failedgrp, ierr
    end subroutine mpix_comm_failure_get_acked
end interface mpix_comm_failure_get_acked

interface pmpix_comm_failure_get_acked
    subroutine pmpix_comm_failure_get_acked(comm, failedgrp, ierr)
        integer, intent(IN) :: comm
        integer, intent(OUT) :: failedgrp, ierr
    end subroutine pmpix_comm_failure_get_acked
end interface pmpix_comm_failure_get_acked

interface mpix_comm_agree
    subroutine mpix_comm_agree(comm, flag, ierr)
      integer, intent(IN) :: comm
      integer, intent(INOUT) :: flag
      integer, intent(OUT) :: ierr
    end subroutine mpix_comm_agree
end interface mpix_comm_agree

interface pmpix_comm_agree
    subroutine pmpix_comm_agree(comm, flag, ierr)
      integer, intent(IN) :: comm
      integer, intent(INOUT) :: flag
      integer, intent(OUT) :: ierr
    end subroutine pmpix_comm_agree
end interface pmpix_comm_agree

interface mpix_comm_iagree
    subroutine mpix_comm_iagree(comm, flag, request, ierr)
      integer, intent(IN) :: comm
      integer, intent(INOUT) :: flag
      integer, intent(OUT) :: request, ierr
    end subroutine mpix_comm_iagree
end interface mpix_comm_iagree

interface pmpix_comm_iagree
    subroutine pmpix_comm_iagree(comm, flag, request, ierr)
      integer, intent(IN) :: comm
      integer, intent(INOUT) :: flag
      integer, intent(OUT) :: request, ierr
    end subroutine pmpix_comm_iagree
end interface pmpix_comm_iagree


!
! Validation: Windows
! Todo
!


!
! Validation: File Handles
! Todo
!


!
