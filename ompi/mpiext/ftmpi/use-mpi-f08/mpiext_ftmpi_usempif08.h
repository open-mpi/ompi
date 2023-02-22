! -*- f90 -*-
!
! Copyright (c) 2018-2022 The University of Tennessee and the University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
!
! $COPYRIGHT$

interface mpix_comm_revoke
subroutine mpix_comm_revoke_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_revoke_f08
end interface mpix_comm_revoke

interface pmpix_comm_revoke
subroutine pmpix_comm_revoke_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_revoke_f08
end interface pmpix_comm_revoke

interface mpix_comm_is_revoked
subroutine mpix_comm_is_revoked_f08(comm,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_is_revoked_f08
end interface mpix_comm_is_revoked

interface pmpix_comm_is_revoked
subroutine pmpix_comm_is_revoked_f08(comm,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_is_revoked_f08
end interface pmpix_comm_is_revoked

interface mpix_comm_failure_ack
subroutine mpix_comm_failure_ack_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_failure_ack_f08
end interface mpix_comm_failure_ack

interface pmpix_comm_failure_ack
subroutine pmpix_comm_failure_ack_f08(comm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_failure_ack_f08
end interface pmpix_comm_failure_ack

interface mpix_comm_failure_get_acked
subroutine mpix_comm_failure_get_acked_f08(comm,failedgrp,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: failedgrp
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_failure_get_acked_f08
end interface mpix_comm_failure_get_acked

interface pmpix_comm_failure_get_acked
subroutine pmpix_comm_failure_get_acked_f08(comm,failedgrp,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: failedgrp
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_failure_get_acked_f08
end interface pmpix_comm_failure_get_acked

interface mpix_comm_get_failed
subroutine mpix_comm_get_failed_f08(comm,failedgrp,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: failedgrp
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_get_failed_f08
end interface mpix_comm_get_failed

interface pmpix_comm_get_failed
subroutine pmpix_comm_get_failed_f08(comm,failedgrp,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(OUT) :: failedgrp
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_get_failed_f08
end interface pmpix_comm_get_failed
interface mpix_comm_agree
subroutine mpix_comm_agree_f08(comm,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(INOUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_agree_f08
end interface mpix_comm_agree

interface mpix_comm_ack_failed
subroutine mpix_comm_ack_failed_f08(comm,num_to_ack,num_acked,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: num_to_ack
   INTEGER, INTENT(OUT) :: num_acked
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_ack_failed_f08
end interface mpix_comm_ack_failed

interface pmpix_comm_ack_failed
subroutine pmpix_comm_ack_failed_f08(comm,num_to_ack,num_acked,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: num_to_ack
   INTEGER, INTENT(OUT) :: num_acked
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_ack_failed_f08
end interface pmpix_comm_ack_failed

interface pmpix_comm_agree
subroutine pmpix_comm_agree_f08(comm,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(INOUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_agree_f08
end interface pmpix_comm_agree

interface mpix_comm_iagree
subroutine mpix_comm_iagree_f08(comm,flag,request,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Request
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(INOUT), ASYNCHRONOUS :: flag ! should use OMPI_ASYNCHRONOUS
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_iagree_f08
end interface mpix_comm_iagree

interface pmpix_comm_iagree
subroutine pmpix_comm_iagree_f08(comm,flag,request,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Request
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(INOUT), ASYNCHRONOUS :: flag ! should use OMPI_ASYNCHRONOUS
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_iagree_f08
end interface pmpix_comm_iagree

interface mpix_comm_shrink
subroutine mpix_comm_shrink_f08(comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine mpix_comm_shrink_f08
end interface mpix_comm_shrink

interface pmpix_comm_shrink
subroutine pmpix_comm_shrink_f08(comm,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
end subroutine pmpix_comm_shrink_f08
end interface pmpix_comm_shrink
