! -*- fortran -*-
! Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
! Copyright (c) 2010-2020 The Trustees of the University of Tennessee.
!                         All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

!
! Error codes
!
       integer MPIX_ERR_PROC_FAILED
       integer MPIX_ERR_PROC_FAILED_PENDING
       integer MPIX_ERR_REVOKED
! These values must match the same define in mpif-common.h
       parameter (MPIX_ERR_PROC_FAILED         = 75)
       parameter (MPIX_ERR_PROC_FAILED_PENDING = 76)
       parameter (MPIX_ERR_REVOKED             = 77)

