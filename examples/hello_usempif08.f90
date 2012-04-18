! -*- f90 -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$
!
! Sample MPI "hello world" application using the MPI-3 mpi_f08 module.
!
program main
    use mpi_f08
    implicit none
    integer :: rank, size, len
    character(len=MPI_MAX_LIBRARY_VERSION_STRING) :: version

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size)
    call MPI_GET_LIBRARY_VERSION(version, len)

    write(*, '("Hello, world, I am ", i2, " of ", i2, ": ", a)') &
          rank, size, version

    call MPI_FINALIZE()
end
