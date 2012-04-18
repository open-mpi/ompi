!
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Sample "example" MPI extension application for Open MPI
!

program main
      implicit none
      include 'mpif.h'
      include 'mpif-ext.h'
      
      integer ierr, rank, size
      
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
      
      write(*, '("Hello, world, I am ", i2, " of ", i2)') rank, size
      call OMPI_PROGRESS(3, MPI_COMM_WORLD, ierr)
      
      call MPI_FINALIZE(ierr)
end program
      
