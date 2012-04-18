! -*- f90 -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$
!
! Simple ring test program using the MPI-3 mpi_f08 module.
!
program ring
  use mpi_f08
  implicit none
  integer :: rank, size, tag, next, from, i, message


! Start up MPI

  call MPI_INIT()
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size)

! Calculate the rank of the next process in the ring.  Use the modulus
! operator so that the last process "wraps around" to rank zero.

  tag = 201
  next = mod((rank + 1), size)
  from = mod((rank + size - 1), size)
      
! If we are the "master" process (i.e., MPI_COMM_WORLD rank 0), put
! the number of times to go around the ring in the message.

  if (rank .eq. 0) then
     message = 10

     write(*, '("Process 0 sending ", i2, " to ", i2, " tag ", i3, " (", i2, " processes in ring)")') message, next, tag, size
     call MPI_SEND(message, 1, MPI_INTEGER, next, tag, MPI_COMM_WORLD)
     write(*, '("Process 0 sent to ", i2)') next
  endif

! Pass the message around the ring.  The exit mechanism works as
! follows: the message (a positive integer) is passed around the ring.
! Each time it passes rank 0, it is decremented.  When each processes
! receives a message containing a 0 value, it passes the message on to
! the next process and then quits.  By passing the 0 message first,
! every process gets the 0 message and can quit normally.

   i = 1
10 call MPI_Recv(message, i, MPI_INTEGER, from, tag, MPI_COMM_WORLD, &
                 MPI_STATUS_IGNORE)

  if (rank .eq. 0) then
     message = message - 1
     write(*, '("Process 0 decremented value: ", i2)') message
  endif

  call MPI_SEND(message, 1, MPI_INTEGER, next, tag, MPI_COMM_WORLD)
      
  if (message .eq. 0) then
     write(*, '("Process ", i2, " exiting")') rank
     goto 20
  endif
  goto 10

! The last process does one extra send to process 0, which needs to be
! received before the program can exit

 20 if (rank .eq. 0) then
     call MPI_RECV(message, 1, MPI_INTEGER, from, tag, MPI_COMM_WORLD, &
                   MPI_STATUS_IGNORE)
  endif

! All done

  call MPI_FINALIZE()
end program
      
