!
! Copyright (c) 2014      Mellanox Technologies, Inc.
!                         All rights reserved.
! Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

program ring_oshmem
    implicit none
    include 'shmem.fh'

    integer*8, save   :: rbuf
    integer*8         :: message
    integer           :: proc, nproc, next
    integer           :: my_pe, num_pes

    rbuf = -1
    message = 10

    call start_pes(0)
    proc = my_pe()
    nproc = num_pes()

!   Calculate the PE number of the next process in the ring.  Use the
!   modulus operator so that the last process "wraps around" to PE 0.

    next = mod((proc + 1), nproc)

    if (proc == 0) then
        write(*, '("Process 0 sending ", i2, " to", i2, " (", i2, " processes in ring)")') message, next, nproc
        call shmem_put8(rbuf, message, 1, next)
        write(*, '("Process 0 sent to ", i2)') next
    end if

!   Pass the message around the ring.  The exit mechanism works as
!   follows: the message (a positive integer) is passed around the
!   ring.  Each time it passes PE 0, it is decremented.  When each
!   processes receives a message containing a 0 value, it passes the
!   message on to the next process and then quits.  By passing the 0
!   message first, every process gets the 0 message and can quit
!   normally.

    do while(message > 0)
        call shmem_int8_wait_until(rbuf, SHMEM_CMP_EQ, message)

        if (proc == 0) then
            message = message - 1
            write(*, '("Process 0 decremented value:", i2)') message
        end if

        call shmem_put8(rbuf, message, 1, next)

        if (proc > 0) then
            message = message - 1
        end if

    end do

!     All done

    write(*, '("Process", i2," exiting.")') proc

end program ring_oshmem
