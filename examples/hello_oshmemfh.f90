!
! Copyright (c) 2014      Mellanox Technologies, Inc.
!                         All rights reserved.
! Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
program hello_oshmem
    implicit none
    include 'shmem.fh'

    integer proc, nproc
    integer shmem_my_pe, shmem_n_pes
    integer major, minor, len
    character(len=SHMEM_MAX_NAME_LEN) name

    call SHMEM_INIT()
    proc = SHMEM_MY_PE()
    nproc = SHMEM_N_PES()
    call SHMEM_INFO_GET_VERSION(major, minor)
    call SHMEM_INFO_GET_NAME(name)

    write(*, '("Hello, world, I am ", i2, " of ", i2, ": (version: ", i0, ".", i0, ")")') proc, nproc, major, minor
    call SHMEM_FINALIZE()

end program hello_oshmem
