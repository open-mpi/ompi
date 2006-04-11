#! /bin/sh
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Do a little error checking

if test ! -f fortran_kinds.sh; then
    echo "ERROR: Cannot find fortran_kinds.sh" >&2
    exit 1
elif test -z fortran_kinds.sh; then
    echo "ERROR: fortran_kinds.sh appears to be empty!" >&2
    exit 1
fi

# Read in the KIND information

. fortran_kinds.sh

# Setup

output=1
allranks="0 $ranks"

#------------------------------------------------------------------------

# Helper functions

start() {
    check_size $2
    if test "$output" = "1"; then        
        echo "interface $1"
    fi
}

end() {
    if test "$output" = "1"; then
        cat <<EOF
end interface $1


EOF
    fi
}

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errorcode, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Abort small
output MPI_Abort
end MPI_Abort

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: origin_addr
  integer, intent(in) :: origin_count
  integer, intent(in) :: origin_datatype
  integer, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
  integer, intent(in) :: target_count
  integer, intent(in) :: target_datatype
  integer, intent(in) :: op
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Accumulate medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Accumulate ${rank} CH "character${dim}"
  output MPI_Accumulate ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Accumulate ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Accumulate ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Accumulate ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Accumulate

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorclass, ierr)
  include 'mpif.h'
  integer, intent(in) :: errorclass
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_class small
output MPI_Add_error_class
end MPI_Add_error_class

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorclass, errorcode, ierr)
  include 'mpif.h'
  integer, intent(in) :: errorclass
  integer, intent(out) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_code small
output MPI_Add_error_code
end MPI_Add_error_code

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, string, ierr)
  include 'mpif.h'
  integer, intent(in) :: errorcode
  character(len=*), intent(in) :: string
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_string small
output MPI_Add_error_string
end MPI_Add_error_string

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(location, address, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: location
  integer, intent(out) :: address
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Address medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Address ${rank} CH "character${dim}"
  output MPI_Address ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Address ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Address ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Address ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Address

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Allgather large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Allgather ${rank} CH "character${dim}"
  output MPI_Allgather ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Allgather ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Allgather ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Allgather ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Allgather

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: displs
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Allgatherv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Allgatherv ${rank} CH "character${dim}"
  output MPI_Allgatherv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Allgatherv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Allgatherv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Allgatherv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Allgatherv

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(size, info, baseptr, ierr)
  include 'mpif.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size
  integer, intent(in) :: info
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: baseptr
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Alloc_mem medium
output MPI_Alloc_mem
end MPI_Alloc_mem

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Allreduce large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Allreduce ${rank} CH "character${dim}"
  output MPI_Allreduce ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Allreduce ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Allreduce ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Allreduce ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Allreduce

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Alltoall large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Alltoall ${rank} CH "character${dim}"
  output MPI_Alltoall ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Alltoall ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Alltoall ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Alltoall ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Alltoall

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, dimension(*), intent(in) :: sendcounts
  integer, dimension(*), intent(in) :: sdispls
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: rdispls
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Alltoallv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Alltoallv ${rank} CH "character${dim}"
  output MPI_Alltoallv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Alltoallv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Alltoallv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Alltoallv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Alltoallv

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, dimension(*), intent(in) :: sendcounts
  integer, dimension(*), intent(in) :: sdispls
  integer, dimension(*), intent(in) :: sendtypes
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: rdispls
  integer, dimension(*), intent(in) :: recvtypes
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Alltoallw large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Alltoallw ${rank} CH "character${dim}"
  output MPI_Alltoallw ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Alltoallw ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Alltoallw ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Alltoallw ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Alltoallw

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_delete small
output MPI_Attr_delete
end MPI_Attr_delete

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, attribute_val, flag, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: attribute_val
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_get small
output MPI_Attr_get
end MPI_Attr_get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, attribute_val, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_put small
output MPI_Attr_put
end MPI_Attr_put

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Barrier small
output MPI_Barrier
end MPI_Barrier

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buffer
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Bcast medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Bcast ${rank} CH "character${dim}"
  output MPI_Bcast ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Bcast ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Bcast ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Bcast ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Bcast

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Bsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Bsend ${rank} CH "character${dim}"
  output MPI_Bsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Bsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Bsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Bsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Bsend

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Bsend_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Bsend_init ${rank} CH "character${dim}"
  output MPI_Bsend_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Bsend_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Bsend_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Bsend_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Bsend_init

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buffer, size, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buffer
  integer, intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Buffer_attach medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Buffer_attach ${rank} CH "character${dim}"
  output MPI_Buffer_attach ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Buffer_attach ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Buffer_attach ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Buffer_attach ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Buffer_attach

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buffer, size, ierr)
  include 'mpif.h'
  ${type}, intent(out) :: buffer
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Buffer_detach medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Buffer_detach ${rank} CH "character${dim}"
  output MPI_Buffer_detach ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Buffer_detach ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Buffer_detach ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Buffer_detach ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Buffer_detach

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  include 'mpif.h'
  integer, intent(in) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cancel small
output MPI_Cancel
end MPI_Cancel

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, maxdims, coords, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_coords small
output MPI_Cart_coords
end MPI_Cart_coords

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(old_comm, ndims, dims, periods, reorder, &
        comm_cart, ierr)
  include 'mpif.h'
  integer, intent(in) :: old_comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  integer, dimension(*), intent(in) :: periods
  integer, intent(in) :: reorder
  integer, intent(out) :: comm_cart
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_create small
output MPI_Cart_create
end MPI_Cart_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, maxdims, dims, periods, coords&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: dims
  integer, dimension(*), intent(out) :: periods
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_get small
output MPI_Cart_get
end MPI_Cart_get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ndims, dims, periods, newrank&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  integer, dimension(*), intent(in) :: periods
  integer, intent(out) :: newrank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_map small
output MPI_Cart_map
end MPI_Cart_map

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, coords, rank, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, dimension(*), intent(in) :: coords
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_rank small
output MPI_Cart_rank
end MPI_Cart_rank

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, direction, disp, rank_source, rank_dest&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: direction
  integer, intent(in) :: disp
  integer, intent(out) :: rank_source
  integer, intent(out) :: rank_dest
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_shift small
output MPI_Cart_shift
end MPI_Cart_shift

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, remain_dims, new_comm, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, dimension(*), intent(in) :: remain_dims
  integer, intent(out) :: new_comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_sub small
output MPI_Cart_sub
end MPI_Cart_sub

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ndims, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: ndims
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cartdim_get small
output MPI_Cartdim_get
end MPI_Cartdim_get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errorcode, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_call_errhandler small
output MPI_Comm_call_errhandler
end MPI_Comm_call_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm1, comm2, result, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm1
  integer, intent(in) :: comm2
  integer, intent(out) :: result
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_compare small
output MPI_Comm_compare
end MPI_Comm_compare

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, newcomm, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create small
output MPI_Comm_create
end MPI_Comm_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  include 'mpif.h'
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create_errhandler small
output MPI_Comm_create_errhandler
end MPI_Comm_create_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierr)
  include 'mpif.h'
  external :: comm_copy_attr_fn
  external :: comm_delete_attr_fn
  integer, intent(out) :: comm_keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create_keyval small
output MPI_Comm_create_keyval
end MPI_Comm_create_keyval

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm
  integer, intent(in) :: comm_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_delete_attr small
output MPI_Comm_delete_attr
end MPI_Comm_delete_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, newcomm, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_dup small
output MPI_Comm_dup
end MPI_Comm_dup

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_free small
output MPI_Comm_free
end MPI_Comm_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_keyval, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_free_keyval small
output MPI_Comm_free_keyval
end MPI_Comm_free_keyval

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, attribute_val, flag, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_attr small
output MPI_Comm_get_attr
end MPI_Comm_get_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, erhandler, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: erhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_errhandler small
output MPI_Comm_get_errhandler
end MPI_Comm_get_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_name, resultlen, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  character(len=*), intent(out) :: comm_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_name small
output MPI_Comm_get_name
end MPI_Comm_get_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_group small
output MPI_Comm_group
end MPI_Comm_group

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_rank small
output MPI_Comm_rank
end MPI_Comm_rank

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_remote_group small
output MPI_Comm_remote_group
end MPI_Comm_remote_group

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, size, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_remote_size small
output MPI_Comm_remote_size
end MPI_Comm_remote_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, attribute_val, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_attr small
output MPI_Comm_set_attr
end MPI_Comm_set_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_errhandler small
output MPI_Comm_set_errhandler
end MPI_Comm_set_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_name, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm
  character(len=*), intent(in) :: comm_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_name small
output MPI_Comm_set_name
end MPI_Comm_set_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, size, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_size small
output MPI_Comm_size
end MPI_Comm_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, color, key, newcomm, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: color
  integer, intent(in) :: key
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_split small
output MPI_Comm_split
end MPI_Comm_split

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, flag, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm
  integer, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_test_inter small
output MPI_Comm_test_inter
end MPI_Comm_test_inter

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(nnodes, ndims, dims, ierr)
  include 'mpif.h'
  integer, intent(in) :: nnodes
  integer, intent(in) :: ndims
  integer, dimension(*), intent(inout) :: dims
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Dims_create small
output MPI_Dims_create
end MPI_Dims_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  include 'mpif.h'
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_create small
output MPI_Errhandler_create
end MPI_Errhandler_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errhandler, ierr)
  include 'mpif.h'
  integer, intent(inout) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_free small
output MPI_Errhandler_free
end MPI_Errhandler_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_get small
output MPI_Errhandler_get
end MPI_Errhandler_get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_set small
output MPI_Errhandler_set
end MPI_Errhandler_set

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, errorclass, ierr)
  include 'mpif.h'
  integer, intent(in) :: errorcode
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Error_class small
output MPI_Error_class
end MPI_Error_class

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, string, resultlen, ierr)
  include 'mpif.h'
  integer, intent(in) :: errorcode
  character(len=*), intent(out) :: string
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Error_string small
output MPI_Error_string
end MPI_Error_string

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Exscan large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Exscan ${rank} CH "character${dim}"
  output MPI_Exscan ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Exscan ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Exscan ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Exscan ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Exscan

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, errorcode, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_call_errhandler small
output MPI_File_call_errhandler
end MPI_File_call_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_close small
output MPI_File_close
end MPI_File_close

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  include 'mpif.h'
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_create_errhandler small
output MPI_File_create_errhandler
end MPI_File_create_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(filename, info, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: filename
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_delete small
output MPI_File_delete
end MPI_File_delete

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, amode, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer, intent(out) :: amode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_amode small
output MPI_File_get_amode
end MPI_File_get_amode

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, flag, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_atomicity small
output MPI_File_get_atomicity
end MPI_File_get_atomicity

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, disp, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer(kind=MPI_OFFSET_KIND), intent(out) :: disp
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_byte_offset small
output MPI_File_get_byte_offset
end MPI_File_get_byte_offset

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(file, errhandler, ierr)
  include 'mpif.h'
  integer, intent(in) :: file
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_errhandler small
output MPI_File_get_errhandler
end MPI_File_get_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, group, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_group small
output MPI_File_get_group
end MPI_File_get_group

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, info_used, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer, intent(out) :: info_used
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_info small
output MPI_File_get_info
end MPI_File_get_info

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_position small
output MPI_File_get_position
end MPI_File_get_position

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_position_shared small
output MPI_File_get_position_shared
end MPI_File_get_position_shared

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_size small
output MPI_File_get_size
end MPI_File_get_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, datatype, extent, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_type_extent small
output MPI_File_get_type_extent
end MPI_File_get_type_extent

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, disp, etype, filetype, datarep&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: disp
  integer, intent(out) :: etype
  integer, intent(out) :: filetype
  character(len=*), intent(out) :: datarep
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_view small
output MPI_File_get_view
end MPI_File_get_view

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iread medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_iread ${rank} CH "character${dim}"
  output MPI_File_iread ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_iread ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_iread ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_iread ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iread

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iread_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_iread_at ${rank} CH "character${dim}"
  output MPI_File_iread_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_iread_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_iread_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_iread_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iread_at

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iread_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_iread_shared ${rank} CH "character${dim}"
  output MPI_File_iread_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_iread_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_iread_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_iread_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iread_shared

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iwrite medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_iwrite ${rank} CH "character${dim}"
  output MPI_File_iwrite ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_iwrite ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_iwrite ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_iwrite ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iwrite

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iwrite_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_iwrite_at ${rank} CH "character${dim}"
  output MPI_File_iwrite_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_iwrite_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_iwrite_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_iwrite_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iwrite_at

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iwrite_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_iwrite_shared ${rank} CH "character${dim}"
  output MPI_File_iwrite_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_iwrite_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_iwrite_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_iwrite_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iwrite_shared

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, filename, amode, info, fh&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  character(len=*), intent(in) :: filename
  integer, intent(in) :: amode
  integer, intent(in) :: info
  integer, intent(out) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_open small
output MPI_File_open
end MPI_File_open

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_preallocate small
output MPI_File_preallocate
end MPI_File_preallocate

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read ${rank} CH "character${dim}"
  output MPI_File_read ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_all ${rank} CH "character${dim}"
  output MPI_File_read_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_all

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_all_begin ${rank} CH "character${dim}"
  output MPI_File_read_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_all_begin

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_all_end ${rank} CH "character${dim}"
  output MPI_File_read_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_all_end

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_at ${rank} CH "character${dim}"
  output MPI_File_read_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_at_all ${rank} CH "character${dim}"
  output MPI_File_read_at_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_at_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_at_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_at_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at_all

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_at_all_begin ${rank} CH "character${dim}"
  output MPI_File_read_at_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_at_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_at_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_at_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at_all_begin

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  ${type}, intent(out) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_at_all_end ${rank} CH "character${dim}"
  output MPI_File_read_at_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_at_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_at_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_at_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at_all_end

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_ordered medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_ordered ${rank} CH "character${dim}"
  output MPI_File_read_ordered ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_ordered ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_ordered ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_ordered ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_ordered

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_ordered_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_ordered_begin ${rank} CH "character${dim}"
  output MPI_File_read_ordered_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_ordered_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_ordered_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_ordered_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_ordered_begin

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_ordered_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_ordered_end ${rank} CH "character${dim}"
  output MPI_File_read_ordered_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_ordered_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_ordered_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_ordered_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_ordered_end

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_read_shared ${rank} CH "character${dim}"
  output MPI_File_read_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_read_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_read_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_read_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_shared

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, whence, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_seek small
output MPI_File_seek
end MPI_File_seek

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, whence, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_seek_shared small
output MPI_File_seek_shared
end MPI_File_seek_shared

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, flag, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_atomicity small
output MPI_File_set_atomicity
end MPI_File_set_atomicity

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(file, errhandler, ierr)
  include 'mpif.h'
  integer, intent(in) :: file
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_errhandler small
output MPI_File_set_errhandler
end MPI_File_set_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, info, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_info small
output MPI_File_set_info
end MPI_File_set_info

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_size small
output MPI_File_set_size
end MPI_File_set_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, disp, etype, filetype, datarep, &
        info, ierr)
  include 'mpif.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: disp
  integer, intent(in) :: etype
  integer, intent(in) :: filetype
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_view small
output MPI_File_set_view
end MPI_File_set_view

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_sync small
output MPI_File_sync
end MPI_File_sync

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write ${rank} CH "character${dim}"
  output MPI_File_write ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_all ${rank} CH "character${dim}"
  output MPI_File_write_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_all

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_all_begin ${rank} CH "character${dim}"
  output MPI_File_write_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_all_begin

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_all_end ${rank} CH "character${dim}"
  output MPI_File_write_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_all_end

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_at ${rank} CH "character${dim}"
  output MPI_File_write_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_at_all ${rank} CH "character${dim}"
  output MPI_File_write_at_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_at_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_at_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_at_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at_all

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_at_all_begin ${rank} CH "character${dim}"
  output MPI_File_write_at_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_at_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_at_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_at_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at_all_begin

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_at_all_end ${rank} CH "character${dim}"
  output MPI_File_write_at_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_at_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_at_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_at_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at_all_end

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_ordered medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_ordered ${rank} CH "character${dim}"
  output MPI_File_write_ordered ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_ordered ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_ordered ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_ordered ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_ordered

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_ordered_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_ordered_begin ${rank} CH "character${dim}"
  output MPI_File_write_ordered_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_ordered_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_ordered_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_ordered_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_ordered_begin

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_ordered_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_ordered_end ${rank} CH "character${dim}"
  output MPI_File_write_ordered_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_ordered_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_ordered_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_ordered_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_ordered_end

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_File_write_shared ${rank} CH "character${dim}"
  output MPI_File_write_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_File_write_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_File_write_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_File_write_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_shared

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ierr)
  include 'mpif.h'
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Finalize small
output MPI_Finalize
end MPI_Finalize

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  include 'mpif.h'
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Finalized small
output MPI_Finalized
end MPI_Finalized

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(base, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: base
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Free_mem medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Free_mem ${rank} CH "character${dim}"
  output MPI_Free_mem ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Free_mem ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Free_mem ${rank} R${kind} "real*${kind}${dim}"
 done
  for kind in $ckinds
  do
    output MPI_Free_mem ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Free_mem

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Gather large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Gather ${rank} CH "character${dim}"
  output MPI_Gather ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Gather ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Gather ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Gather ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Gather

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: displs
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Gatherv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Gatherv ${rank} CH "character${dim}"
  output MPI_Gatherv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Gatherv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Gatherv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Gatherv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Gatherv

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: origin_addr
  integer, intent(in) :: origin_count
  integer, intent(in) :: origin_datatype
  integer, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
  integer, intent(in) :: target_count
  integer, intent(in) :: target_datatype
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Get medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Get ${rank} CH "character${dim}"
  output MPI_Get ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Get ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Get ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Get ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(location, address, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: location
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Get_address medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Get_address ${rank} CH "character${dim}"
  output MPI_Get_address ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Get_address ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Get_address ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Get_address ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Get_address

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_count small
output MPI_Get_count
end MPI_Get_count

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_elements small
output MPI_Get_elements
end MPI_Get_elements

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(name, resultlen, ierr)
  include 'mpif.h'
  character(len=*), intent(out) :: name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_processor_name small
output MPI_Get_processor_name
end MPI_Get_processor_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(version, subversion, ierr)
  include 'mpif.h'
  integer, intent(out) :: version
  integer, intent(out) :: subversion
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_version small
output MPI_Get_version
end MPI_Get_version

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_old, nnodes, index, edges, reorder, &
        comm_graph, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm_old
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  integer, intent(in) :: reorder
  integer, intent(out) :: comm_graph
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_create small
output MPI_Graph_create
end MPI_Graph_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, maxindex, maxedges, index, edges&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: maxindex
  integer, intent(in) :: maxedges
  integer, dimension(*), intent(out) :: index
  integer, dimension(*), intent(out) :: edges
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_get small
output MPI_Graph_get
end MPI_Graph_get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, nnodes, index, edges, newrank&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  integer, intent(out) :: newrank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_map small
output MPI_Graph_map
end MPI_Graph_map

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, maxneighbors, neighbors, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxneighbors
  integer, dimension(*), intent(out) :: neighbors
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_neighbors small
output MPI_Graph_neighbors
end MPI_Graph_neighbors

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, nneighbors, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(out) :: nneighbors
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_neighbors_count small
output MPI_Graph_neighbors_count
end MPI_Graph_neighbors_count

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, nnodes, nedges, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: nnodes
  integer, intent(out) :: nedges
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graphdims_get small
output MPI_Graphdims_get
end MPI_Graphdims_get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  include 'mpif.h'
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Grequest_complete small
output MPI_Grequest_complete
end MPI_Grequest_complete

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(query_fn, free_fn, cancel_fn, extra_state, request&
        , ierr)
  include 'mpif.h'
  external :: query_fn
  external :: free_fn
  external :: cancel_fn
  integer, intent(in) :: extra_state
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Grequest_start small
output MPI_Grequest_start
end MPI_Grequest_start

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, result, ierr)
  include 'mpif.h'
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: result
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_compare small
output MPI_Group_compare
end MPI_Group_compare

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  include 'mpif.h'
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_difference small
output MPI_Group_difference
end MPI_Group_difference

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranks, newgroup, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_excl small
output MPI_Group_excl
end MPI_Group_excl

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, ierr)
  include 'mpif.h'
  integer, intent(inout) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_free small
output MPI_Group_free
end MPI_Group_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranks, newgroup, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_incl small
output MPI_Group_incl
end MPI_Group_incl

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  include 'mpif.h'
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_intersection small
output MPI_Group_intersection
end MPI_Group_intersection

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranges, newgroup, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_range_excl small
output MPI_Group_range_excl
end MPI_Group_range_excl

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranges, newgroup, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_range_incl small
output MPI_Group_range_incl
end MPI_Group_range_incl

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, rank, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_rank small
output MPI_Group_rank
end MPI_Group_rank

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, size, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_size small
output MPI_Group_size
end MPI_Group_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, n, ranks1, group2, ranks2&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: group1
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks1
  integer, intent(in) :: group2
  integer, dimension(*), intent(out) :: ranks2
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_translate_ranks small
output MPI_Group_translate_ranks
end MPI_Group_translate_ranks

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  include 'mpif.h'
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_union small
output MPI_Group_union
end MPI_Group_union

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Ibsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Ibsend ${rank} CH "character${dim}"
  output MPI_Ibsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Ibsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Ibsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Ibsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Ibsend

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, ierr)
  include 'mpif.h'
  integer, intent(out) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_create small
output MPI_Info_create
end MPI_Info_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, ierr)
  include 'mpif.h'
  integer, intent(out) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_delete small
output MPI_Info_delete
end MPI_Info_delete

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, newinfo, ierr)
  include 'mpif.h'
  integer, intent(in) :: info
  integer, intent(out) :: newinfo
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_dup small
output MPI_Info_dup
end MPI_Info_dup

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, ierr)
  include 'mpif.h'
  integer, intent(inout) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_free small
output MPI_Info_free
end MPI_Info_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, valuelen, value, flag&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(in) :: valuelen
  character(len=*), intent(out) :: value
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get small
output MPI_Info_get
end MPI_Info_get

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, nkeys, ierr)
  include 'mpif.h'
  integer, intent(in) :: info
  integer, intent(out) :: nkeys
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_nkeys small
output MPI_Info_get_nkeys
end MPI_Info_get_nkeys

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, n, key, ierr)
  include 'mpif.h'
  integer, intent(in) :: info
  integer, intent(in) :: n
  character(len=*), intent(out) :: key
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_nthkey small
output MPI_Info_get_nthkey
end MPI_Info_get_nthkey

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, valuelen, flag, ierr)
  include 'mpif.h'
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: valuelen
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_valuelen small
output MPI_Info_get_valuelen
end MPI_Info_get_valuelen

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, value, ierr)
  include 'mpif.h'
  integer, intent(inout) :: info
  character(len=*), intent(in) :: key
  character(len=*), intent(in) :: value
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_set small
output MPI_Info_set
end MPI_Info_set

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ierr)
  include 'mpif.h'
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Init small
output MPI_Init
end MPI_Init

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(required, provided, ierr)
  include 'mpif.h'
  integer, intent(in) :: required
  integer, intent(out) :: provided
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Init_thread small
output MPI_Init_thread
end MPI_Init_thread

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  include 'mpif.h'
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Initialized small
output MPI_Initialized
end MPI_Initialized

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(local_comm, local_leader, bridge_comm, remote_leader, tag, &
        newintercomm, ierr)
  include 'mpif.h'
  integer, intent(in) :: local_comm
  integer, intent(in) :: local_leader
  integer, intent(in) :: bridge_comm
  integer, intent(in) :: remote_leader
  integer, intent(in) :: tag
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Intercomm_create small
output MPI_Intercomm_create
end MPI_Intercomm_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(intercomm, high, newintercomm, ierr)
  include 'mpif.h'
  integer, intent(in) :: intercomm
  integer, intent(in) :: high
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Intercomm_merge small
output MPI_Intercomm_merge
end MPI_Intercomm_merge

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, flag, status&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Iprobe small
output MPI_Iprobe
end MPI_Iprobe

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Irecv medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Irecv ${rank} CH "character${dim}"
  output MPI_Irecv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Irecv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Irecv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Irecv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Irecv

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Irsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Irsend ${rank} CH "character${dim}"
  output MPI_Irsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Irsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Irsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Irsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Irsend

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  include 'mpif.h'
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Is_thread_main small
output MPI_Is_thread_main
end MPI_Is_thread_main

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Isend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Isend ${rank} CH "character${dim}"
  output MPI_Isend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Isend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Isend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Isend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Isend

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Issend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Issend ${rank} CH "character${dim}"
  output MPI_Issend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Issend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Issend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Issend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Issend

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(copy_fn, delete_fn, keyval, extra_state, ierr)
  include 'mpif.h'
  external :: copy_fn
  external :: delete_fn
  integer, intent(out) :: keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Keyval_create small
output MPI_Keyval_create
end MPI_Keyval_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(keyval, ierr)
  include 'mpif.h'
  integer, intent(inout) :: keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Keyval_free small
output MPI_Keyval_free
end MPI_Keyval_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, commute, op, ierr)
  include 'mpif.h'
  external :: function
  integer, intent(in) :: commute
  integer, intent(out) :: op
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_create small
output MPI_Op_create
end MPI_Op_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(op, ierr)
  include 'mpif.h'
  integer, intent(inout) :: op
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_free small
output MPI_Op_free
end MPI_Op_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: inbuf
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  ${type}, intent(out) :: outbuf
  integer, intent(out) :: outsize
  integer, intent(inout) :: position
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Pack large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Pack ${rank} CH "character${dim}"
  output MPI_Pack ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Pack ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Pack ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Pack ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Pack

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: datarep
  ${type}, intent(in) :: inbuf
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  ${type}, intent(out) :: outbuf
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize
  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Pack_external large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Pack_external ${rank} CH "character${dim}"
  output MPI_Pack_external ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Pack_external ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Pack_external ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Pack_external ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Pack_external

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datarep, incount, datatype, size, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Pack_external_size small
output MPI_Pack_external_size
end MPI_Pack_external_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(incount, datatype, comm, size, ierr)
  include 'mpif.h'
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Pack_size small
output MPI_Pack_size
end MPI_Pack_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(level)
  include 'mpif.h'
  integer, intent(in) :: level

end subroutine ${procedure}

EOF
}

start MPI_Pcontrol small
output MPI_Pcontrol
end MPI_Pcontrol

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, status, ierr)
  include 'mpif.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Probe small
output MPI_Probe
end MPI_Probe

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: origin_addr
  integer, intent(in) :: origin_count
  integer, intent(in) :: origin_datatype
  integer, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
  integer, intent(in) :: target_count
  integer, intent(in) :: target_datatype
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Put medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Put ${rank} CH "character${dim}"
  output MPI_Put ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Put ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Put ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Put ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Put

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(provided, ierr)
  include 'mpif.h'
  integer, intent(out) :: provided
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Query_thread small
output MPI_Query_thread
end MPI_Query_thread

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)
  include 'mpif.h'
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Recv medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Recv ${rank} CH "character${dim}"
  output MPI_Recv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Recv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Recv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Recv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Recv

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Recv_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Recv_init ${rank} CH "character${dim}"
  output MPI_Recv_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Recv_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Recv_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Recv_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Recv_init

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Reduce large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Reduce ${rank} CH "character${dim}"
  output MPI_Reduce ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Reduce ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Reduce ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Reduce ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Reduce

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcounts
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Reduce_scatter large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Reduce_scatter ${rank} CH "character${dim}"
  output MPI_Reduce_scatter ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Reduce_scatter ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Reduce_scatter ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Reduce_scatter ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Reduce_scatter

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state&
        , ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: datarep
  external :: read_conversion_fn
  external :: write_conversion_fn
  external :: dtype_file_extent_fn
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Register_datarep small
output MPI_Register_datarep
end MPI_Register_datarep

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  include 'mpif.h'
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Request_free small
output MPI_Request_free
end MPI_Request_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, flag, status, ierr)
  include 'mpif.h'
  integer, intent(in) :: request
  integer, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Request_get_status small
output MPI_Request_get_status
end MPI_Request_get_status

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: ibuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Rsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Rsend ${rank} CH "character${dim}"
  output MPI_Rsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Rsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Rsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Rsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Rsend

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Rsend_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Rsend_init ${rank} CH "character${dim}"
  output MPI_Rsend_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Rsend_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Rsend_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Rsend_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Rsend_init

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Scan large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Scan ${rank} CH "character${dim}"
  output MPI_Scan ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Scan ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Scan ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Scan ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Scan

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Scatter large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Scatter ${rank} CH "character${dim}"
  output MPI_Scatter ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Scatter ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Scatter ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Scatter ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Scatter

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcounts
  integer, intent(in) :: displs
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Scatterv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Scatterv ${rank} CH "character${dim}"
  output MPI_Scatterv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Scatterv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Scatterv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Scatterv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Scatterv

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Send medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Send ${rank} CH "character${dim}"
  output MPI_Send ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Send ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Send ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Send ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Send

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Send_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Send_init ${rank} CH "character${dim}"
  output MPI_Send_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Send_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Send_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Send_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Send_init

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  integer, intent(in) :: dest
  integer, intent(in) :: sendtag
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: source
  integer, intent(in) :: recvtag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Sendrecv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Sendrecv ${rank} CH "character${dim}"
  output MPI_Sendrecv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Sendrecv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Sendrecv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Sendrecv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Sendrecv

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)
  include 'mpif.h'
  ${type}, intent(inout) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: sendtag
  integer, intent(in) :: source
  integer, intent(in) :: recvtag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Sendrecv_replace medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Sendrecv_replace ${rank} CH "character${dim}"
  output MPI_Sendrecv_replace ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Sendrecv_replace ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Sendrecv_replace ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Sendrecv_replace ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Sendrecv_replace

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(x, size, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_SIZEOF trivial

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_SIZEOF ${rank} CH "character${dim}"
  output MPI_SIZEOF ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_SIZEOF ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_SIZEOF ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_SIZEOF ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_SIZEOF

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Ssend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Ssend ${rank} CH "character${dim}"
  output MPI_Ssend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Ssend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Ssend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Ssend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Ssend

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Ssend_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Ssend_init ${rank} CH "character${dim}"
  output MPI_Ssend_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Ssend_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Ssend_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Ssend_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Ssend_init

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  include 'mpif.h'
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Start small
output MPI_Start
end MPI_Start

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(inout) :: array_of_requests
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Startall small
output MPI_Startall
end MPI_Startall

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, flag, ierr)
  include 'mpif.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Status_set_cancelled small
output MPI_Status_set_cancelled
end MPI_Status_set_cancelled

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(in) :: datatype
  integer, intent(in) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Status_set_elements small
output MPI_Status_set_elements
end MPI_Status_set_elements

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, flag, status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: request
  integer, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Test small
output MPI_Test
end MPI_Test

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, flag, ierr)
  include 'mpif.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Test_cancelled small
output MPI_Test_cancelled
end MPI_Test_cancelled

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, flag, array_of_statuses, ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: flag
  integer, dimension(count,MPI_STATUS_SIZE), intent(inout) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Testall small
output MPI_Testall
end MPI_Testall

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, index, flag, status&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  integer, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Testany small
output MPI_Testany
end MPI_Testany

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(incount, MPI_STATUS_SIZE), intent(inout) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Testsome small
output MPI_Testsome
end MPI_Testsome

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, status, ierr)
  include 'mpif.h'
  integer, intent(in) :: comm
  integer, intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Topo_test small
output MPI_Topo_test
end MPI_Topo_test

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, ierr)
  include 'mpif.h'
  integer, intent(inout) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_commit small
output MPI_Type_commit
end MPI_Type_commit

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, oldtype, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_contiguous small
output MPI_Type_contiguous
end MPI_Type_contiguous

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(size, rank, ndims, gsize_array, distrib_array, &
        darg_array, psize_array, order, oldtype, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: size
  integer, intent(in) :: rank
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: gsize_array
  integer, dimension(*), intent(in) :: distrib_array
  integer, dimension(*), intent(in) :: darg_array
  integer, dimension(*), intent(in) :: psize_array
  integer, intent(in) :: order
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_darray small
output MPI_Type_create_darray
end MPI_Type_create_darray

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(p, r, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_complex small
output MPI_Type_create_f90_complex
end MPI_Type_create_f90_complex

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(r, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_integer small
output MPI_Type_create_f90_integer
end MPI_Type_create_f90_integer

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(p, r, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_real small
output MPI_Type_create_f90_real
end MPI_Type_create_f90_real

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_hindexed small
output MPI_Type_create_hindexed
end MPI_Type_create_hindexed

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_hvector small
output MPI_Type_create_hvector
end MPI_Type_create_hvector

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_indexed_block small
output MPI_Type_create_indexed_block
end MPI_Type_create_indexed_block

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierr)
  include 'mpif.h'
  external :: type_copy_attr_fn
  external :: type_delete_attr_fn
  integer, intent(out) :: type_keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_keyval small
output MPI_Type_create_keyval
end MPI_Type_create_keyval

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(oldtype, lb, extent, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: oldtype
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extent
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_resized small
output MPI_Type_create_resized
end MPI_Type_create_resized

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_block_lengths, array_of_displacements, array_of_types, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_block_lengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_struct small
output MPI_Type_create_struct
end MPI_Type_create_struct

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ndims, size_array, subsize_array, start_array, order, &
        oldtype, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: size_array
  integer, dimension(*), intent(in) :: subsize_array
  integer, dimension(*), intent(in) :: start_array
  integer, intent(in) :: order
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_subarray small
output MPI_Type_create_subarray
end MPI_Type_create_subarray

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, ierr)
  include 'mpif.h'
  integer, intent(inout) :: type
  integer, intent(in) :: type_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_delete_attr small
output MPI_Type_delete_attr
end MPI_Type_delete_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, newtype, ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_dup small
output MPI_Type_dup
end MPI_Type_dup

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, extent, ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  integer, intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_extent small
output MPI_Type_extent
end MPI_Type_extent

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, ierr)
  include 'mpif.h'
  integer, intent(inout) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_free small
output MPI_Type_free
end MPI_Type_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type_keyval, ierr)
  include 'mpif.h'
  integer, intent(inout) :: type_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_free_keyval small
output MPI_Type_free_keyval
end MPI_Type_free_keyval

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, attribute_val, flag, ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_attr small
output MPI_Type_get_attr
end MPI_Type_get_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(mtype, max_integers, max_addresses, max_datatypes, array_of_integers, &
        array_of_addresses, array_of_datatypes, ierr)
  include 'mpif.h'
  integer, intent(in) :: mtype
  integer, intent(in) :: max_integers
  integer, intent(in) :: max_addresses
  integer, intent(in) :: max_datatypes
  integer, dimension(*), intent(out) :: array_of_integers
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(out) :: array_of_addresses
  integer, dimension(*), intent(out) :: array_of_datatypes
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_contents small
output MPI_Type_get_contents
end MPI_Type_get_contents

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, num_integers, num_addresses, num_datatypes, combiner&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  integer, intent(out) :: num_integers
  integer, intent(out) :: num_addresses
  integer, intent(out) :: num_datatypes
  integer, intent(out) :: combiner
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_envelope small
output MPI_Type_get_envelope
end MPI_Type_get_envelope

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, lb, extent, ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_extent small
output MPI_Type_get_extent
end MPI_Type_get_extent

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_name, resultlen, ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  character(len=*), intent(out) :: type_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_name small
output MPI_Type_get_name
end MPI_Type_get_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datatype, true_lb, true_extent, ierr)
  include 'mpif.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_true_extent small
output MPI_Type_get_true_extent
end MPI_Type_get_true_extent

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_hindexed small
output MPI_Type_hindexed
end MPI_Type_hindexed

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_hvector small
output MPI_Type_hvector
end MPI_Type_hvector

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_indexed small
output MPI_Type_indexed
end MPI_Type_indexed

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, lb, ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  integer, intent(out) :: lb
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_lb small
output MPI_Type_lb
end MPI_Type_lb

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(typeclass, size, type, ierr)
  include 'mpif.h'
  integer, intent(in) :: typeclass
  integer, intent(in) :: size
  integer, intent(out) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_match_size small
output MPI_Type_match_size
end MPI_Type_match_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, attr_val, ierr)
  include 'mpif.h'
  integer, intent(inout) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attr_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_set_attr small
output MPI_Type_set_attr
end MPI_Type_set_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_name, ierr)
  include 'mpif.h'
  integer, intent(inout) :: type
  character(len=*), intent(in) :: type_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_set_name small
output MPI_Type_set_name
end MPI_Type_set_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, size, ierr)
  include 'mpif.h'
  integer, intent(in) :: type
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_size small
output MPI_Type_size
end MPI_Type_size

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_struct small
output MPI_Type_struct
end MPI_Type_struct

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(mtype, ub, ierr)
  include 'mpif.h'
  integer, intent(in) :: mtype
  integer, intent(out) :: ub
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_ub small
output MPI_Type_ub
end MPI_Type_ub

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_vector small
output MPI_Type_vector
end MPI_Type_vector

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: inbuf
  integer, intent(in) :: insize
  integer, intent(inout) :: position
  ${type}, intent(out) :: outbuf
  integer, intent(in) :: outcount
  integer, intent(in) :: datatype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Unpack large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Unpack ${rank} CH "character${dim}"
  output MPI_Unpack ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Unpack ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Unpack ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Unpack ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Unpack

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: datarep
  ${type}, intent(in) :: inbuf
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize
  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position
  ${type}, intent(out) :: outbuf
  integer, intent(in) :: outcount
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Unpack_external large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Unpack_external ${rank} CH "character${dim}"
  output MPI_Unpack_external ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Unpack_external ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Unpack_external ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Unpack_external ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Unpack_external

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, status, ierr)
  include 'mpif.h'
  integer, intent(inout) :: request
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Wait small
output MPI_Wait
end MPI_Wait

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, array_of_statuses, ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, dimension(count, MPI_STATUS_SIZE), intent(inout) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Waitall small
output MPI_Waitall
end MPI_Waitall

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, index, status, ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Waitany small
output MPI_Waitany
end MPI_Waitany

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierr)
  include 'mpif.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(incount, MPI_STATUS_SIZE), intent(inout) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Waitsome small
output MPI_Waitsome
end MPI_Waitsome

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errorcode, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_call_errhandler small
output MPI_Win_call_errhandler
end MPI_Win_call_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_complete small
output MPI_Win_complete
end MPI_Win_complete

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)
  include 'mpif.h'
  ${type}, intent(in) :: base
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size
  integer, intent(in) :: disp_unit
  integer, intent(in) :: info
  integer, intent(in) :: comm
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Win_create medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(:)'  ;  esac
  case "$rank" in  2)  dim=', dimension(:,:)'  ;  esac
  case "$rank" in  3)  dim=', dimension(:,:,:)'  ;  esac
  case "$rank" in  4)  dim=', dimension(:,:,:,:)'  ;  esac
  case "$rank" in  5)  dim=', dimension(:,:,:,:,:)'  ;  esac
  case "$rank" in  6)  dim=', dimension(:,:,:,:,:,:)'  ;  esac
  case "$rank" in  7)  dim=', dimension(:,:,:,:,:,:,:)'  ;  esac

  output MPI_Win_create ${rank} CH "character${dim}"
  output MPI_Win_create ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output MPI_Win_create ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output MPI_Win_create ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output MPI_Win_create ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Win_create

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  include 'mpif.h'
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_create_errhandler small
output MPI_Win_create_errhandler
end MPI_Win_create_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierr)
  include 'mpif.h'
  external :: win_copy_attr_fn
  external :: win_delete_attr_fn
  integer, intent(out) :: win_keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_create_keyval small
output MPI_Win_create_keyval
end MPI_Win_create_keyval

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, ierr)
  include 'mpif.h'
  integer, intent(inout) :: win
  integer, intent(in) :: win_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_delete_attr small
output MPI_Win_delete_attr
end MPI_Win_delete_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(assert, win, ierr)
  include 'mpif.h'
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_fence small
output MPI_Win_fence
end MPI_Win_fence

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  include 'mpif.h'
  integer, intent(inout) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_free small
output MPI_Win_free
end MPI_Win_free

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win_keyval, ierr)
  include 'mpif.h'
  integer, intent(inout) :: win_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_free_keyval small
output MPI_Win_free_keyval
end MPI_Win_free_keyval

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, attribute_val, flag, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_attr small
output MPI_Win_get_attr
end MPI_Win_get_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errhandler, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_errhandler small
output MPI_Win_get_errhandler
end MPI_Win_get_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, group, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_group small
output MPI_Win_get_group
end MPI_Win_get_group

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_name, resultlen, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  character(len=*), intent(out) :: win_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_name small
output MPI_Win_get_name
end MPI_Win_get_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(lock_type, rank, assert, win, ierr)
  include 'mpif.h'
  integer, intent(in) :: lock_type
  integer, intent(in) :: rank
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_lock small
output MPI_Win_lock
end MPI_Win_lock

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, assert, win, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_post small
output MPI_Win_post
end MPI_Win_post

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, attribute_val, ierr)
  include 'mpif.h'
  integer, intent(inout) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_attr small
output MPI_Win_set_attr
end MPI_Win_set_attr

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errhandler, ierr)
  include 'mpif.h'
  integer, intent(inout) :: win
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_errhandler small
output MPI_Win_set_errhandler
end MPI_Win_set_errhandler

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_name, ierr)
  include 'mpif.h'
  integer, intent(inout) :: win
  character(len=*), intent(in) :: win_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_name small
output MPI_Win_set_name
end MPI_Win_set_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, assert, win, ierr)
  include 'mpif.h'
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_start small
output MPI_Win_start
end MPI_Win_start

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, flag, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  integer, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_test small
output MPI_Win_test
end MPI_Win_test

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(rank, win, ierr)
  include 'mpif.h'
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_unlock small
output MPI_Win_unlock
end MPI_Win_unlock

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  include 'mpif.h'
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_wait small
output MPI_Win_wait
end MPI_Win_wait

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Close_port small
output MPI_Close_port
end MPI_Close_port

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Lookup_name small
output MPI_Lookup_name
end MPI_Lookup_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, port_name, ierr)
  include 'mpif.h'
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Open_port small
output MPI_Open_port
end MPI_Open_port

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Publish_name small
output MPI_Publish_name
end MPI_Publish_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Unpublish_name small
output MPI_Unpublish_name
end MPI_Unpublish_name

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  include 'mpif.h'
  integer, intent(inout) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_disconnect small
output MPI_Comm_disconnect
end MPI_Comm_disconnect

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(parent, ierr)
  include 'mpif.h'
  integer, intent(out) :: parent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_parent small
output MPI_Comm_get_parent
end MPI_Comm_get_parent

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fd, intercomm, ierr)
  include 'mpif.h'
  integer, intent(in) :: fd
  integer, intent(out) :: intercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_join small
output MPI_Comm_join
end MPI_Comm_join

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, info, root, comm, newcomm&
        , ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_accept small
output MPI_Comm_accept
end MPI_Comm_accept

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, info, root, comm, newcomm&
        , ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_connect small
output MPI_Comm_connect
end MPI_Comm_connect

#------------------------------------------------------------------------

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(command, argv, maxprocs, info, root, &
        comm, intercomm, array_of_errcodes, ierr)
  include 'mpif.h'
  character(len=*), intent(in) :: command
  character(len=*), dimension(*), intent(in) :: argv
  integer, intent(in) :: maxprocs
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_spawn small
output MPI_Comm_spawn
end MPI_Comm_spawn

#------------------------------------------------------------------------

# The SPAWN_MULTIPLE interface has a nice compile-time check to ensure
# that the "count" parameter matches the dimension of the other
# parameters.  If the constant MPI_ARGVS_NULL is a character array of
# some kind, there is no guarantee that the count value provided by
# the application will match the dimension of MPI_ARGVS_NULL, which
# could therefore result in a[n erroneous] compile-time error.  As
# such, it is simpler to just make MPI_ARGVS_NULL a wholly different
# type (e.g., integer) that matches an entirely different interface
# function.

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    # N = "normal"
    cat <<EOF

subroutine ${procedure}N(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  character(len=*), dimension(*), intent(in) :: array_of_commands
  character(len=*), dimension(count,*), intent(in) :: array_of_argv
  integer, dimension(*), intent(in) :: array_of_maxprocs
  integer, dimension(*), intent(in) :: array_of_info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierr
end subroutine ${procedure}N

EOF
}

start MPI_Comm_spawn_multiple small
output MPI_Comm_spawn_multiple

# Now we do the MPI_ARGVS_NULL variant -- note the different type for
# the array_of_argv argument.

output() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    # AN = "ARGV_NULL variant"
    cat <<EOF

subroutine ${procedure}AN(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierr)
  include 'mpif.h'
  integer, intent(in) :: count
  character(len=*), dimension(*), intent(in) :: array_of_commands
  integer, intent(in) :: array_of_argv
  integer, dimension(*), intent(in) :: array_of_maxprocs
  integer, dimension(*), intent(in) :: array_of_info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierr
end subroutine ${procedure}AN

EOF
}

output MPI_Comm_spawn_multiple
end MPI_Comm_spawn_multiple

