#! /bin/sh
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2012      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2012      Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos Nationa Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Do a little error checking

if test ! -f "$1/fortran_kinds.sh"; then
    echo "ERROR: Cannot find fortran_kinds.sh" >&2
    exit 1
elif test -z "$1/fortran_kinds.sh"; then
    echo "ERROR: fortran_kinds.sh appears to be empty!" >&2
    exit 1
fi

# Read in the KIND information

. "$1/fortran_kinds.sh"

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
end interface


EOF
    fi
}

# A few hard-coded functions that cannot pass through to the F77
# equivalents

start MPI_Wtick small
if test "$output" = "1"; then
    cat <<EOF

function MPI_Wtick()
    double precision MPI_Wtick
end function MPI_Wtick

EOF
fi
end MPI_Wtick

start MPI_Wtime small
if test "$output" = "1"; then
    cat <<EOF

function MPI_Wtime()
    double precision MPI_Wtime
end function MPI_Wtime

EOF
fi
end MPI_Wtime

#------------------------------------------------------------------------

output_1() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errorcode, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Abort small
output_1 MPI_Abort
end MPI_Abort

#------------------------------------------------------------------------

output_3() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorclass, ierr)
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_class small
output_3 MPI_Add_error_class
end MPI_Add_error_class

#------------------------------------------------------------------------

output_4() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorclass, errorcode, ierr)
  integer, intent(in) :: errorclass
  integer, intent(out) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_code small
output_4 MPI_Add_error_code
end MPI_Add_error_code

#------------------------------------------------------------------------

output_5() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, string, ierr)
  integer, intent(in) :: errorcode
  character(len=*), intent(in) :: string
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_string small
output_5 MPI_Add_error_string
end MPI_Add_error_string

#------------------------------------------------------------------------

output_14() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_delete small
output_14 MPI_Attr_delete
end MPI_Attr_delete

#------------------------------------------------------------------------

output_15() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, attribute_val, flag, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_get small
output_15 MPI_Attr_get
end MPI_Attr_get

#------------------------------------------------------------------------

output_16() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, attribute_val, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_put small
output_16 MPI_Attr_put
end MPI_Attr_put

#------------------------------------------------------------------------

output_17() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Barrier small
output_17 MPI_Barrier
end MPI_Barrier

#------------------------------------------------------------------------

output_17_nonblocking() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, request, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Ibarrier small
output_17_nonblocking MPI_Ibarrier
end MPI_Ibarrier

#------------------------------------------------------------------------

output_23() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(in) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cancel small
output_23 MPI_Cancel
end MPI_Cancel

#------------------------------------------------------------------------

output_24() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, maxdims, coords, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_coords small
output_24 MPI_Cart_coords
end MPI_Cart_coords

#------------------------------------------------------------------------

output_25() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(old_comm, ndims, dims, periods, reorder, &
        comm_cart, ierr)
  integer, intent(in) :: old_comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  logical, dimension(*), intent(in) :: periods
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_cart
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_create small
output_25 MPI_Cart_create
end MPI_Cart_create

#------------------------------------------------------------------------

output_26() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, maxdims, dims, periods, coords&
        , ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: dims
  logical, dimension(*), intent(out) :: periods
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_get small
output_26 MPI_Cart_get
end MPI_Cart_get

#------------------------------------------------------------------------

output_27() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ndims, dims, periods, newrank&
        , ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  logical, dimension(*), intent(in) :: periods
  integer, intent(out) :: newrank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_map small
output_27 MPI_Cart_map
end MPI_Cart_map

#------------------------------------------------------------------------

output_28() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, coords, rank, ierr)
  integer, intent(in) :: comm
  integer, dimension(*), intent(in) :: coords
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_rank small
output_28 MPI_Cart_rank
end MPI_Cart_rank

#------------------------------------------------------------------------

output_29() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, direction, disp, rank_source, rank_dest&
        , ierr)
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
output_29 MPI_Cart_shift
end MPI_Cart_shift

#------------------------------------------------------------------------

output_30() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, remain_dims, new_comm, ierr)
  integer, intent(in) :: comm
  logical, dimension(*), intent(in) :: remain_dims
  integer, intent(out) :: new_comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_sub small
output_30 MPI_Cart_sub
end MPI_Cart_sub

#------------------------------------------------------------------------

output_31() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ndims, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: ndims
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cartdim_get small
output_31 MPI_Cartdim_get
end MPI_Cartdim_get

#------------------------------------------------------------------------

output_32() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errorcode, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_call_errhandler small
output_32 MPI_Comm_call_errhandler
end MPI_Comm_call_errhandler

#------------------------------------------------------------------------

output_33() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm1, comm2, result, ierr)
  integer, intent(in) :: comm1
  integer, intent(in) :: comm2
  integer, intent(out) :: result
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_compare small
output_33 MPI_Comm_compare
end MPI_Comm_compare

#------------------------------------------------------------------------

output_34() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, newcomm, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create small
output_34 MPI_Comm_create
end MPI_Comm_create

#------------------------------------------------------------------------

output_34_group() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, tag, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(in) :: tag
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Comm_create_group small
output_34_group MPI_Comm_create_group
end MPI_Comm_create_group

#------------------------------------------------------------------------

output_35() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create_errhandler small
output_35 MPI_Comm_create_errhandler
end MPI_Comm_create_errhandler

#------------------------------------------------------------------------

output_36() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierr)
  include 'mpif-config.h'
  external :: comm_copy_attr_fn
  external :: comm_delete_attr_fn
  integer, intent(out) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create_keyval small
output_36 MPI_Comm_create_keyval
end MPI_Comm_create_keyval

#------------------------------------------------------------------------

output_37() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_delete_attr small
output_37 MPI_Comm_delete_attr
end MPI_Comm_delete_attr

#------------------------------------------------------------------------

output_38() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, newcomm, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_dup small
output_38 MPI_Comm_dup
end MPI_Comm_dup

#------------------------------------------------------------------------

output_38_info() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, info, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: info
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Comm_dup_with_info small
output_38_info MPI_Comm_dup_with_info
end MPI_Comm_dup_with_info

#------------------------------------------------------------------------

output_38_nb() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, newcomm, request, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Comm_idup small
output_38_nb MPI_Comm_idup
end MPI_Comm_idup

#------------------------------------------------------------------------

output_39() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_free small
output_39 MPI_Comm_free
end MPI_Comm_free

#------------------------------------------------------------------------

output_40() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_keyval, ierr)
  integer, intent(inout) :: comm_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_free_keyval small
output_40 MPI_Comm_free_keyval
end MPI_Comm_free_keyval

#------------------------------------------------------------------------

output_41a() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, info_used, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(out) :: info_used
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_info small
output_41a MPI_Comm_get_info
end MPI_Comm_get_info

#------------------------------------------------------------------------

output_41() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, attribute_val, flag, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_attr small
output_41 MPI_Comm_get_attr
end MPI_Comm_get_attr

#------------------------------------------------------------------------

output_42() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, erhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: erhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_errhandler small
output_42 MPI_Comm_get_errhandler
end MPI_Comm_get_errhandler

#------------------------------------------------------------------------

output_43() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_name, resultlen, ierr)
  integer, intent(in) :: comm
  character(len=*), intent(out) :: comm_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_name small
output_43 MPI_Comm_get_name
end MPI_Comm_get_name

#------------------------------------------------------------------------

output_44() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_group small
output_44 MPI_Comm_group
end MPI_Comm_group

#------------------------------------------------------------------------

output_45() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_rank small
output_45 MPI_Comm_rank
end MPI_Comm_rank

#------------------------------------------------------------------------

output_46() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_remote_group small
output_46 MPI_Comm_remote_group
end MPI_Comm_remote_group

#------------------------------------------------------------------------

output_47() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, size, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_remote_size small
output_47 MPI_Comm_remote_size
end MPI_Comm_remote_size

#------------------------------------------------------------------------

output_48a() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, info, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: info
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_info small
output_48a MPI_Comm_set_info
end MPI_Comm_set_info

#------------------------------------------------------------------------

output_48() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, attribute_val, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_attr small
output_48 MPI_Comm_set_attr
end MPI_Comm_set_attr

#------------------------------------------------------------------------

output_49() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_errhandler small
output_49 MPI_Comm_set_errhandler
end MPI_Comm_set_errhandler

#------------------------------------------------------------------------

output_50() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_name, ierr)
  integer, intent(in) :: comm
  character(len=*), intent(in) :: comm_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_name small
output_50 MPI_Comm_set_name
end MPI_Comm_set_name

#------------------------------------------------------------------------

output_51() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, size, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_size small
output_51 MPI_Comm_size
end MPI_Comm_size

#------------------------------------------------------------------------

output_52() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, color, key, newcomm, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: color
  integer, intent(in) :: key
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_split small
output_52 MPI_Comm_split
end MPI_Comm_split

#------------------------------------------------------------------------

output_53() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, flag, ierr)
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_test_inter small
output_53 MPI_Comm_test_inter
end MPI_Comm_test_inter

#------------------------------------------------------------------------

output_54() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(nnodes, ndims, dims, ierr)
  integer, intent(in) :: nnodes
  integer, intent(in) :: ndims
  integer, dimension(*), intent(inout) :: dims
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Dims_create small
output_54 MPI_Dims_create
end MPI_Dims_create

#------------------------------------------------------------------------

output_55() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_create small
output_55 MPI_Errhandler_create
end MPI_Errhandler_create

#------------------------------------------------------------------------

output_56() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errhandler, ierr)
  integer, intent(inout) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_free small
output_56 MPI_Errhandler_free
end MPI_Errhandler_free

#------------------------------------------------------------------------

output_57() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_get small
output_57 MPI_Errhandler_get
end MPI_Errhandler_get

#------------------------------------------------------------------------

output_58() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_set small
output_58 MPI_Errhandler_set
end MPI_Errhandler_set

#------------------------------------------------------------------------

output_59() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, errorclass, ierr)
  integer, intent(in) :: errorcode
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Error_class small
output_59 MPI_Error_class
end MPI_Error_class

#------------------------------------------------------------------------

output_60() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, string, resultlen, ierr)
  integer, intent(in) :: errorcode
  character(len=*), intent(out) :: string
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Error_string small
output_60 MPI_Error_string
end MPI_Error_string

#------------------------------------------------------------------------

output_62() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, errorcode, ierr)
  integer, intent(in) :: fh
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_call_errhandler small
output_62 MPI_File_call_errhandler
end MPI_File_call_errhandler

#------------------------------------------------------------------------

output_63() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, ierr)
  integer, intent(inout) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_close small
output_63 MPI_File_close
end MPI_File_close

#------------------------------------------------------------------------

output_64() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_create_errhandler small
output_64 MPI_File_create_errhandler
end MPI_File_create_errhandler

#------------------------------------------------------------------------

output_65() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(filename, info, ierr)
  character(len=*), intent(in) :: filename
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_delete small
output_65 MPI_File_delete
end MPI_File_delete

#------------------------------------------------------------------------

output_66() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, amode, ierr)
  integer, intent(in) :: fh
  integer, intent(out) :: amode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_amode small
output_66 MPI_File_get_amode
end MPI_File_get_amode

#------------------------------------------------------------------------

output_67() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, flag, ierr)
  integer, intent(in) :: fh
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_atomicity small
output_67 MPI_File_get_atomicity
end MPI_File_get_atomicity

#------------------------------------------------------------------------

output_68() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, disp, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer(kind=MPI_OFFSET_KIND), intent(out) :: disp
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_byte_offset small
output_68 MPI_File_get_byte_offset
end MPI_File_get_byte_offset

#------------------------------------------------------------------------

output_69() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(file, errhandler, ierr)
  integer, intent(in) :: file
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_errhandler small
output_69 MPI_File_get_errhandler
end MPI_File_get_errhandler

#------------------------------------------------------------------------

output_70() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, group, ierr)
  integer, intent(in) :: fh
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_group small
output_70 MPI_File_get_group
end MPI_File_get_group

#------------------------------------------------------------------------

output_71() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, info_used, ierr)
  integer, intent(in) :: fh
  integer, intent(out) :: info_used
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_info small
output_71 MPI_File_get_info
end MPI_File_get_info

#------------------------------------------------------------------------

output_72() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_position small
output_72 MPI_File_get_position
end MPI_File_get_position

#------------------------------------------------------------------------

output_73() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_position_shared small
output_73 MPI_File_get_position_shared
end MPI_File_get_position_shared

#------------------------------------------------------------------------

output_74() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_size small
output_74 MPI_File_get_size
end MPI_File_get_size

#------------------------------------------------------------------------

output_75() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, datatype, extent, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_type_extent small
output_75 MPI_File_get_type_extent
end MPI_File_get_type_extent

#------------------------------------------------------------------------

output_76() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, disp, etype, filetype, datarep&
        , ierr)
  include 'mpif-config.h'
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
output_76 MPI_File_get_view
end MPI_File_get_view

#------------------------------------------------------------------------

output_83() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, filename, amode, info, fh&
        , ierr)
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
output_83 MPI_File_open
end MPI_File_open

#------------------------------------------------------------------------

output_84() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_preallocate small
output_84 MPI_File_preallocate
end MPI_File_preallocate

#------------------------------------------------------------------------

output_97() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, whence, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_seek small
output_97 MPI_File_seek
end MPI_File_seek

#------------------------------------------------------------------------

output_98() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, whence, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_seek_shared small
output_98 MPI_File_seek_shared
end MPI_File_seek_shared

#------------------------------------------------------------------------

output_99() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, flag, ierr)
  integer, intent(in) :: fh
  logical, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_atomicity small
output_99 MPI_File_set_atomicity
end MPI_File_set_atomicity

#------------------------------------------------------------------------

output_100() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(file, errhandler, ierr)
  integer, intent(in) :: file
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_errhandler small
output_100 MPI_File_set_errhandler
end MPI_File_set_errhandler

#------------------------------------------------------------------------

output_101() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, info, ierr)
  integer, intent(in) :: fh
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_info small
output_101 MPI_File_set_info
end MPI_File_set_info

#------------------------------------------------------------------------

output_102() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_size small
output_102 MPI_File_set_size
end MPI_File_set_size

#------------------------------------------------------------------------

output_103() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, disp, etype, filetype, datarep, &
        info, ierr)
  include 'mpif-config.h'
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
output_103 MPI_File_set_view
end MPI_File_set_view

#------------------------------------------------------------------------

output_104() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, ierr)
  integer, intent(in) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_sync small
output_104 MPI_File_sync
end MPI_File_sync

#------------------------------------------------------------------------

output_117() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ierr)
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Finalize small
output_117 MPI_Finalize
end MPI_Finalize

#------------------------------------------------------------------------

output_118() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Finalized small
output_118 MPI_Finalized
end MPI_Finalized

#------------------------------------------------------------------------

output_124() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_count small
output_124 MPI_Get_count
end MPI_Get_count

#------------------------------------------------------------------------

output_125() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_elements small
output_125 MPI_Get_elements
end MPI_Get_elements

#------------------------------------------------------------------------

output_125_x() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: count
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Get_elements_x small
output_125_x MPI_Get_elements_x
end MPI_Get_elements_x

#------------------------------------------------------------------------

output_126() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(name, resultlen, ierr)
  character(len=*), intent(out) :: name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_processor_name small
output_126 MPI_Get_processor_name
end MPI_Get_processor_name

#------------------------------------------------------------------------

output_127() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(version, subversion, ierr)
  integer, intent(out) :: version
  integer, intent(out) :: subversion
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_version small
output_127 MPI_Get_version
end MPI_Get_version

#------------------------------------------------------------------------

output_128() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_old, nnodes, index, edges, reorder, &
        comm_graph, ierr)
  integer, intent(in) :: comm_old
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_graph
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_create small
output_128 MPI_Graph_create
end MPI_Graph_create

#------------------------------------------------------------------------

output_129() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, maxindex, maxedges, index, edges&
        , ierr)
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
output_129 MPI_Graph_get
end MPI_Graph_get

#------------------------------------------------------------------------

output_130() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, nnodes, index, edges, newrank&
        , ierr)
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
output_130 MPI_Graph_map
end MPI_Graph_map

#------------------------------------------------------------------------

output_131() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, maxneighbors, neighbors, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxneighbors
  integer, dimension(*), intent(out) :: neighbors
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_neighbors small
output_131 MPI_Graph_neighbors
end MPI_Graph_neighbors

#------------------------------------------------------------------------

output_132() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, nneighbors, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(out) :: nneighbors
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_neighbors_count small
output_132 MPI_Graph_neighbors_count
end MPI_Graph_neighbors_count

#------------------------------------------------------------------------

output_133() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, nnodes, nedges, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: nnodes
  integer, intent(out) :: nedges
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graphdims_get small
output_133 MPI_Graphdims_get
end MPI_Graphdims_get

#------------------------------------------------------------------------

output_134() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(in) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Grequest_complete small
output_134 MPI_Grequest_complete
end MPI_Grequest_complete

#------------------------------------------------------------------------

output_135() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(query_fn, free_fn, cancel_fn, extra_state, request&
        , ierr)
  include 'mpif-config.h'
  external :: query_fn
  external :: free_fn
  external :: cancel_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Grequest_start small
output_135 MPI_Grequest_start
end MPI_Grequest_start

#------------------------------------------------------------------------

output_136() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, result, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: result
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_compare small
output_136 MPI_Group_compare
end MPI_Group_compare

#------------------------------------------------------------------------

output_137() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_difference small
output_137 MPI_Group_difference
end MPI_Group_difference

#------------------------------------------------------------------------

output_138() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranks, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_excl small
output_138 MPI_Group_excl
end MPI_Group_excl

#------------------------------------------------------------------------

output_139() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, ierr)
  integer, intent(inout) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_free small
output_139 MPI_Group_free
end MPI_Group_free

#------------------------------------------------------------------------

output_140() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranks, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_incl small
output_140 MPI_Group_incl
end MPI_Group_incl

#------------------------------------------------------------------------

output_141() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_intersection small
output_141 MPI_Group_intersection
end MPI_Group_intersection

#------------------------------------------------------------------------

output_142() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranges, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_range_excl small
output_142 MPI_Group_range_excl
end MPI_Group_range_excl

#------------------------------------------------------------------------

output_143() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranges, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_range_incl small
output_143 MPI_Group_range_incl
end MPI_Group_range_incl

#------------------------------------------------------------------------

output_144() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, rank, ierr)
  integer, intent(in) :: group
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_rank small
output_144 MPI_Group_rank
end MPI_Group_rank

#------------------------------------------------------------------------

output_145() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, size, ierr)
  integer, intent(in) :: group
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_size small
output_145 MPI_Group_size
end MPI_Group_size

#------------------------------------------------------------------------

output_146() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, n, ranks1, group2, ranks2&
        , ierr)
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
output_146 MPI_Group_translate_ranks
end MPI_Group_translate_ranks

#------------------------------------------------------------------------

output_147() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_union small
output_147 MPI_Group_union
end MPI_Group_union

#------------------------------------------------------------------------

output_149() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, ierr)
  integer, intent(out) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_create small
output_149 MPI_Info_create
end MPI_Info_create

#------------------------------------------------------------------------

output_150() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, ierr)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_delete small
output_150 MPI_Info_delete
end MPI_Info_delete

#------------------------------------------------------------------------

output_151() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, newinfo, ierr)
  integer, intent(in) :: info
  integer, intent(out) :: newinfo
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_dup small
output_151 MPI_Info_dup
end MPI_Info_dup

#------------------------------------------------------------------------

output_152() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, ierr)
  integer, intent(inout) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_free small
output_152 MPI_Info_free
end MPI_Info_free

#------------------------------------------------------------------------

output_153() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, valuelen, value, flag&
        , ierr)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(in) :: valuelen
  character(len=*), intent(out) :: value
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get small
output_153 MPI_Info_get
end MPI_Info_get

#------------------------------------------------------------------------

output_154() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, nkeys, ierr)
  integer, intent(in) :: info
  integer, intent(out) :: nkeys
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_nkeys small
output_154 MPI_Info_get_nkeys
end MPI_Info_get_nkeys

#------------------------------------------------------------------------

output_155() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, n, key, ierr)
  integer, intent(in) :: info
  integer, intent(in) :: n
  character(len=*), intent(out) :: key
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_nthkey small
output_155 MPI_Info_get_nthkey
end MPI_Info_get_nthkey

#------------------------------------------------------------------------

output_156() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, valuelen, flag, ierr)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: valuelen
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_valuelen small
output_156 MPI_Info_get_valuelen
end MPI_Info_get_valuelen

#------------------------------------------------------------------------

output_157() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, value, ierr)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  character(len=*), intent(in) :: value
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_set small
output_157 MPI_Info_set
end MPI_Info_set

#------------------------------------------------------------------------

output_158() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ierr)
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Init small
output_158 MPI_Init
end MPI_Init

#------------------------------------------------------------------------

output_159() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(required, provided, ierr)
  integer, intent(in) :: required
  integer, intent(out) :: provided
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Init_thread small
output_159 MPI_Init_thread
end MPI_Init_thread

#------------------------------------------------------------------------

output_160() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Initialized small
output_160 MPI_Initialized
end MPI_Initialized

#------------------------------------------------------------------------

output_161() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(local_comm, local_leader, bridge_comm, remote_leader, tag, &
        newintercomm, ierr)
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
output_161 MPI_Intercomm_create
end MPI_Intercomm_create

#------------------------------------------------------------------------

output_162() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(intercomm, high, newintercomm, ierr)
  integer, intent(in) :: intercomm
  logical, intent(in) :: high
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Intercomm_merge small
output_162 MPI_Intercomm_merge
end MPI_Intercomm_merge

#------------------------------------------------------------------------

output_163() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, flag, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Iprobe small
output_163 MPI_Iprobe
end MPI_Iprobe

#------------------------------------------------------------------------

output_166() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Is_thread_main small
output_166 MPI_Is_thread_main
end MPI_Is_thread_main

#------------------------------------------------------------------------

output_169() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(copy_fn, delete_fn, keyval, extra_state, ierr)
  external :: copy_fn
  external :: delete_fn
  integer, intent(out) :: keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Keyval_create small
output_169 MPI_Keyval_create
end MPI_Keyval_create

#------------------------------------------------------------------------

output_170() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(keyval, ierr)
  integer, intent(inout) :: keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Keyval_free small
output_170 MPI_Keyval_free
end MPI_Keyval_free

#------------------------------------------------------------------------

output_171_commutative() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(op, commute, ierr)
  integer, intent(in) :: op
  logical, intent(out) :: commute
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_commutative small
output_171_commutative MPI_Op_commutative
end MPI_Op_commutative

#------------------------------------------------------------------------

output_171() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, commute, op, ierr)
  external :: function
  logical, intent(in) :: commute
  integer, intent(out) :: op
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_create small
output_171 MPI_Op_create
end MPI_Op_create

#------------------------------------------------------------------------

output_172() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(op, ierr)
  integer, intent(inout) :: op
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_free small
output_172 MPI_Op_free
end MPI_Op_free

#------------------------------------------------------------------------

output_175() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datarep, incount, datatype, size, ierr)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Pack_external_size small
output_175 MPI_Pack_external_size
end MPI_Pack_external_size

#------------------------------------------------------------------------

output_176() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(incount, datatype, comm, size, ierr)
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Pack_size small
output_176 MPI_Pack_size
end MPI_Pack_size

#------------------------------------------------------------------------

output_177() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(level)
  integer, intent(in) :: level

end subroutine ${procedure}

EOF
}

start MPI_Pcontrol small
output_177 MPI_Pcontrol
end MPI_Pcontrol

#------------------------------------------------------------------------

output_178() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Probe small
output_178 MPI_Probe
end MPI_Probe

#------------------------------------------------------------------------

output_180() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(provided, ierr)
  integer, intent(out) :: provided
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Query_thread small
output_180 MPI_Query_thread
end MPI_Query_thread

#------------------------------------------------------------------------

output_185() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state&
        , ierr)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  external :: read_conversion_fn
  external :: write_conversion_fn
  external :: dtype_file_extent_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Register_datarep small
output_185 MPI_Register_datarep
end MPI_Register_datarep

#------------------------------------------------------------------------

output_186() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Request_free small
output_186 MPI_Request_free
end MPI_Request_free

#------------------------------------------------------------------------

output_187() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, flag, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Request_get_status small
output_187 MPI_Request_get_status
end MPI_Request_get_status

#------------------------------------------------------------------------

output_197() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(x, size, ierr)
  ${type}, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Sizeof trivial

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_197 MPI_Sizeof ${rank} CH "character${dim}"
  output_197 MPI_Sizeof ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_197 MPI_Sizeof ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_197 MPI_Sizeof ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_197 MPI_Sizeof ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Sizeof

#------------------------------------------------------------------------

output_200() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Start small
output_200 MPI_Start
end MPI_Start

#------------------------------------------------------------------------

output_201() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, ierr)
  integer, intent(in) :: count
  integer, dimension(*), intent(inout) :: array_of_requests
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Startall small
output_201 MPI_Startall
end MPI_Startall

#------------------------------------------------------------------------

output_202() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, flag, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  logical, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Status_set_cancelled small
output_202 MPI_Status_set_cancelled
end MPI_Status_set_cancelled

#------------------------------------------------------------------------

output_203() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(in) :: datatype
  integer, intent(in) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Status_set_elements small
output_203 MPI_Status_set_elements
end MPI_Status_set_elements

#------------------------------------------------------------------------

output_204() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, flag, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Test small
output_204 MPI_Test
end MPI_Test

#------------------------------------------------------------------------

output_205() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, flag, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Test_cancelled small
output_205 MPI_Test_cancelled
end MPI_Test_cancelled

#------------------------------------------------------------------------

output_206() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, flag, array_of_statuses, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE, count), intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Testall small
output_206 MPI_Testall
end MPI_Testall

#------------------------------------------------------------------------

output_207() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, index, flag, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Testany small
output_207 MPI_Testany
end MPI_Testany

#------------------------------------------------------------------------

output_208() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Testsome small
output_208 MPI_Testsome
end MPI_Testsome

#------------------------------------------------------------------------

output_209() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, status, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Topo_test small
output_209 MPI_Topo_test
end MPI_Topo_test

#------------------------------------------------------------------------

output_210() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, ierr)
  integer, intent(inout) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_commit small
output_210 MPI_Type_commit
end MPI_Type_commit

#------------------------------------------------------------------------

output_211() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, oldtype, newtype, ierr)
  integer, intent(in) :: count
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_contiguous small
output_211 MPI_Type_contiguous
end MPI_Type_contiguous

#------------------------------------------------------------------------

output_212() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(size, rank, ndims, gsize_array, distrib_array, &
        darg_array, psize_array, order, oldtype, newtype, ierr)
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
output_212 MPI_Type_create_darray
end MPI_Type_create_darray

#------------------------------------------------------------------------

output_213() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(p, r, newtype, ierr)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_complex small
output_213 MPI_Type_create_f90_complex
end MPI_Type_create_f90_complex

#------------------------------------------------------------------------

output_214() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(r, newtype, ierr)
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_integer small
output_214 MPI_Type_create_f90_integer
end MPI_Type_create_f90_integer

#------------------------------------------------------------------------

output_215() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(p, r, newtype, ierr)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_real small
output_215 MPI_Type_create_f90_real
end MPI_Type_create_f90_real

#------------------------------------------------------------------------

output_216() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
  include 'mpif-config.h'
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
output_216 MPI_Type_create_hindexed
end MPI_Type_create_hindexed

#------------------------------------------------------------------------

output_217() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
  include 'mpif-config.h'
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
output_217 MPI_Type_create_hvector
end MPI_Type_create_hvector

#------------------------------------------------------------------------

output_218() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierr)
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
output_218 MPI_Type_create_indexed_block
end MPI_Type_create_indexed_block

#------------------------------------------------------------------------

output_219() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierr)
  include 'mpif-config.h'
  external :: type_copy_attr_fn
  external :: type_delete_attr_fn
  integer, intent(out) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_keyval small
output_219 MPI_Type_create_keyval
end MPI_Type_create_keyval

#------------------------------------------------------------------------

output_220() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(oldtype, lb, extent, newtype, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: oldtype
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extent
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_resized small
output_220 MPI_Type_create_resized
end MPI_Type_create_resized

#------------------------------------------------------------------------

output_221() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_block_lengths, array_of_displacements, array_of_types, newtype&
        , ierr)
  include 'mpif-config.h'
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
output_221 MPI_Type_create_struct
end MPI_Type_create_struct

#------------------------------------------------------------------------

output_222() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ndims, size_array, subsize_array, start_array, order, &
        oldtype, newtype, ierr)
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
output_222 MPI_Type_create_subarray
end MPI_Type_create_subarray

#------------------------------------------------------------------------

output_223() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, ierr)
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_delete_attr small
output_223 MPI_Type_delete_attr
end MPI_Type_delete_attr

#------------------------------------------------------------------------

output_224() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, newtype, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_dup small
output_224 MPI_Type_dup
end MPI_Type_dup

#------------------------------------------------------------------------

output_225() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, extent, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_extent small
output_225 MPI_Type_extent
end MPI_Type_extent

#------------------------------------------------------------------------

output_226() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, ierr)
  integer, intent(inout) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_free small
output_226 MPI_Type_free
end MPI_Type_free

#------------------------------------------------------------------------

output_227() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type_keyval, ierr)
  integer, intent(inout) :: type_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_free_keyval small
output_227 MPI_Type_free_keyval
end MPI_Type_free_keyval

#------------------------------------------------------------------------

output_228() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, attribute_val, flag, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_attr small
output_228 MPI_Type_get_attr
end MPI_Type_get_attr

#------------------------------------------------------------------------

output_229() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(mtype, max_integers, max_addresses, max_datatypes, array_of_integers, &
        array_of_addresses, array_of_datatypes, ierr)
  include 'mpif-config.h'
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
output_229 MPI_Type_get_contents
end MPI_Type_get_contents

#------------------------------------------------------------------------

output_230() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, num_integers, num_addresses, num_datatypes, combiner&
        , ierr)
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
output_230 MPI_Type_get_envelope
end MPI_Type_get_envelope

#------------------------------------------------------------------------

output_231() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, lb, extent, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_extent small
output_231 MPI_Type_get_extent
end MPI_Type_get_extent

#------------------------------------------------------------------------

output_231_x() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, lb, extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer(kind=MPI_COUNT_KIND), intent(out) :: lb
  integer(kind=MPI_COUNT_KIND), intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Type_get_extent_x small
output_231_x MPI_Type_get_extent_x
end MPI_Type_get_extent_x

#------------------------------------------------------------------------

output_232() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_name, resultlen, ierr)
  integer, intent(in) :: type
  character(len=*), intent(out) :: type_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_name small
output_232 MPI_Type_get_name
end MPI_Type_get_name

#------------------------------------------------------------------------

output_233() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datatype, true_lb, true_extent, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_true_extent small
output_233 MPI_Type_get_true_extent
end MPI_Type_get_true_extent

#------------------------------------------------------------------------

output_233_x() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datatype, true_lb, true_extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: true_lb
  integer(kind=MPI_COUNT_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Type_get_true_extent_x small
output_233_x MPI_Type_get_true_extent_x
end MPI_Type_get_true_extent_x

#------------------------------------------------------------------------

output_234() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
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
output_234 MPI_Type_hindexed
end MPI_Type_hindexed

#------------------------------------------------------------------------

output_235() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
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
output_235 MPI_Type_hvector
end MPI_Type_hvector

#------------------------------------------------------------------------

output_236() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
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
output_236 MPI_Type_indexed
end MPI_Type_indexed

#------------------------------------------------------------------------

output_237() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, lb, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: lb
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_lb small
output_237 MPI_Type_lb
end MPI_Type_lb

#------------------------------------------------------------------------

output_238() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(typeclass, size, type, ierr)
  integer, intent(in) :: typeclass
  integer, intent(in) :: size
  integer, intent(out) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_match_size small
output_238 MPI_Type_match_size
end MPI_Type_match_size

#------------------------------------------------------------------------

output_239() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, attr_val, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attr_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_set_attr small
output_239 MPI_Type_set_attr
end MPI_Type_set_attr

#------------------------------------------------------------------------

output_240() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_name, ierr)
  integer, intent(in) :: type
  character(len=*), intent(in) :: type_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_set_name small
output_240 MPI_Type_set_name
end MPI_Type_set_name

#------------------------------------------------------------------------

output_241() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, size, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_size small
output_241 MPI_Type_size
end MPI_Type_size

#------------------------------------------------------------------------

output_241_x() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, size, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer(kind=MPI_COUNT_KIND), intent(out) :: size
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Type_size_x small
output_241_x MPI_Type_size_x
end MPI_Type_size_x

#------------------------------------------------------------------------

output_242() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype&
        , ierr)
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
output_242 MPI_Type_struct
end MPI_Type_struct

#------------------------------------------------------------------------

output_243() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(mtype, ub, ierr)
  integer, intent(in) :: mtype
  integer, intent(out) :: ub
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_ub small
output_243 MPI_Type_ub
end MPI_Type_ub

#------------------------------------------------------------------------

output_244() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
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
output_244 MPI_Type_vector
end MPI_Type_vector

#------------------------------------------------------------------------

output_247() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Wait small
output_247 MPI_Wait
end MPI_Wait

#------------------------------------------------------------------------

output_248() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, array_of_statuses, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Waitall small
output_248 MPI_Waitall
end MPI_Waitall

#------------------------------------------------------------------------

output_249() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, index, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Waitany small
output_249 MPI_Waitany
end MPI_Waitany

#------------------------------------------------------------------------

output_250() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Waitsome small
output_250 MPI_Waitsome
end MPI_Waitsome

#------------------------------------------------------------------------

output_251() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errorcode, ierr)
  integer, intent(in) :: win
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_call_errhandler small
output_251 MPI_Win_call_errhandler
end MPI_Win_call_errhandler

#------------------------------------------------------------------------

output_252() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_complete small
output_252 MPI_Win_complete
end MPI_Win_complete

#------------------------------------------------------------------------

output_254() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_create_errhandler small
output_254 MPI_Win_create_errhandler
end MPI_Win_create_errhandler

#------------------------------------------------------------------------

output_255() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierr)
  include 'mpif-config.h'
  external :: win_copy_attr_fn
  external :: win_delete_attr_fn
  integer, intent(out) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_create_keyval small
output_255 MPI_Win_create_keyval
end MPI_Win_create_keyval

#------------------------------------------------------------------------

output_256() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, ierr)
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_delete_attr small
output_256 MPI_Win_delete_attr
end MPI_Win_delete_attr

#------------------------------------------------------------------------

output_257() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(assert, win, ierr)
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_fence small
output_257 MPI_Win_fence
end MPI_Win_fence

#------------------------------------------------------------------------

output_258() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  integer, intent(inout) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_free small
output_258 MPI_Win_free
end MPI_Win_free

#------------------------------------------------------------------------

output_259() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win_keyval, ierr)
  integer, intent(inout) :: win_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_free_keyval small
output_259 MPI_Win_free_keyval
end MPI_Win_free_keyval

#------------------------------------------------------------------------

output_260() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, attribute_val, flag, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_attr small
output_260 MPI_Win_get_attr
end MPI_Win_get_attr

#------------------------------------------------------------------------

output_261() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errhandler, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_errhandler small
output_261 MPI_Win_get_errhandler
end MPI_Win_get_errhandler

#------------------------------------------------------------------------

output_262() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, group, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_group small
output_262 MPI_Win_get_group
end MPI_Win_get_group

#------------------------------------------------------------------------

output_263() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_name, resultlen, ierr)
  integer, intent(in) :: win
  character(len=*), intent(out) :: win_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_name small
output_263 MPI_Win_get_name
end MPI_Win_get_name

#------------------------------------------------------------------------

output_264() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(lock_type, rank, assert, win, ierr)
  integer, intent(in) :: lock_type
  integer, intent(in) :: rank
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_lock small
output_264 MPI_Win_lock
end MPI_Win_lock

#------------------------------------------------------------------------

output_265() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, assert, win, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_post small
output_265 MPI_Win_post
end MPI_Win_post

#------------------------------------------------------------------------

output_266() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, attribute_val, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_attr small
output_266 MPI_Win_set_attr
end MPI_Win_set_attr

#------------------------------------------------------------------------

output_267() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errhandler, ierr)
  integer, intent(in) :: win
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_errhandler small
output_267 MPI_Win_set_errhandler
end MPI_Win_set_errhandler

#------------------------------------------------------------------------

output_268() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_name, ierr)
  integer, intent(in) :: win
  character(len=*), intent(in) :: win_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_name small
output_268 MPI_Win_set_name
end MPI_Win_set_name

#------------------------------------------------------------------------

output_269() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, assert, win, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_start small
output_269 MPI_Win_start
end MPI_Win_start

#------------------------------------------------------------------------

output_270() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, flag, ierr)
  integer, intent(in) :: win
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_test small
output_270 MPI_Win_test
end MPI_Win_test

#------------------------------------------------------------------------

output_271() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(rank, win, ierr)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_unlock small
output_271 MPI_Win_unlock
end MPI_Win_unlock

#------------------------------------------------------------------------

output_272() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_wait small
output_272 MPI_Win_wait
end MPI_Win_wait

#------------------------------------------------------------------------

output_273() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, ierr)
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Close_port small
output_273 MPI_Close_port
end MPI_Close_port

#------------------------------------------------------------------------

output_274() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Lookup_name small
output_274 MPI_Lookup_name
end MPI_Lookup_name

#------------------------------------------------------------------------

output_275() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, port_name, ierr)
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Open_port small
output_275 MPI_Open_port
end MPI_Open_port

#------------------------------------------------------------------------

output_276() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Publish_name small
output_276 MPI_Publish_name
end MPI_Publish_name

#------------------------------------------------------------------------

output_277() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Unpublish_name small
output_277 MPI_Unpublish_name
end MPI_Unpublish_name

#------------------------------------------------------------------------

output_278() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_disconnect small
output_278 MPI_Comm_disconnect
end MPI_Comm_disconnect

#------------------------------------------------------------------------

output_279() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(parent, ierr)
  integer, intent(out) :: parent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_parent small
output_279 MPI_Comm_get_parent
end MPI_Comm_get_parent

#------------------------------------------------------------------------

output_280() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fd, intercomm, ierr)
  integer, intent(in) :: fd
  integer, intent(out) :: intercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_join small
output_280 MPI_Comm_join
end MPI_Comm_join

#------------------------------------------------------------------------

output_281() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, info, root, comm, newcomm&
        , ierr)
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
output_281 MPI_Comm_accept
end MPI_Comm_accept

#------------------------------------------------------------------------

output_282() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, info, root, comm, newcomm&
        , ierr)
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
output_282 MPI_Comm_connect
end MPI_Comm_connect

#------------------------------------------------------------------------

output_283() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(command, argv, maxprocs, info, root, &
        comm, intercomm, array_of_errcodes, ierr)
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
output_283 MPI_Comm_spawn
end MPI_Comm_spawn

#------------------------------------------------------------------------

output_284() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierr)
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
end subroutine ${procedure}

EOF
}

start MPI_Comm_spawn_multiple small
output_284 MPI_Comm_spawn_multiple
end MPI_Comm_spawn_multiple

#------------------------------------------------------------------------

output_285() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, message, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: message
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Mprobe small
output_285 MPI_Mprobe
end MPI_Mprobe

#------------------------------------------------------------------------

output_286() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, flag, message, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, intent(out) :: message
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Improbe small
output_286 MPI_Improbe
end MPI_Improbe

#------------------------------------------------------------------------

output_289() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(version, resultlen, ierr)
  character(len=*), intent(out) :: version
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_library_version small
output_289 MPI_Get_library_version
end MPI_Get_library_version

#------------------------------------------------------------------------

output_290() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, split_type, key, info, newcomm, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: split_type
  integer, intent(in) :: key
  integer, intent(in) :: info
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_split_type small
output_290 MPI_Comm_split_type
end MPI_Comm_split_type

#------------------------------------------------------------------------

output_291() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_hindexed_block small
output_291 MPI_Type_create_hindexed_block
end MPI_Type_create_hindexed_block


#------------------------------------------------------------------------

output_292() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_old, n, sources, degrees, destinations, &
        weights, info, reorder, comm_dist_graph, ierror)
  integer, intent(in) :: comm_old
  integer, intent(in) :: n
  integer, dimension(n), intent(in) :: sources
  integer, dimension(n), intent(in) :: degrees
  integer, dimension(n), intent(in) :: destinations
  integer, dimension(n), intent(in) :: weights
  logical, intent(in) :: info
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_dist_graph
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Dist_graph_create small
output_292 MPI_Dist_graph_create
end MPI_Dist_graph_create

#------------------------------------------------------------------------

output_293() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_old, indegree, sources, sourceweights, &
       outdegree, destinations, destweights, info, reorder, &
       comm_dist_graph, ierror)
  integer, intent(in) :: comm_old
  integer, intent(in) :: indegree
  integer, dimension(indegree), intent(in) :: sources
  integer, dimension(indegree), intent(in) :: sourceweights
  integer, intent(in) :: outdegree
  integer, dimension(outdegree), intent(in) :: destinations
  integer, dimension(outdegree), intent(in) :: destweights
  logical, intent(in) :: info
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_dist_graph
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Dist_graph_create_adjacent small
output_293 MPI_Dist_graph_create_adjacent
end MPI_Dist_graph_create_adjacent

#------------------------------------------------------------------------

output_294() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, indegree, outdegree, weighted, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: indegree
  integer, intent(out) :: outdegree
  logical, intent(out) :: weighted
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Dist_graph_neighbors_count small
output_294 MPI_Dist_graph_neighbors_count
end MPI_Dist_graph_neighbors_count

#------------------------------------------------------------------------

output_295() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, maxindegree, sources, sourceweights, &
       maxoutdegree, destinations, destweights, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: maxindegree
  integer, dimension(maxindegree), intent(out) :: sources
  integer, dimension(maxindegree), intent(out) :: sourceweights
  integer, intent(in) :: maxoutdegree
  integer, dimension(maxoutdegree), intent(out) :: destinations
  integer, dimension(maxoutdegree), intent(out) :: destweights
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Dist_graph_neighbors small
output_295 MPI_Dist_graph_neighbors
end MPI_Dist_graph_neighbors

#------------------------------------------------------------------------

output_296() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Win_flush small
output_296 MPI_Win_flush
end MPI_Win_flush

#------------------------------------------------------------------------

output_297() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Win_flush_all small
output_297 MPI_Win_flush_all
end MPI_Win_flush_all

#------------------------------------------------------------------------

output_298() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Win_flush_local small
output_298 MPI_Win_flush_local
end MPI_Win_flush_local

#------------------------------------------------------------------------

output_299() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine ${procedure}

EOF
}

start MPI_Win_flush_local_all small
output_299 MPI_Win_flush_local_all
end MPI_Win_flush_local_all
