# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OMPI_FIND_MPI_AINT_COUNT_OFFSET],[
    _OMPI_FIND_MPI_AINT_TYPE
    _OMPI_FIND_MPI_COUNT_TYPE
    _OMPI_FIND_MPI_OFFSET_TYPE

    if test "$ompi_fortran_happy" == "1" && \
       test "$OMPI_WANT_FORTRAN_BINDINGS" == "1"; then
        _OMPI_FIND_MPI_INTEGER_KIND
        _OMPI_FIND_MPI_ADDRESS_KIND
        _OMPI_FIND_MPI_COUNT_KIND
        _OMPI_FIND_MPI_OFFSET_KIND
    fi
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_AINT_TYPE], [
    # Find the type of MPI_Aint.  We already did the work to figure
    # out what opal_ptrdiff will be; just use that.
    AC_MSG_CHECKING([for type of MPI_Aint])

    MPI_AINT_TYPE=$opal_ptrdiff_t
    # Get the size of this type; we'll need it to figure out Fortran's
    # MPI_ADDRESS_KIND, later
    MPI_AINT_SIZE=$opal_ptrdiff_size
    AC_DEFINE_UNQUOTED(OMPI_MPI_AINT_TYPE, [$MPI_AINT_TYPE],
                       [Type of MPI_Aint])

    AC_MSG_RESULT([$MPI_AINT_TYPE (size: $MPI_AINT_SIZE)])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_COUNT_TYPE], [
    # Find the type of MPI_Count.  Per MPI-3.0 2.5.8, it needs to be
    # as least as large as an int, MPI_Aint, and MPI_Offset (we'll
    # define MPI_Offset later to be the same as an MPI_Count).

    # Note, however, that per
    # https://svn.open-mpi.org/trac/ompi/ticket/4205, MPI_Count cannot
    # be larger than a size_t (e.g., if you're compiling in 32 bit
    # more on a 64 bit platform, long long will be 8 bytes, but size_t
    # will only be 4 bytes).  The entire BTL, PML, and convertor
    # infrastructure rely on sizeof(MPI_Count) being <=
    # sizeof(size_t).  So artificially limit the size of MPI_Count, if
    # necessary.

    # Also note: do not search for int64_t or int32_t, because using
    # these types mean that we would need to include <stdint.h> in
    # mpi.h, which we probably shouldn't do.

    # Finally, note that MPI_Count has an impact on the extent of a
    # datatype, extent defined by the MPI standard as an MPI_Aint.
    # This MPI_Aint is also the largest different between two memory
    # pointers -- the well-known ptrdiff_t.  There *are* systems where
    # the address space is 32 bit but the filesystem space is 64 bit
    # (e.g., BlueGene), and therefore MPI_Aint is 32 bit and
    # MPI_Offset (and therefore MPI_Count) is 64 bit.  Open MPI
    # doesn't currently support this configuration -- re-tooling in
    # the convertor/PML/BML/BTL will be necessary before that can work
    # properly.

    # Keep in mind, however, that while ptrdiff_t and size_t represent
    # similar concepts (length or displacement in memory), one is
    # slightly larger than the other (one is unsigned and the other
    # signed) and there is no requirement for them to be of the same
    # width.  On systems with non-monolithic memory space, the scheme
    # we use below may not work.  On systems with non-monolithic
    # memory space, the scheme we use below may not work.  ...but such
    # systems are pretty rare today.

    MPI_COUNT_TYPE=unknonwn
    AC_MSG_CHECKING([for type of MPI_Count])
    if test $ac_cv_type_long_long = yes && \
       test $ac_cv_sizeof_long_long -le $ac_cv_sizeof_size_t && \
       test $ac_cv_sizeof_long_long -ge $MPI_AINT_SIZE; then
        MPI_COUNT_TYPE="long long"
        MPI_COUNT_DATATYPE=MPI_LONG_LONG
        MPI_COUNT_SIZE=$ac_cv_sizeof_long_long
    elif test $ac_cv_sizeof_long -le $ac_cv_sizeof_size_t && \
         test $ac_cv_sizeof_long -ge $MPI_AINT_SIZE; then
        MPI_COUNT_TYPE=long
        MPI_COUNT_DATATYPE=MPI_LONG
        MPI_COUNT_SIZE=$ac_cv_sizeof_long
    elif test $ac_cv_sizeof_int -le $ac_cv_sizeof_size_t && \
         test $ac_cv_sizeof_int -ge $MPI_AINT_SIZE; then
        MPI_COUNT_TYPE=int
        MPI_COUNT_DATATYPE=MPI_INT
        MPI_COUNT_SIZE=$ac_cv_sizeof_int
    fi

    if test "$MPI_COUNT_TYPE" = "unknown"; then
        AC_MSG_RESULT([not found])
        AC_MSG_WARN([*** Unable to find a good type for MPI_Count])
        AC_MSG_ERROR([Cannot continue])
    fi

    if test $MPI_COUNT_SIZE -eq 8 ; then
        MPI_COUNT_MAX="0x7fffffffffffffffll"
    elif test $MPI_COUNT_SIZE -eq 4 ; then
        MPI_COUNT_MAX="0x7fffffffl"
    elif test $MPI_COUNT_SIZE -eq 2 ; then
        MPI_COUNT_MAX="0x7fff"
    else
        AC_MSG_RESULT([$MPI_COUNT_TYPE (size: $MPI_COUNT_SIZE)])
        AC_MSG_WARN([*** Configure cannot handle this size -- contact Open MPI developers])
        AC_MSG_ERROR([Cannot continue])
    fi

    AC_DEFINE_UNQUOTED(OMPI_MPI_COUNT_SIZE, $MPI_COUNT_SIZE,
                       [Size of the MPI_Count datatype])
    AC_DEFINE_UNQUOTED(OMPI_MPI_COUNT_TYPE, $MPI_COUNT_TYPE,
                       [Type of the MPI_Count datatype])
    AC_DEFINE_UNQUOTED(MPI_COUNT_MAX, $MPI_COUNT_MAX,
                       [Maximum value for an MPI_Count])

    AC_MSG_RESULT([$MPI_COUNT_TYPE (size: $MPI_COUNT_SIZE)])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_OFFSET_TYPE], [
    # Just make MPI_Offset be the same as MPI_Count
    AC_MSG_CHECKING([for type of MPI_Offset])

    MPI_OFFSET_TYPE=$MPI_COUNT_TYPE
    AC_DEFINE_UNQUOTED(OMPI_MPI_OFFSET_TYPE, $MPI_OFFSET_TYPE,
                       [Type of MPI_Offset])
    MPI_OFFSET_SIZE=$MPI_COUNT_SIZE
    AC_DEFINE_UNQUOTED(OMPI_MPI_OFFSET_SIZE, $MPI_OFFSET_SIZE,
                       [Size of the MPI_Offset])
    AC_MSG_RESULT([$MPI_COUNT_TYPE (size: $MPI_OFFSET_SIZE)])

    AC_MSG_CHECKING([for an MPI datatype for MPI_Offset])
    MPI_OFFSET_DATATYPE=$MPI_COUNT_DATATYPE
    AC_DEFINE_UNQUOTED(OMPI_OFFSET_DATATYPE, $MPI_OFFSET_DATATYPE,
                       [MPI datatype corresponding to MPI_Offset])
    AC_MSG_RESULT([$MPI_OFFSET_DATATYPE])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_INTEGER_KIND], [
    # Get the kind value for Fortran MPI_INTEGER_KIND (corresponding
    # to whatever is the same size as a F77 INTEGER -- for the
    # most-likely-will-never-occur case where F77 INTEGER is smaller
    # than an F90 INTEGER; see MPI-2 4.12.6.5.  As with OMPI
    # FORTRAN_CHECK, use the official BIND(C) KIND names (see comment
    # in fortran_check.m4).
    AC_MSG_CHECKING([for MPI_INTEGER_KIND])
    if test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 2; then
        OMPI_MPI_INTEGER_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 4; then
        OMPI_MPI_INTEGER_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 8; then
        OMPI_MPI_INTEGER_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 16; then
        AC_MSG_ERROR([Cannot support Fortran MPI_INTEGER_KIND!])
    fi
    AC_SUBST(OMPI_MPI_INTEGER_KIND)
    AC_MSG_RESULT([$OMPI_MPI_INTEGER_KIND])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_ADDRESS_KIND], [
    # Get the kind value for Fortran MPI_ADDRESS_KIND (corresponding
    # to whatever is big enough to hold MPI_Aint).
    AC_MSG_CHECKING([for MPI_ADDRESS_KIND])
    if test $MPI_AINT_SIZE -eq 2 ; then
        OMPI_MPI_ADDRESS_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $MPI_AINT_SIZE -eq 4 ; then
        OMPI_MPI_ADDRESS_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $MPI_AINT_SIZE -eq 8 ; then
        OMPI_MPI_ADDRESS_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $MPI_AINT_SIZE -eq 16 ; then
        AC_MSG_ERROR([Cannot support Fortran MPI_ADDRESS_KIND!])
    fi
    AC_SUBST(OMPI_MPI_ADDRESS_KIND)
    AC_MSG_RESULT([$OMPI_MPI_ADDRESS_KIND])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_COUNT_KIND], [
    # Get the kind value for Fortran MPI_COUNT_KIND (corresponding to
    # whatever is big enough to hold an MPI_Count)
    AC_MSG_CHECKING([for MPI_COUNT_KIND])
    if test $MPI_COUNT_SIZE -eq 2 ; then
        OMPI_MPI_COUNT_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $MPI_COUNT_SIZE -eq 4 ; then
        OMPI_MPI_COUNT_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $MPI_COUNT_SIZE -eq 8 ; then
        OMPI_MPI_COUNT_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $MPI_COUNT_SIZE -eq 16 ; then
        AC_MSG_ERROR([Cannot support Fortran MPI_COUNT_KIND!])
    fi
    AC_SUBST(OMPI_MPI_COUNT_KIND)
    AC_MSG_RESULT([$OMPI_MPI_COUNT_KIND])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_OFFSET_KIND], [
    # Get the kind value for Fortran MPI_OFFSET_KIND (corresponding to
    # whatever is big enough to hold an MPI_Offset)
    AC_MSG_CHECKING([for MPI_OFFSET_KIND])
    if test $MPI_OFFSET_SIZE -eq 2 ; then
        OMPI_MPI_OFFSET_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $MPI_OFFSET_SIZE -eq 4 ; then
        OMPI_MPI_OFFSET_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $MPI_OFFSET_SIZE -eq 8 ; then
        OMPI_MPI_OFFSET_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $MPI_OFFSET_SIZE -eq 16 ; then
        AC_MSG_ERROR([Cannot support Fortran MPI_OFFSET_KIND!])
    fi
    AC_SUBST(OMPI_MPI_OFFSET_KIND)
    AC_MSG_RESULT([$OMPI_MPI_OFFSET_KIND])
])
